# ==============================================================================
# THESIS CODE
# Project: Undergraduate Thesis - "From Intention to Birth. On the Impact of Socio-
# economic Factors on the Probability of Realizing Fertility Intentions"
# Description: This script cleans survey data, performs regression modeling (Logit),
#              evaluates model performance, and generates tables for the thesis.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. LOAD LIBRARIES
# ------------------------------------------------------------------------------
library(readstata13)
library(dplyr)
library(tidyr)
library(stargazer)
library(car)
library(lmtest)
library(sandwich)
library(caret)
library(kableExtra)
library(corrplot)
library(flextable)
library(modelsummary)
library(marginaleffects)
library(pROC)
library(DescTools)
library(ResourceSelection)
library(performance)

# ------------------------------------------------------------------------------
# 2. HELPER FUNCTIONS
# ------------------------------------------------------------------------------

# Backward elimination function based on p-value
backward_elimination <- function(model, threshold = 0.05) {
    current_model <- model
    repeat {
        coeffs <- summary(current_model)$coefficients
        p_values <- coeffs[-1, 4, drop = FALSE]
        
        if (nrow(p_values) == 0) {
            cat("All explanatory variables removed.\n")
            break
        }
        
        max_p <- max(p_values)
        if (max_p > threshold) {
            var_to_remove <- rownames(p_values)[which.max(p_values)]
            cat("Removing:", var_to_remove, "| p-value =", round(max_p, 4), "\n")
            new_formula <- update(formula(current_model), paste(". ~ . -", var_to_remove))
            current_model <- update(current_model, formula = new_formula)
        } else {
            break
        }
    }
    cat("\n--- Elimination Complete ---\n")
    return(current_model)
}

# Function to build and compare models (AIC/BIC)
find_best_model <- function(dane, dependent_var, independent_vars, continuous_vars = NULL) {
    base_formula <- as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + ")))
    base_model <- glm(base_formula, data = dane, family = binomial(link = "logit"))
    
    interaction_part <- paste0("(", paste(independent_vars, collapse = " + "), ")^2")
    squared_part <- ""
    if (!is.null(continuous_vars)) {
        squared_part <- paste0(" + ", paste0("I(", continuous_vars, "^2)", collapse = " + "))
    }
    
    full_formula <- as.formula(paste(dependent_var, "~", interaction_part, squared_part))
    
    cat("Starting model search (Stepwise)...\n")
    model_aic <- step(base_model, scope = list(lower = base_formula, upper = full_formula), direction = "both", trace = 0, k = 2)
    
    n_obs <- nrow(base_model$model)
    model_bic <- step(base_model, scope = list(lower = base_formula, upper = full_formula), direction = "both", trace = 0, k = log(n_obs))
    
    return(list(model_aic = model_aic, model_bic = model_bic))
}

# ------------------------------------------------------------------------------
# 3. DATA LOADING AND PROCESSING
# ------------------------------------------------------------------------------

# Load raw datasets
wave_1 <- readstata13::read.dta13("Poland_wave1.dta")
wave_2 <- readstata13::read.dta13("Poland_wave2.dta")
earnings <- readstata13::read.dta13("Earnings_wave1.dta")

# Select and rename variables
wave_2_sel <- wave_2 %>%
    select(brid, bage, bnumbiol, b622) %>%
    rename(id = brid, age_w2 = bage, fertility_w2 = bnumbiol, fert_int_w2 = b622)

wave_1_sel <- wave_1 %>%
    select(arid, aage, asex, numbiol, amarstat, aparstat, a302bAgeR,
           ageoldest, aeduc, a150AgeR, aactstat, a381, a122, a145,
           a402, a202, a628_a, a628_b, a628_c, a627_a, a627_b,
           a627_c, a622, a624, a626, a630, a5113, a1102mnth, aplace,
           a204a, a203a, a407, ageyoungest) %>%
    rename(
        id = arid, age = aage, sex = asex, fertility = numbiol, married = amarstat, par_stat = aparstat,
        age_at_marriage = a302bAgeR, age_oldest = ageoldest, education = aeduc,
        education_age = a150AgeR, emp_status = aactstat, part_emp_status = a381,
        ownership_status = a122, ownership_sat = a145, housework_sat = a402, childcare_sat = a202,
        child_finance = a628_a, child_work = a628_b, child_housing = a628_c,
        eff_freedom = a627_a, eff_employment = a627_b, eff_finance = a627_c,
        fert_intentions_y = a622, fert_inentions_atall = a624,
        more_children = a626, intend_children = a630, fath_educ = a5113, rel = a1102mnth, place = aplace,
        pomoc_r = a204a, pomoc_inst = a203a, sat_relacja = a407, wiek_najmlod = ageyoungest
    )

dane_par <- wave_1 %>% select(arid, a370) %>% rename(id = arid, partner = a370)
dane_par$id <- dane_par$id + 0.26

# Merge and basic cleaning
DANE <- inner_join(wave_1_sel, wave_2_sel, by = "id") %>%
    mutate(
        child_definitely_not = ifelse(fert_intentions_y == "definitely not", 1, 0),
        child_probably_not = ifelse(fert_intentions_y == "probably not", 1, 0),
        child_probably_yes = ifelse(fert_intentions_y == "probably yes", 1, 0),
        child_definitely_yes = ifelse(fert_intentions_y == "definitely yes", 1, 0),
        fertility_gap = fertility_w2 - fertility,
        success = ifelse(fert_intentions_y %in% c("definitely yes", "probably yes") & fertility_gap > 0, 1, 0),
        fert_int_w2 = if_else(fertility_gap > 0, "child born", as.character(fert_int_w2))
    ) %>%
    filter(!is.na(fert_intentions_y) & !is.na(fert_int_w2) & !is.na(fertility_gap))

# Handle conditional logic for survey variables
DANE <- DANE %>%
    mutate(
        age_at_marriage = ifelse(married != "married", 0, age_at_marriage),
        age_oldest = ifelse(fertility == 0, 0, age_oldest),
        childcare_sat = ifelse(fertility == 0 | par_stat != "co-resident partner", 0, childcare_sat),
        part_emp_status = ifelse(par_stat != "co-resident partner", 0, part_emp_status),
        housework_sat = ifelse(par_stat != "co-resident partner", 0, housework_sat),
        sat_relacja = ifelse(par_stat != "co-resident partner", 0, sat_relacja),
        pomoc_r = ifelse(fertility == 0, 0, pomoc_r),
        pomoc_inst = ifelse(fertility == 0, 0, pomoc_inst),
        wiek_najmlod = ifelse(fertility == 0, 0, wiek_najmlod)
    )

# Merge Earnings
DANE$id <- DANE$id + 0.26
DATA_joined <- inner_join(DANE, earnings, by = "id")

DF_done <- DATA_joined %>%
    rename(log_earnings = `_5_log_earnings_imputed`, par_log_earnings = `_5_p_log_earnings_imputed`, eduk_ojca = fath_educ, religijnosc = rel) %>%
    mutate(par_log_earnings = ifelse(par_stat == "no partner", 0, par_log_earnings),
           earnings = exp(log_earnings)) %>%
    filter(child_definitely_not == 0 & child_probably_not == 0)

DF_done <- inner_join(DF_done, dane_par, by = "id")

# Final Feature Engineering
df_p <- DF_done %>%
    mutate(
        kobieta = ifelse(sex == "female", 1, 0),
        po_slubie = ifelse(married == "married", 1, 0),
        eduk_w = ifelse(education %in% c("isced 5 - first stage of tertiary", "isced 6 - second stage of tertiary"), 1, 0),
        n_dzieci = fertility,
        zatrudniony = ifelse(emp_status == "employed or self-employed", 1, 0),
        mieszkanie = ifelse(ownership_status == "owner_(1)", 1, 0),
        wiek_pier_dziecko = age - age_oldest,
        eduk_w_ojca = ifelse(eduk_ojca %in% c("isced 5 - first stage of tertiary", "isced 6 - second stage of tertiary"), 1, 0),
        zdecydowanie_tak = child_definitely_yes,
        realizacja = success,
        ifpartner = ifelse(partner == "no partner", 0, 1),
        place = case_match(place,
                           "city with 500 thousands inhabitants and more" ~ "(6)_miasto > 500tys",
                           "city with 200 to 499 thousands inhabitants" ~ "(5)_miasto 200-499tys",
                           "city with 100 - 199 thousands inhabitants and more" ~ "(4)_miasto 100-199tys",
                           "city with 20 - 99 thousands inhabitants and more" ~ "(3)_miasto 20-99tys",
                           "city with less than 20 thousands inhabitants" ~ "(2)_miasto < 20tys",
                           "village" ~ "(1)_wies"
        ),
        experience = age - education_age,
        bezdzietny = ifelse(n_dzieci == 0, 1, 0),
        dochod = exp(log_earnings + 1),
        dochod_part = exp(par_log_earnings + 1),
        wsp_dochod = dochod + dochod_part,
        wsp_log_dochod = log(wsp_dochod),
        lata_po_slubie = ifelse(po_slubie == 1, age - age_at_marriage, 0),
        wies = ifelse(place == "(1)_wies", 1, 0),
        bardzo_male_miasto = ifelse(place == "(2)_miasto < 20tys", 1, 0),
        male_miasto = ifelse(place == "(3)_miasto 20-99tys", 1, 0),
        srednie_miasto = ifelse(place == "(4)_miasto 100-199tys", 1, 0),
        duze_miasto = ifelse(place == "(5)_miasto 200-499tys", 1, 0),
        metropolia = ifelse(place == "(6)_miasto > 500tys", 1, 0),
        kwartyl_dochodu = ntile(wsp_dochod, 4),
        kwartyl_drugi = ifelse(kwartyl_dochodu == 2, 1, 0),
        kwartyl_trzeci = ifelse(kwartyl_dochodu == 3, 1, 0),
        kwartyl_czwarty = ifelse(kwartyl_dochodu == 4, 1, 0)
    ) %>%
    filter(ifpartner > 0) %>%
    na.omit()

df_p$wiek2 <- df_p$age^2
df_p$lata_po_slubie2 <- df_p$lata_po_slubie^2

# ------------------------------------------------------------------------------
# 4. MODELING
# ------------------------------------------------------------------------------

# Final logistic regression model
model_final <- glm(
    realizacja ~ age + wiek2 + kobieta + experience + po_slubie + lata_po_slubie + 
                 lata_po_slubie2 + zatrudniony + zdecydowanie_tak + bardzo_male_miasto + 
                 male_miasto + srednie_miasto + duze_miasto + metropolia + 
                 kwartyl_drugi + kwartyl_trzeci + kwartyl_czwarty, 
    data = df_p, 
    family = binomial(link = "logit")
)

summary(model_final)

# ------------------------------------------------------------------------------
# 5. DIAGNOSTICS & EVALUATION
# ------------------------------------------------------------------------------

# Check Multicollinearity
vif(model_final)

# Performance table
compare_performance(model_final)

# Cross-Validation setup
ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)
df_p$realizacja_factor <- factor(df_p$realizacja, levels = c(0, 1), labels = c("No", "Yes"))

model_cv <- train(
    realizacja_factor ~ age + wiek2 + kobieta + experience + po_slubie + lata_po_slubie + lata_po_slubie2 + zatrudniony + 
    zdecydowanie_tak + bardzo_male_miasto + male_miasto + srednie_miasto + duze_miasto + metropolia + kwartyl_drugi + kwartyl_trzeci + kwartyl_czwarty,
    data = df_p,
    method = "glm",
    family = binomial(link = "logit"),
    trControl = ctrl,
    metric = "ROC"
)

print(model_cv)

# ------------------------------------------------------------------------------
# 6. OUTPUT & TABLES
# ------------------------------------------------------------------------------

# Export model results to HTML
stargazer(model_final, 
          type = "html", 
          dep.var.labels = "Realization of Intentions (1 = Success)",
          out = "final_model_results.html")

# Create AME table
efekty_ame <- avg_slopes(model_final)
print(summary(efekty_ame))
