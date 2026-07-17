# Bachelor-Thesis
This respository includes Bachelor Thesis and code in R that was used to prepare this BT at the FoES at University of Warsaw. The BT is written in polish. The title of BT in english is "From Intention to Birth. On the Impact of Socioeconomic Factors on the Probability of Realizing Fertility Intentions"

# CODE
The pipeline inside code.R is structured into six clean, logical modules:
1. Libraries & Helper Functions
- Imports all mandatory modeling and utility packages.
- Declares helper functions:
      backward_elimination(): An automated feature selection function based on custom p-value thresholds.
      find_best_model(): A stepwise selection function optimizing AIC or BIC across model interaction spaces.
2. Data Loading & Processing
- Loads the panel datasets and renames variables to intuitive English naming conventions.
- Applies strict logical corrections for survey skip-patterns (e.g., setting partner-related variables to 0 or NA for single respondents, adjusting childless logic).
- Corrects ID key structures to properly join the imputed earnings data (id <- id + 0.26 adjustment to match the Stata merge structure).
3. Feature Engineering

Identifies the target subpopulation by removing respondents who had negative fertility intentions ("definitely/probably not"). Constructs key explanatory variables:
+ Demographic & Social: Gender (kobieta), Age & Age Squared (wiek2), Education Level (eduk_w).Relationship & Marriage: Marital Status (po_slubie), Marriage Duration & its square (lata_po_slubie, lata_po_slubie2).
+ Economic Context: Employment (zatrudniony), Joint Household Income Quartiles (kwartyl_dochodu).
+ Geographic Context: Multi-level city-size indicators (ranging from villages (wies) to metropolises (metropolia)).
4. Logistic Regression Modeling
- Fits the final logistic regression model (model_final) predicting the probability of successful fertility realization.
5. Diagnostics & Cross-Validation
- Multicollinearity: Computes Variance Inflation Factors (VIF) to ensure independent variables do not exhibit problematic collinearity.
- Goodness-of-Fit: Leverages the performance package to evaluate pseudo-$R^2$, AIC, and BIC.
- Predictive Validity: Employs a 5-fold cross-validation technique via caret to estimate the model's out-of-sample ROC AUC score.
6. Formatting & Reporting
- Exports regression tables containing log-odds, standard errors, and significance stars to a production-ready HTML file (final_model_results.html) using stargazer.
- Computes and displays Average Marginal Effects (AMEs) using the marginaleffects package, allowing for straightforward, intuitive interpretation of probabilities.
