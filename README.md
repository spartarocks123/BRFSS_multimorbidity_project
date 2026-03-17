# BRFSS Multimorbidity Project

**Author:** Mohammed Amish-Malik  
**Purpose:** Portfolio artifact using 2024 BRFSS data to study the associations between multimorbidity, routine healthcare utilization, and deferred care due to cost.  

**Folder Structure:**
- `data/` → Simulated or example dataset showing variable structure
- `scripts/` → R scripts for data cleaning, analysis, and visualization. SAS was used to create descriptive tables. SQL, Python, and/or Power BI/Tableau will be used later to replicate project as well to demonstrate proficiency. 
- `figures/` → Placeholder for plots
- `tables/` → Placeholder for output tables
  
**Methods:** Collapse the conditions into multimorbidity burden category. Compare against the “healthy” baseline and interpretation reasons as odds of routine care/cost barrier among people with multimorbidity relative to no chronic conditions. Outcomes: Primary: Had a routine care in past 12 months (Yes/No) Secondary: Deferred care due to cost (Yes/No). Covariates are Age, Sex, Race/ethnicity, Insurance status, Income category, & Education. The following chronic conditions were used in this analysis: Heart attack / Myocardial infarction or Coronary heart disease, Stroke, Asthma, Any cancer (collapsed from skin cancer and other cancers), Chronic obstructive pulmonary disease (COPD), emphysema, or chronic bronchitis, Depressive disorder (including major, minor, or dysthymia), Chronic kidney disease, Diabetes (chronic; excludes pregnancy-only or pre-diabetes). Weighted descriptive statistics and weighted survey logistic regression were conducted for this analysis of the cross-sectional dataset. 

**Results:** So far, there appears to be a dose-response relationship between the number of chronic conditions and routine healthcare utilization. As the number of chronic conditions increase in an individual, the predicted probability of routine checkup increases. This is expected and correlates with what has been found in the literature. Interestingly, the predicted probability of delayed care due to cost also increases in accordance with the number of chronic conditions. All the p values are less than 0.01 and the OR increases as the number of chronic conditions increases. 

**Note:** No raw BRFSS data is included. The dataset used is simulated for reproducibility. Fully reproducible using the scripts in scripts/. Figures and tables will be saved in figures/ and tables/. This dataset is simulated for replication purposes. Variable names match BRFSS, but values are generated synthetically

Happy to receive any professional feedback—please keep it constructive and focused on analysis, methodology, or reproducibility.

**Note on diagnostics:**
- VIF and Hosmer-Lemeshow tests are standard diagnostics for GLMs.
- Recently learned that VIG and Hosmer-Lemeshow tests don't fully account for weights, strata, or clustering. This dataset uses svyglm (survey-weighted logistic regression). Will utilize different tests instead.

**Note on Assumptions:**
- Independent Observations: BRFSS tends to survey people from the same counties, states, neighboorhoods, etc. This is demonstrated by the fact that strata and clustering are used. However, svydesign(...) code in R accounts for this. Therefore, this assumption has been met.
- Correct model: The outcomes of interest are yes/no. Therefore, this assumption has been met.
- Linearity:
- Multicollinearity: All intercept, adjusted predictors had VIFs below 5, indicating that multicollinearity is not a concern in this model. Therefore, this assumption has been met. 
- Separation:
- Overdispersion: Since weights and clusters are involved in this dataset, the observations are not entirely independent from each other. However, using quasibinomial modeling for both outcomes addresses this concern as it adjusts for variance in the dataset.

**Note on coding steps:**
- Weighted Logistic Models: "!is.na" step was conducted for all the variables to ensure no missing values were utilized in the logistic regression. This was a complete case analysis. svyglm () handles weights and clusters. 
- Variables: The bernoulli distribution was utilized to demonstrate the prevalence of the variables. Adjusted them so that they reflect the prevalence of the original BRFSS dataset more accurately. Completed for the chronic conditions, will conduct the same for the other variables (Covariates, weighting, strata, and clusting variables if feasible and necessary) 
- Logit-scale: The Odds Ratios (ORs) from the original BRFSS dataset are converted to log-odds (logit) probabilities in the simulation dataset. This was done so that the simulated dataset matches the original dataset's relationships.

***Current Steps:***
- Adjust the covariates and other variables (strata, cluster, weighting) to reflect the original BRFSS dataset more accurately.
- Chronic conditions are simulated independently; co-occurrence patterns may not reflect real multimorbidity correlations in BRFSS. See if that could be adjusted as well. Tetrachoric correlation may hold promise for this issue. Chronic conditions are simulated independently; therefore, co-occurrence patterns may not fully reflect real multimorbidity correlations in BRFSS. While tetrachoric correlations could theoretically model such associations, this is unnecessary because the BRFSS measures these conditions as binary indicators (a person either has the condition or doesn’t). cc_cancer is not an independent measure. Instead, it is derived from cc_skin_ca and cc_other_ca. Therefore, no separate Bernoulli draw is required. For all chronic conditions, independent Bernoulli simulations are appropriate and consistent with the codebook.
- Some category variables may be treated as continuous (e.g. income level, race, education level). Therefore, readjust those variables. 
- Review variable selection decisions
- Evaluate model assumptions and robustness
- Conduct sensitivity analyses if appropriate. Will conduct a sensitivity analysis on Men vs Women as well as Insured vs Uninsured. 
- Finalize interpretation of results and documentation
- Replicate analysis with SQL & Python
- Document technical rationale through code comments. If any changes are needed, that will be explained by the code comments or README.md.

***Completed Steps:***
- Resampled survey design variables from the original BRFSS dataset (strata, clustering, and weights). Adjusted the prevalence of all the variables to reflect the original dataset as well.
- A weighted proportion of missing (NA) values was made for all predictors, covariates, and outcomes in this complete-case survey analysis. Although svyglm drops NA, documenting NAs demonstrates transparency and if any bias occurs due to the potential volume of missing data. The table demonstrated no missing values for the simulated dataset. 


