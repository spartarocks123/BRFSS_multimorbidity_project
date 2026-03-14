# BRFSS Multimorbidity Project

**Author:** Mohammed Amish-Malik  
**Purpose:** Portfolio artifact using 2024 BRFSS data to study the associations between multimorbidity, routine healthcare utilization, and deferred care due to cost.  

**Folder Structure:**
- `data/` → Simulated or example dataset showing variable structure
- `scripts/` → R scripts for data cleaning, analysis, and visualization. SAS was used to create descriptive tables. SQL & Python will be used later to replicate project as well to demonstrate proficiency. 
- `figures/` → Placeholder for plots
- `tables/` → Placeholder for output tables
  
**Methods:** Collapse the conditions into multimorbidity burden category. compare against the “healthy” baseline and interpretation reasons as odds of routine care/cost barrier among people with multimorbidity relative to no chronic conditions. Outcomes: Primary: Had a routine care in past 12 months (Yes/No) Secondary: Deferred care due to cost (Yes/No). Covariates are Age, Sex, Race/ethnicity, Insurance status, Income category, & Education. The following chronic conditions were used in this analysis: Heart attack / Myocardial infarction or Coronary heart disease, Stroke, Asthma, Any cancer (collapsed from skin cancer and other cancers), Chronic obstructive pulmonary disease (COPD), emphysema, or chronic bronchitis, Depressive disorder (including major, minor, or dysthymia), Chronic kidney disease, Diabetes (chronic; excludes pregnancy-only or pre-diabetes). Weighted descriptive statistics and weighted survey logistic regression were conducted for this analysis of the cross-sectional dataset. 

**Results:** So far, there appears to be a dose-response relationship between the number of chronic conditions and routine healthcare utilization. As the number of chronic conditions increase in an individual, the predicted probability of routine checkup increases. This is expected and correlates with what has been found in the literature. Interestingly, the predicted probability of delayed care due to cost also increases in accordance with the number of chronic conditions. All the p values are less than 0.01 and the OR increases as the number of chronic conditions increases. 

**Note:** No raw BRFSS data is included. The dataset used is simulated for reproducibility. Fully reproducible using the scripts in scripts/. Figures and tables will be saved in figures/ and tables/. This dataset is simulated for replication purposes. Variable names match BRFSS, but values are generated synthetically

***Next Steps:***
- Review variable selection decisions
- Evaluate model assumptions and robustness
- Conduct sensitivity analyses if appropriate
- Finalize interpretation of results and documentation
- Replicate analysis with SQL & Python
- Document technical rationale through code comments. If any changes are needed, that will be explained by the code comments or README.md. 

Happy to receive any professional feedback—please keep it constructive and focused on analysis, methodology, or reproducibility.

**Note on diagnostics:**
- VIF and Hosmer-Lemeshow tests are standard diagnostics for GLMs.
- Since we are using svyglm (survey-weighted logistic regression),
- Recently learned that VIG and Hosmer-Lemeshow tests don't fully account for weights, strata, or clustering. This dataset uses svyglm (survey-weighted logistic regression). Will utilize different tests instead.

**Note on Assumptions:**
- Independent Observations: BRFSS tends to survey people from the same counties, states, neighboorhoods, etc. This is demonstrated by the fact that strata and clustering are used. However, svydesign(...) code in R accounts for this. Therefore, this assumption has been met.
- Correct model: The outcomes of interest are yes/no. Therefore, this assumption has been met.
- Linearity:
- Multicollinearity: All intercept, adjusted predictors had VIFs below 5, indicating that multicollinearity is not a concern in this model. Therefore, this assumption has been met. 
- Separation:
- Overdispersion: Since weights and clusters are involved in this dataset, the observations are not entirely independent from each other. However, using quasibinomial modeling for both outcomes addresses this concern as it adjusts for variance in the dataset. 
