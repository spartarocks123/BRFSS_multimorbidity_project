**Multimorbidity Burden and its Associations with Routine Healthcare Utilization and Delayed Care due to Cost: Analysis of BRFSS 2024**

**Author:** Muhammad Amish-Malik

---

## 1. Purpose

This project examines how multimorbidity (the presence of multiple chronic conditions) is associated with healthcare utilization and financial barriers to care in the U.S. adult population.

Specifically, it evaluates:

- Whether individuals with more chronic conditions are more likely to attend routine checkups
- Whether they are also more likely to delay care due to cost barriers

---

## 2. Data

- **Source:** Behavioral Risk Factor Surveillance System (BRFSS), 2024
- **Type:** Cross-sectional, survey-weighted dataset
- **Structure:** Derived analytic dataset with cleaned and recoded variables

### Key Variables:

- **Outcomes:**
    - Routine checkup in past 12 months (Yes/No)
    - Deferred care due to cost (Yes/No)
- **Exposure:**
    - Multimorbidity (number of chronic conditions)
- **Covariates:**
    - Age
    - Sex
    - Race/ethnicity
    - Insurance status
    - Income
    - Education
- **Chronic Conditions Included:**
    - Cardiovascular disease (heart attack, CHD)
    - Stroke
    - Asthma
    - Cancer
    - COPD
    - Depressive disorder
    - Chronic kidney disease
    - Diabetes

---

## **3. Methods**

### Data Processing

- Cleaned and recoded BRFSS variables into analytic categories.
- Removed invalid or missing categories (e.g., “Don’t know / Refused”).
- Constructed multimorbidity variable.
- Applied weighted reference level selection (most frequent categories as baseline).

**SQL Workaround & Skills Highlighted:**

Due to complications with importing the BRFSS dataset directly into MySQL, the derived analytic table `brfss_cleaned` was manually created with explicit column types. Sample rows were inserted using `INSERT INTO ... VALUES` statements. SQL is currently being used to demonstrate key data processing operations, including:

- **Table creation with constraints** (`PRIMARY KEY`, `UNIQUE`, `NOT NULL`, `DEFAULT`)
- **Data insertion and updates** (`INSERT INTO`)
- **Aggregation** (`COUNT()`, `SUM()`, `AVG()`, `GROUP BY`, `HAVING`)
- **NULL handling** (`COALESCE()`)

Codes for calculating derived columns, such as **multimorbidity count**, **categorical multimorbidity**, and **binary indicators for routine care and cost barriers**, were generated using **R**.

### Survey Design

- Accounted for:
    - Primary sampling units (PSU)
    - Strata (with singleton handling)
    - Sampling weights (`LLCPWT`)

### Modeling Approach

- Survey-weighted logistic regression (`svyglm`, `quasibinomial`) in **R**.
- Separate models for:
    - Routine care
    - Cost-related delay
- Complete-case analysis for model inputs.
- Models adjusted for **age, sex, race/ethnicity, education, income, and insurance status**, with “no chronic conditions” as the reference category for multimorbidity.

### Post-Estimation

- Odds ratios with 95% CI (custom extraction pipeline).
- Model evaluation:
    - ROC / AUC (weighted)
    - Calibration plots (decile-based observed vs predicted)

### SAS Contribution

- Generated **weighted descriptive tables** for baseline characteristics and chronic condition prevalence.

**Integration Summary:**

- **SAS**: weighted descriptive tables for accurate baseline representation.
- **R**: survey-weighted logistic models, derived variables, and post-estimation evaluation.
- **Tableau**: ORs of routine care and delayed care due to cost are compared to each other through a graph.
  
## 4. Results

- A **dose-response relationship** was observed between multimorbidity and healthcare utilization
- As the number of chronic conditions increased:
    - Probability of routine checkups **increased significantly**
    - Probability of delaying care due to cost **also increased**
- All associations were statistically significant (**p < 0.01**)
- Odds ratios (OR) increased with higher multimorbidity burden
- Models demonstrated moderate-to-strong discrimination (AUC: 0.75 for routine care; 0.81 for cost-related delay).
  - **NOTE:** These estimates should be interpreted with caution as ROC/AUC calculations incorporated sampling weights but did not fully account for clustering and stratification.
- Calibration plots show both model lines closely align with the dashed line, **demonstrating good calibration** across all deciles.
- Sensitivity analyses were attempted within gender and insurance subgroups. However, insufficient variability in these groups prevented estimation of results.

---

## 5. Key Insights

- Individuals with higher disease burden are more engaged with healthcare systems  
- However, they also face disproportionately higher financial barriers to care
- This suggests a structural inefficiency: populations with the greatest clinical need are also at elevated risk of cost-related access limitations.
---
## 6. Limitations

- The survey demonstrates a cross-sectional design. Therefore, no causal inference could be determined. 
- Self-reported BRFSS data subject to recall and reporting bias
- ROC/AUC estimates incorporate weights but did not fully account for complex survey design (clustering, stratification)
- Complete-case analysis may introduce selection bias (e.g., participants with partial missingness across model variables were excluded).

## 7. Implications for Practice (“So What?”)

- **High-need populations are not access-secure**
    - Individuals with multimorbidity engage more with routine care but are also more likely to delay care due to cost, demonstrating gaps in financial protection.
- **Targeted interventions are warranted**
    - Patients with ≥2 chronic conditions could have a greater need for the following:
        - Cost assistance programs
        - Insurance navigation support
        - Care coordination services
- **Healthcare systems may be inefficiently allocating resources**
    - Increased utilization without reduced financial barriers suggests that access alone does not equate to affordability.
- **Policy relevance**
    - Findings support expansion of:
        - Subsidized care programs
        - Preventive care coverage
        - Chronic disease management initiatives with financial safeguards

### 8. Reproducibility

All analyses were conducted in R using survey-weighted methods to properly account for the BRFSS sampling design.

The project is organized into a clear, step-by-step script workflow:

**/scripts**

* `00_setup.R` — Sets file paths and loads required libraries
* `01_load_data.R` — Loads raw BRFSS data
* `02_data_manipulation.R` — Cleans data and creates analysis variables
* `03_modeling.R` — Builds survey-weighted regression models
* `04_inference.R` — Generates predicted probabilities, figures, and model evaluation metrics

**/folders**

* `/data` — Contains the cleaned dataset (`brfss_clean.csv`)
* `/models` — Saved model objects
* `/tables` — Regression results and summary tables
* `/figures` — Final visualizations

Reproducibility is supported by:

* Environment variables for flexible file paths (`BRFSS_DATA`, `BRFSS_PROJECT`)
* Consistent file handling using `file.path()`
* A sequential workflow (scripts run from 00 → 04)
* Saving all key outputs (data, models, tables, figures)

This project uses publicly available BRFSS data:

* Raw data is not included due to size constraints
* All steps to clean, analyze, and generate results are fully reproducible using the provided scripts

## 9. Deliverables

- Survey-weighted regression outputs (odds ratios, 95% CI)
- Model diagnostics (ROC/AUC, calibration plots)
- Reproducible data pipeline across R, SAS, and SQL
- Tableau visualization comparing the odds ratios of routine care vs cost-related delay [View Interactive Dashboard](https://public.tableau.com/app/profile/muhammad.a.malik/viz/ORRoutinevsCostBarrier/ORGraph) 
