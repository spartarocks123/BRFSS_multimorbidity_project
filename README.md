**Multimorbidity Burden and its Associations with Routine Healthcare Utilization and Delayed Care due to Cost: Analysis of BRFSS 2024**

**Author:** Muhammad Amish-Malik

---

## 1. Problem

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
- **Power BI/Tableau**: will be used for visualization and presentation.
- **Python**: Survey-weighted descriptive tables and creating multimorbidity counts will be demonstrated.
  
## 4. Results

- A **dose-response relationship** was observed between multimorbidity and healthcare utilization
- As the number of chronic conditions increased:
    - Probability of routine checkups **increased significantly**
    - Probability of delaying care due to cost **also increased**
- All associations were statistically significant (**p < 0.01**)
- Odds ratios (OR) increased with higher multimorbidity burden
- Based on the AUC results, the statistical model demonstrates **good discrimination** between individuals who had a routine checkup or delayed care due to cost versus those who did not.  
  - For routine checkups, the model correctly distinguishes individuals **approximately 75% of the time**.  
  - For delayed care due to cost, it correctly distinguishes individuals **approximately 81% of the time**.
- Calibration plots show both model lines closely align with the dashed line, **demonstrating good calibration** across all deciles.
- Sensitivity analyses were attempted within gender and insurance subgroups. However, insufficient variability in these groups prevented estimation of results.

---

## 5. Key Insights

- Individuals with higher disease burden are more engaged with healthcare systems  
- However, they also face disproportionately higher financial barriers to care  

This suggests a structural inefficiency: populations with the greatest clinical need are also at elevated risk of cost-related access limitations.
---

## 6. Reproducibility

- All analysis conducted in **R** using survey-weighted methods
- Project structure:
    - `/data` → Derived analytic dataset with cleaned and recoded variables
    - `/scripts` → Data cleaning, variable construction, modeling, and visualization
    - `/tables` → Output tables
    - `/figures` → Visualizations
- This project uses a **derived dataset** based on publicly available BRFSS data:
    - Raw data is not included
    - All transformations are reproducible via scripts in `/scripts`
