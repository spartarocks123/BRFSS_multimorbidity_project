# Multimorbidity Burden and Healthcare Access

**Author:** Muhammad Amish-Malik

---

## Project Abstract

- Analyzed **2024 Behavioral Risk Factor Surveillance System (BRFSS)** using survey-weighted methods
- Built **multivariable logistic regression models (`svyglm`)** to evaluate healthcare utilization and cost barriers
- Identified a **dose-response relationship**:
    - ↑ chronic conditions → ↑ routine care utilization
    - ↑ chronic conditions → ↑ cost-related care delays
- Model performance:
    - AUC = **0.75** (routine care)
    - AUC = **0.81** (cost-related delay)
- Delivered a **reproducible pipeline** across R, SAS, SQL, and Tableau

---

## Key Visualization

**Tableau Interactive Dashboard:**

[View on Tableau Public](https://public.tableau.com/app/profile/muhammad.a.malik/viz/ORRoutineCarevsCostBarrier/ORGraph)

---

## Objective

Evaluate how **multimorbidity impacts both healthcare routine utilization and financial access barriers** in U.S. adults.

---

## Methods & Technical Approach

- **Design:** Cross-sectional, survey-weighted analysis
- **Data Source:** 2024 BRFSS
- **Exposure:** Number of chronic conditions (multimorbidity)
- **Outcomes:**
    - Routine checkup (past 12 months)
    - Delayed care due to cost
- **Covariates:** Age, sex, race/ethnicity, income, education, insurance

### Modeling

- Survey-weighted logistic regression (`svyglm`, quasibinomial)
- Reference group: **0 chronic conditions**
- Complete-case analysis

### Model Evaluation

- ROC / AUC (weighted)
- Calibration plots (decile-based observed vs predicted)

---

## Tech Stack

- **R** → survey modeling, feature engineering, post-estimation
- **SAS** → weighted descriptive statistics
- **SQL** → structured data operations (`GROUP BY`, `COALESCE`, constraints)
- **Tableau** → odds ratio visualization

---

## Key Findings

- **Dose-response relationship confirmed**
- Higher multimorbidity →
    - ↑ likelihood of routine care utilization
    - ↑ likelihood of delaying care due to cost
- Indicates a **system-level inefficiency**:
    - High-need populations are engaged with care but **not financially protected**

---

## Why This Matters

- Identifies **high-risk populations (≥2 conditions)** for targeted intervention
- Supports development of:
    - Cost assistance programs
    - Insurance navigation strategies
    - Chronic disease care coordination
- Suggests **access ≠ affordability** in current healthcare systems

---

## Limitations

- Cross-sectional design → no causal inference
- Self-reported BRFSS data (recall/reporting bias)
- ROC/AUC partially accounts for survey design (weights only)
- Complete-case analysis may introduce selection bias

---

## Reproducibility

Project structured for full reproducibility:

**/scripts**

- `00_setup.R` — environment + libraries
- `01_load_data.R` — data ingestion
- `02_data_manipulation.R` — cleaning + feature engineering
- `03_modeling.R` — regression models
- `04_figures.R` — figures ROC/AUC, Calibration plots

**/folders**

- `/data` → cleaned dataset
- `/models` → saved model objects
- `/tables` → regression outputs
- `/figures` → visualizations

### Reproducibility Features

- Environment variables for file paths
- Sequential script pipeline (00 → 04)
- Saved outputs (data, models, tables, figures)

---

## Skills Demonstrated

- Survey-weighted statistical modeling
- Multivariable regression & inference
- Data cleaning and feature engineering
- Cross-platform analytics (**R, SAS, SQL**)
- Data visualization
- Reproducible research pipelines

---

## Deliverables

- Survey-weighted regression outputs (ORs, 95% CI)
- Model diagnostics (ROC/AUC, calibration)
- Tableau dashboard for odds ratio comparison
- Fully reproducible multi-language workflow
