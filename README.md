# Multimorbidity Burden and Healthcare Access

**Author:** Mohammed Amish-Malik

## Overview

**Goal:** Evaluate how multimorbidity influences both healthcare engagement and cost-related barriers among U.S. adults.

Analyzed 2024 Behavioral Risk Factor Surveillance System (BRFSS) data to examine how increasing chronic disease burden impacts healthcare utilization and financial access barriers.

Survey-weighted logistic regression models identified a clear pattern:

- Individuals with more chronic conditions were more likely to engage with routine healthcare
- The same populations were also more likely to delay care due to cost

This highlights a healthcare systems gap where high-need populations remain financially vulnerable despite increased healthcare engagement.

---

# Key Results

- Built survey-weighted multivariable logistic regression models using BRFSS 2024 data
- Identified a dose-response relationship between multimorbidity and:
    - increased routine healthcare utilization
    - increased cost-related delays in care
- Models demonstrated stable discriminatory performance:
    - Routine care model: AUC = 0.75
    - Cost-barrier model: AUC = 0.81
- Survey-weighted cross-validation showed consistent performance across validation folds
- Developed a reproducible analytics pipeline across R, SAS, SQL, and Tableau

---

# Technical Approach

## Data Source

- 2024 BRFSS (Behavioral Risk Factor Surveillance System)

## Exposure

- Number of chronic conditions (multimorbidity burden)

## Outcomes

- Routine medical checkup within the past 12 months
- Delayed medical care due to cost

## Covariates

- Age
- Sex
- Race/ethnicity
- Income
- Education
- Insurance status

## Modeling & Validation

- Survey-weighted logistic regression (`svyglm`)
- Survey-weighted ROC/AUC evaluation
- Calibration assessment
- Survey-weighted cross-validation

---

# Tech Stack

| Tool | Purpose |
| --- | --- |
| R | Survey modeling, validation, feature engineering |
| SAS | Weighted descriptive statistics |
| SQL | Structured data transformation and querying |
| Tableau | Interactive healthcare data visualization |

---

# Reproducibility & Workflow

Designed a modular analytics workflow with:

- sequential script execution
- environment-variable based file paths
- saved model outputs and figures
- organized project directories for reproducibility

```
/scripts
  00_setup.R
  01_load_data.R
  02_data_manipulation.R
  03_modeling.R
  04_figures.R
  05_validation.R

/data
/tables
/figures
/models
```

---

# Why This Matters

This project demonstrates how healthcare analytics can identify populations at elevated risk for:

- delayed medical care
- financial barriers to care
- chronic disease burden

Potential applications include:

- population health initiatives
- care coordination programs
- insurance navigation strategies
- targeted cost-assistance interventions

The findings suggest that healthcare access does not necessarily translate to healthcare affordability for high-need populations.

---

# Skills Demonstrated

- Healthcare data analytics
- Survey-weighted statistical modeling
- Multivariable regression analysis
- Cross-validation and model evaluation
- Data cleaning and feature engineering
- SQL-based data operations
- Tableau dashboard development
- Reproducible research workflows

---

# Deliverables

- Survey-weighted regression outputs
- ROC/AUC and calibration diagnostics
- Tableau visualization dashboard
- Reproducible multi-language analytics pipeline

---

# Tableau Dashboard

**Interactive Dashboard:**

[View on Tableau Public](https://public.tableau.com/app/profile/muhammad.a.malik/viz/ORRoutineCarevsCostBarrier/ORGraph)
