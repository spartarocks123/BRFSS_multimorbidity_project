# Multimorbidity Burden and Healthcare Access

**Author:** Muhammad Amish-Malik

---

# Problem

How does increasing chronic disease burden affect both healthcare utilization and financial access barriers among U.S. adults?

Using 2024 BRFSS data, this project evaluated whether individuals with greater multimorbidity were more likely to:

- engage with routine healthcare
- delay care due to cost

---

# Approach

Built a reproducible healthcare analytics pipeline using:

- survey-weighted logistic regression (`svyglm`)
- survey-weighted ROC/AUC evaluation
- PSU-level cross-validation
- calibration assessment

## Data Source

- 2024 BRFSS (Behavioral Risk Factor Surveillance System)

## Key Predictors

- multimorbidity burden
- insurance status
- age
- sex
- race/ethnicity
- income
- education

---

# Tech Stack

| Tool | Purpose |
|---|---|
| R | Modeling, validation, feature engineering |
| SAS | Weighted descriptive statistics |
| SQL | Data transformation and querying |
| Tableau | Visualization dashboard |

---

# Key Results

- Higher multimorbidity was associated with:
  - increased routine healthcare utilization
  - increased cost-related delays in care
- Models demonstrated stable discrimination:
  - Routine care model: AUC = **0.75**
  - Cost-barrier model: AUC = **0.81**
- PSU-level survey-weighted cross-validation showed consistent performance across validation folds
- Developed a modular reproducible analytics workflow across R, SAS, SQL, and Tableau
- Calibration plots demonstrated strong agreement between predicted and observed outcome probabilities

---

# So What?

The findings suggest a healthcare systems gap:

> High-need populations remain engaged with healthcare systems but are not financially protected from barriers to care.

Potential applications include:

- population health initiatives
- care coordination
- targeted financial assistance programs

---

# Reproducibility

```text
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

Key workflow features:

- Sequential script architecture (00 -> 05)
- Reproducible outputs 

---

# Deliverables

- Survey-weighted regression outputs
- ROC/AUC and calibration diagnostics
- Tableau dashboard
- Reproducible healthcare analytics pipeline

---

# Tableau Dashboard
[View on Tableau Public](https://public.tableau.com/app/profile/muhammad.a.malik/viz/ORRoutineCarevsCostBarrier/ORGraph)
