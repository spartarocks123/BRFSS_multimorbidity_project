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

# Technical Stack

| Tool | Purpose |
|---|---|
| R | Modeling, validation, feature engineering |
| SAS | Weighted descriptive statistics |
| SQL | Data transformation and querying |
| Tableau | Visualization dashboard |
| Git/GitHub | Version control, reproducibility, and project tracking |
| Python *(coming soon)* | Automated data quality checks |

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
- Survey-weighted calibration plots demonstrated strong correlation between predicted and observed outcome probabilities

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

## Running the Project

Run scripts in order:

1. `scripts/00_setup.R`
2. `scripts/01_load_data.R`
3. `scripts/02_data_manipulation.R`
4. `scripts/03_modeling.R`
5. `scripts/04_figures.R`
6. `scripts/05_validation.R`

Required local setup:

- Download the 2024 BRFSS XPT file
- Create a local `.Renviron` file with `BRFSS_DATA` and `BRFSS_PROJECT`
- `.Renviron` is excluded from GitHub via `.gitignore`
