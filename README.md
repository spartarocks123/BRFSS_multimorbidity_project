**Multimorbidity burden and its Associations with Routine Healthcare Utilization and Delayed Care due to Cost: Analysis of BRFSS 2024 Data**

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

## 3. Methods

- Constructed a multimorbidity variable by aggregating chronic conditions
- Conducted **weighted descriptive analyses**
- Applied **survey-weighted logistic regression (svyglm in R)**
- Compared outcomes across multimorbidity levels using a “no chronic condition” baseline

---

## 4. Results

- A **dose-response relationship** was observed between multimorbidity and healthcare utilization
- As the number of chronic conditions increased:
    - Probability of routine checkups **increased significantly**
    - Probability of delaying care due to cost **also increased**
- All associations were statistically significant (**p < 0.01**)
- Odds ratios increased with higher multimorbidity burden

---

## 5. Key Insights

- Individuals with higher disease burden are more engaged with healthcare systems **but also face greater financial barriers**
- Increased utilization does not eliminate access inequities, particularly cost-related delays

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
