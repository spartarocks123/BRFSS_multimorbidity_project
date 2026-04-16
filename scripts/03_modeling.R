# ------------------------------
# 3. Survey Design
# ------------------------------

#The following code replaces singleton strata with a common label "singleton"
#This groups all lonely strata together so variance can be computed. 
#Variance estimates for strata with a single unit cannot be computed normally

derv_br <- derv_br %>%
  mutate(STSTR2 = as.character(STSTR))

singleton_strata <- names(table(derv_br$STSTR2))[table(derv_br$STSTR2) == 1]
derv_br$STSTR2[derv_br$STSTR2 %in% singleton_strata] <- "singleton"

options(survey.lonely.psu = "adjust")

derv_design <- svydesign(
  ids     = ~PSU,
  strata  = ~STSTR2,
  weights = ~LLCPWT,
  data    = derv_br,
  nest    = TRUE
)

# ------------------------------
# 4. Models (complete case)
# ------------------------------
#This code filters out any observations that have missing (NA) values
design_routine_derv <- subset(
  derv_design,
  !is.na(routine_care) &
    !is.na(cc_cat2) &
    !is.na(agegrp) &
    !is.na(sex) &
    !is.na(race) &
    !is.na(educ) &
    !is.na(income) &
    !is.na(insured)
)

# This code uses svyglm() from the survey package to fit a survey-weighted logistic regression model.
# It accounts for:
# - Sampling weights (LLCPWT)
# - Clustering (PSU)
# - Stratification (STSTR2)

# family = quasibinomial() is used instead of binomial() to allow for overdispersion.
# Overdispersion occurs when the observed variance is greater than what the standard binomial model assumes.

# In survey data like BRFSS, overdispersion can arise due to:
# - Complex sampling design (clustering within PSUs)
# - Unobserved heterogeneity between individuals
# - Model misspecification (missing variables or imperfect fit)

# quasibinomial() adjusts the variance (standard errors) using a dispersion parameter,
# leading to more robust and reliable inference (wider, more realistic confidence intervals).

model_routine_derv <- svyglm(
  routine_care ~ cc_cat2 + agegrp + sex + race + educ + income + insured,
  design = design_routine_derv,
  family = quasibinomial()
)

design_cost_derv <- subset(
  derv_design,
  !is.na(cost_barrier) &
    !is.na(cc_cat2) &
    !is.na(agegrp) &
    !is.na(sex) &
    !is.na(race) &
    !is.na(educ) &
    !is.na(income) &
    !is.na(insured)
)

model_cost_derv <- svyglm(
  cost_barrier ~ cc_cat2 + agegrp + sex + race + educ + income + insured,
  design = design_cost_derv,
  family = quasibinomial()
)

# ------------------------------
# 5. Predicted Probabilities
# ------------------------------

#This code generates predicted probabilities from survey-weighted logistic regression models for each level of multimorbidity (cc_cat2)
#drop_na(x) removes any missing levels to ensure a clean dataset for plotting.
pred_routine_derv <- ggpredict(model_routine_derv, terms = "cc_cat2") %>% drop_na(x)
pred_cost_derv    <- ggpredict(model_cost_derv, terms = "cc_cat2") %>% drop_na(x)


# ------------------------------
# 6. Clean OR Extraction
# ------------------------------
# Convert survey-weighted logistic regression coefficients to odds ratios (ORs)
# with 95% confidence intervals, clean variable labels, and formatted p-values.
# This produces tables for routine care and cost barrier models.

source("scripts/00_setup.R")

# ------------------------------
# Load Models
# ------------------------------
model_routine <- readRDS(file.path(output_path, "data/model_routine.rds"))
model_cost    <- readRDS(file.path(output_path, "data/model_cost.rds"))

# ------------------------------
# Tidy OR Extraction (broom)
# ------------------------------

routine_or <- tidy(model_routine, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Variable = case_when(
      
      # Multimorbidity
      term == "cc_cat21" ~ "1 chronic condition",
      term == "cc_cat22" ~ "2 chronic conditions",
      term == "cc_cat23+" ~ "≥3 chronic conditions",
      
      # Sex
      term == "sex2" ~ "Female",
      term == "sex1" ~ "Male",
      
      # Insurance
      term == "insuredUninsured" ~ "Uninsured",
      
      # Age
      term == "agegrp1" ~ "Age 18 to 24",
      term == "agegrp2" ~ "Age 25 to 29",
      term == "agegrp3" ~ "Age 30 to 34",
      term == "agegrp4" ~ "Age 35 to 39",
      term == "agegrp5" ~ "Age 40 to 44",
      term == "agegrp6" ~ "Age 45 to 49",
      term == "agegrp7" ~ "Age 50 to 54",
      term == "agegrp8" ~ "Age 55 to 59",
      term == "agegrp9" ~ "Age 60 to 64",
      term == "agegrp10" ~ "Age 65 to 69",
      term == "agegrp11" ~ "Age 70 to 74",
      term == "agegrp12" ~ "Age 75 to 79",
      term == "agegrp13" ~ "Age 80 or older",
      
      # Race
      term == "race2" ~ "Black only, non-Hispanic",
      term == "race3" ~ "American Indian or Alaskan Native only, Non-Hispanic",
      term == "race4" ~ "Asian only, non-Hispanic",
      term == "race5" ~ "Native Hawaiian or other Pacific Islander only, Non-Hispanic",
      term == "race6" ~ "Other race only, non-Hispanic",
      term == "race7" ~ "Multiracial, non-Hispanic",
      term == "race8" ~ "Hispanic",
      
      # Education
      term == "educ1" ~ "Did not graduate High School",
      term == "educ2" ~ "Graduated High School",
      term == "educ3" ~ "Attended College or Technical School",
      term == "educ4" ~ "Graduated from College or Technical School",
      
      # Income
      term == "income1" ~ "Less than $15,000",
      term == "income2" ~ "$15,000 to < $25,000",
      term == "income3" ~ "$25,000 to < $35,000",
      term == "income4" ~ "$35,000 to < $50,000",
      term == "income5" ~ "$50,000 to < $100,000",
      term == "income6" ~ "$100,000 to < $200,000",
      term == "income7" ~ "$200,000 or more",
      
      TRUE ~ term
    )
  ) %>%
  select(Variable, estimate, conf.low, conf.high, p.value) %>%
  rename(
    OR = estimate,
    CI_lower = conf.low,
    CI_upper = conf.high,
    p_value = p.value
  ) %>%
  mutate(
    OR = round(OR, 2),
    CI_lower = round(CI_lower, 2),
    CI_upper = round(CI_upper, 2),
    p_value = signif(p_value, 3)
  )

# Repeat for cost model
cost_or <- tidy(model_cost, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Variable = case_when(
      term == "cc_cat21" ~ "1 chronic condition",
      term == "cc_cat22" ~ "2 chronic conditions",
      term == "cc_cat23+" ~ "≥3 chronic conditions",
      TRUE ~ term
    )
  ) %>%
  select(Variable, estimate, conf.low, conf.high, p.value) %>%
  rename(
    OR = estimate,
    CI_lower = conf.low,
    CI_upper = conf.high,
    p_value = p.value
  )

# ------------------------------
# Save Outputs
# ------------------------------
write_csv(routine_or, file.path(output_path, "tables/routine_odds_ratios.csv"))
write_csv(cost_or, file.path(output_path, "tables/cost_odds_ratios.csv"))
# ------------------------------
# 7. Save Tables
# ------------------------------
dir.create("tables", showWarnings = FALSE)

write_csv(
  routine_or,
  "/filepath/routine_odds_ratios.csv"
)

write_csv(
  cost_or,
  "/filepath/cost_odds_ratios.csv"
)
