
source("scripts/02_data_manipulation.R")

# ------------------------------
# Survey Design
# ------------------------------

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
# Models (complete case)
# ------------------------------

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
# Save Models (for reproducibility)
# ------------------------------
saveRDS(model_routine_derv, file.path(model_path, "model_routine_derv.rds"))
saveRDS(model_cost_derv,    file.path(model_path, "model_cost_derv.rds"))

# ------------------------------
# Predicted Probabilities
# ------------------------------

pred_routine_derv <- ggpredict(model_routine_derv, terms = "cc_cat2") %>% drop_na(x)
pred_cost_derv    <- ggpredict(model_cost_derv, terms = "cc_cat2") %>% drop_na(x)

# Optional sanity checks
list.files(output_dir)
list.files(table_path)
list.files(model_path)

# ------------------------------
# Clean OR Extraction (broom)
# ------------------------------

routine_or <- tidy(model_routine_derv, conf.int = TRUE, exponentiate = TRUE) %>%
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

# Cost model ORs
cost_or <- tidy(model_cost_derv, conf.int = TRUE, exponentiate = TRUE) %>%
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
write_csv(routine_or, file.path(table_path, "routine_odds_ratios.csv"))
write_csv(cost_or,    file.path(table_path, "cost_odds_ratios.csv"))
