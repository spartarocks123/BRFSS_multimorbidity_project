# ------------------------------
# 03_modeling.R
# ------------------------------

source("scripts/02_data_manipulation.R")

# ==========================================================
# Survey Design
# ==========================================================

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

# ==========================================================
# Complete-case analytic subsets
# ==========================================================

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

# ==========================================================
# Survey-weighted logistic regression models
# ==========================================================

model_routine_derv <- svyglm(
  routine_care ~ cc_cat2 + agegrp + sex + race + educ + income + insured,
  design = design_routine_derv,
  family = quasibinomial()
)

model_cost_derv <- svyglm(
  cost_barrier ~ cc_cat2 + agegrp + sex + race + educ + income + insured,
  design = design_cost_derv,
  family = quasibinomial()
)

# ==========================================================
# Predicted probabilities
# ==========================================================

pred_routine_derv <- ggpredict(model_routine_derv, terms = "cc_cat2") %>% drop_na(x)
pred_cost_derv    <- ggpredict(model_cost_derv, terms = "cc_cat2") %>% drop_na(x)

# write_csv(pred_routine_derv, file.path(output_dir, "pred_routine.csv"))
# write_csv(pred_cost_derv,    file.path(output_dir, "pred_cost.csv"))

# Sanity checks
# list.files(model_path)
list.files(table_path)
# list.files(output_dir)

# ==========================================================
# Odds Ratio extraction helper 
# ==========================================================

clean_or_table <- function(model, var_map) {
  tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      Variable = dplyr::recode(term, !!!var_map, .default = term)
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
}

# ==========================================================
# Variable mappings
# ==========================================================

var_map_full <- c(
  # Chronic condition category
  "cc_cat21"  = "1 chronic condition",
  "cc_cat22"  = "2 chronic conditions",
  "cc_cat23+" = "≥3 chronic conditions",
  
  # Age group
  "agegrp2"  = "Age 25 to 29",
  "agegrp3"  = "Age 30 to 34",
  "agegrp4"  = "Age 35 to 39",
  "agegrp5"  = "Age 40 to 44",
  "agegrp6"  = "Age 45 to 49",
  "agegrp7"  = "Age 50 to 54",
  "agegrp8"  = "Age 55 to 59",
  "agegrp9"  = "Age 60 to 64",
  "agegrp10" = "Age 65 to 69",
  "agegrp11" = "Age 70 to 74",
  "agegrp12" = "Age 75 to 79",
  "agegrp13" = "Age 80 or older",
  
  # Sex
  "sex1" = "Male",
  "sex2" = "Female",
  
  # Race / ethnicity
  "race2" = "Black only, non-Hispanic",
  "race3" = "American Indian or Alaskan Native only, non-Hispanic",
  "race4" = "Asian only, non-Hispanic",
  "race5" = "Native Hawaiian or other Pacific Islander only, non-Hispanic",
  "race6" = "Other race only, non-Hispanic",
  "race7" = "Multiracial, non-Hispanic",
  "race8" = "Hispanic",
  
  # Education
  "educ1" = "Did not graduate high school",
  "educ2" = "Graduated high school",
  "educ3" = "Attended college or technical school",
  
  # Income
  "income1" = "Less than $15,000",
  "income2" = "$15,000 to < $25,000",
  "income3" = "$25,000 to < $35,000",
  "income4" = "$35,000 to < $50,000",
  "income6" = "$100,000 to < $200,000",
  "income7" = "$200,000 or more",
  
  # Insurance
  "insuredUninsured" = "Uninsured"
)

var_map_simple <- c(
  # Chronic condition category
  "cc_cat21"  = "1 chronic condition",
  "cc_cat22"  = "2 chronic conditions",
  "cc_cat23+" = "≥3 chronic conditions",
  
  # Age group
  "agegrp2"  = "Age 25 to 29",
  "agegrp3"  = "Age 30 to 34",
  "agegrp4"  = "Age 35 to 39",
  "agegrp5"  = "Age 40 to 44",
  "agegrp6"  = "Age 45 to 49",
  "agegrp7"  = "Age 50 to 54",
  "agegrp8"  = "Age 55 to 59",
  "agegrp9"  = "Age 60 to 64",
  "agegrp10" = "Age 65 to 69",
  "agegrp11" = "Age 70 to 74",
  "agegrp12" = "Age 75 to 79",
  "agegrp13" = "Age 80 or older",
  
  # Sex
  "sex1" = "Male",
  "sex2" = "Female",
  
  # Race / ethnicity
  "race2" = "Black only, non-Hispanic",
  "race3" = "American Indian or Alaskan Native only, non-Hispanic",
  "race4" = "Asian only, non-Hispanic",
  "race5" = "Native Hawaiian or other Pacific Islander only, non-Hispanic",
  "race6" = "Other race only, non-Hispanic",
  "race7" = "Multiracial, non-Hispanic",
  "race8" = "Hispanic",
  
  # Education
  "educ1" = "Did not graduate high school",
  "educ2" = "Graduated high school",
  "educ3" = "Attended college or technical school",
  
  # Income
  "income1" = "Less than $15,000",
  "income2" = "$15,000 to < $25,000",
  "income3" = "$25,000 to < $35,000",
  "income4" = "$35,000 to < $50,000",
  "income6" = "$100,000 to < $200,000",
  "income7" = "$200,000 or more",
  
  # Insurance
  "insuredUninsured" = "Uninsured"
)

# ==========================================================
# OR Tables
# ==========================================================

routine_or <- clean_or_table(model_routine_derv, var_map_full)
cost_or    <- clean_or_table(model_cost_derv, var_map_simple)

# ==========================================================
# Save outputs
# ==========================================================

write_csv(routine_or, file.path(table_path, "routine_odds_ratios.csv"))
write_csv(cost_or,    file.path(table_path, "cost_odds_ratios.csv"))

