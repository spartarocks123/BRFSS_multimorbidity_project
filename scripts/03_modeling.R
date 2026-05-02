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

write_csv(pred_routine_derv, file.path(output_dir, "pred_routine.csv"))
write_csv(pred_cost_derv,    file.path(output_dir, "pred_cost.csv"))

# Sanity checks
list.files(model_path)
list.files(table_path)
list.files(output_dir)

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
  "cc_cat21" = "1 chronic condition",
  "cc_cat22" = "2 chronic conditions",
  "cc_cat23+" = "≥3 chronic conditions",
  
  "sex2" = "Female",
  "sex1" = "Male",
  
  "insuredUninsured" = "Uninsured"
)

var_map_simple <- c(
  "cc_cat21" = "1 chronic condition",
  "cc_cat22" = "2 chronic conditions",
  "cc_cat23+" = "≥3 chronic conditions"
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

