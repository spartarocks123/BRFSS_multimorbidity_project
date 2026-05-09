# ------------------------------
# 05_validation.R
# ------------------------------

source("scripts/02_data_manipulation.R")

library(tidyverse)
library(survey)
library(pROC)

set.seed(123)

# ==========================================================
# Validation helper function
# ==========================================================

run_cv_auc <- function(data, outcome_name) {
  
  # Complete-case filtering
  cv_data <- data %>%
    filter(
      !is.na(.data[[outcome_name]]),
      !is.na(cc_cat2),
      !is.na(agegrp),
      !is.na(sex),
      !is.na(race),
      !is.na(educ),
      !is.na(income),
      !is.na(insured),
      !is.na(LLCPWT),
      !is.na(STSTR),
      !is.na(PSU)
    )
  
  # PSU-level folds
  psu_folds <- cv_data %>%
    distinct(PSU) %>%
    mutate(
      fold = sample(rep(1:5, length.out = n()))
    )
  
  cv_data <- cv_data %>%
    left_join(psu_folds, by = "PSU")
  
  # ==========================================================
  # Cross-validation loop
  # ==========================================================
  
  cv_results <- map_dfr(1:5, function(k) {
    
    train_data <- cv_data %>%
      filter(fold != k)
    
    test_data <- cv_data %>%
      filter(fold == k)
    
    options(survey.lonely.psu = "adjust")
    
    train_design <- svydesign(
      ids = ~PSU,
      strata = ~STSTR,
      weights = ~LLCPWT,
      data = train_data,
      nest = TRUE
    )
    
    # Dynamic formula
    model_formula <- as.formula(
      paste(
        outcome_name,
        "~ cc_cat2 + agegrp + sex + race + educ + income + insured"
      )
    )
    
    model <- svyglm(
      model_formula,
      design = train_design,
      family = quasibinomial()
    )
    
    test_data <- test_data %>%
      mutate(
        pred = as.numeric(
          predict(
            model,
            newdata = test_data,
            type = "response"
          )
        )
      )
    
    roc_obj <- roc(
      response = test_data[[outcome_name]],
      predictor = test_data$pred,
      weights = test_data$LLCPWT,
      quiet = TRUE
    )
    
    tibble(
      fold = k,
      outcome = outcome_name,
      AUC = as.numeric(auc(roc_obj))
    )
  })
  
  return(cv_results)
}

# ==========================================================
# Run CV for both outcomes
# ==========================================================

cv_routine <- run_cv_auc(
  data = derv_br,
  outcome_name = "routine_care"
)

cv_cost <- run_cv_auc(
  data = derv_br,
  outcome_name = "cost_barrier"
)

# Combine results
cv_results <- bind_rows(cv_routine, cv_cost)

cv_results

# ==========================================================
# Summary table
# ==========================================================

cv_summary <- cv_results %>%
  group_by(outcome) %>%
  summarise(
    mean_AUC = mean(AUC),
    sd_AUC = sd(AUC),
    min_AUC = min(AUC),
    max_AUC = max(AUC),
    .groups = "drop"
  )

cv_summary

# ==========================================================
# Save outputs
# ==========================================================

write_csv(
  cv_results,
  file.path(table_path, "cv_auc_results.csv")
)

write_csv(
  cv_summary,
  file.path(table_path, "cv_auc_summary.csv")
)