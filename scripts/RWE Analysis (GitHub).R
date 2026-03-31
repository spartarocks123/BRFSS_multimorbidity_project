# ==========================================================
# Derived Dataset Analysis
# ==========================================================

library(tidyverse)
library(survey)
library(ggeffects)
library(ggplot2)
library(scales)
library(viridis)
library(pROC)

# ------------------------------
# 1. Load + Subsample Dataset
# ------------------------------
derv_br <- read_csv("/Users/moh/Desktop/Research Assistant/BRFSS_multimorbidity_project/data/brfss_example.csv", show_col_types = FALSE)

# ------------------------------
# 2. Ensure Correct Factor Order
# ------------------------------

set_ref_weighted <- function(x, w) {
  
  # Ensure factor
  x <- factor(x)
  
  df <- data.frame(x = x, w = w)
  
  freq <- df %>%
    group_by(x) %>%
    summarise(w_sum = sum(w, na.rm = TRUE), .groups = "drop")
  
  ref_level <- as.character(freq$x[which.max(freq$w_sum)])
  
  # 🔥 Clean levels safely
  new_levels <- unique(c(ref_level, setdiff(levels(x), ref_level)))
  
  factor(x, levels = new_levels)
}
# 🔥 FILTER HERE
derv_br <- derv_br %>%
  filter(
    agegrp != "14",
    race   != "9",
    educ   != "9",
    income != "9"
  )

# 🔥 APPLY REFERENCE LEVEL LOGIC HERE
derv_br <- derv_br %>%
  mutate(
    cc_cat2 = set_ref_weighted(cc_cat2, LLCPWT),
    agegrp  = set_ref_weighted(agegrp, LLCPWT),
    sex     = set_ref_weighted(sex, LLCPWT),
    race    = set_ref_weighted(race, LLCPWT),
    educ    = set_ref_weighted(educ, LLCPWT),
    income  = set_ref_weighted(income, LLCPWT),
    insured = set_ref_weighted(insured, LLCPWT)
  )

# ------------------------------
# 3. Survey Design
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
# 4. Models (complete case)
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
# 5. Predicted Probabilities
# ------------------------------
pred_routine_derv <- ggpredict(model_routine_derv, terms = "cc_cat2") %>% drop_na(x)
pred_cost_derv    <- ggpredict(model_cost_derv, terms = "cc_cat2") %>% drop_na(x)

# ------------------------------
# 6. Clean OR Extraction
# ------------------------------
extract_or_clean <- function(model){
  
  coef_table <- summary(model)$coefficients
  
  tibble(
    term     = rownames(coef_table),
    OR       = exp(coef_table[, "Estimate"]),
    CI_lower = exp(coef_table[, "Estimate"] - 1.96 * coef_table[, "Std. Error"]),
    CI_upper = exp(coef_table[, "Estimate"] + 1.96 * coef_table[, "Std. Error"]),
    p_value  = coef_table[, "Pr(>|t|)"]
  ) %>%
    filter(term != "(Intercept)") %>%
    
    mutate(
      Variable = case_when(
        
        # Multimorbidity
        term == "cc_cat21" ~ "1 chronic condition",
        term == "cc_cat22" ~ "2 chronic conditions",
        term == "cc_cat23+" ~ "≥3 chronic conditions",
        
        # Sex
        term == "sex2" ~ "Female",
        
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
    
    select(Variable, OR, CI_lower, CI_upper, p_value) %>%
    
    mutate(
      OR = round(OR, 2),
      CI_lower = round(CI_lower, 2),
      CI_upper = round(CI_upper, 2),
      p_value = signif(p_value, 3)
    )
}

# ✅ Correct function calls
routine_or <- extract_or_clean(model_routine_derv)
cost_or    <- extract_or_clean(model_cost_derv)

# ------------------------------
# 7. Save Tables
# ------------------------------
dir.create("tables", showWarnings = FALSE)

write_csv(
  routine_or,
  "/Users/moh/Desktop/Research Assistant/BRFSS_multimorbidity_project/tables/routine_odds_ratios.csv"
)

write_csv(
  cost_or,
  "/Users/moh/Desktop/Research Assistant/BRFSS_multimorbidity_project/tables/cost_odds_ratios.csv"
)

# ------------------------------
# 7. Figure 1: Routine Checkup
# ------------------------------
fig1 <- ggplot(pred_routine_derv, aes(x = x, y = predicted)) +
  geom_col(fill = viridis(1, option = "C", alpha = 0.85), width = 0.6) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.2,
    color = "black",
    linewidth = 1.3,
    alpha = 0.9
  ) +
  geom_text(
    aes(label = percent(predicted, accuracy = 1),
        y = conf.high + 0.015),
    size = 4.2,
    fontface = "bold"
  ) +
  scale_y_continuous(
    labels = NULL,
    breaks = NULL,
    limits = c(0, max(pred_routine_derv$conf.high) * 1.1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Number of Chronic Conditions",
    y = NULL,
    title = "Figure 1: Adjusted Predicted Probability of Routine Checkup by Multimorbidity",
    caption = "Derived analytic dataset; survey-weighted logistic regression with 95% CI"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.line.x = element_blank(),   # 🔥 remove X-axis line
    axis.line.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)  # 🔥 center title
  )
dir.create("figures", showWarnings = FALSE)
ggsave(
  filename = "/Users/moh/Desktop/Research Assistant/BRFSS_multimorbidity_project/figures/Figure 1: Routine Checkup.pdf",
  plot = fig1,
  width = 15,
  height = 10,
  dpi = 600
)

# ------------------------------
# 8. Figure 2: Cost Barrier
# ------------------------------
fig2 <-ggplot(pred_cost_derv, aes(x = x, y = predicted)) +
  geom_col(fill = viridis(1, option = "D", alpha = 0.85), width = 0.6) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.2,
    color = "black",
    linewidth = 1.3,
    alpha = 0.9
  ) +
  geom_text(
    aes(label = percent(predicted, accuracy = 1),
        y = conf.high + 0.01),
    size = 4.2,
    fontface = "bold"
  ) +
  scale_y_continuous(
    labels = NULL,
    breaks = NULL,
    limits = c(0, max(pred_cost_derv$conf.high) * 1.1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Number of Chronic Conditions",
    y = NULL,
    title = "Figure 2: Adjusted Predicted Probability of Cost Barrier by Multimorbidity",
    caption = "Derived analytic dataset; survey-weighted logistic regression with 95% CI"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.line.x = element_blank(),   # 🔥 remove X-axis line
    axis.line.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)  # 🔥 center title
  )
dir.create("figures", showWarnings = FALSE)
ggsave(
  filename = "/Users/moh/Desktop/Research Assistant/BRFSS_multimorbidity_project/figures/Figure 2: Cost Barrier.pdf",
  plot = fig2,
  width = 15,
  height = 10,
  dpi = 600
)


# ------------------------------
# 9. ROC / AUC
# ------------------------------
plot_roc_auc <- function(model, design, outcome, color){
  
  probs <- predict(model, type = "response")
  
  roc_obj <- roc(
    design$variables[[outcome]],
    probs,
    weights = design$variables$LLCPWT
  )
  
  plot(roc_obj, col = color, lwd = 2, main = paste("ROC -", outcome))
  
  tibble(
    outcome = outcome,
    AUC = as.numeric(auc(roc_obj))
  )
}

auc_routine <- plot_roc_auc(model_routine_derv, design_routine_derv, "routine_care", "blue")
auc_cost    <- plot_roc_auc(model_cost_derv, design_cost_derv, "cost_barrier", "red")

auc_all <- bind_rows(auc_routine, auc_cost)

write_csv(
  auc_all,
  "/Users/moh/Desktop/Research Assistant/BRFSS_multimorbidity_project/tables/AUC Results.csv")

# ------------------------------
# 10. Calibration plots with saving
# ------------------------------
calibration_plot <- function(model, design, outcome, color, filename, folder_path){
  # Ensure folder exists
  dir.create(folder_path, showWarnings = FALSE, recursive = TRUE)
  
  # Compute decile-level observed vs predicted probabilities
  data <- design$variables %>%
    mutate(
      pred = predict(model, type = "response"),
      decile = ntile(pred, 10)
    ) %>%
    group_by(decile) %>%
    summarise(
      obs = sum(.data[[outcome]] * LLCPWT) / sum(LLCPWT),
      pred = mean(pred),
      .groups = "drop"
    )
  
  # Create the calibration plot
  fig <- ggplot(data, aes(x = pred, y = obs)) +
    geom_point(color = color, size = 3) +
    geom_line(color = color, linewidth = 1) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    labs(
      x = "Mean Predicted Probability",
      y = "Observed Probability",
      title = paste("Calibration -", outcome)
    ) +
    theme_minimal(base_size = 14)
  
  # Save the plot to the specified folder
  ggsave(
    filename = file.path(folder_path, paste0(filename, ".pdf")),
    plot = fig,
    width = 8,
    height = 6,
    dpi = 600
  )
  
  return(fig)
}

# ------------------------------
# Example usage
# ------------------------------
github_fig_path <- "/Users/moh/Desktop/Research Assistant/BRFSS_multimorbidity_project/figures"

# Routine care calibration plot
calibration_plot(
  model = model_routine_derv,
  design = design_routine_derv,
  outcome = "routine_care",
  color = "blue",
  filename = "Calibration_Routine_Care",
  folder_path = github_fig_path
)

# Cost barrier calibration plot
calibration_plot(
  model = model_cost_derv,
  design = design_cost_derv,
  outcome = "cost_barrier",
  color = "red",
  filename = "Calibration_Cost_Barrier",
  folder_path = github_fig_path
)

# ==========================================================
# Male Subgroup Sensitivity Analysis (fixed)
# ==========================================================

# ------------------------------
# 1. Subset to Male Subgroup
# ------------------------------
df_male <- derv_br %>%
  filter(sex == "Male", !is.na(LLCPWT)) %>%   # male only, drop missing weights
  mutate(across(where(is.factor), ~ fct_drop(.))) %>% # drop unused levels
  mutate(STSTR2 = as.character(STSTR2))

# Handle singleton strata
singleton_strata <- names(table(df_male$STSTR2))[table(df_male$STSTR2) == 1]
df_male$STSTR2[df_male$STSTR2 %in% singleton_strata] <- "singleton"

options(survey.lonely.psu = "adjust")

# ------------------------------
# 2. Prepare survey design
# ------------------------------
dsg_male <- svydesign(
  ids     = ~PSU,
  strata  = ~STSTR2,
  weights = ~LLCPWT,
  data    = df_male,
  nest    = TRUE
)

# ------------------------------
# 3. Dynamic variable filtering
# Only keep factors with >1 level
# ------------------------------
keep_vars <- function(df, vars){
  vars[sapply(df[vars], function(x) !(is.factor(x) && nlevels(x) < 2))]
}

model_vars <- c("cc_cat2", "agegrp", "sex", "race", "educ", "income", "insured")

# ------------------------------
# 4. Fit Routine Care Model
# ------------------------------
vars_routine <- keep_vars(df_male, model_vars)

if(length(vars_routine) == 0){
  stop("No predictors with more than one level left for the male subgroup (routine care).")
}

formula_routine <- as.formula(
  paste("routine_care ~", paste(vars_routine, collapse = " + "))
)

model_routine_male <- svyglm(
  formula_routine,
  design = dsg_male,
  family = quasibinomial()
)

routine_or_male <- extract_or_clean(model_routine_male) %>%
  mutate(outcome = "Routine Care", subgroup = "Male")

# ------------------------------
# 5. Fit Cost Barrier Model
# ------------------------------
vars_cost <- keep_vars(df_male, model_vars)

if(length(vars_cost) == 0){
  stop("No predictors with more than one level left for the male subgroup (cost barrier).")
}

formula_cost <- as.formula(
  paste("cost_barrier ~", paste(vars_cost, collapse = " + "))
)

model_cost_male <- svyglm(
  formula_cost,
  design = dsg_male,
  family = quasibinomial()
)

cost_or_male <- extract_or_clean(model_cost_male) %>%
  mutate(outcome = "Cost Barrier", subgroup = "Male")

# ------------------------------
# 6. Combine and Save Results
# ------------------------------
sensitivity_results_male <- bind_rows(routine_or_male, cost_or_male)

write_csv(
  sensitivity_results_male,
  "/Users/moh/Desktop/Research Assistant/BRFSS_multimorbidity_project/tables/sensitivity_odds_ratios_male.csv"
)
