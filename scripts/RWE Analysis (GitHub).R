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
# 1. Load Derived Dataset
# ------------------------------
derv_br <- read_csv("/Users/moh/Desktop/Research Assistant/BRFSS_multimorbidity_project/data/brfss_example.csv", show_col_types = FALSE)

# ------------------------------
# 2. Ensure Correct Factor Order (BEFORE survey design)
# ------------------------------
derv_br <- derv_br %>%
  mutate(
    cc_cat2 = factor(cc_cat2, levels = c("0","1","2","3+")),
    cc_cat2 = relevel(cc_cat2, ref = "0"),
    agegrp  = factor(agegrp),
    sex     = factor(sex),
    race    = factor(race),
    educ    = factor(educ),
    income  = factor(income),
    insured = factor(insured)
  )

# ------------------------------
# 3. Adjust Survey Design
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
# 4. Weighted Logistic Models (ONLY drop outcome NAs)
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
# 5. Extract Predicted Probabilities
# ------------------------------
pred_routine_derv <- ggpredict(model_routine_derv, terms = "cc_cat2") %>%
  drop_na(x)

pred_cost_derv <- ggpredict(model_cost_derv, terms = "cc_cat2") %>%
  drop_na(x)
# ------------------------------
# 6. Extract Odds Ratios
# ------------------------------
extract_or <- function(model, drop_intercept = FALSE){
  coef_table <- summary(model)$coefficients
  
  res <- tibble(
    Variable = rownames(coef_table),
    OR       = exp(coef_table[, "Estimate"]),
    CI_lower = exp(coef_table[, "Estimate"] - 1.96 * coef_table[, "Std. Error"]),
    CI_upper = exp(coef_table[, "Estimate"] + 1.96 * coef_table[, "Std. Error"]),
    p_value  = coef_table[, "Pr(>|t|)"]
  )
  
  if(drop_intercept) res <- filter(res, Variable != "(Intercept)")
  
  res %>%
    mutate(
      OR = round(OR,2),
      CI_lower = round(CI_lower,2),
      CI_upper = round(CI_upper,2),
      p_value = signif(p_value,3)
    )
}

routine_or <- extract_or(model_routine_derv, TRUE)
cost_or    <- extract_or(model_cost_derv, TRUE)


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
  filename = "/Users/moh/Desktop/Research Assistant/BRFSS_multimorbidity_project/figures/Figure 1: Routine Checkup.png",
  plot = fig1,
  width = 6,
  height = 4,
  dpi = 300
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
  filename = "/Users/moh/Desktop/Research Assistant/BRFSS_multimorbidity_project/figures/Figure 2: Cost Barrier.png",
  plot = fig1,
  width = 6,
  height = 4,
  dpi = 300
)

# ------------------------------
# 9. ROC / AUC (FIXED)
# ------------------------------
plot_roc_auc <- function(model, design, outcome, color){
  probs <- predict(model, type = "response")
  roc_obj <- roc(design$variables[[outcome]], probs,
                 weights = design$variables$LLCPWT)
  
  plot(roc_obj, col = color, lwd = 2, main = paste("ROC -", outcome))
  auc(roc_obj)
}

auc_routine <- plot_roc_auc(model_routine_derv, design_routine_derv, "routine_care", "blue")
auc_cost    <- plot_roc_auc(model_cost_derv, design_cost_derv, "cost_barrier", "red")

# ------------------------------
# 10. Calibration plots
# ------------------------------
calibration_plot <- function(model, design, outcome, color){
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
  
  ggplot(data, aes(x = pred, y = obs)) +
    geom_point(color = color, size = 3) +
    geom_line(color = color, linewidth = 1) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    labs(
      x = "Mean Predicted Probability",
      y = "Observed Probability",
      title = paste("Calibration -", outcome)
    ) +
    theme_minimal(base_size = 14)
}

calibration_plot(model_routine_derv, design_routine_derv, "routine_care", "blue")
calibration_plot(model_cost_derv, design_cost_derv, "cost_barrier", "red")

# ------------------------------
# 11. Sensitivity analyses
# ------------------------------
subset_designs <- list(
  male = subset(derv_design, sex == levels(derv_br$sex)[1]),
  female = subset(derv_design, sex == levels(derv_br$sex)[2]),
  insured = subset(derv_design, insured == "Insured"),
  uninsured = subset(derv_design, insured == "Uninsured")
)


sensitivity_models <- map(subset_designs, function(dsg){
  list(
    routine = svyglm(routine_care ~ cc_cat2 + agegrp + sex + race + educ + income + insured,
                     design = dsg, family = quasibinomial()),
    cost    = svyglm(cost_barrier ~ cc_cat2 + agegrp + sex + race + educ + income + insured,
                     design = dsg, family = quasibinomial())
  )
})

sensitivity_or <- map(sensitivity_models, ~map(.x, extract_or))

