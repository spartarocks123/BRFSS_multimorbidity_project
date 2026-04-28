
source("scripts/03_inference.R")

# ------------------------------
# Figure 1: Routine Checkup
# ------------------------------

fig1 <- ggplot(pred_routine_derv, aes(x = x, y = predicted)) +
  geom_col(
    fill = viridis(1, option = "C", alpha = 0.85),
    width = 0.6
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.2,
    color = "black",
    linewidth = 1.3,
    alpha = 0.9
  ) +
  geom_text(
    aes(
      label = percent(predicted, accuracy = 1),
      y = conf.high + 0.015
    ),
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
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Ensure figures directory exists (already handled in setup, but safe)
dir.create(figure_path, recursive = TRUE, showWarnings = FALSE)

# Save Figure 1
ggsave(
  filename = file.path(figure_path, "figure_1_routine_checkup.pdf"),
  plot = fig1,
  width = 15,
  height = 10,
  dpi = 600
)

# ------------------------------
# Figure 2: Cost Barrier
# ------------------------------

fig2 <- ggplot(pred_cost_derv, aes(x = x, y = predicted)) +
  geom_col(fill = viridis(1, option = "D", alpha = 0.85), width = 0.6) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.2,
    color = "black",
    linewidth = 1.3,
    alpha = 0.9
  ) +
  geom_text(
    aes(
      label = percent(predicted, accuracy = 1),
      y = conf.high + 0.01
    ),
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
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Save Figure 2
ggsave(
  filename = file.path(figure_path, "figure_2_cost_barrier.pdf"),
  plot = fig2,
  width = 15,
  height = 10,
  dpi = 600
)

# ------------------------------
# ROC / AUC
# ------------------------------

plot_roc_auc <- function(model, design, outcome, color){
  
  probs <- predict(model, type = "response")
  
  roc_obj <- roc(
    design$variables[[outcome]],
    probs,
    weights = design$variables$LLCPWT
  )
  
  plot(
    roc_obj,
    col = color,
    lwd = 2,
    main = paste("ROC -", outcome)
  )
  
  tibble(
    outcome = outcome,
    AUC = as.numeric(auc(roc_obj))
  )
}

auc_routine <- plot_roc_auc(
  model_routine_derv,
  design_routine_derv,
  "routine_care",
  "blue"
)

auc_cost <- plot_roc_auc(
  model_cost_derv,
  design_cost_derv,
  "cost_barrier",
  "red"
)

auc_all <- bind_rows(auc_routine, auc_cost)

# Save AUC table
write_csv(
  auc_all,
  file.path(table_path, "auc_results.csv")
)

# ------------------------------
# Calibration plots
# ------------------------------

calibration_plot <- function(model, design, outcome, color, filename, folder_path){
  
  dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
  
  data <- design$variables %>%
    mutate(
      pred = predict(model, type = "response"),
      decile = ntile(pred, 10)
    ) %>%
    group_by(decile) %>%
    summarise(
      obs = sum(.data[[outcome]] * LLCPWT) / sum(LLCPWT),
      pred = weighted.mean(pred, LLCPWT),
      .groups = "drop"
    )
  
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
  
  ggsave(
    filename = file.path(folder_path, paste0(filename, ".pdf")),
    plot = fig,
    width = 8,
    height = 6,
    dpi = 600
  )
  
  return(fig)
}

# Use predefined figure_path instead of hardcoded path
calibration_plot(
  model = model_routine_derv,
  design = design_routine_derv,
  outcome = "routine_care",
  color = "blue",
  filename = "calibration_routine_care",
  folder_path = figure_path
)

calibration_plot(
  model = model_cost_derv,
  design = design_cost_derv,
  outcome = "cost_barrier",
  color = "red",
  filename = "calibration_cost_barrier",
  folder_path = figure_path
)

# ==========================================================
# Sensitivity Analysis
# ==========================================================
# After subsetting the dataset to the male subgroup and removing missing survey weights,
# all candidate predictors (cc_cat2, agegrp, sex, race, educ, income, insured)
# collapsed to zero usable levels. This occurred because:
#  1. NAs were removed during filtering.
#  2. Factor levels with no observations were dropped (fct_drop()).
#  3. The remaining sample had no variability in any predictor.
# As a result, logistic regression models (svyglm) could not be estimated for the subgroups.
# Attempting to run a sensitivity analysis would produce errors or meaningless results.
# Therefore, sensitivity analyses were not feasible with the available data.



