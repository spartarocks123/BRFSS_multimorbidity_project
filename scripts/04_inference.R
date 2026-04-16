

# ------------------------------
# 7. Figure 1: Routine Checkup
# ------------------------------

# Create a ggplot object using predicted probabilities dataset
fig1 <- ggplot(pred_routine_derv, aes(x = x, y = predicted)) +
  
  # Bar plot (column chart) of predicted probabilities
  geom_col(
    fill = viridis(1, option = "C", alpha = 0.85),  # color palette (single color from viridis)
    width = 0.6                                     # controls bar width
  ) +
  
  # Add error bars representing 95% confidence intervals
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),  # lower and upper CI bounds
    width = 0.2,                             # width of the error bar caps
    color = "black",                         # error bar color
    linewidth = 1.3,                         # thickness of error bars
    alpha = 0.9                              # slight transparency
  ) +
  
  # Add text labels above each bar showing predicted probability as a percentage
  geom_text(
    aes(
      label = percent(predicted, accuracy = 1),  # format probability as % (rounded to whole number)
      y = conf.high + 0.015                      # position text slightly above upper CI
    ),
    size = 4.2,                                  # text size
    fontface = "bold"                            # bold labels
  ) +
  
  # Customize Y-axis appearance
  scale_y_continuous(
    labels = NULL,   # remove Y-axis labels
    breaks = NULL,   # remove tick marks
    limits = c(0, max(pred_routine_derv$conf.high) * 1.1),  # dynamic upper bound (10% above max CI)
    expand = expansion(mult = c(0, 0.05))  # small padding at top
  ) +
  
  # Add axis labels, title, and caption
  labs(
    x = "Number of Chronic Conditions",  # X-axis label
    y = NULL,                            # remove Y-axis label
    title = "Figure 1: Adjusted Predicted Probability of Routine Checkup by Multimorbidity",
    caption = "Derived analytic dataset; survey-weighted logistic regression with 95% CI"
  ) +
  
  # Apply minimal theme with larger base font size
  theme_minimal(base_size = 16) +
  
  # Further customize theme elements (remove clutter for publication-style figure)
  theme(
    axis.line.x = element_blank(),   # remove X-axis line
    axis.line.y = element_blank(),   # remove Y-axis line
    axis.ticks = element_blank(),    # remove axis ticks
    axis.text.y = element_blank(),   # remove Y-axis text labels
    panel.grid.minor = element_blank(),  # remove minor gridlines
    panel.grid = element_blank(),        # remove all gridlines
    plot.title = element_text(hjust = 0.5)  # center-align title
  )

# Create "figures" directory if it does not already exist
dir.create("figures", showWarnings = FALSE)

# Save the figure to disk as a high-resolution PDF
ggsave(
  filename = "/filepath/Figure 1: Routine Checkup.pdf",
  plot = fig1,     # plot object to save
  width = 15,      # width in inches (large for publication)
  height = 10,     # height in inches
  dpi = 600        # high resolution for print-quality output
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
    axis.line.x = element_blank(),   # remove X-axis line
    axis.line.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)  # center title
  )
dir.create("figures", showWarnings = FALSE)
ggsave(
  filename = "/filepath/Figure 2: Cost Barrier.pdf",
  plot = fig2,
  width = 15,
  height = 10,
  dpi = 600
)


# ------------------------------
# 9. ROC / AUC
# ------------------------------

# Define a reusable function to:
# 1) generate predicted probabilities from a model
# 2) compute a (weighted) ROC curve
# 3) plot the ROC curve
# 4) return the AUC as a tidy tibble
plot_roc_auc <- function(model, design, outcome, color){
  
  # Generate predicted probabilities (P(Y=1)) from the fitted model
  probs <- predict(model, type = "response")
  
  # Create ROC object using:
  # - true outcome values from the survey design object
  # - predicted probabilities from the model
  # - sampling weights (LLCPWT) for weighted ROC estimation
  roc_obj <- roc(
    design$variables[[outcome]],      # observed binary outcome (0/1)
    probs,                            # predicted probabilities
    weights = design$variables$LLCPWT # BRFSS sampling weights
  )
  
  # Plot ROC curve with specified color and formatting
  plot(
    roc_obj,
    col = color,                      # line color for the ROC curve
    lwd = 2,                          # line width
    main = paste("ROC -", outcome)    # dynamic plot title
  )
  
  # Return AUC in a tidy format for later steps in the script
  tibble(
    outcome = outcome,                # outcome name (label)
    AUC = as.numeric(auc(roc_obj))   # extract AUC and coerce to numeric
  )
}

# Apply function to routine care model
# - generates ROC plot
# - returns 1-row tibble with AUC
auc_routine <- plot_roc_auc(
  model_routine_derv,
  design_routine_derv,
  "routine_care",
  "blue"
)

# Apply function to cost barrier model
auc_cost <- plot_roc_auc(
  model_cost_derv,
  design_cost_derv,
  "cost_barrier",
  "red"
)

# Combine both AUC results into a single dataframe
auc_all <- bind_rows(auc_routine, auc_cost)

# Write final AUC table to CSV file (absolute file path)
write_csv(
  auc_all,
  "/filepath/AUC Results.csv"
)

# ------------------------------
# 10. Calibration plots
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
      pred = weighted.mean(pred, LLCPWT),
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
github_fig_path <- "/filepath/figures"

# Routine care calibration plot
calibration_plot(
  model = model_routine_derv,
  design = design_routine_derv,
  outcome = "routine_care",
  color = "blue",
  filename = "Calibration: Routine Care",
  folder_path = github_fig_path
)

# Cost barrier calibration plot
calibration_plot(
  model = model_cost_derv,
  design = design_cost_derv,
  outcome = "cost_barrier",
  color = "red",
  filename = "Calibration: Cost Barrier",
  folder_path = github_fig_path
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



