library(haven)
library(dplyr)
library(survey)
library(ggplot2)
library(viridis)
library(scales)
library(broom)
library(tidyverse)
library(ggeffects)
library(pROC)

# ------------------------------
# Load Data
# ------------------------------
brfss <- read_xpt("/filepath/LLCP2024.XPT ")

# Clean names (removes leading underscores)
names(brfss) <- gsub("^_", "", names(brfss))

# ------------------------------
# Clean + Recode
# ------------------------------
brfss <- brfss %>%
  select(
    MICHD, CVDSTRK3, ASTHMS1, CHCSCNC1, CHCOCNC1,
    CHCCOPD3, ADDEPEV3, CHCKDNY2, DIABETE4,
    CHECKUP1, MEDCOST1, HLTHPL2,
    AGEG5YR, SEXVAR, RACE, EDUCAG, INCOMG1,
    LLCPWT, STSTR, PSU
  ) %>%
  
  mutate(
    # Chronic conditions
    across(
      c(MICHD, CVDSTRK3, ASTHMS1, CHCSCNC1, CHCOCNC1,
        CHCCOPD3, ADDEPEV3, CHCKDNY2, DIABETE4),
      ~ ifelse(. == 1, 1, 0),
      .names = "cc_{.col}"
    )
  ) %>%
  
  mutate(
    cc_count = rowSums(select(., starts_with("cc_")), na.rm = TRUE),
    
    cc_cat2 = factor(
      case_when(
        cc_count == 0 ~ "0",
        cc_count == 1 ~ "1",
        cc_count == 2 ~ "2",
        cc_count >= 3 ~ "3+",
        TRUE ~ NA_character_
      ),
      levels = c("0", "1", "2", "3+")
    ),
    
    # Outcomes
    routine_care = case_when(
      CHECKUP1 == 1 ~ 1,
      CHECKUP1 %in% c(2,3,4,8) ~ 0,
      TRUE ~ NA_real_
    ),
    
    cost_barrier = case_when(
      MEDCOST1 == 1 ~ 1,
      MEDCOST1 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    insured = factor(case_when(
      HLTHPL2 == 1 ~ "Insured",
      HLTHPL2 == 2 ~ "Uninsured",
      TRUE ~ NA_character_
    )),
    agegrp = factor(AGEG5YR),
    sex    = factor(SEXVAR),
    race   = factor(RACE),
    educ   = factor(EDUCAG),
    income = factor(INCOMG1)
  ) %>%
  
  filter(!is.na(routine_care), !is.na(cost_barrier)) %>%
  
  mutate(across(c(cc_cat2, agegrp, sex, race, educ, income, insured), droplevels))

# ------------------------------
# Keep Only Clean Variables
# ------------------------------
brfss_clean <- brfss %>%
  select(
    # Outcomes
    routine_care,
    cost_barrier,
    
    # Derived exposure
    cc_count,
    cc_cat2,
    
    # Covariates (recoded)
    insured,
    agegrp,
    sex,
    race,
    educ,
    income,
    
    # (Optional) weights if needed for survey analysis
    LLCPWT, STSTR, PSU
  )

# ------------------------------
# Save cleaned dataset
# ------------------------------
dir.create("data", showWarnings = FALSE, recursive = TRUE)

write.csv(brfss_clean, "/filepath/brfss_example.csv", row.names = FALSE)


# ==========================================================
# Derived Dataset Analysis (GitHub)
# ==========================================================

# ------------------------------
# 1. Load + Subsample Dataset
# ------------------------------
derv_br <- read_csv("/filepath/brfss_example.csv", show_col_types = FALSE)

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
  
  # Clean levels safely
  new_levels <- unique(c(ref_level, setdiff(levels(x), ref_level)))
  
  factor(x, levels = new_levels)
}

# This code filters the "Don't Know/ Refused" responses from BRFSS
derv_br <- derv_br %>%
  filter(
    agegrp != "14",
    race   != "9",
    educ   != "9",
    income != "9"
  )

# This code takes account the weighting when it comes to setting reference levels. 
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
    
    select(Variable, OR, CI_lower, CI_upper, p_value) %>%
    
    mutate(
      OR = round(OR, 2),
      CI_lower = round(CI_lower, 2),
      CI_upper = round(CI_upper, 2),
      p_value = signif(p_value, 3)
    )
}

routine_or <- extract_or_clean(model_routine_derv)
cost_or    <- extract_or_clean(model_cost_derv)

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



