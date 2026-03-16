# ------------------------------
# Simulate Example BRFSS Dataset (Adjusted Prevalences & Survey Variables)
# ------------------------------
library(haven)
library(tidyverse)
library(survey)
library(ggeffects)
library(scales)
library(viridis)
library(car)
library(pROC)
library(ResourceSelection)
library(pscl)

use_cached        <- TRUE
compute_figures   <- TRUE
save_outputs      <- TRUE
generate_sim_brfss <- TRUE

if (generate_sim_brfss) {
  set.seed(123)
  n <- 460000L
  
  bern <- function(p) rbinom(n, 1, p)
  
  # --- Covariate probabilities based on weighted BRFSS prevalences ---
  age_probs <- c(0.122, 0.076, 0.091, 0.077, 0.086, 0.067, 0.076, 0.069,
                 0.084, 0.070, 0.063, 0.047, 0.053, 0.018)
  sex_probs <- c(0.491, 0.509)
  race_probs <- c(0.56, 0.115, 0.011, 0.061, 0.004, 0.011, 0.026, 0.192, 0.021)
  educ_probs <- c(0.111, 0.272, 0.295, 0.316, 0, 0, 0, 0, 0.006)
  income_probs <- c(0.049, 0.072, 0.089, 0.101, 0.225, 0.181, 0.076, 0, 0.208)
  insured_probs <- c(0.086, 0.914)
  
  # --- Resample survey design variables from the real BRFSS ---
  psu_vals   <- sample(brfss_keep$PSU, n, replace = TRUE)
  strata_vals <- sample(brfss_keep$STSTR, n, replace = TRUE)
  weight_vals <- sample(brfss_keep$LLCPWT, n, replace = TRUE)
  
  sim_brfss <- tibble(
    # Chronic conditions
    cc_mi       = bern(0.068),
    cc_stroke   = bern(0.035),
    cc_asthma   = bern(0.103),
    cc_skin_ca  = bern(0.056),
    cc_other_ca = bern(0.084),
    cc_copd     = bern(0.063),
    cc_depress  = bern(0.21),
    cc_ckd      = bern(0.041),
    cc_diabetes = bern(0.125),
    cc_cancer   = bern(0.119),
    
    # Covariates
    AGEG5YR = sample(1:14, n, replace = TRUE, prob = age_probs),
    SEXVAR  = sample(c(1,2), n, replace = TRUE, prob = sex_probs),
    RACE    = sample(1:9, n, replace = TRUE, prob = race_probs),
    EDUCAG  = sample(1:9, n, replace = TRUE, prob = educ_probs),
    INCOMG1 = sample(1:9, n, replace = TRUE, prob = income_probs),
    HLTHPL2 = sample(c(1,2), n, replace = TRUE, prob = insured_probs),
    
    # Survey design variables (resampled from real BRFSS)
    PSU    = psu_vals,
    STSTR  = strata_vals,
    LLCPWT = weight_vals
  )
  
  # --- Derived variables ---
  sim_brfss <- sim_brfss %>%
    mutate(
      cc_count = rowSums(across(cc_mi:cc_diabetes)),
      cc_cat2  = case_when(
        cc_count == 0 ~ "0",
        cc_count == 1 ~ "1",
        cc_count == 2 ~ "2",
        cc_count >= 3 ~ "3+"
      ),
      cc_cat2 = factor(cc_cat2, levels = c("0","1","2","3+"))
    )
}
  
  # ------------------------------
  # Target log-odds coefficients (β) based on original ORs
  # ------------------------------
  beta_list <- list(
    intercept  = log(0.44 / (1 - 0.44)),
    cc_cat21   = log(1.48),
    cc_cat22   = log(2.05),
    cc_cat23p  = log(2.67),
    AGEG5YR    = log(1.16),
    SEXVAR     = log(1.53),   # 2=Female
    RACE       = log(0.98),   # per unit increase
    EDUCAG     = log(1.09),
    INCOMG1    = log(1.04),
    HLTHPL2    = log(0.98)    # 2=Insured
  )
  
  # ------------------------------
  # Function to compute linear predictor
  # ------------------------------
  compute_lp <- function(df, beta) {
    lp <- beta$intercept +
      ifelse(df$cc_cat2=="1", beta$cc_cat21, 0) +
      ifelse(df$cc_cat2=="2", beta$cc_cat22, 0) +
      ifelse(df$cc_cat2=="3+", beta$cc_cat23p, 0) +
      df$AGEG5YR * beta$AGEG5YR +
      (df$SEXVAR==2) * beta$SEXVAR +
      df$RACE * beta$RACE +
      df$EDUCAG * beta$EDUCAG +
      df$INCOMG1 * beta$INCOMG1 +
      (df$HLTHPL2==2) * beta$HLTHPL2
    return(lp)
  }
  
  # ------------------------------
  # Simulate outcomes using logistic function
  # ------------------------------
  # Routine care: use original βs
  lp_routine <- compute_lp(sim_brfss, beta_list)
  sim_brfss$routine_care <- rbinom(n, 1, plogis(lp_routine))
  
  # Cost barrier: scale βs to approximate original BRFSS ORs
  # Here we adjust intercept + scale numeric predictors to match original cost_barrier ORs
  beta_list_cost <- beta_list
  beta_list_cost$intercept <- log(0.37 / (1 - 0.37))
  beta_list_cost$cc_cat21   <- log(1.73)
  beta_list_cost$cc_cat22   <- log(2.24)
  beta_list_cost$cc_cat23p  <- log(2.97)
  beta_list_cost$AGEG5YR    <- log(0.86)
  beta_list_cost$SEXVAR     <- log(1.21)
  beta_list_cost$RACE       <- log(1.09)
  beta_list_cost$EDUCAG     <- log(0.82)
  beta_list_cost$INCOMG1    <- log(0.89)
  beta_list_cost$HLTHPL2    <- log(1.04)
  
  lp_cost <- compute_lp(sim_brfss, beta_list_cost)
  sim_brfss$cost_barrier <- rbinom(n, 1, plogis(lp_cost))
  
  # ------------------------------
  # Save simulated dataset
  # ------------------------------
  if(!dir.exists("data")) dir.create("data")
  write.csv(sim_brfss, file.path("data","brfss_example.csv"), row.names = FALSE)


# ==========================================================
# Simulated Dataset Analysis (Replication of BRFSS Workflow)
# ==========================================================

library(tidyverse)
library(survey)
library(ggeffects)
library(ggplot2)
library(scales)
library(viridis)

# ------------------------------
# 1. Load Simulated Dataset
# ------------------------------
sim_brfss <- read_csv("data/brfss_example.csv", show_col_types = FALSE)

# Optional check
glimpse(sim_brfss)

# ------------------------------
# 2. Recode variables
# ------------------------------
sim_brfss <- sim_brfss %>%
  mutate(
    # Only cc_cat2 is treated as a factor
    cc_cat2 = factor(cc_cat2, levels = c("0","1","2","3+")),
    cc_cat2 = relevel(cc_cat2, ref = "0")
  )

# ------------------------------
# 3. Adjust Survey Design
# ------------------------------
sim_brfss <- sim_brfss %>%
  mutate(STSTR2 = as.character(STSTR))

singleton_strata <- names(table(sim_brfss$STSTR2))[table(sim_brfss$STSTR2) == 1]
sim_brfss$STSTR2[sim_brfss$STSTR2 %in% singleton_strata] <- "singleton"

options(survey.lonely.psu = "adjust")

sim_design <- svydesign(
  ids     = ~PSU,
  strata  = ~STSTR2,
  weights = ~LLCPWT,
  data    = sim_brfss,
  nest    = TRUE
)

# ------------------------------
# 4. Weighted Logistic Models
# ------------------------------
design_routine_sim <- subset(
  sim_design,
  !is.na(routine_care) &
    !is.na(cc_cat2) &
    !is.na(AGEG5YR) &
    !is.na(SEXVAR) &
    !is.na(RACE) &
    !is.na(EDUCAG) &
    !is.na(INCOMG1) &
    !is.na(HLTHPL2)
)

model_routine_sim <- svyglm(
  routine_care ~ cc_cat2 + AGEG5YR + SEXVAR + RACE + EDUCAG + INCOMG1 + HLTHPL2,
  design = design_routine_sim,
  family = quasibinomial()
)

design_cost_sim <- subset(
  sim_design,
  !is.na(cost_barrier) &
    !is.na(cc_cat2) &
    !is.na(AGEG5YR) &
    !is.na(SEXVAR) &
    !is.na(RACE) &
    !is.na(EDUCAG) &
    !is.na(INCOMG1) &
    !is.na(HLTHPL2)
)

model_cost_sim <- svyglm(
  cost_barrier ~ cc_cat2 + AGEG5YR + SEXVAR + RACE + EDUCAG + INCOMG1 + HLTHPL2,
  design = design_cost_sim,
  family = quasibinomial()
)

# ------------------------------
# 5. Extract Predicted Probabilities
# ------------------------------
pred_routine_sim <- ggpredict(model_routine_sim, terms = "cc_cat2") %>%
  mutate(x = factor(x, levels = c("0","1","2","3+"))) %>%
  filter(!is.na(x))

pred_cost_sim <- ggpredict(model_cost_sim, terms = "cc_cat2") %>%
  mutate(x = factor(x, levels = c("0","1","2","3+"))) %>%
  filter(!is.na(x))

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
      OR       = round(OR,2),
      CI_lower = round(CI_lower,2),
      CI_upper = round(CI_upper,2),
      p_value  = signif(p_value,3)
    )
}

routine_or <- extract_or(model_routine_sim, TRUE)
cost_or    <- extract_or(model_cost_sim, TRUE)

github_dir <- ".../tables/"

write.csv(routine_or, file = paste0(github_dir, "routine_or.csv"), row.names = FALSE)
write.csv(cost_or, file = paste0(github_dir, "cost_or.csv"), row.names = FALSE)

# ------------------------------
# 7. Figure 1: Routine Checkup
# ------------------------------
ggplot(pred_routine_sim, aes(x = x, y = predicted)) +
  geom_col(fill = viridis(1, option = "C", alpha = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.25, color = "gray20", linewidth = 1.5) +
  geom_text(aes(label = percent(predicted, accuracy = 1),
                y = conf.high + 0.02),
            size = 4, fontface = "bold") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, max(pred_routine_sim$conf.high)*1.1)) +
  labs(
    x = "Number of Chronic Conditions",
    y = NULL,
    title = "Figure 1: Adjusted Predicted Probability of Routine Checkup by Multimorbidity",
    caption = "Simulated data example; survey-weighted logistic regression with 95% CI"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face="bold", size=18, hjust=0.5),
    axis.title.x = element_text(face="bold"),
    axis.text.x = element_text(color="black", size=14),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# ------------------------------
# 8. Figure 2: Cost Barrier
# ------------------------------
ggplot(pred_cost_sim, aes(x = x, y = predicted)) +
  geom_col(fill = viridis(1, option = "D", alpha = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.25, color = "gray20", linewidth = 1.5) +
  geom_text(aes(label = percent(predicted, accuracy = 1),
                y = conf.high + 0.005),
            size = 4, fontface = "bold") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, max(pred_cost_sim$conf.high)*1.1)) +
  labs(
    x = "Number of Chronic Conditions",
    y = NULL,
    title = "Figure 2: Adjusted Predicted Probability of Cost Barrier by Multimorbidity",
    caption = "Simulated data example; survey-weighted logistic regression with 95% CI"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face="bold", size=18, hjust=0.5),
    axis.title.x = element_text(face="bold"),
    axis.text.x = element_text(color="black", size=14),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# ------------------------------
# 9. Save Simulated Dataset
# ------------------------------
write_csv(sim_brfss, ".../sim_brfss.csv")


# ------------------------------
# 10. Multicollinearity
# ------------------------------

set.seed(123)
small_brfss <- sim_brfss %>% sample_n(10000)

small_design <- svydesign(
  ids     = ~PSU,
  strata  = ~STSTR2,
  weights = ~LLCPWT,
  data    = small_brfss,
  nest    = TRUE
)

# ------------------------------
# Survey-weighted logistic models
# ------------------------------
small_model_routine <- svyglm(
  routine_care ~ cc_cat2 + AGEG5YR + SEXVAR + RACE + EDUCAG + INCOMG1 + HLTHPL2,
  design = small_design,
  family = quasibinomial()
)

small_model_cost <- svyglm(
  cost_barrier ~ cc_cat2 + AGEG5YR + SEXVAR + RACE + EDUCAG + INCOMG1 + HLTHPL2,
  design = small_design,
  family = quasibinomial()
)

library(svydiags)

# Build numeric predictor matrix for routine care model
X_routine <- model.matrix(~ cc_cat2 + AGEG5YR + SEXVAR + RACE + EDUCAG + INCOMG1 + HLTHPL2,
                          data = model.frame(small_model_routine))
X_routine <- X_routine[, colnames(X_routine) != "(Intercept)"]

# Survey weights vector
w <- weights(small_design)

# VIF with survey adjustment
svy_vif_routine <- svyvif(
  mobj  = small_model_routine,
  X     = X_routine,
  w     = w,
  stvar = "STSTR2",
  clvar = "PSU"
)

print(svy_vif_routine)

# ------------------------------
# 11. Model Discrimination (ROC / AUC)
# ------------------------------
library(pROC)

# --- Routine care ---
pred_probs_routine <- predict(small_model_routine, type = "response")
roc_routine <- roc(small_brfss$routine_care, pred_probs_routine, weights = small_brfss$LLCPWT)
cat("AUC - Routine care:", auc(roc_routine), "\n")
plot(roc_routine, col = "blue", lwd = 2, main = "ROC Curve - Routine Care")

# --- Cost barrier ---
pred_probs_cost <- predict(small_model_cost, type = "response")
roc_cost <- roc(small_brfss$cost_barrier, pred_probs_cost, weights = small_brfss$LLCPWT)
cat("AUC - Cost barrier:", auc(roc_cost), "\n")
plot(roc_cost, col = "red", lwd = 2, main = "ROC Curve - Cost Barrier")


# ------------------------------
# 12. Calibration
# ------------------------------
# predicted probabilities
small_brfss$pred_routine <- predict(small_model_routine, type = "response")
small_brfss$pred_cost    <- predict(small_model_cost, type = "response")

# create 10 bins of predicted probabilities for routine care
small_brfss <- small_brfss %>%
  mutate(
    decile_routine = ntile(pred_routine, 10),
    decile_cost    = ntile(pred_cost, 10)
  )

library(dplyr)

# routine care calibration
calib_routine <- small_brfss %>%
  group_by(decile_routine) %>%
  summarise(
    obs = sum(routine_care * LLCPWT) / sum(LLCPWT),
    pred = mean(pred_routine)
  )

# cost barrier calibration
calib_cost <- small_brfss %>%
  group_by(decile_cost) %>%
  summarise(
    obs = sum(cost_barrier * LLCPWT) / sum(LLCPWT),
    pred = mean(pred_cost)
  )

library(ggplot2)

# routine care
ggplot(calib_routine, aes(x = pred, y = obs)) +
  geom_point(color = "blue", size = 3) +
  geom_line(color = "blue", linewidth = 1) +   # updated
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    x = "Mean Predicted Probability",
    y = "Observed Probability",
    title = "Calibration Plot - Routine Care"
  ) +
  theme_minimal(base_size = 14)

# cost barrier
ggplot(calib_cost, aes(x = pred, y = obs)) +
  geom_point(color = "red", size = 3) +
  geom_line(color = "red", linewidth = 1) +   # updated
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    x = "Mean Predicted Probability",
    y = "Observed Probability",
    title = "Calibration Plot - Cost Barrier"
  ) +
  theme_minimal(base_size = 14)


# ------------------------------
# 12. Pseudo R^2
# ------------------------------

pseudoR2_svyglm <- function(model, design) {
  # Null model (intercept only)
  null_model <- update(model, . ~ 1)
  
  # Log-likelihoods
  ll_full <- as.numeric(logLik(model))
  ll_null <- as.numeric(logLik(null_model))
  
  # McFadden R^2
  r2_mcf <- 1 - (ll_full / ll_null)
  return(r2_mcf)
}

# Example for routine care
r2_routine <- pseudoR2_svyglm(small_model_routine, small_design)
cat("McFadden Pseudo R^2 - Routine Care:", r2_routine, "\n")

# Example for cost barrier
r2_cost <- pseudoR2_svyglm(small_model_cost, small_design)
cat("McFadden Pseudo R^2 - Cost Barrier:", r2_cost, "\n")


# ------------------------------
# 12. Goodness-of-fit
# ------------------------------

library(dplyr)

# --- Routine care ---
small_brfss <- small_brfss %>%
  mutate(
    decile_routine = ntile(pred_routine, 10)
  )

hl_routine <- small_brfss %>%
  group_by(decile_routine) %>%
  summarise(
    obs = sum(routine_care * LLCPWT),
    n   = sum(LLCPWT),
    exp = sum(pred_routine * LLCPWT)
  ) %>%
  mutate(
    chi_sq = (obs - exp)^2 / (exp * (1 - (exp / n)))
  )

HL_stat_routine <- sum(hl_routine$chi_sq)
df_routine <- nrow(hl_routine) - 2  # df = number of groups - 2
p_value_routine <- 1 - pchisq(HL_stat_routine, df_routine)

cat("Weighted Hosmer-Lemeshow Approx - Routine Care\n")
cat("Chi-square:", HL_stat_routine, "  df:", df_routine, "  p-value:", p_value_routine, "\n")

# --- Cost barrier ---
small_brfss <- small_brfss %>%
  mutate(
    decile_cost = ntile(pred_cost, 10)
  )

hl_cost <- small_brfss %>%
  group_by(decile_cost) %>%
  summarise(
    obs = sum(cost_barrier * LLCPWT),
    n   = sum(LLCPWT),
    exp = sum(pred_cost * LLCPWT)
  ) %>%
  mutate(
    chi_sq = (obs - exp)^2 / (exp * (1 - (exp / n)))
  )

HL_stat_cost <- sum(hl_cost$chi_sq)
df_cost <- nrow(hl_cost) - 2
p_value_cost <- 1 - pchisq(HL_stat_cost, df_cost)

cat("Weighted Hosmer-Lemeshow Approx - Cost Barrier\n")
cat("Chi-square:", HL_stat_cost, "  df:", df_cost, "  p-value:", p_value_cost, "\n")


# ------------------------------
# Check Weighted NA Proportions for All Relevant Variables
# ------------------------------

relevant_vars <- c(
  "routine_care", "cost_barrier",
  "cc_cat2", "AGEG5YR", "SEXVAR", "RACE", "EDUCAG", "INCOMG1", "HLTHPL2"
)

na_summary <- sim_brfss %>%
  select(all_of(relevant_vars)) %>%
  summarise(across(everything(), ~mean(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "NA_Fraction") %>%
  mutate(NA_Percent = scales::percent(NA_Fraction, accuracy = 0.01))

# Display in console
print(na_summary)


