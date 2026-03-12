# ------------------------------
# Simulate Example BRFSS Dataset (Logit-Scale Probabilities, Covariates as Single Variables)
# ------------------------------
library(haven)
library(tidyverse)
library(survey)
library(ggeffects)
library(scales)
library(viridis)
library(car)
library(pROC)
install.packages("ResourceSelection")   # run once if not installed
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
  
  # ------------------------------
  # Covariates
  # ------------------------------
  sim_brfss <- tibble(
    cc_mi       = bern(0.10),
    cc_stroke   = bern(0.05),
    cc_asthma   = bern(0.15),
    cc_skin_ca  = bern(0.02),
    cc_other_ca = bern(0.03),
    cc_copd     = bern(0.10),
    cc_depress  = bern(0.20),
    cc_ckd      = bern(0.05),
    cc_diabetes = bern(0.10),
    
    AGEG5YR = sample(1:13, n, replace = TRUE),
    SEXVAR  = sample(c(1,2), n, replace = TRUE),
    RACE    = sample(1:5, n, replace = TRUE),
    EDUCAG  = sample(1:6, n, replace = TRUE),
    INCOMG1 = sample(1:8, n, replace = TRUE),
    HLTHPL2 = sample(c(1,2), n, replace = TRUE),
    
    LLCPWT = runif(n, 0.5, 2),
    PSU    = sample(1:250, n, replace = TRUE),
    STSTR  = sample(1:100, n, replace = TRUE)
  )
  
  # ------------------------------
  # Derived variables
  # ------------------------------
  sim_brfss <- sim_brfss %>%
    mutate(
      cc_count = rowSums(across(cc_mi:cc_diabetes)),
      cc_cat2 = case_when(
        cc_count == 0 ~ "0",
        cc_count == 1 ~ "1",
        cc_count == 2 ~ "2",
        cc_count >= 3 ~ "3+"
      ),
      cc_cat2 = factor(cc_cat2, levels = c("0","1","2","3+"))
    )
  
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
}



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


# Residual and null deviance from svyglm
res_dev  <- model_routine_sim$deviance
null_dev <- model_routine_sim$null.deviance

# McFadden-style pseudo-R²
pseudo_r2_routine <- 1 - (res_dev / null_dev)
pseudo_r2_routine

res_dev_cost  <- model_cost_sim$deviance
null_dev_cost <- model_cost_sim$null.deviance
pseudo_r2_cost <- 1 - (res_dev_cost / null_dev_cost)
pseudo_r2_cost



