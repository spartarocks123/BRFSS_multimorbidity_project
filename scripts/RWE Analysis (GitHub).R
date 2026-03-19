# =========================================
# Simulate BRFSS Example Dataset & Analysis
# =========================================
library(tidyverse)
library(survey)
library(ggeffects)
library(viridis)
library(scales)
library(pROC)

set.seed(123)
n <- 10000L

# ------------------------------
# 1. Simulate covariates (from real distributions)
# ------------------------------

cov_vars <- c("agegrp","sex","race","educ","income","insured")

sim_brfss <- data.frame(
  lapply(cov_vars, function(v) {
    sample(brfss_keep[[v]], n, replace = TRUE)
  })
)

names(sim_brfss) <- cov_vars

# ------------------------------
# 2. Simulate exposure (cc_cat2)
# ------------------------------

sim_brfss$cc_cat2 <- sample(
  brfss_keep$cc_cat2,
  n,
  replace = TRUE
)

# ------------------------------
# 3. Ensure factor levels match original data
# ------------------------------

sim_brfss <- sim_brfss %>%
  mutate(
    across(all_of(cov_vars),
           ~ factor(.x, levels = levels(brfss_keep[[cur_column()]]))),
    cc_cat2 = factor(cc_cat2, levels = levels(brfss_keep$cc_cat2))
  )

# ------------------------------
# 4. Define simulation function (logistic model)
# ------------------------------

simulate_outcome <- function(df, beta) {
  mm <- model.matrix(~ cc_cat2 + agegrp + sex + race + educ + income + insured, data = df)
  probs <- plogis(mm %*% beta)
  rbinom(nrow(df), 1, probs)
}

# ------------------------------
# 5. Define beta coefficients
# ------------------------------
# NOTE: Length must match model.matrix columns exactly

beta_routine <- c(
  log(0.44/(1-0.44)),   # intercept
  rep(log(1.16), 3),    # cc_cat2 levels (excluding reference)
  rep(log(1.02), length(levels(sim_brfss$agegrp)) - 1),
  log(1.10),            # sex
  rep(log(1.05), length(levels(sim_brfss$race)) - 1),
  rep(log(1.03), length(levels(sim_brfss$educ)) - 1),
  rep(log(0.97), length(levels(sim_brfss$income)) - 1),
  log(0.90)             # insured
)

# ------------------------------
# 6. Simulate outcomes
# ------------------------------

sim_brfss <- sim_brfss %>%
  mutate(
    routine_care = simulate_outcome(., beta_routine)
  )

# ------------------------------
# 2. Simulate survey design variables
# ------------------------------
# replace brfss_keep$PSU, STSTR, LLCPWT with arbitrary sampling for demo
sim_brfss <- sim_brfss %>%
  mutate(PSU = sample(1:500, n, replace=TRUE),
         STSTR = sample(1:100, n, replace=TRUE),
         LLCPWT = runif(n, 0.5, 1.5),
         STSTR2 = factor(ifelse(duplicated(STSTR)|duplicated(STSTR,fromLast=TRUE), STSTR, "singleton")))

options(survey.lonely.psu="adjust")

# ------------------------------
# 3. Function to simulate outcome based on log-odds
# ------------------------------
simulate_outcome <- function(df, beta) {
  mm <- model.matrix(~ cc_cat2 + AGEG5YR + SEXVAR + RACE + EDUCAG + INCOMG1 + HLTHPL2, data=df)
  probs <- plogis(mm %*% beta)
  rbinom(nrow(df), 1, probs)
}

# ------------------------------
# Automatically compute log-odds from prevalence
# ------------------------------

# Function to convert prevalence to log-odds
prev2logit <- function(p) log(p / (1 - p))

# 1. Compute overall prevalence for each outcome
prev_routine <- 0.44  # or compute from data: mean(sim_brfss$routine_care, na.rm=TRUE)
prev_cost    <- 0.37  # or compute from data

# 2. Set intercept = log-odds of overall prevalence
#    All other covariates set to 0 → outcome prevalence matches overall target
beta_routine_auto <- c(prev2logit(prev_routine), rep(0, ncol(model.matrix(~ cc_cat2 + AGEG5YR + SEXVAR + RACE + EDUCAG + INCOMG1 + HLTHPL2, sim_brfss)))-1)
beta_cost_auto    <- c(prev2logit(prev_cost),    rep(0, ncol(model.matrix(~ cc_cat2 + AGEG5YR + SEXVAR + RACE + EDUCAG + INCOMG1 + HLTHPL2, sim_brfss)))-1)

# 3. Simulate outcomes
sim_brfss <- sim_brfss %>%
  mutate(
    routine_care = simulate_outcome(., beta_routine_auto),
    cost_barrier = simulate_outcome(., beta_cost_auto)
  )

# ------------------------------
# 4. Survey design
# ------------------------------
sim_design <- svydesign(ids=~PSU, strata=~STSTR2, weights=~LLCPWT, data=sim_brfss, nest=TRUE)

# ------------------------------
# 5. Fit weighted logistic models
# ------------------------------
fit_svyglm <- function(outcome, design, extra_cov=NULL){
  formula <- as.formula(paste(outcome,"~ cc_cat2 + AGEG5YR + SEXVAR + RACE + EDUCAG + INCOMG1 + HLTHPL2", if(!is.null(extra_cov)) paste("+", extra_cov)))
  svyglm(formula, design=design, family=quasibinomial())
}

models <- list(
  routine_main = fit_svyglm("routine_care", sim_design),
  cost_main    = fit_svyglm("cost_barrier", sim_design)
)

# ------------------------------
# 6. Extract ORs
# ------------------------------
extract_or <- function(model, drop_intercept=TRUE){
  coefs <- summary(model)$coefficients
  res <- tibble(
    Variable = rownames(coefs),
    OR       = exp(coefs[, "Estimate"]),
    CI_lower = exp(coefs[, "Estimate"] - 1.96*coefs[, "Std. Error"]),
    CI_upper = exp(coefs[, "Estimate"] + 1.96*coefs[, "Std. Error"]),
    p_value  = coefs[, "Pr(>|t|)"]
  )
  if(drop_intercept) res <- filter(res, Variable!="(Intercept)")
  res %>% mutate(across(c(OR, CI_lower, CI_upper), ~round(.,2)), p_value=signif(p_value,3))
}

or_results <- map(models, extract_or)

# ------------------------------
# 7. Predicted probabilities for plotting
# ------------------------------
pred_probs <- map(models, ~ggpredict(.x, terms="cc_cat2") %>% mutate(x=factor(x, levels=c("0","1","2","3+"))))

# ------------------------------
# 8. Figure 1: Routine Checkup
# ------------------------------
ggplot(pred_routine_sim, aes(x = x, y = predicted)) +
  geom_col(fill = viridis(1, option = "C", alpha = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25, color = "gray20", linewidth = 1.5) +
  geom_text(aes(label = percent(predicted, accuracy = 1), y = conf.high + 0.02),
            size = 4, fontface = "bold") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, max(pred_routine_sim$conf.high)*1.1)) +
  labs(x = "Number of Chronic Conditions", y = NULL,
       title = "Figure 1: Adjusted Predicted Probability of Routine Checkup by Multimorbidity",
       caption = "Simulated data example; survey-weighted logistic regression with 95% CI") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face="bold", size=18, hjust=0.5),
        axis.title.x = element_text(face="bold"),
        axis.text.x = element_text(color="black", size=14),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# ------------------------------
# 9. Figure 2: Cost Barrier
# ------------------------------
ggplot(pred_cost_sim, aes(x = x, y = predicted)) +
  geom_col(fill = viridis(1, option = "D", alpha = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25, color = "gray20", linewidth = 1.5) +
  geom_text(aes(label = percent(predicted, accuracy = 1), y = conf.high + 0.005),
            size = 4, fontface = "bold") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, max(pred_cost_sim$conf.high)*1.1)) +
  labs(x = "Number of Chronic Conditions", y = NULL,
       title = "Figure 2: Adjusted Predicted Probability of Cost Barrier by Multimorbidity",
       caption = "Simulated data example; survey-weighted logistic regression with 95% CI") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face="bold", size=18, hjust=0.5),
        axis.title.x = element_text(face="bold"),
        axis.text.x = element_text(color="black", size=14),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# ------------------------------
# 8. ROC / AUC function
# ------------------------------
plot_roc_auc <- function(model, data, outcome, color){
  probs <- predict(model, type="response")
  roc_obj <- roc(data[[outcome]], probs, weights=data$LLCPWT)
  plot(roc_obj, col=color, lwd=2, main=paste("ROC -", outcome))
  auc(roc_obj)
}

auc_routine <- plot_roc_auc(models$routine_main, sim_brfss, "routine_care","blue")
auc_cost    <- plot_roc_auc(models$cost_main, sim_brfss, "cost_barrier","red")

# ------------------------------
# 9. Calibration plots
# ------------------------------
calibration_plot <- function(model, data, outcome, color){
  data <- data %>% mutate(pred = predict(model, type="response"),
                          decile = ntile(pred, 10)) %>%
    group_by(decile) %>%
    summarise(obs = sum(.data[[outcome]]*LLCPWT)/sum(LLCPWT),
              pred = mean(pred))
  ggplot(data, aes(x=pred,y=obs)) +
    geom_point(color=color, size=3) +
    geom_line(color=color, linewidth=1) +
    geom_abline(intercept=0,slope=1, linetype="dashed", color="gray") +
    labs(x="Mean Predicted Probability", y="Observed Probability",
         title=paste("Calibration -", outcome)) +
    theme_minimal(base_size=14)
}

calibration_plot(models$routine_main, sim_brfss, "routine_care","blue")
calibration_plot(models$cost_main, sim_brfss, "cost_barrier","red")

# ------------------------------
# 10. Sensitivity analyses (gender & insurance)
# ------------------------------
subset_designs <- list(
  male = subset(sim_design, SEXVAR==1),
  female = subset(sim_design, SEXVAR==2),
  insured = subset(sim_design, HLTHPL2==2),
  uninsured = subset(sim_design, HLTHPL2==1)
)

sensitivity_models <- map(subset_designs, function(dsg){
  list(
    routine = fit_svyglm("routine_care", dsg),
    cost    = fit_svyglm("cost_barrier", dsg)
  )
})

sensitivity_or <- map(sensitivity_models, ~map(.x, extract_or))


