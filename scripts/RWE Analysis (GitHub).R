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
# 1. Covariates and chronic conditions
# ------------------------------
cov_probs <- sim_brfss %>%
  select(AGEG5YR, SEXVAR, RACE, EDUCAG, INCOMG1, HLTHPL2) %>%
  map(~ prop.table(table(.x)) %>% as.numeric())

sim_brfss <- map_dfc(cov_probs, ~sample(seq_along(.x), n, replace=TRUE, prob=.x)) %>%
  mutate(across(everything(), factor))


cc_probs <- c(0.068,0.035,0.103,0.056,0.084,0.063,0.21,0.041,0.125)
cc_vars <- c("cc_mi","cc_stroke","cc_asthma","cc_skin_ca","cc_other_ca","cc_copd","cc_depress","cc_ckd","cc_diabetes")
sim_brfss[cc_vars] <- map_dfc(cc_probs, ~rbinom(n,1,.x))
sim_brfss <- sim_brfss %>%
  mutate(cc_count = rowSums(across(all_of(cc_vars))),
         cc_cat2  = cut(cc_count, breaks=c(-1,0,1,2,Inf), labels=c("0","1","2","3+")))

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
  rbinom(nrow(df),1,probs)
}

# Example beta vectors (intercept + dummy variables in model.matrix order)
beta_routine <- c(log(0.44/(1-0.44)), rep(log(1.16),13), log(1.53), rep(log(0.98),8), rep(log(1.09),4), rep(log(1.04),7), log(0.98))
beta_cost    <- c(log(0.37/(1-0.37)), rep(log(0.86),13), log(1.21), rep(log(1.09),8), rep(log(0.82),4), rep(log(0.89),7), log(1.04))

sim_brfss <- sim_brfss %>%
  mutate(
    routine_care = simulate_outcome(., beta_routine),
    cost_barrier = simulate_outcome(., beta_cost)
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


