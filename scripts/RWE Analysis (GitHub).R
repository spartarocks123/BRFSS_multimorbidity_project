# ==========================================================
# Simulated Dataset Analysis (Replication of BRFSS Workflow)
# NA-free version
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
sim_br <- read_csv("data/brfss_example.csv", show_col_types = FALSE)

# ------------------------------
# 2. Adjust Survey Design
# ------------------------------
sim_br <- sim_br %>%
  mutate(STSTR2 = as.character(STSTR))

singleton_strata <- names(table(sim_br$STSTR2))[table(sim_br$STSTR2) == 1]
sim_br$STSTR2[sim_br$STSTR2 %in% singleton_strata] <- "singleton"

options(survey.lonely.psu = "adjust")
sim_design <- svydesign(
  ids     = ~PSU,
  strata  = ~STSTR2,
  weights = ~LLCPWT,
  data    = sim_br,
  nest    = TRUE
)

# ------------------------------
# 3. Ensure Correct Factor Order
# ------------------------------
sim_br <- sim_br %>%
  mutate(
    cc_cat2 = factor(cc_cat2, levels = c("0","1","2","3+")),
    cc_cat2 = relevel(cc_cat2, ref = "0"),
    agegrp  = factor(agegrp),
    sex     = factor(sex),
    race    = factor(race),
    educ    = factor(educ),
    income  = factor(income),
    insured = factor(insured)
  ) %>%
  # Simulate outcomes dependent on multimorbidity
  mutate(
    p_cost = case_when(
      cc_cat2 == "0"  ~ 0.06,
      cc_cat2 == "1"  ~ 0.09,
      cc_cat2 == "2"  ~ 0.12,
      cc_cat2 == "3+" ~ 0.15,
      TRUE            ~ 0.09
    ),
    p_rout = case_when(
      cc_cat2 == "0"  ~ 0.78,
      cc_cat2 == "1"  ~ 0.80,
      cc_cat2 == "2"  ~ 0.82,
      cc_cat2 == "3+" ~ 0.84,
      TRUE            ~ 0.80
    ),
    cost_barrier = rbinom(n(), size = 1, prob = p_cost),
    routine_care = rbinom(n(), size = 1, prob = p_rout)
  ) %>%
  select(-p_cost, -p_rout)

# ------------------------------
# 4. Rebuild Survey Design with New Outcomes
# ------------------------------
sim_design <- update(
  sim_design,
  cc_cat2      = sim_br$cc_cat2,
  agegrp       = sim_br$agegrp,
  sex          = sim_br$sex,
  race         = sim_br$race,
  educ         = sim_br$educ,
  income       = sim_br$income,
  insured      = sim_br$insured,
  routine_care = sim_br$routine_care,
  cost_barrier = sim_br$cost_barrier
)

# ------------------------------
# 5. Weighted Logistic Models
# ------------------------------
design_routine_sim <- subset(sim_design,
                             !is.na(routine_care) &
                               !is.na(cc_cat2) &
                               !is.na(agegrp) &
                               !is.na(sex) &
                               !is.na(race) &
                               !is.na(educ) &
                               !is.na(income) &
                               !is.na(insured)
)

model_routine_sim <- svyglm(
  routine_care ~ cc_cat2 + agegrp + sex + race + educ + income + insured,
  design = design_routine_sim,
  family = quasibinomial()
)

design_cost_sim <- subset(sim_design,
                          !is.na(cost_barrier) &
                            !is.na(cc_cat2) &
                            !is.na(agegrp) &
                            !is.na(sex) &
                            !is.na(race) &
                            !is.na(educ) &
                            !is.na(income) &
                            !is.na(insured)
)

model_cost_sim <- svyglm(
  cost_barrier ~ cc_cat2 + agegrp + sex + race + educ + income + insured,
  design = design_cost_sim,
  family = quasibinomial()
)

# ------------------------------
# 6. Extract Predicted Probabilities
# ------------------------------
pred_routine_sim <- ggpredict(model_routine_sim, terms = "cc_cat2") %>%
  mutate(x = factor(x, levels = c("0","1","2","3+"))) %>%
  filter(!is.na(x))  # remove any residual NA

pred_cost_sim <- ggpredict(model_cost_sim, terms = "cc_cat2") %>%
  mutate(x = factor(x, levels = c("0","1","2","3+"))) %>%
  filter(!is.na(x))  # remove any residual NA

# ------------------------------
# 7. Optional: Extract Odds Ratios
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
    mutate(OR = round(OR,2), CI_lower = round(CI_lower,2), CI_upper = round(CI_upper,2),
           p_value = signif(p_value,3))
}

routine_or <- extract_or(model_routine_sim, TRUE)
cost_or    <- extract_or(model_cost_sim, TRUE)

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


