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
write_csv(sim_brfss, "/Users/moh/Desktop/Research Assistant/BRFSS_multimorbidity_project/data/sim_brfss.csv")

