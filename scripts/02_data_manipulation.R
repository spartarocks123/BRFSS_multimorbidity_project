
source("scripts/01_load_data.R")

# ------------------------------
# Clean + Recode (FIXED)
# ------------------------------
brfss <- brfss %>%
  select(
    MICHD, CVDSTRK3, ASTHMS1, CHCSCNC1, CHCOCNC1,
    CHCCOPD3, ADDEPEV3, CHCKDNY2, DIABETE4,
    CHECKUP1, MEDCOST1, HLTHPL2,
    AGEG5YR, SEXVAR, RACE, EDUCAG, INCOMG1,
    LLCPWT, STSTR, PSU
  ) %>%
  
  # ---- FIX 1: force labelled variables into stable numeric form ----
mutate(across(
  c(MICHD, CVDSTRK3, ASTHMS1, CHCSCNC1, CHCOCNC1,
    CHCCOPD3, ADDEPEV3, CHCKDNY2, DIABETE4,
    CHECKUP1, MEDCOST1, HLTHPL2,
    AGEG5YR, SEXVAR, RACE, EDUCAG, INCOMG1),
  ~ as.numeric(as.character(.))
)) %>%
  
  # ------------------------------
# Chronic conditions (safe recode)
# ------------------------------
mutate(
  across(
    c(MICHD, CVDSTRK3, ASTHMS1, CHCSCNC1, CHCOCNC1,
      CHCCOPD3, ADDEPEV3, CHCKDNY2, DIABETE4),
    ~ case_when(
      . == 1 ~ 1,
      . == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    .names = "cc_{.col}"
  )
) %>%
  
  # ------------------------------
# Derived variables
# ------------------------------
mutate(
  cc_count = rowSums(across(starts_with("cc_")), na.rm = TRUE),
  
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
  
  routine_care = case_when(
    CHECKUP1 == 1 ~ 1,
    CHECKUP1 %in% c(2, 3, 4, 8) ~ 0,
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
  
  filter(!is.na(routine_care), !is.na(cost_barrier))

# ------------------------------
# Keep Only Clean Variables
# ------------------------------
brfss_clean <- brfss %>%
  select(
    routine_care,
    cost_barrier,
    cc_count,
    cc_cat2,
    insured,
    agegrp,
    sex,
    race,
    educ,
    income,
    LLCPWT, STSTR, PSU
  )

# ------------------------------
# Save cleaned dataset
# ------------------------------
dir.create("data", showWarnings = FALSE, recursive = TRUE)

write.csv(brfss_clean, "/Users/moh/Desktop/Research Assistant/BRFSS_multimorbidity_project/data/brfss_example.csv", row.names = FALSE)


# ==========================================================
# Derived Dataset Analysis (GitHub)
# ==========================================================

# ------------------------------
# Load + Subsample Dataset
# ------------------------------
derv_br <- read_csv("/Users/moh/Desktop/Research Assistant/BRFSS_multimorbidity_project/data/brfss_example.csv", show_col_types = FALSE)

# ------------------------------
# Ensure Correct Factor Order
# ------------------------------

#This code helps set the reference = category representing the largest weighted population

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
    !(agegrp=="14"|
        race=="9"|
        educ=="9"|
        income=="9")
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

