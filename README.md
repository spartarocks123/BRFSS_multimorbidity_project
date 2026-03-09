# BRFSS Multimorbidity Project

**Author:** Mohammed Amish-Malik  
**Purpose:** Portfolio artifact using 2024 BRFSS data to study the associations between multimorbidity, routine healthcare utilization, and deferred care due to cost.  

**Folder Structure:**
- `data/` → Simulated or example dataset showing variable structure
- `scripts/` → R scripts for data cleaning, analysis, and visualization. SAS was used to create descriptive tables. SQL will be used later to replicate project as well. 
- `figures/` → Placeholder for plots
- `tables/` → Placeholder for output tables

**Note:** No raw BRFSS data is included. All datasets are simulated or structure-only for reproducibility.

**Methods:** Chronic Health Condition section. Collapse the conditions into multimorbidity burden category. compare against the “healthy” baseline and interpretation reasons as odds of routine care/cost barrier among people with multimorbidity relative to no chronic conditions. Heart attack (myocardial infarction) or Coronary heart disease (_MICHD) 
Stroke (CVDSTRK3) 
Asthma (_ASTHMS1) 
Any cancer (collapse Skin cancer (not melanoma) and Melanoma or any other types of cancer) (CHCSCNC1; CHCOCNC1) 
Chronic obstructive pulmonary disease), emphysema or chronic bronchitis (CHCCOPD3) 
Depressive disorder (including depression, major depression, dysthymia, or minor depression) (ADDEPEV3)
Not including kidney stones, bladder infection or incontinence, were you ever told you had kidney disease (CHCKDNY2)
Ever told) (you had) diabetes?  (If ï¿½Yesï¿½ and respondent is female, ask ï¿½Was this only when you were pregnant?ï¿½. If Respondent says pre-diabetes or borderline diabetes, use response code 4.) (DIABETE4) (Recoded in which only 1 == chronic diabetes while the others count as No since some of them only occurred during pregnancy). 

Outcomes: Primary: Had a routine care in past 12 months (Yes/No) (CHECKUP1) 
Secondary: Deferred care due to cost (Yes/No) (MEDCOST1) 

**Results:** So far, there appears to be a dose-response relationship between the number of chronic conditions and routine healthcare utilization. As the number of chronic conditions increase in an individual, the predicted probability of routine checkup increases. This is expected and correlates with what has been found in the literature. Interestingly, the predicted probability of delayed care due to cost also increases in accordance with the number of chronic conditions. All the p values are less than 0.01 and the OR for each number of chronic condition increases as well. 

Happy to receive any professional feedback—please keep it constructive and focused on analysis, methodology, or reproducibility.
