/* ==========================================
   Descriptive Summary of BRFSS Variables using survey weights and covariates
   ========================================== */

/* Step 1: Import the cleaned CSV dataset */
proc import datafile=".../sim_brfss.csv"
    out=brfss_keep
    dbms=csv
    replace;
    getnames=yes;
run;

/* Optional: Check dataset contents */
proc contents data=brfss_keep; 
run;

/* --- PDF Output for Covariates --- */
ods pdf file=".../covariates.pdf";

title "Weighted Distribution of Categorical Covariates by Multimorbidity Category";
proc surveyfreq data=brfss_keep;
    strata STSTR;
    cluster PSU;
    weight LLCPWT;
    tables cc_cat2*(SEXVAR RACE EDUCAG INCOMG1 HLTHPL2) / row cl;
run;

ods pdf close;

/* --- PDF Output for Outcomes --- */
ods pdf file=".../outcomes.pdf";

title "Weighted Distribution of Outcomes by Multimorbidity Category";
proc surveyfreq data=brfss_keep;
    strata STSTR;
    cluster PSU;
    weight LLCPWT;
    tables cc_cat2*(routine_care cost_barrier) / row cl;
run;

ods pdf close;
