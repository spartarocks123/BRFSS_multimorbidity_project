CREATE TABLE brfss_cleaned
  SELECT *,
  FROM brfss_example
    -- Multimorbidity count (handles NULLs correctly)
    (
      COALESCE(cc_mi, 0) +
      COALESCE(cc_stroke, 0) +
      COALESCE(cc_asthma, 0) +
      COALESCE(cc_copd, 0) +
      COALESCE(cc_diabetes, 0) +
      COALESCE(cc_ckd, 0) +
      COALESCE(cc_depress, 0)
    ) AS cc_count

SELECT
  *,

  -- Multimorbidity category
  CASE
    WHEN cc_count = 0 THEN '0'
    WHEN cc_count = 1 THEN '1'
    WHEN cc_count = 2 THEN '2'
    ELSE '3+'
  END AS cc_cat2,

  -- Binary encoding: routine care
  CASE 
    WHEN routine_care = 'Yes' THEN 1
    WHEN routine_care = 'No' THEN 0
    ELSE NULL
  END AS routine_binary,

  -- Binary encoding: cost barrier
  CASE 
    WHEN cost_barrier = 'Yes' THEN 1
    WHEN cost_barrier = 'No' THEN 0
    ELSE NULL
  END AS cost_binary

  -- Remove missing values (complete-case logic) invalid BRFSS codes

FROM temporary_table
WHERE
  agegrp IS NOT NULL
  AND sex IS NOT NULL
  AND race IS NOT NULL
  AND educ IS NOT NULL
  AND income IS NOT NULL
  AND insured IS NOT NULL
  AND agegrp != '14'
  AND race != '9'
  AND educ != '9'
  AND income != '9';

