-- Create table with explicit column types and basic constraints
CREATE TABLE brfss_cleaned (
    id INT PRIMARY KEY,                  -- unique respondent ID
    agegrp VARCHAR(2),
    sex VARCHAR(10),
    race VARCHAR(2),
    educ VARCHAR(2),
    income VARCHAR(2),
    insured VARCHAR(3),
    routine_care VARCHAR(3),
    cost_barrier VARCHAR(3),
    cc_mi TINYINT DEFAULT 0,
    cc_stroke TINYINT DEFAULT 0,
    cc_asthma TINYINT DEFAULT 0,
    cc_copd TINYINT DEFAULT 0,
    cc_diabetes TINYINT DEFAULT 0,
    cc_ckd TINYINT DEFAULT 0,
    cc_depress TINYINT DEFAULT 0,
    cc_count TINYINT,
    cc_cat2 VARCHAR(20),
    routine_binary TINYINT,
    cost_binary TINYINT
);

-- Insert sample rows manually
INSERT INTO brfss_cleaned (
    id, agegrp, sex, race, educ, income, insured,
    routine_care, cost_barrier,
    cc_mi, cc_stroke, cc_asthma, cc_copd, cc_diabetes, cc_ckd, cc_depress,
    cc_count, cc_cat2, routine_binary, cost_binary
)
VALUES 
(1, '01', 'Male', '01', '03', '05', 'Yes', 'Yes', 'No', 1,0,0,1,0,0,0, 2, '2', 1, 0),
(2, '02', 'Female', '02', '04', '07', 'No', 'No', 'Yes', 0,1,1,0,1,1,1, 5, '+3', 0, 1);

-- Example filtering rows
SELECT *
FROM brfss_cleaned
WHERE agegrp IS NOT NULL
  AND sex IS NOT NULL
  AND race IS NOT NULL
  AND educ IS NOT NULL 
  AND income IS NOT NULL
  AND insured IS NOT NULL;

-- Example of multimorbidity count using COALESCE()
SELECT id,
       (COALESCE(cc_mi,0) + COALESCE(cc_stroke,0) + COALESCE(cc_asthma,0) +
        COALESCE(cc_copd,0) + COALESCE(cc_diabetes,0) + COALESCE(cc_ckd,0) +
        COALESCE(cc_depress,0)) AS cc_count
FROM brfss_cleaned;

-- CASE-WHEN for feature engineering
SELECT 
    cc_count,
    CASE 
        WHEN cc_count = 1 THEN '1'
        WHEN cc_count = 2 THEN '2'
        ELSE '+3'
    END AS cc_cat2
FROM brfss_cleaned;

-- CASE-WHEN for feature engineering (permanent version)
UPDATE brfss_cleaned
SET cc_cat2 = CASE
    WHEN cc_count = 1 THEN '1'
    WHEN cc_count = 2 THEN '2'
    ELSE '+3'
END;

-- Aggregation & Sorting
SELECT COUNT(*)
FROM brfss_cleaned

SELECT sex,
COUNT(*)
FROM brfss_cleaned
GROUP BY sex;

SELECT MIN(cc_count)
FROM brfss_cleaned;

SELECT MAX(cc_count)
FROM brfss_cleaned;


SELECT AVG(cc_count)
FROM brfss_cleaned;

SELECT *
FROM brfss_cleaned
WHERE cc_count > (
    SELECT AVG(cc_count) FROM brfss_cleaned
);

SELECT 
    SUM(CASE WHEN cc_count = 1 THEN 1 ELSE 0 END) AS n1,
    SUM(CASE WHEN cc_count = 2 THEN 1 ELSE 0 END) AS n2,
    SUM(CASE WHEN cc_count >= 3 THEN 1 ELSE 0 END) AS n3plus
FROM brfss_cleaned;

