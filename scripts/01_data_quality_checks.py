# ------------------------------
# 01_data_quality_checks.py
# ------------------------------
import pandas as pd
from pathlib import Path

 # Project paths
project_path = Path.cwd()
data_path = project_path / "data" / "brfss_clean.csv"
output_path = project_path / "tables" / "python_data_quality_report.csv"
 
 # Load cleaned dataset
df = pd.read_csv(data_path)
 
 # Basic dataset checks
print("Rows:", df.shape[0])
print("Columns:", df.shape[1])
print("\nColumn names:")
print(df.columns.tolist())
 
 # Expected columns
expected_columns = [
    "routine_care",
    "cost_barrier",
    "cc_count",
    "cc_cat2",
    "insured",
    "agegrp",
    "sex",
    "race",
    "educ",
    "income",
    "LLCPWT",
    "STSTR",
    "PSU"
]

missing_columns = [col for col in expected_columns if col not in df.columns]

print("\nMissing expected columns:")
print(missing_columns)

# Missingness report
missing_report = (
    df.isna()
    .sum()
    .reset_index()
)

missing_report.columns = ["variable", "missing_count"]
missing_report["missing_percent"] = round(
    missing_report["missing_count"] / len(df) * 100, 2
)

print("\nMissingness report:")
print(missing_report)

# Duplicate rows
duplicate_count = df.duplicated().sum()
print("\nDuplicate rows:", duplicate_count)

# Outcome prevalence
print("\nRoutine care prevalence:")
print(df["routine_care"].value_counts(normalize=True, dropna=False))

print("\nCost barrier prevalence:")
print(df["cost_barrier"].value_counts(normalize=True, dropna=False))

# Multimorbidity distribution
print("\nMultimorbidity category distribution:")
print(df["cc_cat2"].value_counts(normalize=True, dropna=False))

# Save report
missing_report.to_csv(output_path, index=False)

print("\nData quality report saved to:")
print(output_path)
