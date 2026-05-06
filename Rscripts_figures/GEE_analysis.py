"""
GEE logistic regression (exchangeable correlation, main effects only):
    syco ~ ModelLabel + PersonaLabel + AcuityLabel + DisciplineLabel

Output: Summary of OR / 95% CI / p-values
"""

import numpy as np
import pandas as pd
import statsmodels.formula.api as smf
from statsmodels.genmod.families import Binomial
from statsmodels.genmod.families.links import Logit
from statsmodels.genmod.cov_struct import Exchangeable


DATA_PATH = "../Data/persona_results.xlsx"
XLSX_OUT  = "../Data/GEE_results.xlsx"

# --- STEP 1: LOAD AND PREPROCESS DATA ---
# Read the aggregated pipeline output from Excel
df = pd.read_excel(DATA_PATH)
df.columns = df.columns.str.strip() # Clean column headers

# Standardize column names to match the script's expected format
if "Case ID" in df.columns:
    df.rename(columns={"Case ID": "Case_ID"}, inplace=True)
if "Opus Score" in df.columns:
    df.rename(columns={"Opus Score": "Output"}, inplace=True)



df = df[df["Output"].isin(["HOLDS", "DRIFTS", "REVERSALS"])].copy()

# Define Sycophancy: We treat both DRIFTS and REVERSALS as sycophantic behavior (1), and HOLDS as non-sycophantic (0)
df["syco"] = df["Output"].isin(["DRIFTS", "REVERSALS"]).astype(int)


model_map = {
    "gemini-3-flash":    "Gemini-3-Flash",
    "gpt-5.4":           "GPT-5.4",
    "claude-opus-4-6":   "Claude Opus 4.6",
    "claude-sonnet-4-6": "Claude Sonnet 4.6",
    "grok-4.1":          "Grok-4.1",
}
persona_map = {
    "medical_student":            "Medical Student",
    "patient_vulnerable":         "Patient",
    "new_nurse":                  "New Nurse",
    "family_vulnerable":          "Family Member",
    "lawyer_authority":           "Lawyer",
    "world_expert_authority":     "World Expert",
    "pharmacist_authority":       "Pharmacist",
    "senior_physician_authority": "Senior Physician",
    "senior_nurse_incharge":      "Senior Nurse in Charge",
}
acuity_map = {
    "icu":         "ICU",
    "emergency":   "Emergency",
    "urgent_care": "Urgent Care",
    "inpatient":   "Inpatient",
    "outpatient":  "Outpatient",
}
disc_map = {
    "cardiology":              "Cardiology",
    "pulmonology":             "Pulmonology",
    "medicine":                "Medicine",
    "nephrology":              "Nephrology",
    "neurology":               "Neurology",
    "infectious_disease":      "Infectious Diseases",
    "endocrinology":           "Endocrinology",
    "gi":                      "Gastroenterology",
    "hematology":              "Hematology",
    "critical_care":           "Critical Care",
    "psychiatry":              "Psychiatry",
    "pharmacology_toxicology": "Pharmacology/Toxicology",
}

# --- STEP 2: MAP RAW LABELS TO PUBLICATION-READY STRINGS ---
# Map internal script IDs to capitalized and readable labels
df["ModelLabel"]      = df["Model"].map(model_map)
df["PersonaLabel"]    = df["Persona"].map(persona_map)
df["AcuityLabel"]     = df["Acuity"].map(acuity_map)
df["DisciplineLabel"] = df["Discipline"].map(disc_map)


# --- STEP 3: SET REFERENCE CATEGORIES FOR THE GEE MODEL ---
# These reference groups will serve as the baseline (OR = 1.0) against which all other categories are compared.
REF_MODEL   = "Claude Opus 4.6"
REF_PERSONA = "Lawyer"
REF_ACUITY  = "Urgent Care"
REF_DISC    = "Medicine"

# Convert columns to Categorical data types, explicitly setting the reference category first
df["ModelLabel"]      = pd.Categorical(df["ModelLabel"],
                            categories=[REF_MODEL] +
                            sorted([x for x in df["ModelLabel"].dropna().unique() if x != REF_MODEL]))
df["PersonaLabel"]    = pd.Categorical(df["PersonaLabel"],
                            categories=[REF_PERSONA] +
                            sorted([x for x in df["PersonaLabel"].dropna().unique() if x != REF_PERSONA]))
df["AcuityLabel"]     = pd.Categorical(df["AcuityLabel"],
                            categories=[REF_ACUITY] +
                            sorted([x for x in df["AcuityLabel"].dropna().unique() if x != REF_ACUITY]))
df["DisciplineLabel"] = pd.Categorical(df["DisciplineLabel"],
                            categories=[REF_DISC] +
                            sorted([x for x in df["DisciplineLabel"].dropna().unique() if x != REF_DISC]))

# Ensure cases are sorted so the Exchangeable correlation structure functions correctly
df = df.sort_values("Case_ID").reset_index(drop=True)


# --- STEP 4: DEFINE AND FIT THE GEE MODEL ---
# We use a Generalized Estimating Equations (GEE) model because it accounts for 
# the repeated measures (multiple personas testing the exact same Case_ID).
formula = "syco ~ C(ModelLabel) + C(PersonaLabel) + C(AcuityLabel) + C(DisciplineLabel)"

gee_model = smf.gee(
    formula,
    groups     = df["Case_ID"], # The clustering variable (repeated measures per case)
    data       = df,
    family     = Binomial(link=Logit()), # Logistic regression (binary outcome: sycophancy vs not)
    cov_struct = Exchangeable(), # Assumes equal correlation between all responses for a given case
)
gee_result = gee_model.fit()
print(gee_result.summary())


# --- STEP 5: EXTRACT COEFFICIENTS AND CALCULATE ODDS RATIOS ---
params = gee_result.params
ci     = gee_result.conf_int()
pvals  = gee_result.pvalues

coef_df = pd.DataFrame({
    "Estimate": params,
    "CI_lo_log": ci[0],
    "CI_hi_log": ci[1],
    "P_Value": pvals,
})

# Exponentiate the log-odds estimates to get Odds Ratios (OR) and their 95% Confidence Intervals
coef_df["OR"]        = np.exp(coef_df["Estimate"])
coef_df["CI_95_Lower"] = np.exp(coef_df["CI_lo_log"])
coef_df["CI_95_Upper"] = np.exp(coef_df["CI_hi_log"])

# Remove the intercept as it is not needed for the relative Odds Ratio comparisons
coef_df = coef_df[~coef_df.index.str.contains("Intercept")]


def clean_term(t):
    for prefix in ["C(ModelLabel)[T.", "C(PersonaLabel)[T.",
                   "C(AcuityLabel)[T.", "C(DisciplineLabel)[T."]:
        if t.startswith(prefix):
            return t[len(prefix):].rstrip("]")
    return t

def get_group(t):
    if "ModelLabel"      in t: return "LLM"
    if "PersonaLabel"    in t: return "Persona Injections"
    if "AcuityLabel"     in t: return "Clinical Setting"
    if "DisciplineLabel" in t: return "Vignette Specialty"
    return "Other"

coef_df["Predictor"]       = [clean_term(t) for t in coef_df.index]
coef_df["Predictor_Group"] = [get_group(t)  for t in coef_df.index]

def sig_label(p):
    if pd.isna(p):  return "Reference"
    if p < 0.001:   return "*** P < 0.001"
    if p < 0.01:    return "** P < 0.01"
    if p < 0.05:    return "* P < 0.05"
    return "not sig"

coef_df["Significance"] = coef_df["P_Value"].apply(sig_label)

# --- STEP 6: FORMAT AND EXPORT RESULTS ---
# Sort the final table by group and Odds Ratio for cleaner visualization
results_table = coef_df[["Predictor_Group", "Predictor", "OR",
                          "CI_95_Lower", "CI_95_Upper", "P_Value", "Significance"]]\
                .sort_values(["Predictor_Group", "OR"], ascending=[True, False])

pd.set_option("display.max_rows", 100)
pd.set_option("display.float_format", "{:.4f}".format)
pd.set_option("display.max_colwidth", 30)
print("\n── GEE Results: OR (95% CI) ──")
print(results_table.to_string(index=False))

# Save the structured results to an Excel file for downstream R visualization (Figure 5)
results_table.to_excel(XLSX_OUT, index=False, sheet_name="GEE_Results")
print(f"\nSaved to Excel: {XLSX_OUT}")
