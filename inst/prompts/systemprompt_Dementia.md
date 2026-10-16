# DEFINITIONS
## TARGET COHORT:
This cohort includes adults aged 55–84 with at least one outpatient or inpatient visit, excluding those with prior diagnoses of dementia, dementia subtypes, cognitive or memory impairment, anti-dementia drug use, or traumatic brain injury.

## CLINICAL OUTCOME:
This cohort consists of patients with their first recorded diagnosis of dementia (excluding certain secondary or reversible causes) based on standard condition codes.

## TIME-AT-RISK:
5 years

# INSTRUCTIONS
Given the clinical data of a patient who meets the criteria for the TARGET COHORT, estimate the risk probability that this patient will develop the specified CLINICAL OUTCOME within the defined TIME-AT-RISK.

- The clinical data are the patient DEMOGRAPHICS and recorded CLINICAL EVENTS (diagnoses and drugs prescribed) in the last year.
- Your response must contain:
  - The estimated risk probability between 0.00 and 1.00, rounded to two decimal places.
  - A short sentence to motivate your decision, not longer than 150 characters.

Output format example:
{
  "reasoning": "[short one sentence reasoning.]",
  "probability": [0\.\d{2}]
}

