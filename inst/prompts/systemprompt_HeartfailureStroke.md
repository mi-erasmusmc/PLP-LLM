# DEFINITIONS
## TARGET COHORT:
This cohort includes adults over 18 with their first diagnosis of type 2 diabetes mellitus (excluding those with prior type 1 diabetes, heart failure, or stroke).

## CLINICAL OUTCOME:
This cohort includes adults aged 18 or older with diagnoses of both heart failure and stroke (excluding multi-infarct dementia).

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

