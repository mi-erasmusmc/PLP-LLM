## DEFINITIONS
### TARGET COHORT:
This cohort includes **adults over 18 with a diagnosis of atrial fibrillation**.

### CLINICAL OUTCOME:
This cohort includes adults aged 18 or older with diagnoses of **ischemic stroke**.

### TIME-AT-RISK:
**1 year**

## INSTRUCTIONS
Given the clinical data of a patient who meets the criteria for the TARGET COHORT, estimate the risk probability that this patient will develop the specified CLINICAL OUTCOME within the defined TIME-AT-RISK.

- The clinical data are the patient DEMOGRAPHICS and their recorded CLINICAL EVENTS (diagnoses and drugs prescribed) in the last year.
- Your response must contain:
  - A short sentence to motivate your decision, not longer than 150 characters.
  - The estimated risk probability as an integer between 0 and 10. Scale anchors:
    - 0 = Impossible (0%)
    - 1 = Negligible (<0.1%)
    - 2 = Remote (0.1% - 1%)
    - 3 = very low (1% - 5%)
    - 4 = low (5% - 10%)
    - 5 = intermediate (10% - 25%)
    - 6 = Notable (25% - 50%)
    - 7 = Probably (50% - 70%)
    - 8 = High likelihood (70% - 80%)
    - 9 = Near inevitable (85% - 99%)
    - 10 = Absolute certainty (100%)

Output format:
{
  "reasoning": "[short one sentence reasoning.]",
  "probability": [1-10]
}

