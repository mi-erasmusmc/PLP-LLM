## PREDICTION TASK

Estimate the risk that a given patient from the target population: [adults over 18 with a diagnosis of atrial fibrillation], will develop the outcome: [ischemic stroke], within the time-at-risk: [1 year], using the provided clinical data of the last: [1 year].

## GENERAL INSTRUCTIONS

- Analyse the patient's demographics and recorded clinical events.
- The clinical data include:
    - Diagnoses at ICD-10 level 3.
    - Drug prescriptions at RxNorm ingredient level.
- Identify which demographics, diagnoses, or prescriptions positively or negatively influence the risk estimate.
- Your response must include:
    - One short sentence motivating the decision, no longer than 150 characters.
    - An list of up to 5 specific clinical-data features that positively or negatively influence the estimated risk, ordered by absolute influence.
    - An estimated risk probability between 0 and 1, rounded to 2 decimal places. 
    
Risk probability scale anchors:
- 0.00 = Impossible
- 0.10 = Negligible
- 0.20 = Remote
- 0.30 = Very low
- 0.40 = Low
- 0.50 = Intermediate
- 0.60 = Notable
- 0.70 = Probably
- 0.80 = High likelihood
- 0.90 = Near inevitable
- 1.00 = Absolute certainty

## OUTPUT
Output format: 
{ 
  "reasoning": "[Short, one or two sentence reasoning.]", 
  "top_5_features": [ 
    {
      "feature_name": "[Exact feature name as presented in the clinical data]",
      "positive" : true/false
    }
  ],
  "probability": 0.00
}
