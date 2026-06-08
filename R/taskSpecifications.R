taskSpecifications <- function(plp, output) {
  
  if (plp == "Eunomia") {
    
    # Get connection details
    connectionDetails <- Eunomia::getEunomiaConnectionDetails()
    
    # Create cohorts
    Eunomia::createCohorts(connectionDetails)
    
    # Select cohorts
    targetId <- 4
    cohortNames <- 'target'
    outcomeId <- 3
    outcomeNames <- 'outcome'
    
    # Select population
    populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(
      requireTimeAtRisk = F,
      riskWindowStart = 1,
      riskWindowEnd = 365)
    
    databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      cohortDatabaseSchema = "main",
      cohortTable = "cohort",
      targetId = targetId,
      outcomeIds = outcomeId,
      outcomeDatabaseSchema = "main",
      outcomeTable =  "cohort",
      cdmDatabaseName = 'eunomia'
    )
    
  } else {
    
    ### Get specifications  ###
    if (plp == "CHADS2") { # 1 jan 2018 – 1 jan 2023 (IPCI-Q end: 1 jan 2024)
      
      # Select cohorts
      # From https://github.com/mi-erasmusmc/PlpBenchmarks/tree/main/inst/cohorts
      targetId <- 1650
      cohortNames <- 'patients with atrial fibrillation'
      outcomeId <- 1651
      outcomeNames <- 'ischemic stroke'
      
      # Select population
      populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(firstExposureOnly = FALSE,
                                                                                  washoutPeriod = 365,
                                                                                  minTimeAtRisk = 364,
                                                                                  riskWindowStart = 1,
                                                                                  startAnchor = "cohort start",
                                                                                  riskWindowEnd = 365,
                                                                                  endAnchor = "cohort start",
                                                                                  removeSubjectsWithPriorOutcome = FALSE)
    }
  }
  
  if (output == "populationSettings") {
    return (populationSettings)
  } else if (output == "targetId") {
    return(targetId)
  } else if (output == "outcomeId") {
    return(outcomeId)
  } else if (output == "databaseDetails") {
    return(databaseDetails)
  }
}
