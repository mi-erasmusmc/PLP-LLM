### Pre-process data and train models ###

createPredictors <- function(plp, covariates, databaseDetails, outputFolder) {
  
  # Add cohort ids
  databaseDetails$targetId <-taskSpecifications(plp, "targetId")
  databaseDetails$outcomeIds <-taskSpecifications(plp, "outcomeId")
  
  # Possible covariates
  if (covariates == "Default") {
    covariateSettingList <- FeatureExtraction::createDefaultCovariateSettings()
  } else if (covariates == "AgeSex") {
    covariateSettingList <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, useDemographicsAge = T)
  } else if (covariates == "Full") {
    covariateSettingList <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T,
                                                                       useDemographicsAge = T,
                                                                       useConditionGroupEraLongTerm = T,
                                                                       useDrugGroupEraLongTerm = T)
  } else if (covariates == "GroupedFeatures") {
    covariateSettingList <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T,
                                                                       useDemographicsAge = T,
                                                                       useConditionGroupEraShortTerm = T,
                                                                       useDrugGroupEraShortTerm = T)
  }
  
  # Selected covariates
  # TODO: check if works
  if (file.exists(paste0(outputFolder, "/plpData_", plp,"_", covariates))) {
    print("load plpData")
    plpData <- loadPlpData(paste0(outputFolder, "/plpData_", plp, "_", covariates))
  } else {
    print("create plpData")
    plpData <- PatientLevelPrediction::getPlpData(
      databaseDetails = databaseDetails,
      restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(sampleSize=50000),
      covariateSettings = covariateSettingList
    )
    

    if (covariates == "GroupedFeatures") {
      
      # TODO: apply mapping
      
      
    }
    
    # Save covariates
    covData <- FeatureExtraction::aggregateCovariates(plpData$covariateData)
    write.csv(as.data.frame(covData$covariates), file.path(outputFolder, paste0("covData_", plp, "_", covariates, ".csv")), row.names = FALSE)
    
    PatientLevelPrediction::savePlpData(plpData = plpData, file=paste0(outputFolder, "/plpData_", plp, "_", covariates))
    
  }
  
  return(plpData)
}
