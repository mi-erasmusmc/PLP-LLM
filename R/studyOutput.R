
studyOutput <- function(outputFolder) {
  
  
  
  
}


# if (subgroup) {
#   populationSettingsF <- populationSettings
#   populationSettingsF$subpopulation <- "FEMALE"
#   
#   populationSettingsM <- populationSettings
#   populationSettingsM$subpopulation <- "MALE"
#   
#   splitSettings <- plpResults$model$modelDesign$splitSettings
#   attr(splitSettings, "fun") <- "inputSplitter"
#   
#   print(paste0("run in subgroup FEMALE ", m))
#   plpResultsF <- PatientLevelPrediction::runPlp(plpData = plpData,
#                                                 outcomeId = outcomeId,
#                                                 modelSettings = model$modelSettings,
#                                                 analysisId = paste0(model$analysisId, "_", covariates, "_FEMALE"), # Add name of selected covariates
#                                                 analysisName = model$analysisName,
#                                                 populationSettings = populationSettingsF,
#                                                 splitSettings = splitSettings,
#                                                 sampleSettings = createSampleSettings(),
#                                                 featureEngineeringSettings = createFeatureEngineeringSettings(),
#                                                 preprocessSettings = createPreprocessSettings(minFraction=0.001,
#                                                                                               normalize = F,
#                                                                                               removeRedundancy = F),
#                                                 logSettings = createLogSettings(),
#                                                 executeSettings = createExecuteSettings(runSplitData = T,
#                                                                                         runSampleData = F,
#                                                                                         runFeatureEngineering = F,
#                                                                                         runPreprocessData = T,
#                                                                                         runModelDevelopment = T,
#                                                                                         runCovariateSummary = T),
#                                                 saveDirectory = outputFolder)
#   
#   print(paste0("run in subgroup MALE ", m))
#   plpResultsM <- PatientLevelPrediction::runPlp(plpData = plpData,
#                                                 outcomeId = outcomeId,
#                                                 modelSettings = model$modelSettings,
#                                                 analysisId = paste0(model$analysisId, "_", covariates, "_MALE"), # Add name of selected covariates
#                                                 analysisName = model$analysisName,
#                                                 populationSettings = populationSettingsM,
#                                                 splitSettings = splitSettings,
#                                                 sampleSettings = createSampleSettings(),
#                                                 featureEngineeringSettings = createFeatureEngineeringSettings(),
#                                                 preprocessSettings = createPreprocessSettings(minFraction=0.001,
#                                                                                               normalize = F,
#                                                                                               removeRedundancy = F),
#                                                 logSettings = createLogSettings(),
#                                                 executeSettings = createExecuteSettings(runSplitData = T,
#                                                                                         runSampleData = F,
#                                                                                         runFeatureEngineering = F,
#                                                                                         runPreprocessData = T,
#                                                                                         runModelDevelopment = T,
#                                                                                         runCovariateSummary = T),
#                                                 saveDirectory = outputFolder)
#   
# }

### Validate models in subgroups ###
# if (subgroups) {
#   
#   print("validate in subgroups")
#   
#   ### Run for all models
#   for (m in names(list_models)) {
#     
#     print(paste0("validate ", m))
#     
#     ### Load model
#     plpResults <- loadPlpResult(file.path(outputFolder, paste0(m, "_", plp, "_", covariates), "plpResult"))
#     
#     # aggregate model, internal validation in subpopulations
#     validationDesignList <- createValidationDesign(targetId=cohortId,
#                                                    outcomeId=outcomeId,
#                                                    populationSettings = NULL,
#                                                    restrictPlpDataSettings = list(plpResults$model$modelDesign$restrictPlpDataSettings),
#                                                    plpModelList = list(plpResults$model),
#                                                    recalibrate = NULL,
#                                                    runCovariateSummary = FALSE)
#     
#     plpExternalResultEunomia <- validateExternal(validationDesignList = validationDesignList,
#                                                  databaseDetails = databaseDetails,
#                                                  logSettings = createLogSettings(verbosity = "INFO",
#                                                                                  logName = "validatePLP"),
#                                                  outputFolder = file.path(outputFolder, paste0("subgroups_", m, "_", plp, "_", covariates)),
#                                                  rerun = TRUE,
#                                                  internalSplitSettings = plpResults$model$modelDesign$splitSettings,
#                                                  subpopulationList = list("FEMALE", "MALE"))
#   } 
# }