
runModels <- function(plp, plpData, model, iteration, outputFolder) {
  
  populationSettings <- taskSpecifications(plp, "populationSettings")
  seed <- 1000 + iteration

  if (model == "ExistingScore") { # existing score models
    model_settings <- list(analysisId=paste0("existing_CHADS2"),
                                      analysisName=paste0("Apply CHADS2"),
                                      modelSettings=setCHADS2())

  } else if (startsWith(model, "LLM_")) { # large language models
    systemprompt_file<-paste0("systemprompt_", "Dementia",".md")
    print(systemprompt_file) # just to be certain during run time
    systemprompt<-readLines(file.path(root, "inst", "prompts", systemprompt_file))
    format_typeobject<-type_object(reasoning = type_string(),
                                   probability = type_number()) # format for the structured output, it cannot do anything else then return a single number and a string.
    
    if (model == "LLM_Qwen3") {
      model_settings <- list(analysisId=paste0("LLM_qwen_", plp),
                             analysisName=paste0("LLM qwen"),
                             modelSettings=setLLM(model_name="qwen3-30b-a3b",
                                                  systemprompt = systemprompt,
                                                  format_typeobject = format_typeobject,
                                                  parallel = TRUE))
      
      
    } else if (model == "LLM_MedGemma") {
      model_settings <- list(analysisId=paste0("LLM_medgemma_", plp),
                             analysisName=paste0("LLM medgemma"),
                             modelSettings=setLLM(model_name="medgemma-27b-text-it",
                                                  systemprompt = systemprompt,
                                                  format_typeobject = format_typeobject,
                                                  parallel = TRUE))
    }
  
  } else { # machine learning algorithms
    
    # LASSO
    if (model == "LASSO") {
      model_settings <- list(analysisId=paste0("LASSO_", plp),
                             analysisName=paste0("LASSO logistic regression"),
                             modelSettings=PatientLevelPrediction::setLassoLogisticRegression(variance = 0.01))
    } else if (model == "RandomForest") { # python function
      model_settings <- list(analysisId=paste0("RandomForest_", plp),
                                            analysisName=paste0("Random forest"),
                                            modelSettings=PatientLevelPrediction::setRandomForest(ntrees=list(10, 50, 100), maxDepth=list(2, 4, 8)))
    } else if (model == "XGBoost") { # r function
      model_settings <- list(analysisId=paste0("XGBoost_", plp),
                                       analysisName=paste0("Gradient boosting machine"),
                                       modelSettings=PatientLevelPrediction::setGradientBoostingMachine(ntrees=c(10, 50, 100), maxDepth=c(2, 4, 8)))
    }
  }
  
  plpResults <- PatientLevelPrediction::runPlp(plpData = plpData,
                                               modelSettings = model_settings$modelSettings,
                                               analysisId = paste0(model_settings$analysisId, "_", covariates, "_", iteration), 
                                               analysisName = model_settings$analysisName,
                                               populationSettings = populationSettings,
                                               splitSettings = createDefaultSplitSetting(testFraction = 0.25,
                                                                                         trainFraction = 0.75,
                                                                                         splitSeed=seed, # Change seed per iteration
                                                                                         nfold = 3,
                                                                                         type = 'stratified'),
                                               sampleSettings = createSampleSettings(),
                                               featureEngineeringSettings = createFeatureEngineeringSettings(),
                                               preprocessSettings = createPreprocessSettings(minFraction = 0.001,
                                                                                             normalize = F,
                                                                                             removeRedundancy = F),
                                               logSettings = createLogSettings(),
                                               executeSettings = createExecuteSettings(runSplitData = T,
                                                                                       runSampleData = F,
                                                                                       runFeatureEngineering = F,
                                                                                       runPreprocessData = T,
                                                                                       runModelDevelopment = T,
                                                                                       runCovariateSummary = T),
                                               saveDirectory = outputFolder)
  
}
