
runModels <- function(plp, plpData, model, iteration, outputFolder) {
  
  populationSettings <- taskSpecifications(plp, "populationSettings")
  seed <- 1000 + iteration

  if (model == "ExistingScore") { # existing score models
    model_settings <- list(analysisId=paste0("existing_CHADS2"),
                                      analysisName=paste0("Apply CHADS2"),
                                      modelSettings=setCHADS2())

  } else if (startsWith(model, "LLM_")) { # large language models
    systemprompt_file<-paste0("systemprompt_", "StrokeInAF(CHADS2)_v2",".md")
    print(systemprompt_file) # just to be certain during run time
    systemprompt<-readLines(file.path(root, "inst", "prompts", systemprompt_file))
    format_typeobject<<-type_object(reasoning = type_string(required = T),
                                   top_5_features = type_array(
                                     items = type_object(
                                       feature_name=type_string(),
                                       positive=type_boolean()
                                       ),
                                    required = T
                                     ),
                                   probability = type_number(required = T)) # format for the structured output, it cannot do anything else then return a single number and a string.
    assign("format_typeobject",format_typeobject, envir = .GlobalEnv)
    
    if (model == "LLM_Qwen3") {
      model_settings <- list(analysisId=paste0("LLM_qwen_", plp),
                             analysisName=paste0("LLM qwen3"),
                             modelSettings=setLLM(model_name="qwen3:30b",
                                                  systemprompt = systemprompt,
                                                  format_typeobject = TRUE,
                                                  temperature = 0.2,
                                                  tempsavelocation = paste0(outputFolder,"/LLM_qwen3_temp_it",iteration),
                                                  parallel = TRUE))
      
      
    } else if (model == "LLM_Qwen3.8") {
      model_settings <- list(analysisId=paste0("LLM_qwen_", plp),
                             analysisName=paste0("LLM qwen3.8"),
                             modelSettings=setLLM(model_name="qwen3.8",
                                                  systemprompt = systemprompt,
                                                  format_typeobject = TRUE,
                                                  temperature = 0.2,
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
