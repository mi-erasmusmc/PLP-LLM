
# https://github.com/OHDSI/PredictionComparison/blob/master/R/chads2Model.R

#' @export
setCHADS2 <- function(){

  param <- list()

  attr(param, 'settings') <- list(
    modelType = 'existing',
    modelName = 'CHADS2'
  )

  attr(param, 'modelType') <- 'binary'
  attr(param, 'saveType') <- 'RtoJson'

  result <- list(
    fitFunction = "fitCHADS2",
    param = param
  )
  class(result) <- 'modelSettings'

  return(result)
}

#' @export
fitCHADS2 <- function(trainData,
                      modelSettings,
                      search = 'none',
                      analysisId,
                      ...) {

  param <- modelSettings$param
  settings <- attr(param, 'settings')

  # Getting predictions on train set:
  tempModel <- list(model = list(modelType = attr(param, 'modelType')))
  attr(tempModel, "modelType") <- attr(param, 'modelType')

  prediction <- predictCHADS2(
    plpModel = tempModel,
    cohort = trainData$labels,
    data = trainData
  )
  prediction$evaluationType <- 'Train'

  result <- list(
    model = list(modelType = attr(param, 'modelType')),

    preprocessing = list(
      featureEngineering = attr(trainData, "metaData")$featureEngineering,
      tidyCovariates = attr(trainData$covariateData, "metaData")$tidyCovariateDataSettings,
      requireDenseMatrix = F
    ),

    prediction = prediction,

    modelDesign = PatientLevelPrediction::createModelDesign(
      targetId = attr(trainData, "metaData")$targetId, # added
      outcomeId = attr(trainData, "metaData")$outcomeId, # added
      restrictPlpDataSettings = attr(trainData, "metaData")$restrictPlpDataSettings,
      covariateSettings = attr(trainData, "metaData")$covariateSettings,
      populationSettings = attr(trainData, "metaData")$populationSettings,
      featureEngineeringSettings = attr(trainData, "metaData")$featureEngineeringSettings,
      preprocessSettings = attr(trainData$covariateData, "metaData")$preprocessSettings,
      modelSettings = modelSettings, # modified
      splitSettings = attr(trainData, "metaData")$splitSettings,
      sampleSettings = attr(trainData, "metaData")$sampleSettings
    ),

    trainDetails = list(
      analysisId = analysisId,
      analysisSource = '',
      developmentDatabase = attr(trainData, "metaData")$cdmDatabaseSchema,
      attrition = attr(trainData, "metaData")$attrition,
      trainingTime =  "0.0 mins",
      trainingDate = Sys.Date(),
      modelName = settings$modelType,
      finalModelParameters = list()
    ),

    covariateImportance = NULL
  )

  class(result) <- 'plpModel'
  attr(result, 'predictionFunction') <- 'predictCHADS2'
  attr(result, 'modelType') <- attr(modelSettings$param, 'modelType')
  attr(result, 'saveType') <- attr(modelSettings$param, 'saveType')
  return(result)
}

#' @export
predictCHADS2 <- function(plpModel, data, cohort) {
  chads2_scores <- computeChads2FromCovariateData(data$covariateData, cohort$rowId)
  prediction <- merge(cohort, chads2_scores[,c("rowId", "probs")], by='rowId', all.x=TRUE)
  attr(prediction, "metaData")$modelType <- attr(plpModel, "modelType")

  return(prediction)
}


computeChads2FromCovariateData <- function(plpData) {
  # Congestive heart failure (CHF) = 1, ICD-10 3-char: I50 (45581779)
  # Hypertension = 1, ICD-10 3-char: I10 (45591453), I11 (45548007), I12 (45567162), I13 (45576864), I15 (45562338
  # Age ≥ 75 = 1 (from age variable,included as continuous covariateValue can be found in covariateId = 1002) 
  # Diabetes mellitus = 1, ICD-10 3-char: E10 (45576436), E11 (45571656), E12 (45586141), E13 (45566732), E14 (45571663)
  # Stroke / TIA / thromboembolism = 2, ICD-10 3-char: I63 (45543186), I64 (45548032), G45 (45562092), I69 (45543192)
  
  covariates <- as.data.frame(covariateData$covariates)
  
  # Define predictors. 
  predictor_covariate_ids <- list(
    `CHF history` = c(45581779),
    `Hypertension history` = c(45591453, 45548007, 45567162, 45576864, 45562338),
    `Age >= 75` = c(1002),
    `Diabetes mellitus history` = c(45576436, 45571656, 45586141, 45566732, 45571663),
    `Stroke or TIA symptoms previously` = c(45543186, 45548032, 45562092, 45543192)
  )
  
  # Reverse lookup: covariateId -> predictor.
  covariate_to_predictor <- unlist(
    lapply(names(predictor_covariate_ids), function(p) {
      setNames(rep(p, length(predictor_covariate_ids[[p]])), predictor_covariate_ids[[p]])
    })
  )
  
  # Add predictor column; NA if not in dictionary.
  covariates$predictor <- covariate_to_predictor[as.character(covariates$covariateId)]
  # covariates$predictor <- unname(covariates$predictor)
  
  # Convert covariateValue into 0/1 based on threshold.
  covariates <- covariates %>%
    mutate(covariateValue = ifelse(predictor == "Age >= 75",
                                   as.integer(as.numeric(covariateValue) >= 75),
                                   as.integer(covariateValue > 0)))
  
  df_model <- covariates %>%
    group_by(rowId, predictor) %>%
    summarise(value = max(covariateValue, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = predictor,
      values_from = value,
      values_fill = 0
    )
  
  # Then compute the composite score. 
  # Make sure all predictors exist as columns.
  all_predictors <- names(predictor_covariate_ids)
  for (p in all_predictors) {
    if (!p %in% names(df_model)) {
      df_model[[p]] <- 0
    }
  }
  
  df_model <- df_model %>%
    mutate(
      score =
        `CHF history` +
        `Hypertension history` +
        `Diabetes mellitus history` +
        2L * `Stroke or TIA symptoms previously` +
        `Age >= 75`
    )
  
  # Convert to predicted probability
  score_to_prob <- c(0 = 0.019, 1 = 0.029, 2 = 0.040, 3 = 0.059, 4 = 0.085, 5 = 0.125, 6 = 0.182)
  df_model$probs <- score_to_prob[df_model$score]
  
  return(df_model[,c("rowId", "score", "probs")])
}
