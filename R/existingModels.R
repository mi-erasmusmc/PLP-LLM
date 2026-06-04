
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
  tempModel <- list(model = NULL)
  attr(tempModel, "modelType") <- attr(param, 'modelType')

  prediction <- predictCHADS2(
    plpModel = tempModel,
    cohort = trainData$labels,
    data = trainData
  )
  prediction$evaluationType <- 'Train'

  result <- list(
    model = NULL,

    preprocessing = list(
      featureEngineering = attr(trainData, "metaData")$featureEngineering,#learned mapping
      tidyCovariates = attr(trainData$covariateData, "metaData")$tidyCovariateDataSettings,  #learned mapping
      requireDenseMatrix = F
    ),

    prediction = prediction,

    modelDesign = PatientLevelPrediction::createModelDesign(
      targetId = attr(trainData, "metaData")$targetId, # added
      outcomeId = attr(trainData, "metaData")$outcomeId, # added
      restrictPlpDataSettings = attr(trainData, "metaData")$restrictPlpDataSettings, # made this restrictPlpDataSettings
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
      analysisSource = '', #TODO: add from model
      developmentDatabase = attr(trainData, "metaData")$cdmDatabaseSchema,
      attrition = attr(trainData, "metaData")$attrition,
      trainingTime =  "0.0 mins",
      trainingDate = Sys.Date(),
      modelName = settings$modelType,
      finalModelParameters = list() #TODO: add parameters
      # hyperParamSearch = cvPerFold
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

  # CHADS2 model (with ids referring to predictors PhenotypeLibrary)
  # CHF history + 1 -> # 1154
  # hypertension history + 1 -> # 1198
  # age >= 75 year + 1 -> -> ifelse(# 1002 >= 75, 1 , 0)
  # diabetes mellitus history + 1 -> max (# 1193, # 1194) -> now 888
  # stroke or TIA previously + 2 -> # 1657 x 2

  # Convert to dense covariates
  covariates <- as.data.frame(data$covariateData$covariates)

  # Select only covariates included in model
  included_covs <- c(1154, 1198, 888, 1657)
  # included_covs <- c(1154, 1198, 1193, 1194, 1657)
  covariates <- covariates[covariates$covariateId %in% c(1002, included_covs*100000+1999),]

  # Reshape covariate data
  denseData <- reshape2::dcast(covariates, rowId ~ covariateId, value.var = 'covariateValue', fill = 0)

  # Apply existing model
  denseData$`1002` <- ifelse( denseData$`1002` >= 75, 1, 0)
  # denseData$`119301999` <- max(denseData$`119301999`, denseData$`119401999`)
  # denseData$`119401999` <- NULL
  denseData$`165701999` <- denseData$`165701999`*2
  denseData$rowId <- NULL

  denseData$sum <- rowSums(denseData) # CHADS2 score

  score_to_prob <- list("0" = 0.019,
                        "1" = 0.029,
                        "2" = 0.04,
                        "3" = 0.059,
                        "4" = 0.085,
                        "5" = 0.125,
                        "6" = 0.182)

  prob <- sapply(denseData$sum, function(i) score_to_prob[[as.character(i)]]) # CHADS2 probability

  prediction <- data.frame(rowId=cohort$rowId, value=as.numeric(prob))

  # return the cohorts as a data frame with the prediction added as
  # a new column with the column name 'value'
  prediction <- merge(cohort, prediction, by='rowId', all.x=T)
  attr(prediction, "metaData")$modelType <- plpModel$model$modelType

  return(prediction)
}

addCHADS2 <- function(plpData) {

  covariateData <- plpData$covariateData

  # update covariates
  covariates <- as.data.frame(covariateData$covariates)

  # CHADS2 model (with ids referring to predictors PhenotypeLibrary)
  # CHF history + 1 -> # 1154
  # hypertension history + 1 -> # 1198
  # age >= 75 year + 1 -> -> ifelse(# 1002 >= 75, 1 , 0)
  # diabetes mellitus history + 1 -> max (# 1193, # 1194) -> now 888
  # stroke or TIA previously + 2 -> # 1657 x 2

  # Select only covariates included in model
  included_covs <- c(1154, 1198, 888, 1657)
  score <- covariates[covariates$covariateId %in% c(1002, included_covs*100000+1999),]

  # Apply existing model
  score$covariateValue[score$covariateId == '1002']<-ifelse(score$covariateValue[score$covariateId == '1002'] >= 75, 1, 0) # age
  score$covariateValue[score$covariateId == '165701999']<-score$covariateValue[score$covariateId == '165701999']*2 # hypertension

  chads2 <- score %>% group_by(rowId) %>% summarise(covariateValue=sum(covariateValue))  # CHADS2 score
  chads2[is.na(chads2)] <- 0
  # chads2 <- chads2[!is.na(chads2$covariateValue),]
  chads2$covariateId <- 77701999

  # plot(chads2$covariateValue[order(chads2$covariateValue)])

  # Add to original covariates
  covariates <- rbind(covariates,chads2)

  # update analysisRef
  analysisRef <- as.data.frame(covariateData$analysisRef)
  analysisRef <- rbind(analysisRef, list(999, "CHADS2", "custom", -36500, -1, "N", "Y"))

  # update covariateRef
  covariateRef <- as.data.frame(covariateData$covariateRef)
  covariateRef <- rbind(covariateRef, list(77701999, "CHADS2", 999, 0))

  result <- Andromeda::andromeda(
    covariates = covariates,
    covariateRef = covariateRef,
    analysisRef = analysisRef
  )

  attr(result, "metaData") <- attr(covariateData, "metaData")
  class(result) <- "CovariateData"

  plpData$covariateData <- result

  return(plpData)
}


