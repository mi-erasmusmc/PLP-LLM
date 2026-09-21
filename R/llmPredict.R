library(ellmer)
library(dplyr)
library(tidyr)
library(stringr)

setLLM <- function(model_name,
                   systemprompt, format_typeobject, 
                   temperature=0.7, max_tokens=NULL, 
                   parallel=TRUE, seed=NULL){
  param<-list(
    model_name=model_name,
    systemprompt=systemprompt,
    format_typeobject=format_typeobject,
    temperature=temperature,
    max_tokens=max_tokens,
    parallel=parallel
  )
  
  attr(param, "settings") <- list(
    modelName = "LLM",
    requiresDensMatrix = TRUE,
    seed=seed
  )
  
  result <-list(
    fitFunction = "fitLLM",
    param=param
  )
  
  class(result)<-"modelSettings"
  
  return(result)
}

fitLLM <- function(trainData, modelSettings, search = "adaptive", analysisId, ...){
  print("in train")
  
  #testing
  # trainData_in<<-trainData
  # modelSettings_in<<-modelSettings
  # search_in<<-search
  # analysisId_in<<-modelSettings
  
  ## pass an almost empty plpModel
  param <- modelSettings$param
  result <- list(model = NULL, 
                 preprocessing = NULL, 
                 prediction = NULL, 
                 modelDesign = PatientLevelPrediction::createModelDesign(
                   targetId = attr(trainData,"metaData")$targetId, 
                   outcomeId = attr(trainData,"metaData")$outcomeId, 
                   restrictPlpDataSettings = attr(trainData,"metaData")$restrictPlpDataSettings, 
                   covariateSettings = attr(trainData,"metaData")$covariateSettings, 
                   populationSettings = attr(trainData,"metaData")$populationSettings, 
                   featureEngineeringSettings = attr(trainData,"metaData")$featureEngineeringSettings, 
                   preprocessSettings = attr(trainData$covariateData,"metaData")$preprocessSettings, 
                   modelSettings = modelSettings,
                   splitSettings = attr(trainData, "metaData")$splitSettings, 
                   sampleSettings = attr(trainData, "metaData")$sampleSettings), 
                 trainDetails = NULL,
                 covariateImportance = NULL
  )
  class(result) <- "plpModel"
  attr(result, "predictionFunction") <- "predictLLM"
  attr(result, "modelType") <- "binary"
  attr(result, "saveType") <- "RtoJson"
  plpModel_out<<-result
  return(result)
}

predictLLM <- function(plpModel, data, cohort){
  print("in predict")
  ParallelLogger::logTrace("predict LLM start")
  
  # Testing
  # plpModel_in<<-plpModel
  # data_in<<-data
  # cohort_in<<-cohort
  
  ## convert the data to a list of text prompts
  covariates<-data.frame(data$covariateData$covariates) %>%
    left_join(data.frame(data$covariateData$covariateRef), by = join_by(covariateId)) %>%
    filter(rowId %in% cohort$rowId) %>%
    arrange(analysisId)
  
  covariates_edit_names<-covariates %>%
    separate_wider_delim(covariateName, delim="index: ", names = c("period","name"), too_few = "align_end") %>%
    mutate(name=ifelse(name=="age in years", paste0(name, " = ",covariateValue), name))
  
  covariates_texts<-covariates_edit_names %>%
    mutate(type=ifelse(analysisId<100,"### DEMOGRAPHICS", "### CLINICAL EVENTS")) %>%
    group_by(rowId, type) %>%
    summarise(text=paste0(name, collapse = "\n")) %>%
    mutate(text=paste0(type, ":\n", text)) %>%
    arrange(desc(type)) %>%
    group_by(rowId) %>%
    summarise(text=paste0(text, collapse = "\n\n")) %>%
    ungroup() %>%
    mutate(text=paste0("## CLNICAL DATA\n", text))
  
  chat<-chat_openai(
    model = plpModel$modelDesign$modelSettings$param$model_name,
    api_key = "",
    base_url = "http://192.168.0.226:1234/v1", #http://localhost:1234/v1",
    params = params(temperature = plpModel$modelDesign$modelSettings$param$temperature,
                    max_tokens = plpModel$modelDesign$modelSettings$param$max_tokens),
    system_prompt = plpModel$modelDesign$modelSettings$param$systemprompt
  )
  
  if(plpModel$modelDesign$modelSettings$param$parallel){
    print(covariates_texts$text[1])
    results<-parallel_chat_structured(
      chat = chat,
      prompts = as.list(covariates_texts$text),
      type = plpModel$modelDesign$modelSettings$param$format_typeobject,
      max_active = 50,
      rpm = 1000
    )
    prediction<-data.frame(rowId=covariates_texts$rowId, value=results$probability)
    prediction<-merge(cohort, prediction, by = "rowId", 
                      all.x = TRUE, fill = 0)
    attr(prediction, "metaData")$modelType <- "binary"
  } else {
    stop("erroooooohr")
  }
  return(prediction)
}


### USE
# model_name<-"qwen3-30b-a3b"
# model_name<-"phi-4@q8_0"
# systemprompt<-paste0("Given the demographic information of a ", 
#                      "",
#                      "patient and their different conditions in the last year, estimate the risk probability between 0 and 1 ",
#                      "of this patient developing Dementia in the next five years.")
# format_typeobject<-type_object(
#   probability = type_number()
# )

# modelSettings<-setLLM(model_name=model_name,
#                       systemprompt = systemprompt,
#                       format_typeobject = format_typeobject,
#                       parallel = TRUE)