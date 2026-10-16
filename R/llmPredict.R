library(ellmer)
library(dplyr)
library(tidyr)
library(stringr)

setLLM <- function(model_name,
                   systemprompt, format_typeobject, tempsavelocation=NULL,
                   temperature=0.7, max_tokens=NULL, 
                   parallel=TRUE, seed=NULL){
  param<-list(
    model_name=model_name,
    systemprompt=systemprompt,
    tempsavelocation=tempsavelocation,
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
  
  # plpModel<-plpModel_in
  # data<-data_in
  # cohort<-cohort_in
  
  # stop()
  
  ## convert the data to a list of text prompts
  covariates<-data.frame(data$covariateData$covariates) %>%
    left_join(data.frame(data$covariateData$covariateRef), by = join_by(covariateId)) %>%
    filter(rowId %in% cohort$rowId) %>%
    arrange(analysisId)
  
  covariates_edit_names<-covariates %>%
    separate_wider_delim(covariateName, delim="index: ", names = c("period","name"), too_few = "align_end") %>%
    mutate(name=ifelse(name=="age in years", paste0(name, " = ",covariateValue), name))
  
  covariates_texts<-covariates_edit_names %>%
    mutate(subsection=case_when(analysisId<100 ~ "",
                          analysisId %in% c(201:216) ~ "\n#### CONDITIONS",
                          analysisId %in% c(401:416) ~ "\n#### MEDICATIONS")) %>%
    group_by(rowId, subsection) %>%
    summarise(text=paste0(name, collapse = "\n"),
              analysisId=max(analysisId)) %>%
    mutate(text=paste0(subsection, "\n", text)) %>%
    arrange(analysisId) %>%
    mutate(section=ifelse(analysisId<100,"### DEMOGRAPHICS", "\n### CLINICAL EVENTS")) %>%
    group_by(rowId, section) %>%
    summarise(text=paste0(text, collapse = "\n"),
              analysisId=max(analysisId)) %>%
    mutate(text=paste0(section, "", text)) %>%
    arrange(analysisId) %>%
    group_by(rowId) %>%
    summarise(text=paste0(text, collapse = "\n")) %>%
    ungroup() %>%
    mutate(text=paste0("## CLNICAL DATA\n", text))
 
  chat<-chat_openai(
    model = plpModel$modelDesign$modelSettings$param$model_name,
    #model = "qwen3.8",
    #model = "qwen3:30b",
    api_key = "",
    #base_url = "http://192.168.0.226:1234/v1", #http://localhost:1234/v1", # lm studio
    base_url = "http://res-srv-lin-03:11434/v1",
    api_args = list(temperature = plpModel$modelDesign$modelSettings$param$temperature, reasoning_effort = "none"),
    system_prompt = plpModel$modelDesign$modelSettings$param$systemprompt
  )
  
  ## test debug
  # t0<-Sys.time()
  # response <- chat$chat_structured(
  #   covariates_texts$text[1],
  #   type = format_typeobject
  # )
  # print(Sys.time()-t0)
  
  
  if(plpModel$modelDesign$modelSettings$param$parallel){
    print(covariates_texts$text[1])
    # t0<-Sys.time()
    # print("LLM inference starting...")
    # results<-parallel_chat_structured(
    #   chat = chat,
    #   prompts = as.list(covariates_texts$text),
    #   #prompts = as.list(covariates_texts$text)[c(1:200)],
    #   type = format_typeobject,
    #   max_active = 10,
    #   rpm = 1000
    # )
    # print(Sys.time()-t0)
    
    t0 <- Sys.time()
    print("LLM inference starting (batched)...")
    
    # --- config ---
    batch_size   <- 200L
    save_path    <- paste0(plpModel$modelDesign$modelSettings$param$tempsavelocation,"/results.rds")       # cumulative results file
    chat             <- chat
    format_type      <- format_typeobject
    max_active       <- 10
    rpm              <- 1000
    
    if (!dir.exists(plpModel$modelDesign$modelSettings$param$tempsavelocation)) {
      dir.create(plpModel$modelDesign$modelSettings$param$tempsavelocation, recursive = TRUE)
    }
    
    timeout_sec   <- 60L       # max seconds per call before we treat it as a failure
    max_retries   <- 10L       # max attempts per batch
    backoff_base  <- 2         # exponential backoff: 1s, 2s, 4s, 8s, ...
    
    continue_from_saved <- TRUE
    
    # --- possibly load prior results ---
    all_prompts <- as.list(covariates_texts$text)#[1:400]
    n           <- length(all_prompts)
    
    results   <- NULL
    start_idx <- 1L
    
    if (continue_from_saved && file.exists(save_path)) {
      results   <- readRDS(save_path)
      start_idx <- nrow(results) + 1L
      print(paste0("Resuming: already have ", nrow(results),
                   " results.  Next prompt index: ", start_idx))
      
      if (start_idx > n) {
        print("All prompts already processed. Nothing to do.")
      }
    } else {
      print(paste0("Starting fresh. Total prompts: ", n))
    }
    
    # --- retry wrapper ---
    call_with_retry <- function(prompts, label) {
      last_err <- NULL
      
      for (attempt in seq_len(max_retries)) {
        t_start <- Sys.time()
        
        outcome <- tryCatch(
          {
            res <- parallel_chat_structured(
              chat       = chat,
              prompts    = prompts,
              type       = format_type,
              max_active = max_active,
              rpm        = rpm
              # timeout_sec -> set at the HTTP layer, see note below
            )
            res
          },
          error = function(e) e
        )
        
        elapsed <- as.numeric(Sys.time() - t_start)
        
        # success?
        if (!inherits(outcome, "error")) {
          print(paste0("  ", label, "  attempt ", attempt,
                       ": OK (", round(elapsed, 1), "s)"))
          return(outcome)
        }
        
        # record + report
        last_err <- outcome
        print(paste0("  ", label, "  attempt ", attempt, "/", max_retries,
                     "  FAILED (", round(elapsed, 1),
                     "s):  ", conditionMessage(outcome)))
        
        # don't sleep after the last attempt
        if (attempt < max_retries) {
          delay <- backoff_base ^ (attempt - 1)
          print(paste0("    retrying in ", delay, "s ..."))
          Sys.sleep(delay)
        }
      }
      
      # exhausted all retries
      stop(sprintf("[%s] All %d attempts failed. Last error: %s",
                   label, max_retries, conditionMessage(last_err)),
           call. = FALSE)
    }
    
    # --- main loop ---
    if (start_idx <= n) {
      done_in_run <- 0L
      
      while (start_idx <= n) {
        lo <- start_idx
        hi <- min(start_idx + batch_size - 1L, n)
        label <- sprintf("Batch [prompts %d:%d]", lo, hi)
        
        print(paste0("", label))
        
        batch_results <- call_with_retry(
          prompts = all_prompts[lo:hi],
          label   = label
        )
        
        if(is.null(results)){
          results <- batch_results
        } else {
          results <- rbind(results, batch_results)
        }
        saveRDS(results, save_path)
        
        done_in_run <- done_in_run + (hi - lo + 1L)
        print(paste0("  cumulative: ", nrow(results), "/", n,
                     "  |  saved."))
        
        start_idx <- hi + 1L
      }
    }
    
    print(paste0("\nComplete.  Total elapsed: ",
                 round(as.numeric(Sys.time() - t0), 2), "s"))
    print(paste0("Results: ", nrow(results), "/", n,
                 "  |  ", normalizePath(save_path)))
    
    saveRDS(results, paste0(plpModel$modelDesign$modelSettings$param$tempsavelocation,"/final_results_",format(Sys.time(), "%Y%m%d_%H%M"),".rds"))
    #file.rename(save_path, paste0(plpModel$modelDesign$modelSettings$param$tempsavelocation,"/results_",format(Sys.time(), "%Y%m%d_%H%M"),".rds"))
    
    print("LLM inference ended")
    prediction<-results %>%
      mutate(rowId=covariates_texts$rowId[c(1:nrow(results))],
             .before = reasoning)
    prediction<-merge(cohort, prediction, by = "rowId", 
                      all.x = TRUE, fill = 0)
    prediction<-prediction %>%
      mutate(probability=ifelse(is.na(probability), 0, probability)) %>%
      mutate(value=probability)
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