groupingfunction <- function(covariateData,connectionDetails){
  # 101-108 conditionOccurence, 201-216 ConditionGroup, 301-304 DrugExp, 401-410 DrugEra, 409-416 DrugGroupEra
  analysisRef <- covariateData$analysisRef %>% as.data.frame()
  covariateRef_df <- covariateData$covariateRef %>% as.data.frame()
  print(paste("number of covariateIds in covref:",length(unique(covariateRef_df$covariateId))))
  covariates_df <- covariateData$covariates %>% as.data.frame()
  print(paste("number of covariateIds in covariates:",length(unique(covariates_df$covariateId))))
  analyses<-analysisRef$domainId
  if (any(c("Condition","Drug") %in% analyses)){
    print("starting mapping")
    connection <- DatabaseConnector::connect(connectionDetails)
    
    if("Condition" %in% analyses){
      print("mapping conditions")
      
      ## Get the mapping table
      condition_mapping <- DatabaseConnector::querySql(connection, sql="select * from aniektom.condition_snomed_to_icd10")
      
      con_covariateRef_df_mapping <- covariateRef_df %>%
        filter(analysisId %in% c(101:108,201:216)) %>%
        left_join(condition_mapping,by = join_by("conceptId"=="condition_concept_id")) %>%
        filter(!is.na(concept_name)) %>% ## remove all covariates that do not really exist in the database.
        filter(!is.na(icd_name)) %>% ## remove all covariates that could not be mapped.
        mutate(covariateName=paste0(strsplit(covariateName,":")[[1]][1],": ",tricode," ",icd_name),
               covariateId_new=icd_concept_id*1000+analysisId,
               conceptId=icd_concept_id) %>%
        select(covariateId, covariateId_new, covariateName, analysisId, conceptId)
      print(paste("number of covariateIds in condition covref new:",length(unique(con_covariateRef_df_mapping$covariateId_new))))
      print(paste("number of condition analysisIds:",length(unique(con_covariateRef_df_mapping$analysisId))))
      
      covariateRef_condition <- con_covariateRef_df_mapping %>%
        mutate(covariateId=covariateId_new, valueAsConceptId=NA, collisions=NA)%>%
        select(-covariateId_new) %>%
        distinct()
      
      # Apply the mapping to the covariates
      covariates_condition <- covariates_df %>%
        left_join(con_covariateRef_df_mapping[,c("covariateId","covariateId_new")],
                  by = join_by(covariateId), relationship = "many-to-many") %>%
        mutate(covariateId=covariateId_new) %>%
        filter(!is.na(covariateId)) %>% ## filter out the covariateids that cannot be mapped.
        select(-covariateId_new) %>%
        distinct()
      print(paste("number of covariateIds in condition covariates new:",length(unique(covariates_condition$covariateId))))
    } else {
      covariateRef_condition<-NULL
      covariates_condition<-NULL
    }
    
    if("Drug" %in% analyses){
      print("mapping drugs")
      
      covariateRef_drug <- covariateRef_df %>%
        filter(analysisId %in% c(301:416))
      
      covariates_drug <- covariates_df %>%
        filter(covariateId %in% covariateRef_drug$covariateId)
      
    } else {
      covariateRef_drug <- NULL
      covariates_drug <- NULL
    }
  }
  
  ## rebuild the covariateData object
  print("rebuilding covariates")
  
  covariateRef_others <- covariateRef_df%>%
    filter(!analysisId %in% c(101:108,201:216,301:304,401:416))
  covariateRef_new <- rbind(covariateRef_others, covariateRef_condition, covariateRef_drug)
  print(paste("number of covariateIds in covariateRef new:",length(unique(covariateRef_new$covariateId))))
  print(paste("number of analysisIds in covariateRef new:",length(unique(covariateRef_new$analysisId))))
  
  covariates_others <- covariates_df %>%
    filter(covariateId %in% covariateRef_others$covariateId)
  covariates_new <- rbind(covariates_others,covariates_condition,covariates_drug)
  print(paste("number of covariateIds in covariates new:",length(unique(covariates_new$covariateId))))
  
  analysisRef_new <- analysisRef %>%
    filter(analysisId %in% covariateRef_new$analysisId)
  print(paste("number of analysisIds in analysisRef new:",length(unique(covariateRef_new$analysisId))))
  
  result <- Andromeda::andromeda(
    covariates = covariates_new,
    covariateRef = covariateRef_new,
    analysisRef = analysisRef_new
  )
  
  attr(result, "metaData") <- attributes(covariateData)$metaData
  class(result) <- "CovariateData"
  print("Group mapping done.")
  covariateData<-result
  return(result)
}
