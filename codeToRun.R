
### Settings
rerun <- FALSE
plptasks <- c("CHADS2")
covariates = "AgeSex" # "GroupedFeatures"
iterations <- 2

### Libraries & paths
root <- getwd()
name <- Sys.Date()

outputFolder <- file.path(root, "output", name)
if (!dir.exists(outputFolder)) {
  dir.create(outputFolder, recursive = TRUE)
}

### Files
library(PatientLevelPrediction)
library(rlang)
source("~/Documents/Projects/PLP-LLM/R/taskSpecifications.R")
source("~/Documents/Projects/PLP-LLM/R/generateCohorts.R")
source("~/Documents/Projects/PLP-LLM/R/createPredictors.R")
source("~/Documents/Projects/PLP-LLM/R/runModels.R")
# source("~/Documents/Projects/PLP-LLM/R/existingModels.R")
source("~/Documents/Projects/PLP-LLM/R/llmPredict.R")

### Database connection
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = Sys.getenv("DB_DBMS"),
                                                                server = paste(Sys.getenv("DB_SERVER"), "CDM-S-20250415", sep="/"),
                                                                user = Sys.getenv("DB_USER"),
                                                                password = Sys.getenv("DB_PW"),
                                                                port = Sys.getenv("DB_PORT"))

databaseDetails <- PatientLevelPrediction::createDatabaseDetails(connectionDetails = connectionDetails,
                                                                 cdmDatabaseSchema = "cdm",
                                                                 cdmDatabaseId = "CDM-S-20250415", 
                                                                 cohortDatabaseSchema = "aniektom",
                                                                 cohortTable = "LLM_cohorts",
                                                                 cdmDatabaseName = "IPCI")

### Generate target and outcome cohorts
if (rerun) {
  generateCohorts(root = root,
                  csv = "cohortsToCreate_tasks.csv",
                  databaseDetails = databaseDetails,
                  connectionDetails = connectionDetails,
                  createDBtables = TRUE, # set to FALSE after 1 time running
                  getCohortsATLAS = FALSE) 
}

#### Run models
for (plp in plptasks) {
  
  # Predictors
  plpData <- createPredictors(plp = plp,
                              covariates = covariates,
                              databaseDetails = databaseDetails,
                              outputFolder = outputFolder)
  
  # Model
  models <- c("LASSO") # "ExistingScore", "LLM_Qwen3"
  
  for (m in models) {
    iterations_m <- ifelse(m != "ExistingScore", iterations, 1)
    
    for (i in 1:iterations_m) {
      runModels(plp = plp,
                plpData = plpData,
                model = m,
                iteration = i,
                outputFolder = outputFolder)
    }
  }
}

### Evaluation metrics & output figures
studyOutput(outputFolder)



