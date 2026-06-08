### Settings
test <- TRUE
rerun <- FALSE
plptasks <- if (test) c("Eunomia") else c("CHADS2")
covariates <- "AgeSex" # "GroupedFeatures"
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
source(file.path(root, "R", "taskSpecifications.R"))
source(file.path(root, "R", "generateCohorts.R"))
source(file.path(root, "R", "createPredictors.R"))
source(file.path(root, "R", "runModels.R"))
# source(file.path(root, "R", "existingModels.R"))
source(file.path(root, "R", "llmPredict.R"))
source(file.path(root, "R", "studyOutput.R"))

### Database setup
if (test) {
  databaseDetails <- taskSpecifications("Eunomia", "databaseDetails")
} else {
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = Sys.getenv("DB_DBMS"),
    server = paste(Sys.getenv("DB_SERVER"), "CDM-S-20250415", sep = "/"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PW"),
    port = Sys.getenv("DB_PORT")
  )

  databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "cdm",
    cdmDatabaseId = "CDM-S-20250415",
    cohortDatabaseSchema = "aniektom",
    cohortTable = "LLM_cohorts",
    cdmDatabaseName = "IPCI"
  )
}

### Generate target and outcome cohorts
if (rerun && !test) {
  generateCohorts(
    root = root,
    csv = "cohortsToCreate_tasks.csv",
    databaseDetails = databaseDetails,
    connectionDetails = connectionDetails,
    createDBtables = TRUE,
    getCohortsATLAS = FALSE
  )
}

### Run models
for (plp in plptasks) {
  plpData <- createPredictors(
    plp = plp,
    covariates = covariates,
    databaseDetails = databaseDetails,
    outputFolder = outputFolder
  )

  models <- c("LASSO") # "ExistingScore", "LLM_Qwen3"

  for (m in models) {
    iterations_m <- ifelse(m != "ExistingScore", iterations, 1)

    for (i in 1:iterations_m) {
      runModels(
        plp = plp,
        plpData = plpData,
        model = m,
        iteration = i,
        outputFolder = outputFolder
      )
    }
  }
}

### Evaluation metrics & output figures
studyOutput(outputFolder)
