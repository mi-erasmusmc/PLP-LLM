
#' generateCohorts
#'
#' @param root folder
#' @param csv cohortsToCreate csv file
#' @param databaseDetails 
#' @param connectionDetails 
#' @param first new DB tables
#' @param continue new ATLAS cohorts
#'
#' @return
#' @export
#'
#' @examples
generateCohorts <- function(root, csv, databaseDetails, connectionDetails, createDBtables, getCohortsATLAS) {
  
  # Load file
  cohortsToCreate <- read.csv(file.path(root, "inst", csv))
  
  # Create cohort tables
  if (createDBtables) {
    cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable =  databaseDetails$cohortTable)
    
    CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                        cohortDatabaseSchema = databaseDetails$cohortDatabaseSchema,
                                        cohortTableNames = cohortTableNames,
                                        incremental = TRUE)
    
    # This is to add 'cohort_censor_stats'
    cohortTableNames <- CohortGenerator::getCohortTableNames()
    CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                        cohortDatabaseSchema = databaseDetails$cohortDatabaseSchema,
                                        cohortTableNames = cohortTableNames,
                                        incremental = TRUE)
  }
  
  # Retrieve definitions from ATLAS
  if (getCohortsATLAS) {
    for (i in 1:nrow(cohortsToCreate)) {
      writeLines(paste("Copying cohort:", cohortsToCreate$cohortName[i]))
      ROhdsiWebApi::insertCohortDefinitionInPackage(cohortId = cohortsToCreate$cohortId[i],
                                                    name = cohortsToCreate$cohortName[i],
                                                    jsonFolder = file.path(root, "inst", "cohorts", "JSON"),
                                                    sqlFolder = file.path(root, "inst", "cohorts", "SQL"),
                                                    baseUrl = "http://Res-Srv-OHDSI:8080/WebAPI",
                                                    generateStats = F)
    }
  }
  
  connection <- DatabaseConnector::connect(connectionDetails)
  
  # Generate target / outcome / predictor cohorts first time running study
  for (i in 1:nrow(cohortsToCreate)) {
    writeLines(paste("Creating cohort:", cohortsToCreate$cohortName[i]))
    sql <- SqlRender::readSql(paste0(root, "/inst/cohorts/SQL/", cohortsToCreate$cohortName[i], ".sql"))
    sql <- SqlRender::render(sql,
                             vocabulary_database_schema = databaseDetails$cdmDatabaseSchema,
                             cdm_database_schema = databaseDetails$cdmDatabaseSchema,
                             target_database_schema = databaseDetails$cohortDatabaseSchema,
                             target_cohort_table = databaseDetails$cohortTable,
                             target_cohort_id = cohortsToCreate$cohortId[i])
    sql <- SqlRender::translate(sql,
                                targetDialect = attr(connection, "dbms"))
    DatabaseConnector::executeSql(connection, sql)
  }
}

