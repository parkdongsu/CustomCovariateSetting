#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param connection,oracleTempSchema,cdmDatabaseSchema,cohortTable,cohortId,cdmVersion,rowIdField,covariateSettings,aggregated
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' getTopicFromNoteSettings()

# load packages



# load packages
if(!require(rJava)) {
    install.packages('rJava')
}
if(!require(KoNLP)) {
    install.packages('KoNLP')
}
if(!require(devtools)) {
    install.packages('devtools')
}

if(!require(openNLP)) {
    install.packages('openNLP')
}
if(!require(NLP)) {
    install.packages('NLP')
}
if(!require(parallel)) {
    install.packages("parallel")
}

library(KoNLP)
library(rJava)
library(stringr)
library(parallel)
useSejongDic()



getTopicFromNoteSettings <- function(connection,
                                     oracleTempSchema = NULL,
                                     cdmDatabaseSchema,
                                     cohortTable = "cohort",
                                     cohortId = -1,
                                     cdmVersion = "5",
                                     rowIdField = "subject_id",
                                     covariateSettings,
                                     aggregated = FALSE){

    writeLines('Constructing TopicFromNote')
    if (covariateSettings$useTopicFromNote == FALSE) {
        return(NULL)
    }
    if (covariateSettings$useDictionary == TRUE){
        # Some SQL to construct the covariate:
        sql <- paste(
            'SELECT * @row_id_field AS row_id,',
            'n.NOTE_TEXT AS covariate_id,',
            '1 AS covariate_value',
            'FROM @cdm_database_schema.NOTE n',
            'JOIN @cohort_table c',
            'ON n.person_id = c.subject_id',
            'AND n.NOTE_DATE = c.COHORT_START_DATE',
            'WHERE NOTE_TYPE_CONCEPT_ID = 44814637',
            "{@cohort_id != -1} ? {AND cohort_definition_id = @cohort_id}")

        sql <- SqlRender::renderSql(sql,
                                    cohort_table = cohortTable,
                                    cohort_id = cohortId,
                                    row_id_field = rowIdField,
                                    cdm_database_schema = cdmDatabaseSchema)$sql
        sql <- SqlRender::translateSql(sql, targetDialect = attr(connection, "dbms"))$sql



        # Retrieve the covariate:
        covariates <- DatabaseConnector::querySql.ffdf(connection, sql)

        row_id              <-  covariates$ROW_ID
        covariates_value    <- covariates$COVARIATE_ID

        covariates <- WORD_LOAD(row_id,covariates_value)

        # Convert colum names to camelCase:
        colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))

        # Construct covariate reference:
        covariateRef <- data.frame(covariateId = 1,
                                   covariateName = "Length of observation",
                                   analysisId = 1,
                                   conceptId = 0)
        covariateRef <- ff::as.ffdf(covariateRef)
        # Construct analysis reference:
        analysisRef <- data.frame(analysisId = 1,
                                  analysisName = "Length of observation",
                                  domainId = "Demographics",
                                  startDay = 0,
                                  endDay = 0,
                                  isBinary = "N",
                                  missingMeansZero = "Y")
        analysisRef <- ff::as.ffdf(analysisRef)
    }

    if (aggregated)
        stop("Aggregation not supported")

    # Construct analysis reference:
    metaData <- list(sql = sql, call = match.call())
    result <- list(covariates = covariates,
                   covariateRef = covariateRef,
                   analysisRef = analysisRef,
                   metaData = metaData)
    class(result) <- "covariateData"
    return(result)

}
