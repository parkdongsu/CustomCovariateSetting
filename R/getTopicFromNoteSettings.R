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
library(caret)
library(dplyr)
library(text2vec)
library(e1071)
useSejongDic()



getTopicFromNoteSettings <- function(connection,
                                     oracleTempSchema = NULL,
                                     cdmDatabaseSchema,
                                     cohortTable = "cohort",
                                     #cohortId = -1, #cohortId 미지정
                                     cohortId = cohortId, # cohortId 지정
                                     cdmVersion = "5",
                                     rowIdField = "subject_id",
                                     conceptId = conceptId,
                                     covariateSettings,
                                     aggregated = FALSE){

    writeLines('Constructing TopicFromNote')
    if (covariateSettings$useTopicFromNote == FALSE) {
        return(NULL)
    }
    if (covariateSettings$useDictionary == TRUE){
        # Some SQL to construct the covariate:
        sql <- paste(
            'SELECT top 100 @row_id_field AS row_id,',
            'n.NOTE_TEXT AS covariate_id,',
            '1 AS covariate_value',
            'FROM @cdm_database_schema.NOTE n',
            'JOIN @cohort_table c',
            'ON n.person_id = c.subject_id',
            'AND n.NOTE_DATE = c.COHORT_START_DATE',
            'WHERE NOTE_TYPE_CONCEPT_ID = @concept_id',
            #cohord_id가 지정되었을 때
            'AND cohort_definition_id = @cohort_id'
            )
        #cohort_id가 지정되지 않았을 때
        #"{@cohort_id != -1} ? {AND cohort_definition_id = @cohort_id}"

        sql <- SqlRender::renderSql(sql,
                                    cohort_table = cohortTable,
                                    cohort_id = cohortId,
                                    concept_id = conceptId,
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

        if(covariateSettings$useTextToVec == TRUE){
            ##Text2Vec
            covariates <- covariates

            covariateId.factor<-as.factor(covariates$covariateId)

            covariateRef  <- data.frame(covariateId = seq(levels(covariateId.factor)),
                                        covariateName = levels(covariateId.factor),
                                        analysisId = 1,
                                        conceptId = 0)
            covariateRef <- ff::as.ffdf(covariateRef)
        }

        if(covariateSettings$useTopicModeling == TRUE){

            covariates.df<-data.frame(covariates)

            covariates.df$rowId <- as.numeric(as.factor(covariates$rowId))
            covariates.df$covariateId<-as.numeric(as.factor(covariates$covariateId))
            covariates.df

            data <- Matrix::sparseMatrix(i=covariates.df$rowId,
                                         j=covariates.df$covariateId,
                                         x=covariates.df$covariateValue,
                                         dims=c(max(covariates.df$rowId), max(covariates.df$covariateId))) # edit this to max(map$newIds)


            colnames(data) <- unique(covariates.df$covariateId)

            ##Topic Modeling
            lda_model = text2vec::LDA$new(n_topics = covariateSettings$numberOfTopics, doc_topic_prior = 0.1, topic_word_prior = 0.01)
            doc_topic_distr =   lda_model$fit_transform(x = data, n_iter = 1000,
                                                        convergence_tol = 0.001, n_check_convergence = 25,
                                                        progressbar = FALSE)

            doc_topic_distr_df <- data.frame(doc_topic_distr)

            zzz <- unique(data.frame(covariates$rowId,covariates.df$rowId,stringsAsFactors = F))


            #return 값이 row_id는 그대로/   id는 topic   /    value는 0.3

            covariateRef  <- data.frame(covariateId = seq(levels(covariateId.factor)),
                                        covariateName = levels(covariateId.factor),
                                        analysisId = 1,
                                        conceptId = 0)
            covariateRef <- ff::as.ffdf(covariateRef)
        }

        if(covariateSettings$useGloVe == TRUE){
            break
        }

        if(covariateSettings$useAutoencoder == TRUE){
            break
        }






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
