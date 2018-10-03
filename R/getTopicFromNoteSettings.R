#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @connection connection,oracleTempSchema,cdmDatabaseSchema,cohortTable,cohortId,cdmVersion,rowIdField,covariateSettings,aggregated
#' @oracleTempSchema createCovariateSetting
#' @cdmDatabaseSchema
#' @cohortTable
#' @cohortId
#' @cdmVersion
#' @rowIdField
#' @noteConceptId
#' @covariateSettings
#' @aggregated
#' getTopicFromNoteSettings()

# load packages
if(!require(rJava)) {
    install.packages('rJava')
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
if(!require(caret)) {
    install.packages("caret")
}
if(!require(caret)) {
    install.packages("text2vec")
}
if(!require(caret)) {
    install.packages("e1071")
}


#library(KoNLP)
library(rJava)
library(stringr)
library(parallel)
library(caret)
library(dplyr)
library(text2vec)
library(e1071)

getTopicFromNoteSettings <- function(connection,
                                     oracleTempSchema = NULL,
                                     cdmDatabaseSchema,
                                     cohortTable = "cohort",
                                     cohortId = -1,
                                     cdmVersion = "5",
                                     rowIdField = "subject_id",
                                     #noteConceptId = noteConceptId,
                                     covariateSettings,
                                     aggregated = FALSE){

    writeLines('Constructing TopicFromNote')
    if (covariateSettings$useTopicFromNote == FALSE) {
        return(NULL)
    }
    if (covariateSettings$useDictionary == TRUE){
        # Some SQL to construct the covariate:
        sql <- paste(
            'SELECT',
            '{@sampleSize != -1} ? {TOP @sampleSize}',
            " @row_id_field AS row_id,",
            'n.NOTE_TEXT AS covariate_id,',
            '1 AS covariate_value',
            'FROM @cdm_database_schema.NOTE n',
            'JOIN @cohort_table c',
            'ON n.person_id = c.subject_id',
            'AND n.NOTE_DATE = c.COHORT_START_DATE',
            'WHERE NOTE_TYPE_CONCEPT_ID = @note_concept_id',
            '{@cohort_id != -1} ? {AND cohort_definition_id = @cohort_id}'
            )

        sql <- SqlRender::renderSql(sql,
                                    cohort_table = cohortTable,
                                    cohort_id = cohortId,
                                    note_concept_id = covariateSettings$noteConceptId,
                                    row_id_field = rowIdField,
                                    sampleSize=covariateSettings$sampleSize,
                                    cdm_database_schema = cdmDatabaseSchema)$sql
        sql <- SqlRender::translateSql(sql, targetDialect = attr(connection, "dbms"))$sql



        # Retrieve the covariate:
        rawCovariates <- DatabaseConnector::querySql.ffdf(connection, sql)
        colnames(rawCovariates)<-tolower(colnames(rawCovariates))
        row_id              <-  rawCovariates$row_id
        covariates_value    <- rawCovariates$covariate_id

        covariates <- wordToCovariate(row_id,covariates_value,useDictionary)

        # Convert colum names to camelCase:
        colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))
        rowIds<-levels(as.factor(covariates$rowId))

        if(covariateSettings$useTextToVec == TRUE){
            ##Text2Vec
            covariateId.factor<-as.factor(covariates$covariateId)
            covariateRef  <- data.frame(covariateId = seq(levels(covariateId.factor)),
                                        covariateName = levels(covariateId.factor),
                                        analysisId = 0,
                                        conceptId = 0)
            covariateRef <- ff::as.ffdf(covariateRef)
        }

        if(covariateSettings$useTopicModeling == TRUE){

            covariates.df<-data.frame(covariates)
            covariates.df$rowId <- as.numeric(as.factor(covariates$rowId))
            covariates.df$covariateId<-as.numeric(as.factor(covariates$covariateId))

            data <- Matrix::sparseMatrix(i=covariates.df$rowId,
                                         j=covariates.df$covariateId,
                                         x=covariates.df$covariateValue,
                                         dims=c(max(covariates.df$rowId), max(covariates.df$covariateId))) # edit this to max(map$newIds)
            colnames(data) <- unique(covariates.df$covariateId)

            dim(data)

            ##Topic Modeling
            lda_model = text2vec::LDA$new(n_topics = covariateSettings$numberOfTopics, doc_topic_prior = 0.1, topic_word_prior = 0.01)
            doc_topic_distr = lda_model$fit_transform(x = data, n_iter = 1000,
                                                        convergence_tol = 0.001, n_check_convergence = 25,
                                                        progressbar = FALSE)

            doc_topic_distr_df <- data.frame(doc_topic_distr)
            covariateIds<-as.numeric(paste0(as.numeric(1:length(doc_topic_distr_df)),'00'))
            colnames(doc_topic_distr_df)<-covariateIds
            doc_topic_distr_df$rowId<-rowIds

            covariates<-reshape2::melt(doc_topic_distr_df,id.var = "rowId",
                                               variable.name="covariateId",
                                               value.name = "covariateValue")
            ##need to remove 0
            covariateRef  <- data.frame(covariateId = covariateIds,
                                        covariateName = paste0("Topic",covariateIds),
                                        analysisId = 0,
                                        conceptId = 0)
            covariateRef <- ff::as.ffdf(covariateRef)
        }

        if(covariateSettings$useGloVe == TRUE){
            stop("useGlove has not not supported yet")
        }

        if(covariateSettings$useAutoencoder == TRUE){
            stop("useAutoencoder has not not supported yet")
        }

        # Construct analysis reference:
        analysisRef <- data.frame(analysisId = 0,
                                  analysisName = "Features from Note",
                                  domainId = "Note",
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
