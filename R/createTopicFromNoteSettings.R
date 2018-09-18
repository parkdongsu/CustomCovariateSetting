# load packages
if(!require(parallel)) {
    install.packages("parallel")
}

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

library(parallel)
library(KoNLP)
library(rJava)
library(stringr)
library(parallel)
useSejongDic()



#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param useTopicFromNote use = TURE, not use = FALSE
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' createTopicFromNoteSettings()
createTopicFromNoteSettings <- function(useTopicFromNote = TRUE){
    covariateSettings <- list(useTopicFromNote = useTopicFromNote)
    attr(covariateSettings,'fun') <- 'getTopicFromNoteSettings'
    class(covariateSettings) <- 'covariateSettings'
    return(covariateSettings)
}







