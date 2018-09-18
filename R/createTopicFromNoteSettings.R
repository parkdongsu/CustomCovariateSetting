#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param useTopicFromNote use = TURE, not use = FALSE
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' createTopicFromNoteSettings()
createTopicFromNoteSettings <- function(useTopicFromNote = TRUE,
                                        useDictionary=TRUE,
                                        useEmbedding = TRUE){
    covariateSettings <- list(useTopicFromNote = useTopicFromNote,
                              useDictionary=useDictionary,
                              useEmbedding = useEmbedding)
    attr(covariateSettings,'fun') <- 'getTopicFromNoteSettings'
    class(covariateSettings) <- 'covariateSettings'
    return(covariateSettings)
}







