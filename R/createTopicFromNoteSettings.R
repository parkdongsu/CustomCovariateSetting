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
                                        useTextToVec = FALSE,
                                        useTopicModeling=FALSE,
                                        numberOfTopics=10L,
                                        useGloVe = FALSE,
                                        LatentDimensionForGlove = 100L,
                                        useAutoencoder=FALSE,
                                        LatentDimensionForAutoEncoder = 100L){
    if (sum (useTextToVec,useTopicModeling,useGloVe,useAutoencoder) != 1 ) print ("Choose only one among useTextToVec,useTopicModeling,useGloVe,useAutoencoder")
    covariateSettings <- list(useTopicFromNote = useTopicFromNote,
                              useDictionary=useDictionary,
                              useEmbedding = useEmbedding)
    attr(covariateSettings,'fun') <- 'getTopicFromNoteSettings'
    class(covariateSettings) <- 'covariateSettings'
    return(covariateSettings)
}



#



