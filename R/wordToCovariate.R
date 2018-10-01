#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param rowid,covariatesvalue
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' WORD_LOAD()
wordToCovariate <- function(rowid,covariatesvalue,useDictionary){

    result_xml_df <- NoteXmlParser(rowid,covariatesvalue)

    if(useDictionary){result_xml_df <- LanguagePreProcessingFunction(result_xml_df)}

    df <- ExtractorFromDictionary(result_xml_df)
    df <- cbind(df,rep(1,nrow(df)))
    colnames(df) <- c('row_id','covariate_id','covariate_value')

    return(df)
}
