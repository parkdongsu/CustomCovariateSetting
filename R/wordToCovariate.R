#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param rowid,covariatesvalue
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' wordToCovariate()
wordToCovariate <- function(rowid,covariatesvalue,useDictionary,language){

    result_xml_df <- NoteXmlParser(rowid,covariatesvalue)

    doc.df <- LanguagePreProcessingFunction(result_xml_df)

    df <- ExtractorFromDictionary(doc.df,language)
    df <- cbind(df,rep(1,nrow(df)))
    colnames(df) <- c('row_id','covariate_id','covariate_value')


    return(df)
}
