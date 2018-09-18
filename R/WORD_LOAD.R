#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param rowid,covariatesvalue
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' WORD_LOAD()
WORD_LOAD <- function(rowid,covariatesvalue){
    
    result_xml_df <- XML_PASING_FUNCTION(rowid,covariatesvalue)
    
    doc.df <- NLP_PROCESSING_FUNCTION(result_xml_df)
    
    
    
    return(doc.df)
}
