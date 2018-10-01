#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' diction()
diction <- function(){

    dic <- read.csv(paste(getwd(),'/MedicalDictionary/KOREA.csv',sep=''),stringsAsFactors = F)
    return(dic)
}

