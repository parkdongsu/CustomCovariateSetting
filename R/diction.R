#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' diction()
diction <- function(){
    dic <- read.csv('./DIC/medi_dic.csv',stringsAsFactors = F)
    return(dic)
}
