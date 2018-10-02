#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' diction()
diction <- function(){
    if(language=="Korean") {
        dicDb = kor_dictionary_db
    } else {
            stop("Currently only Korean is available")
        }
    return(dicDb)
}




