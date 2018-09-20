#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param wordlist
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' K_POS_EXTRACTION()
K_POS_EXTRACTION <- function(wordlist){

    wordlist <- gsub('/F+','/CW+',wordlist)
    wordlist <- gsub('/NC+','/CW+',wordlist)

    pos_start <- as.vector(gregexpr('[^+]+\\/CW[+]',wordlist)[[1]])
    pos_length <- as.vector(attr(gregexpr('[^+]+\\/CW[+]',wordlist)[[1]],'match.length'))

    pos_end <- pos_start+pos_length-5

    word_data = rep(NA,length(pos_start))
    word <- c()
    for(i in 1:length(pos_start)){
        word_data[i] <- substr(wordlist,pos_start[i],pos_end[i])
        word <- paste(word,word_data[i])
    }
    word <- substr(word,2,nchar(word))

    return(word)
}
