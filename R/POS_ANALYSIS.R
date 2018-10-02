#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param word_df
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' POS_ANALYSIS()
POS_ANALYSIS <- function(word_df){
    word_list <- KoNLP::SimplePos22(word_df)
    result_word_list <-c()
    if(length(word_list) ==1){
        word_vector <- word_list[[1]]
        result_word_list <- c(word_vector)
    }
    else{
        word_vector <- word_list[[1]]
        for (k in 2:length(word_list)){
            word_vector <- paste(word_vector,'+',word_list[[k]],sep = '')
        }
        result_word_list <- c(result_word_list,word_vector)
    }
    return(result_word_list)
}



