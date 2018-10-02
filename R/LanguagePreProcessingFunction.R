#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param result_xml_df
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' LanguagePreProcessingFunction()
LanguagePreProcessingFunction <- function(result_xml_df){

    numCores <- parallel::detectCores() - 1

    myCluster <- parallel::makeCluster(numCores)

    search_df <- result_xml_df[result_xml_df$`<MN>`=='현병력',]

    tag ='<TD>'

    search_df[,tag][is.na(search_df[,tag])] <- ""

    for (i in nrow(search_df):1){
        if(search_df[i,tag] == ""){
            search_df <- search_df[-i,]
        }
    }

    xml_df <- search_df[tag]

    word_df <- data.frame('diagnosis' = parallel::parApply(myCluster,xml_df,1,NLP_PROCESSING),stringsAsFactors = F)

    doc.df <- data.frame(c(word_df),'row_id' = search_df$row_id,stringsAsFactors = F)

    #result_word_list <- apply(word_df,1,POS_ANALYSIS)

    #result_word_list<- unlist(result_word_list)

    #doc.list <- parallel::parLapply(myCluster,result_word_list,K_POS_EXTRACTION)

    #doc.df <- data.frame('word' = unlist(doc.list),'row_id' = search_df$row_id ,stringsAsFactors = FALSE)

    parallel::stopCluster(myCluster)

    return(doc.df)

}
