#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param rowid,covariatesvalue
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' XML_PASING_FUNCTION()
XML_PASING_FUNCTION <- function(rowid,covariatesvalue){

    numCores <- parallel::detectCores() - 1

    myCluster <- parallel::makeCluster(numCores)

    diagnosis_list <- parallel::parLapply(cl = myCluster, X = covariatesvalue, fun = XML_PARSING)

    result_xml_list <- parallel::parLapply(cl = myCluster, X = diagnosis_list, fun = DIAG_PROCESSING)

    for (i in 1:length(result_xml_list)){
        result_xml_list[[i]][,'row_id'] <- rowid[i]
    }

    max_col <- 0
    for(i in 1:length(result_xml_list)){
        col_value <- length(result_xml_list[[i]])
        if(max_col < col_value){
            max_col <- col_value
        }
    }

    div = 1000
    flag <- 0
    if(div >= length(result_xml_list)){
        for(i in 1:length(result_xml_list)){
            if(length(result_xml_list[[i]]) == max_col){
                result_tmp_df <- rbind(result_tmp_df,result_xml_list[[i]])
            }
        }
        flag <- 1
        result_xml_df <- result_tmp_df
    }
    if(flag == 0 ){
        for(i in 1:length(result_xml_list)){

            if(length(result_xml_list[[i]]) == max_col){

                result_tmp_df <- rbind(result_tmp_df,result_xml_list[[i]])

                if(i%%div ==0 & i>=div){
                    result_xml_df <- rbind(result_xml_df,result_tmp_df)
                    result_tmp_df <- data.frame(stringsAsFactors = FALSE)
                }
            }
            else{

            }
        }

        result_xml_df <- rbind(result_xml_df,result_tmp_df)
    }

    # 클러스터 중지
    parallel::stopCluster(myCluster)

    return(result_xml_df)
}
