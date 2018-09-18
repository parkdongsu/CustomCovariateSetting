#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param doc.df
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' DIC_COMPARE()
DIC_COMPARE <- function(doc.df){

    dictionary <- diction()
    colnames(dictionary) <- c('word')

    kor_tmp_word <- c()
    kor_word <- c()

    for(i in 1:nrow(doc.df)){
        only_kor <-strsplit(doc.df$'word'[i],' ')[[1]]

        only_kor <-unique(only_kor)

        kor_tmp_word <- c(kor_tmp_word,only_kor)

        if(i%%100 == 0){
            kor_word <- c(kor_word,kor_tmp_word)
            kor_tmp_word <- c()
        }
    }
    kor_word <- c(kor_word,kor_tmp_word)
    kor_word_unique <- unique(kor_word)

    df <- data.frame(word = c(kor_word_unique),stringsAsFactors = FALSE)

    merge_df <- merge(df,dictionary,by.x = 'word')

    n_gram_1 <-  as.vector(merge_df[,1])

    n_gram_union <- n_gram_1

    kor_tmp_word <- c()
    kor_word <- c()
    for(i in 1:nrow(doc.df)){

        only_kor <-strsplit(doc.df$word[i],' ')[[1]]
        only_kor <-unique(only_kor)

        kor_tmp_word <- c(kor_tmp_word,only_kor)

        if(i%%100 == 0){
            kor_word <- c(kor_word,kor_tmp_word)
            kor_tmp_word <- c()
        }
    }
    kor_word <- c(kor_word,kor_tmp_word)
    kor_word_unique <- unique(kor_word)

    word_storage <- c()
    word_tmp_storage <- c()

    for(i in 1:length(n_gram_union)){

        word_tmp_storage <- c(word_tmp_storage,kor_word_unique[grep(n_gram_union[i],kor_word_unique)])
        if(i %% 10 == 0){
            word_storage <- c(word_storage,word_tmp_storage)
            word_tmp_storage <- c()
        }
    }
    word_storage <- c(word_storage,word_tmp_storage)

    diag_word_tmp_df <- data.frame(stringsAsFactors = F)
    diag_word_df <- data.frame(stringsAsFactors = F)
    for(i in 1:nrow(doc.df)){
        word <- strsplit(doc.df$'word'[i],' ')[[1]]

        #영어는 따로 분리
        eng_word <- gsub('[^a-zA-Z]','',word)
        eng_word[length(eng_word)+1] <- c("")
        only_eng <- eng_word[-which(eng_word == "")]


        diag_word <- c(intersect(word,word_storage),only_eng)

        diag_word_tmp_tmp_df <- data.frame('row_id' = rep(doc.df$row_id[i],length(diag_word)),'word' = diag_word,stringsAsFactors = F)

        #rbind 나눠서 진행
        diag_word_tmp_df <- rbind(diag_word_tmp_df,diag_word_tmp_tmp_df)
        if(i %% 100 == 0){
            diag_word_df <- rbind(diag_word_df,diag_word_tmp_df)
            diag_word_tmp_df <- data.frame(stringsAsFactors = F)
        }
    }
    diag_word_df <- rbind(diag_word_df,diag_word_tmp_df)

    return(diag_word_df)
}

