get_score_per_word <- function(word_list ,score_list){
  word_list <- unlist(unique(strsplit(word_list,"\\.")))
  score_tmp <- 0
  k = 0
  
  for(i in 1:length(word_list)){
    if (word_list[i] %in% score_list$word){
      k = k + 1
      score_tmp <- score_tmp + as.numeric(score_list$score[which(word_list[i] %in% score_list$word)])
    }
  }
  if(k < 5 && k != 0){
    return((score_tmp-3/k)/k)
  } else {
    return(score_tmp)
  }
}