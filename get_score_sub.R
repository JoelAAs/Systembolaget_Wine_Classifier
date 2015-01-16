get_score_sub <- function(in_str, current_score){
  
  if(in_str %in% current_score$name){
    score <- current_score$score[grepl(in_str,current_score$name)]
  }else{
    score <- NA
  }
  return(score)
  
}