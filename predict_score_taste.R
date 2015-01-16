predict_score_taste <- function(all_wine,current_score_taste){
  source('~/Rscripts/Webcrawler/get_score_per_word.R')
  
  score_out <- unlist(lapply(all_wine$smak, function(x) x<- get_score_per_word(x,current_score_taste)))
  
  return(score_out)
}