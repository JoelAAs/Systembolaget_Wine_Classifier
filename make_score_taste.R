make_score_taste <- function(wine_in){
  
  wine_in      <- wine_in[which(wine_in$real_score != 99),]
  
  all_taste    <- unlist(lapply(wine_in$smak, function(x) x<- strsplit(x,"\\.")))
  unique_taste <- unique(all_taste)
  
  taste_score  <- data.frame(word = character(0), score = numeric(0), stringsAsFactors = F)
  
  for (i in 1:length(unique_taste)){
    score_tmp           <- data.frame(unique_taste[i], mean(wine_in$real_score[which(grepl(unique_taste[i] ,wine_in$smak))]),stringsAsFactors = F)
    colnames(score_tmp) <- c("word","score")
    taste_score         <- rbind(taste_score,score_tmp)
  }
 return(taste_score) 
}