predict_score_bar <- function(wine_in, current_score_bar){
  
  score_bar <- c() 

  for (i in 1:length(wine_in$Artikelid)){
    k = 0
    Fyll <- get_score_sub(wine_in$fyllighet[i], current_score_bar[[1]])
    if(is.na(Fyll)){
      k = k + 1
    }
    Strav <- get_score_sub(wine_in$stravhet[i], current_score_bar[[2]])
    if(is.na(Strav)){
      k = k + 1
    }
    Frukts <- get_score_sub(wine_in$fruktsyra[i], current_score_bar[[3]])
    if(is.na(Frukts)){
      k = k + 1
    }
    
    p = 5*k/3
    if(k == 4){
      p = NA
    }
    score_bar[i] <- mean(c(Frukts,Strav,Fyll),na.rm=TRUE)- p
  }
  
  return(score_bar)
  
  
}