predict_score_RCGY <-function(wine_in, current_score_RCGY){
  score_RCGY <- c() 
  
  wine_in$simple_region <- unlist(lapply(wine_in$Ursprung, function(x) x<- strsplit(x,"\\.")[[1]][1]))
  wine_in$class_grape   <- get_class_wine(wine_in$RavarorBeskrivning)
  
  for (i in 1:length(wine_in$Artikelid)){
    k = 0
    R <- get_score_sub(wine_in$simple_region[i], current_score_RCGY)
    if(is.na(R)){
      k= k +1
    }
    C <- get_score_sub(wine_in$Ursprung[i], current_score_RCGY)
    if(is.na(C)){
      k= k +1
    }
    G <- get_score_sub(wine_in$class_grape[i], current_score_RCGY)
    if(is.na(G)){
      k= k +1
    }
    Y <- get_score_sub(wine_in$Argang[i], current_score_RCGY)
    if(is.na(Y)){
      k= k +1
    }
    p = 5*k/4
    if(k == 4){
      p = NA
    }
    score_RCGY[i] <- mean(c(R,C,G,Y),na.rm=TRUE)-p
  }
  
  return(score_RCGY)
}