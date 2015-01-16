make_score_Bar <- function(wine_in){
  
  
  table_fyllighet            <- data.frame(name = character(0),score = numeric(0), stringsAsFactors=F)
  table_stravhet             <- data.frame(name = character(0), score = numeric(0),  stringsAsFactors=F)
  table_fruktsyra            <- data.frame(name = character(0),score = numeric(0), stringsAsFactors=F)
  
  wine_in      <- wine_in[which(wine_in$real_score != 99),]
  fyllighet    <- unique(wine_in$fyllighet)
  stravhet     <- unique(wine_in$stravhet)
  fruktsyra    <- unique(wine_in$fruktsyra)
  
  for(i in 1:length(fyllighet)){
    tmp            <- data.frame(fyllighet[i],
                                 mean(wine_in$real_score[which(wine_in$fyllighet == fyllighet[i])]),
                                 stringsAsFactors = F)
    colnames(tmp)  <- c("name","score")
    table_fyllighet <- rbind(table_fyllighet,tmp)
  }
  
  for(i in 1:length(stravhet)){
    tmp            <- data.frame(stravhet[i],
                                 mean(wine_in$real_score[which(wine_in$stravhet == stravhet[i])]),
                                 stringsAsFactors = F)
    colnames(tmp)  <- c("name","score")
    table_stravhet <- rbind(table_stravhet,tmp)
  }
  
  for(i in 1:length(fruktsyra)){
    tmp            <- data.frame(fruktsyra[i],
                                 mean(wine_in$real_score[which(wine_in$fruktsyra == fruktsyra[i])]),
                                 stringsAsFactors = F)
    colnames(tmp)  <- c("name","score")
    table_fruktsyra <- rbind(table_fruktsyra,tmp)
  }
  
  return(list(table_fyllighet,table_stravhet,table_fruktsyra))
  
}
