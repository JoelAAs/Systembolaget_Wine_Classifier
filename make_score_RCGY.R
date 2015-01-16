make_score_RCGY <- function(wine_in){
  
  
  table_out             <- data.frame(name =character(0),score = numeric(0), stringsAsFactors=F)
  wine_in               <- wine_in[which(is.finite(wine_in$GivenScore)),]
  wine_in$simple_region <- unlist(lapply(wine_in$Ursprung, function(x) x<- strsplit(x,"\\.")[[1]][1]))
  wine_in$class_grape   <- unlist(get_class_wine(wine_in$RavarorBeskrivning))
  regions <- unlist(unique(wine_in$simple_region))
  regions <- regions[!is.na(regions)]
  country <- unique(wine_in$Ursprunglandnamn)
  country <- country[!is.na(country)]
  grape   <- unique(wine_in$class_grape)
  grape   <- grape[!is.na(grape)]
  year    <- unique(wine_in$Argang)
  year    <- year[!is.na(year)]
  
  for(i in 1:length(regions)){
    tmp <- data.frame(regions[i],mean(wine_in$GivenScore[which(wine_in$simple_region == regions[i])]), stringsAsFactors = F)
    colnames(tmp) <- c("name", "score")
    table_out <- rbind(table_out,tmp)
  }
  
  for(i in 1:length(country)){
    tmp <- data.frame(country[i],mean(wine_in$GivenScore[which(wine_in$Ursprunglandnamn == country[i])], stringsAsFactors = F))
    colnames(tmp) <- c("name", "score")
    table_out <- rbind(table_out,tmp)
  }
  
  for(i in 1:length(grape)){
    tmp <- data.frame(grape[i],mean(wine_in$GivenScore[which(wine_in$class_grape == grape[i])], stringsAsFactors = F))
    colnames(tmp) <- c("name", "score")
    table_out <- rbind(table_out,tmp)
  }
  
  for(i in 1:length(year)){
    tmp <- data.frame(year[i],mean(wine_in$GivenScore[which(wine_in$Argang == year[i])], stringsAsFactors = F))
    colnames(tmp) <- c("name", "score")
    table_out <- rbind(table_out,tmp)
  }
  
  return(table_out)
}
