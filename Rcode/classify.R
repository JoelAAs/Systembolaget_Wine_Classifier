# get_class_wine(invar)
# BRIEF: Sanitizes invar from known "useless" words. Intended to be used for
#         grape strings.
# ARGUMENTS:
# invar = The string to sanitize.
# RETURNS: The sanitized version of invar.
get_class_wine <- function(invar) {

  # Santitize the invar string from the following strings.
  outvar <- lapply(invar , function(x) gsub("[0-9/%]+", "", str_trim(tolower(x))))
  outvar <- lapply(outvar, function(x) gsub("samt ovriga druvsorter", "", x))
  outvar <- lapply(outvar, function(x) gsub("huvudsakligen", "", x))
  outvar <- lapply(outvar, function(x) gsub(" ", "", x))
  outvar <- lapply(outvar, function(x) gsub("\\.", "", x))

  # Replace the following strings with a separator string (".").
  outvar <- lapply(outvar, function(x) gsub("och", "\\.", x))
  outvar <- lapply(outvar, function(x) gsub("samt", "\\.", x))
  outvar <- lapply(outvar, function(x) gsub(",", "\\.", x))

  return(outvar)

}

# get_score_per_word(word_list, score_list)
# BRIEF:
get_score_per_word <- function(word_list, score_list){
  word_list <- unlist(unique(strsplit(word_list, "\\.")))
  score_tmp <- 0
  k = 0

  for(i in 1:length(word_list)){
    if (word_list[i] %in% score_list$word){
      k = k + 1
      score_tmp <- score_tmp +
	    as.numeric(score_list$score[which(word_list[i] == score_list$word)])
    }
  }
  if(k/length(word_list) < 0.3){
    if (k == 0){
      return(score_tmp)
    } else {
      return((score_tmp-3/k)/k)
    }
  } else {
    return(score_tmp/k)
  }
}

# get_score_sub(in_str, current_score)
# BRIEF: Returns the current score of in_str, based on current_score.
# ARGUMENTS:
# in_str        = The term to lookup the score for.
# current_score = The dictionary to use.
# RETURNS: The score for the term if found, NA otherwise.
get_score_sub <- function(in_str, current_score){

  if(in_str %in% current_score$name){
    score <- current_score$score[which(in_str == current_score$name)]
  }else{
    score <- NA
  }
  return(score)

}

# make_score_Bar(wine_in)
# BRIEF:
make_score_Bar <- function(wine_in){


  table_fyllighet            <- data.frame(name = character(0),score = numeric(0), stringsAsFactors=F)
  table_stravhet             <- data.frame(name = character(0), score = numeric(0),  stringsAsFactors=F)
  table_fruktsyra            <- data.frame(name = character(0),score = numeric(0), stringsAsFactors=F)

  wine_in      <- wine_in[which(is.finite(wine_in$GivenScore)),]
  fyllighet    <- unique(wine_in$fyllighet)
  stravhet     <- unique(wine_in$stravhet)
  fruktsyra    <- unique(wine_in$fruktsyra)

  for(i in 1:length(fyllighet)){
    tmp            <- data.frame(fyllighet[i],
                                 mean(wine_in$GivenScore[which(wine_in$fyllighet == fyllighet[i])]),
                                 stringsAsFactors = F)
    colnames(tmp)  <- c("name","score")
    table_fyllighet <- rbind(table_fyllighet,tmp)
  }

  for(i in 1:length(stravhet)){
    tmp            <- data.frame(stravhet[i],
                                 mean(wine_in$GivenScore[which(wine_in$stravhet == stravhet[i])]),
                                 stringsAsFactors = F)
    colnames(tmp)  <- c("name","score")
    table_stravhet <- rbind(table_stravhet,tmp)
  }

  for(i in 1:length(fruktsyra)){
    tmp            <- data.frame(fruktsyra[i],
                                 mean(wine_in$GivenScore[which(wine_in$fruktsyra == fruktsyra[i])]),
                                 stringsAsFactors = F)
    colnames(tmp)  <- c("name","score")
    table_fruktsyra <- rbind(table_fruktsyra,tmp)
  }

  return(list(table_fyllighet,table_stravhet,table_fruktsyra))

}

# make_score_RCGY(wine_in)
# BRIEF:
make_score_RCGY <- function(wine_in){


  table_out             <- data.frame(name =character(0),score = numeric(0), type = character(0), stringsAsFactors=F)
  wine_in               <- wine_in[which(is.finite(wine_in$GivenScore)),]
  wine_in$simple_region <- unlist(lapply(wine_in$Ursprung, function(x) x<- strsplit(x,",")[[1]][1]))
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
    tmp <- data.frame(regions[i],mean(wine_in$GivenScore[
	    which(wine_in$simple_region == regions[i])]), "Region",
	    stringsAsFactors = F)
    colnames(tmp) <- c("name", "score", "type")
    table_out <- rbind(table_out,tmp)
  }

  for(i in 1:length(country)){
    tmp <- data.frame(country[i],mean(wine_in$GivenScore[
	    which(wine_in$Ursprunglandnamn == country[i])]), "Country",
	    stringsAsFactors = F)
    colnames(tmp) <- c("name", "score", "type")
    table_out <- rbind(table_out,tmp)
  }

  for(i in 1:length(grape)){
    tmp <- data.frame(grape[i],mean(wine_in$GivenScore[
	    which(wine_in$class_grape == grape[i])]),  "Grape",
	    stringsAsFactors = F)
    colnames(tmp) <- c("name", "score", "type")
    table_out <- rbind(table_out,tmp)
  }

  for(i in 1:length(year)){
    tmp <- data.frame(year[i],mean(wine_in$GivenScore[
	    which(wine_in$Argang == year[i])]), "Year",
	    stringsAsFactors = F)
    colnames(tmp) <- c("name", "score", "type")
    table_out <- rbind(table_out,tmp)
  }

  return(table_out)
}

# make_score_taste(wine_in)
# BRIEF:
make_score_taste <- function(wine_in){

  wine_in      <- wine_in[which(is.finite(wine_in$GivenScore)),]

  all_taste    <- unlist(lapply(wine_in$smak, function(x) x<- strsplit(x,"\\.")))
  unique_taste <- unique(all_taste)

  taste_score  <- data.frame(word = character(0), score = numeric(0), stringsAsFactors = F)

  for (i in 1:length(unique_taste)){

    score_tmp           <- data.frame(unique_taste[i], mean(wine_in$GivenScore[which(grepl(unique_taste[i], wine_in$smak, F, F, T))]),stringsAsFactors = F)
    colnames(score_tmp) <- c("word","score")
    taste_score         <- rbind(taste_score,score_tmp)
  }

  return(taste_score)
}

# predict_score_RCGY(wine_in, current_score_RCGT)
# BRIEF:
predict_score_RCGY <-function(wine_in, current_score_RCGY){
  score_RCGY <- c()

  wine_in$simple_region <- unlist(lapply(wine_in$Ursprung, function(x) x<- strsplit(x,",")[[1]][1]))
  wine_in$class_grape   <- get_class_wine(wine_in$RavarorBeskrivning)

  for (i in 1:length(wine_in$Artikelid)){

    k = 0
    R <- get_score_sub(wine_in$simple_region[i], current_score_RCGY)
    if(is.na(R)) {
      k = k + 1
    }
    C <- get_score_sub(wine_in$Ursprunglandnamn[i], current_score_RCGY)
    if(is.na(C)) {
      k = k + 1
    }
    G <- get_score_sub(wine_in$class_grape[i], current_score_RCGY)
    if(is.na(G)) {
      k = k + 1
    }
    Y <- get_score_sub(wine_in$Argang[i], current_score_RCGY)
    if(is.na(Y)) {
      k = k + 1
    }
    p = 50*k/4
    if(k == 4){
      p = 0
    }

    score_RCGY[i] <- mean(c(R,C,G,Y),na.rm=TRUE) - p
  }

  return(score_RCGY)
}

# predict_score_bar(wine_in, current_score_bar)
# BRIEF:
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

    p = 50*k/3
    if(k == 3){
      p = 0
    }

    score_bar[i] <- mean(c(Frukts,Strav,Fyll),na.rm=TRUE)- p

  }

  return(score_bar)

}

# predict_score_taste(wine_in, current_score_taste)
# BRIEF:
predict_score_taste <- function(all_wine,current_score_taste){

  score_out <- unlist(lapply(all_wine$smak, function(x) x<- get_score_per_word(x,current_score_taste)))
  score_out[score_out == 0] = NaN
  return(score_out)
}

# classify_wines(winefile, scorefile)
# BRIEF: Creates and returns a dataframe which contains all relevant information
#         about the wines, in addition to the calculated scores.
# ARGUMENTS:
# winefile  = The CSV file created by the crawler, containing information about
#              the wines.
# scorefile = CSV file that contains article id and score mappings.
# RETURNS: A dataframe containing information about the wines and their scores.
classify_wines <- function(winefile, scorefile) {

    # Read all the wine data and metadata.
    all_wine_data <- read.csv(winefile, stringsAsFactors = F)
    all_wine_data = all_wine_data[!duplicated(all_wine_data$Varnummer), ]

    # Read the user provided scores.
    all_wine_scores <- read.csv(scorefile, stringsAsFactors = F)

    # Merge the wine data and metadata with the user given scores.
    all_wine = merge(x = all_wine_data, y = all_wine_scores,
                by = "Varnummer", all = TRUE)

    # Calculate the current scores for each property.
    current_score_RCGY       <- make_score_RCGY(all_wine)
    current_score_bar        <- make_score_Bar(all_wine)
    current_score_taste      <- make_score_taste(all_wine)

    # Predict the score for all wines based on previous scores.
    all_wine$RCGY_predicted  <- predict_score_RCGY(all_wine,current_score_RCGY)
    all_wine$bar_predicted   <- predict_score_bar(all_wine, current_score_bar)
    all_wine$Taste_predicted <- predict_score_taste(all_wine,current_score_taste)
    all_wine$PredictedScore  <- apply(all_wine[,
					c("RCGY_predicted", "bar_predicted","Taste_predicted")],
					1,
					function(x) mean(x[!is.nan(unlist(x))],
							rm.na=T))

    # Score prediction using negative log frequency. (see "get_unique_combinations.R")
    all_wine <- predict_function_negativelog(all_wine)
    all_wine <- classifyRegressionTrees(all_wine)
    all_wine$MeanPredicted <- apply(all_wine[,36:40], 1, mean)

    # Order the wines by predicted score. and return the dataframe..
    return(all_wine[order(all_wine$PredictedScore,decreasing = T),])

}
