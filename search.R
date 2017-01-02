# search_wine_artnr(artnr)
# BRIEF: Returns the wine that has the article number artnr.
# ARGUMENTS:
# artnr = The article number that describes the wine.
# PRE:   Requires the all_wine frame to be built by wine_classify.
search_wine_artnr <- function(artnr) {
    return(all_wines[all_wines$Varnummer == artnr, ])
}

# search_wine_name(name)
# BRIEF: Returns all wines that contains the string given as argument.
# ARGUMENTS:
# name = The string to search for in wine names.
# RETURNS: All wines whose name contains the given string.
# PRE:   Requires the all_wine frame to be built by wine_classify.
search_wine_name <- function(name) {

  return(all_wines[
	    which(grepl(name, all_wines$Namn, F, F, T) |
	    grepl(name, all_wines$Namn2, F, F, T)),])

}

# search_wine_grape(grape)
# BRIEF: Returns all wines whose RavarorBeskrivning string contains grape.
# ARGUMENTS:
# grape = The grape name to search for.
# RETURNS: All wines that contain grape in RavarorBeskrivning.
# PRE: Requires the all_wine frame to be built by wine_classify.
search_wine_grape <- function(grape) {

    return(all_wines[
        which(grepl(grape, all_wines$RavarorBeskrivning, F, F, T)),])

}

# search_wine_predscore_price(minscore, maxscore, minprice, maxprice)
# BRIEF: Returns all wines that have a score between minscore and maxscore, with
#         a price between minprice and maxscore. Both intervals are inclusive.
# ARGUMENTS:
# minscore = The lowest score to allow.
# maxscore = The highest score to allow.
# minprice = The lowest price to allow.
# maxprice = The highest price to allow.
# RETURNS: All wines within the given score and price range.
# PRE: Requires the all_wines frame to be built by classify.
search_wine_predscore_price <- function(minscore, maxscore, minprice, maxprice) {

  return(all_wines[which(
	    all_wines$PredictedScore >= minscore &
	    all_wines$PredictedScore <= maxscore &
	    (all_wines$PrisPerLiter * 0.75) >= minprice &
	    (all_wines$PrisPerLiter * 0.75) <= maxprice)
	,])

}

# search_topN(N, show_only_new)
# BRIEF: Gives the top N wines based on rating. If show_only_new is not given
#         (or is given as F), this includes wines which already have a score, if
#         show_only_new is given as T, all wines which already have a score will
#         be ignored.
# ARGUMENTS:
# N             = The number of wines to show.
# show_only_new = If T, includes only wines that have not been assigned a real
#                  score, otherwise, include all wines.
# PRE:   Requires the all_wine frame to be built by wine_classify.
search_topN <- function(N, show_only_new = F) {

    if(show_only_new) {
	result <- head(all_wines[!is.finite(all_wines$GivenScore),], N)
    } else {
	result <- head(all_wines, N)
    }

    # Return the result
    return(result)

}

# search_topN(N, show_only_new)
# BRIEF: Gives the top N wines based on rating. If show_only_new is not given
#         (or is given as F), this includes wines which already have a score, if
#         show_only_new is given as T, all wines which already have a score will
#         be ignored.
# ARGUMENTS:
# N             = The number of wines to show.
# show_only_new = If T, includes only wines that have not been assigned a real
#                  score, otherwise, include all wines.
# PRE:   Requires the all_wine frame to be built by wine_classify.
search_topNFreq <- function(N, show_only_new = F) {
  all_wines_tmp <- all_wines[order(all_wines$NegLogPred,decreasing = T),]

  if(show_only_new) {
	  result <- head(all_wines_tmp[!is.finite(all_wines_tmp$GivenScore),], N)
  } else {
	  result <- head(all_wines_tmp, N)
  }
  # Return the result
  return(result)

}

# search_taste_terms(tvec)
# BRIEF: Returns all wines which contains the specified taste strings.
# ARGUMENTS:
# tvec = A vector of search terms.
search_taste_terms <- function(tvec) {

  search_string <- "^"

  for(i in 1:length(tvec)) {

    search_string <- paste(search_string, "(?=.*\\b", tvec[i], "\\b)", sep="")

  }

  search_string <- paste(search_string, ".*$", sep="")

  return(all_wines[which(grepl(search_string, all_wines$smak, F, T)),])

}

# search_region()
# BRIEF: Returns information about the scores of regions.
# PRE: Requires the classify.R source, for access to classification data and the
#       all_wines frame.
search_region <- function() {

  regions <- make_score_RCGY(all_wines)
  regions <- regions[which(regions$type == "Region"),]
  return(regions[order(regions$score, decreasing = TRUE), c("name", "score")])

}

# search_grapes()
# BRIEF: Returns information about the scores of grapes and grape combinations.
# PRE: Requires the classify.R source, for access to classification data and the
#       all_wines frame.
search_grapes <- function() {

  grapes <- make_score_RCGY(all_wines)
  grapes <- grapes[which(grapes$type == "Grape"),]
  return(grapes[order(grapes$score, decreasing = TRUE), c("name", "score")])

}

# search_taste()
# BRIEF: Returns information about the scores of taste terms.
# PRE: Requires the classify.R source, for access to classifictation data and
#       the all_wines frame.
search_taste <- function() {

  taste <- make_score_taste(all_wines)
  return(taste[order(taste$score, decreasing = TRUE), c("word", "score")])

}

# search_wine_unclear_score()
# BRIEF: Returns the wines in order of how much the different score classes
#         differ. This might indicate that there is insufficient data for that
#         wine, and that it will help increase the overall predictions if it is
#         given a score.
search_wine_unclear_score <- function() {

  my_wine <- all_wines

  for(i in 1:length(my_wine$Varnummer)) {

    myvec = c(my_wine$RCGY_predicted[i],
	      my_wine$Taste_predicted[i],
	      my_wine$bar_predicted[i])

    mymin <- min(myvec, na.rm = T)

    mymax <- max(myvec, na.rm = T)

    if(is.finite(mymin) & is.finite(mymax)) {
      my_wine$PredScoreDiff[i] = (mymax - mymin)
    } else {
      my_wine$PredScoreDiff[i] = 0
    }

    if(!is.na(my_wine$GivenScore[i])) {
      my_wine$PredScoreTest[i] = "X"
    } else {
      my_wine$PredScoreTest[i] = ""
    }

  }

  return(my_wine[order(my_wine$PredScoreDiff, decreasing = T),])

}
