# search_wine(artnr)
# BRIEF: Searches all_wine for the wine with article number artnr and prints
#         the predicted or real score, depending on if it is known or not.
# ARGUMENTS:
# artnr = The article id of the wine to search for.
# PRE:   Requires the all_wine frame to be built by wine_classify.
search_wine <- function(artnr) {

    # Find the first wine that matches the given article number. If the wine
    #  is sold in different sizes there might be several results, head gives us
    #  just the first.
    wine = head(all_wines[which(all_wines$Varnummer == artnr), 
	c("Varnummer", "Namn", "GivenScore", "PredictedScore")], 1)

    # Make sure we actually got a result.
    if(nrow(wine) > 0) {

	# Extract the information we need to human readable variable names.
	wine_name = wine[[2]]
	wine_real = wine[[3]]
	wine_pred = wine[[4]]

	# A score of 99 is used as a placeholder for untested wines.
	if(!is.finite(wine_real)) {

	    message("New wine ", wine_name, " is predicted as ", wine_pred)

	} else {

	    message("Previously tested ", wine_name, " was given ", wine_real)

	}

    } else {

	message("Could not find such a wine.")

    }

}

# search_wine_artnr(artnr)
# BRIEF: Returns the wine that has the article number artnr.
# ARGUMENTS:
# artnr = The article number that describes the wine.
# PRE:   Requires the all_wine frame to be built by wine_classify.
search_wine_artnr <- function(artnr) {
    return(all_wines[all_wines$Varnummer == artnr, ])
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
	result = head(all_wines[!is.finite(all_wines$GivenScore),], N)
    } else {
	result = head(all_wines, N)
    }

    # Return the result
    return(result)

}

# search_grapes()
# BRIEF: Returns information about the scores of grapes and grape combinations.
# PRE: Requires the classify.R source, for access to classification data and the
#       all_wines frame.
search_grapes <- function() {
  
  grapes = make_score_RCGY(all_wines)
  grapes = grapes[which(grapes$type == "Grape"),]
  return(grapes[order(grapes$score, decreasing = TRUE), c("name", "score")])

}
