# search_wine
# BRIEF: Searches all_wine for the wine with article number artnr and prints
#         the predicted or real score, depending on if it is known or not.
# PRE:   Requires the all_wine frame to be built by wine_classify.
search_wine <- function(artnr) {

    # Find the first wine that matches the given article number. If the wine
    #  is sold in different sizes there might be several results, head gives us
    #  just the first.
    wine = head(all_wines[which(all_wines$Varnummer == artnr), 
	c("Varnummer", "Namn", "GivenScore", "predicted_sum")], 1)

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

