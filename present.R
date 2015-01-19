# present_wine_tbl(wine_table)
# BRIEF: Displays a small information table for all wines in wine_table.
# ARGUMENTS:
# wine_table = The data frame containing the wines to list.
present_wine_tbl <- function(wine_table) {
 
  message("")

  for(i in 1:length(wine_table$Varnummer)) {
   
    message("")
    
    full_name = wine_table$Namn[i]
    if(nchar(wine_table$Namn2[i]) != 0) {
      full_name = paste(full_name, "-", wine_table$Namn2[i])
    }

    message("+----[ ", full_name, " ]-------------")
    message("| Nr: ", wine_table$Varnummer[i], 
	    "   Predicted score: ", round(wine_table$PredictedScore[i]),
	    "   Given score: ", wine_table$GivenScore[i])
    message("| ")
    
    smak_str = wine_table$smak[i]
    smak_str = gsub("\\.", ", ", smak_str)

    message("| Taste keywords: ")
    message("| ", smak_str)
    message("|")
    message("| Grape: ", wine_table$RavarorBeskrivning[i],
	    "   Year: ", wine_table$Argang[i])

    message("|")
    message("| Price: ", round(wine_table$PrisPerLiter[i] * 0.75), " kr",
	    "   Region: ", wine_table$Ursprung[i], 
	    ", ",wine_table$Ursprunglandnamn[i])

  }

}

# present_wine_lst(wine_table)
# BRIEF: Displays a list of the wines in wine_table
# ARGUMENTS:
# wine_table = THe data frame containing the wines to list.
present_wine_lst <- function(wine_table) {

  print(wine_table[,
    c("Varnummer", "Namn", "GivenScore", "PredictedScore", "PrisPerLiter")])

}
