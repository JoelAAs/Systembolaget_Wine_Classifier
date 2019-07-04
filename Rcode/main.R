
# The locale must be set to C for the string functions to work properly on
#  GNU/Linux and FreeBSD systems.
Sys.setlocale(locale="C")
data_path = "/home/joel/Documents/R/Systembolaget_Wine_Classifier/data/"
lib_path = "/home/joel/Documents/R/Systembolaget_Wine_Classifier/Rcode/"
# Load the configuration file, which contains lib_path names.
source(paste0(lib_path, "config.R"))

# Load libraries.
source(paste0(lib_path, "libraries.R"))

# Include the actual program.
#source(paste0(lib_path, "crawler.R")
source(paste0(lib_path, "classify.R"), encoding="UTF-8")
source(paste0(lib_path, "search.R"))
source(paste0(lib_path, "present.R"))
source(paste0(lib_path, "logFreqRegressionTree.R"))
source(paste0(lib_path, "regressionTree.R"))
source(paste0(lib_path, "error_and_validation.R"))


# Create the dataframe the search functions will operate on.
message("Predicting scores...")
con = file(paste0(data_path, "info.txt"))
database_age = as.Date(readLines(con, n=1))
close(con)
if (!exists("all_wines")){

  # Read all the wine data and metadata.
  all_wine_data <- read.csv(file_wine_database, stringsAsFactors = F)
  all_wine_data = all_wine_data[!duplicated(all_wine_data$Varnummer), ]

  # Read the user provided scores.
  all_wine_scores <- read.csv(file_wine_scores, stringsAsFactors = F)

  # Merge the wine data and metadata with the user given scores.
  all_wine = merge(x = all_wine_data, y = all_wine_scores,
                   by = "Varnummer", all = TRUE)

  all_wines <- classify_wines(all_wine)
  date_created = Sys.time()

  Generate
}
validate_wines(all_wine)

message("Done!")

# Print some use info
message(" -> Usage: search_wine(artnr) where artnr is a Systembolaget article id.")
