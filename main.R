
# The locale must be set to C for the string functions to work properly on
#  GNU/Linux and FreeBSD systems.
Sys.setlocale(locale="C")

# Load the configuration file, which contains path names.
source("./config.R")

# Include the actual program.
source("./crawler.R")
source("./classify.R", encoding="UTF-8")
source("./search.R")
source("./present.R")
source("./get_unique_combinations.R")

# Make sure the wine database has been created.
if(!file.exists(file_wine_database)) {

    message(" -> The wine database does not seem to exist. It will be rebuilt.")
    crawler_create_wine_database(file_alla_artiklar, file_wine_database)

}

# Create the dataframe the search functions will operate on.
message("Predicting scores...")
all_wines <- classify_wines(file_wine_database, file_wine_scores)
message("Done!")

# Print some use info
message(" -> Usage: search_wine(artnr) where artnr is a Systembolaget article id.")
