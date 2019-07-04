# This file contains the configuration data for the Rscripts.
data_path = "/home/joel/Documents/R/Systembolaget_Wine_Classifier/data/"

# The path where the Alla+Artiklar.csv file is found, which is used as base for
#  the wine database.
file_alla_artiklar = paste0(data_path, "Alla+Artiklar.csv")

# The location of the wine database, which consists of Alla+Artiklar.csv plus
#  the mined wine details from systembolaget.se.
file_wine_database = paste0(data_path, "wine_database.csv")

# The location of the wine score file, which maps Systembolaget article ids to
#  scores.
file_wine_scores   = paste0(data_path, "wine_scores.csv")

# Older wine_data_bases arein data folder. If SCrawler.py is run it will append the
#  new data to the current wine_database.csv

file_error_data = paste0(data_path, "wine_error.csv")

current_method_columns = c(
  "PredictedScore", "NegLogPred",
  "RpartPred", "BaggingPred",
  "RandomForestPred")
