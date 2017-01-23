# This file contains the configuration data for the Rscripts.

# The path where the Alla+Artiklar.csv file is found, which is used as base for 
#  the wine database.
file_alla_artiklar = "./../data/Alla+Artiklar.csv"

# The location of the wine database, which consists of Alla+Artiklar.csv plus
#  the mined wine details from systembolaget.se.
file_wine_database = "./../data/wine_database.csv"

# The location of the wine score file, which maps Systembolaget article ids to 
#  scores.
file_wine_scores   = "./../data/wine_scores.csv"

# Older wine_data_bases arein data folder. If SCrawler.py is run it will append the
#  new data to the current wine_database.csv
