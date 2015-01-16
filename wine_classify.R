# The locale must be set to C for the string functions to work properly.
Sys.setlocale(locale="C")

# Read all the wine data and metadata.
all_wine_data <- read.csv("./data_mine_wine_score.csv", stringsAsFactors = F)
all_wine_data = all_wine_data[!duplicated(all_wine_data$Varnummer), ]

# Read the user provided scores.
all_wine_scores <- read.csv("./wine_scores.csv", stringsAsFactors = F)

# Merge the wine data and metadata with the user given scores.
all_wine = merge(x = all_wine_data, y = all_wine_scores, 
		    by = "Varnummer", all = TRUE)

# Load the functions needed to calculate predicted scores.
source('./get_score_sub.R', encoding='UTF-8')
source('./get_class_wine.R', encoding='UTF-8')
source('./make_score_RCGY.R', encoding='UTF-8')
source('./make_score_Bar.R', encoding='UTF-8')
source('./make_score_taste.R', encoding='UTF-8')
source('./predict_score_bar.R', encoding='UTF-8')
source('./predict_score_RCGY.R', encoding='UTF-8')
source('./predict_score_taste.R', encoding='UTF-8')

# Fungerar
current_score_RCGY       <- make_score_RCGY(all_wine)
# Fungerar
current_score_bar        <- make_score_Bar(all_wine)
# Fungerar
current_score_taste      <- make_score_taste(all_wine)

# Fungerar
all_wine$RCGY_predicted  <- predict_score_RCGY(all_wine,current_score_RCGY)
# Fungerar
all_wine$bar_predicted   <- predict_score_bar(all_wine, current_score_bar)
# Fungerar
all_wine$Taste_predicted <- predict_score_taste(all_wine,current_score_taste)

all_wine$predicted_sum   <- (all_wine$RCGY_predicted + all_wine$bar_predicted + all_wine$Taste_predicted)/3

# Order the wines by predicted sum.
all_wine <- all_wine[order(all_wine$predicted_sum,decreasing = T),]
