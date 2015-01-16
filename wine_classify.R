all_wine <- read.csv("./data_mine_wine_score.csv", stringsAsFactors = F)

source.with.encoding('./get_score_sub.R', encoding='UTF-8')
source.with.encoding('./get_class_wine.R', encoding='UTF-8')

source.with.encoding('./make_score_RCGY.R', encoding='UTF-8')
source.with.encoding('./make_score_Bar.R', encoding='UTF-8')
source.with.encoding('./make_score_taste.R', encoding='UTF-8')
source.with.encoding('./predict_score_bar.R', encoding='UTF-8')
source.with.encoding('./predict_score_RCGY.R', encoding='UTF-8')
source.with.encoding('./predict_score_taste.R', encoding='UTF-8')

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


all_wine <- all_wine[order(all_wine$predicted_sum,decreasing = T),]