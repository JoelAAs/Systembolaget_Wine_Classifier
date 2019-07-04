# leave_oneout_regression(allWine)
# BRIEF: leave-one-out error estimation recursive partitioned regression trees,
# bootstrap aggregated regression trees and random forest.
# ARGUMENTS:
# allWine    = Data frame of all information from systembolaget
# RETURNS: dataframe with each method as error:
leave_oneout_regression <- function(allWine) {
  allWineTmp <- allWine[allWine$smak != "inte testat", ]
  wineTastes <- sapply(allWineTmp$smak, function(x) unique(unlist(strsplit(x,"\\."))))
  wineTastes <- sapply(wineTastes, function(x) x[x != ""])
  names(wineTastes) <- allWineTmp$Varnummer
  uniTaste <- unique(unlist(wineTastes))

  m <- matrix(0, ncol = length(uniTaste), nrow = nrow(allWineTmp))
  binTaste <- as.data.frame(m)
  colnames(binTaste)  <- uniTaste
  row.names(binTaste) <- allWineTmp$Varnummer

  for(i in 1:length(wineTastes)) {
    binTaste[names(wineTastes[i]), unlist(wineTastes[i])] = 1
  }

  givenScore <- sapply(rownames(binTaste), function(x) allWineTmp[allWineTmp$Varnummer == x, "GivenScore"])
  binDataDT <- cbind(binTaste, givenScore)
  names(binDataDT) <- make.names(names(binDataDT))
  binDataDTPred   <- binDataDT[is.na(binDataDT$givenScore),]
  binDataDTScored <- binDataDT[!is.na(binDataDT$givenScore),]

  error_frame <- data.frame(
    Varnummer = character(0),
    GivenScore = numeric(0),
    RpartPred_test = numeric(0),
    RandomForestPred_test = numeric(0),
    stringsAsFactors = F)

  for (out in sample(1:nrow(binDataDTScored), nrow(binDataDTScored))){
    train_set = binDataDTScored[-out,]
    test_set  = binDataDTScored[out,]
    minimumSplits = 2
    rePartFit <- rpart(givenScore~., data=train_set, control=rpart.control(minsplit = minimumSplits))
    baggFit   <- bagging(givenScore~., data=train_set, control=rpart.control(minsplit = minimumSplits))
    randFoFit <- randomForest(givenScore~., data=train_set)

    ## Adding predicted scores
    scoreFrame <- data.frame(Varnummer = rownames(test_set), GivenScore=test_set$givenScore, stringsAsFactors = F)
    scoreFrame$RpartPred_test        <- predict(rePartFit, test_set[,-ncol(test_set)])
    scoreFrame$RpartPred_test      <- predict(baggFit,   test_set[,-ncol(test_set)])
    scoreFrame$RandomForestPred_test <- predict(randFoFit, test_set[,-ncol(test_set)])

    error_frame <- rbind(error_frame, scoreFrame)
  }
  return(error_frame)
}


# classifyRegressionTrees(allWine)
# BRIEF: Predict a score for all using recursive partitioned regression trees,
# bootstrap aggregated regression trees and random forest.
# ARGUMENTS:
# allWine    = Data frame of all information from systembolaget
# RETURNS: The wine dataframe with added predictions
classifyRegressionTrees<- function(allWine) {
  ## Remove all non-review wines
  allWineTmp <- allWine[allWine$smak != "inte testat", ]
  ## Split to get all specific keywords (NOTE:why are there still duplicates?)
  wineTastes <- sapply(allWineTmp$smak, function(x) unique(unlist(strsplit(x,"\\."))))
  ## NOTE:And these things?
  wineTastes <- sapply(wineTastes, function(x) x[x != ""])
  names(wineTastes) <- allWineTmp$Varnummer
  uniTaste <- unique(unlist(wineTastes))

  ## Create binary array of all kewords
  m <- matrix(0, ncol = length(uniTaste), nrow = nrow(allWineTmp))
  binTaste <- as.data.frame(m)
  colnames(binTaste)  <- uniTaste
  row.names(binTaste) <- allWineTmp$Varnummer

  for(i in 1:length(wineTastes)) {
    # Populate matrix
    binTaste[names(wineTastes[i]), unlist(wineTastes[i])] = 1
  }

  ## NOTE:Phylogenetic view for future?
  givenScore <- sapply(rownames(binTaste), function(x) allWineTmp[allWineTmp$Varnummer == x, "GivenScore"])

  binDataDT <- cbind(binTaste, givenScore)
  names(binDataDT) <- make.names(names(binDataDT))
  binDataDTPred   <- binDataDT[is.na(binDataDT$givenScore),]
  binDataDTScored <- binDataDT[!is.na(binDataDT$givenScore),]

  ## Regression tree stuff. Givenscore as output agains all. Minimum attributes per split set to two
  ## NOTE: Add more splits when there is more data
  minimumSplits = 2
  rePartFit <- rpart(givenScore~., data=binDataDTScored, control=rpart.control(minsplit = minimumSplits))
  baggFit   <- bagging(givenScore~., data=binDataDTScored, control=rpart.control(minsplit = minimumSplits))
  randFoFit <- randomForest(givenScore~., data=binDataDTScored)

  ## Adding predicted scores
  scoreFrame <- data.frame(Varnummer = rownames(binDataDT), stringsAsFactors = F)
  scoreFrame$RpartPred     <- predict(rePartFit, binDataDT[,-ncol(binDataDTScored)])
  scoreFrame$BaggingPred    <- predict(baggFit,   binDataDT[,-ncol(binDataDTScored)])
  scoreFrame$RandomForestPred <- predict(randFoFit, binDataDT[,-ncol(binDataDTScored)])

  ## Adding non-reviewed wines
   noRew <- allWine[allWine$smak == "inte testat", "Varnummer"]
   m     <- matrix(NA, length(noRew), 4)
   colnames(m) <- colnames(scoreFrame)
   m[, 1] <- noRew


   scoreFrame <- rbind(scoreFrame, m)

   allWine <- merge(allWine, scoreFrame, by="Varnummer")

   return(allWine)
 }


# train_models(scoredwinefile)
# BRIEF: Trains models from dataset and returns them
# ARGUMENTS:
# winefile  = The CSV file created by the crawler, containing information about
#              the wines and given scores.
# RETURNS: List contaning the trained models
train_models <- function(scoredwinefile){
  allWineTmp <- allWine[allWine$smak != "inte testat", ]
  ## Split to get all specific keywords (NOTE:why are there still duplicates?)
  wineTastes <- sapply(allWineTmp$smak, function(x) unique(unlist(strsplit(x,"\\."))))
  ## NOTE:And these things?
  wineTastes <- sapply(wineTastes, function(x) x[x != ""])
  names(wineTastes) <- allWineTmp$Varnummer
  uniTaste <- unique(unlist(wineTastes))

  ## Create binary array of all kewords
  m <- matrix(0, ncol = length(uniTaste), nrow = nrow(allWineTmp))
  binTaste <- as.data.frame(m)
  colnames(binTaste)  <- uniTaste
  row.names(binTaste) <- allWineTmp$Varnummer

  for(i in 1:length(wineTastes)) {
    #print(wineTastes[i])
    binTaste[names(wineTastes[i]), unlist(wineTastes[i])] = 1
  }

  ## NOTE:Phylogenetic view for future?
  #distWine <- dist(binTaste,method = "binary")
  #hc       <- hclust(distWine)

  givenScore <- sapply(rownames(binTaste), function(x) allWineTmp[allWineTmp$Varnummer == x, "GivenScore"])

  binDataDT <- cbind(binTaste, givenScore)
  names(binDataDT) <- make.names(names(binDataDT))
  binDataDTPred   <- binDataDT[is.na(binDataDT$givenScore),]
  binDataDTScored <- binDataDT[!is.na(binDataDT$givenScore),]

  ## Regression tree stuff. Givenscore as output agains all. Minimum attributes per split set to two
  ## NOTE: Add more splits when there is more data
  minimumSplits = 2
  rePartFit <- rpart(givenScore~., data=binDataDTScored, control=rpart.control(minsplit = minimumSplits))
  baggFit   <- bagging(givenScore~., data=binDataDTScored, control=rpart.control(minsplit = minimumSplits))
  randFoFit <- randomForest(givenScore~., data=binDataDTScored)

  wine_tree <- create_full_tree(scoredwinefile)

}
