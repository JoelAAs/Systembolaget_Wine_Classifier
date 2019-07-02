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



## Error stuff for testing. main accurucy testing and prediction variation elsewhere
#errorFrame <- data.frame(GivenScore = binDataDTScored$givenScore)
#errorFrame$errRe     <- (binDataDTScored$givenScore -  rePred)^2
#errorFrame$errBag    <- (binDataDTScored$givenScore - bagPred)^2
#errorFrame$errRandFo <- (binDataDTScored$givenScore - randFoPred)^2

#plotFrame <- melt(errorFrame, id.vars = 1)
#ggplot(plotFrame) +
#  geom_density(aes(x=value, fill=variable), alpha = 0.2)

#print(mseRe)
#print(mseBag)
#print(mseRandFo)
