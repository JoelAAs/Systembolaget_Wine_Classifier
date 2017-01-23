#calculate_combination_weights <- function(all_wine_data, all_wine_scores){
#  allWine     = merge(x = all_wine_data, y = all_wine_scores,
#                      by = "Varnummer", all = TRUE)
#  allWine     = allWine[!duplicated(allWine$Namn), ]
#  allWine     = allWine[!duplicated(allWine$Varnummer), ]
#  allWine     = allWine[!is.na(allWine$smak), ]
#  scoredWine  = allWine[!is.na(allWine$GivenScore),]
#
#  for (i in 1:nrow(scoredWine)) {
#    leftOutTest <- scoredWine[i, ] # AKA Leave-one-out cross-validation
#    traningSet  <- scoredWine[-i,]
#    trainedTree <- create_full_tree(traningSet) # TODO: förstå varför den klagar, borde inte vara för mycket (förmycket data blir fel? borde inte) Av någon anledning är coveage högre än vad det borde
#
#    testWords             <- sort(unlist(strsplit(leftOutTest$smak,"\\.")))
#    testWords             <- testWords[testWords != ""]  #NOTE: Fulhack för någonstans lägger den in ".."
#    testScoreDF           <- create_combination_score_DF(testWords, trainedTree)
#    testScoreDF$FullScore <- testScoreDF$DepthScore*testScoreDF$Coverage
#
#    if(!is.null(nrow(testScoreDF)) && nrow(testScoreDF) != 0){
#      meanScoreDepthTmp           <- ddply(testScoreDF,
#                                         c("Depth"),
#                                         function(x) c(sum(x$FullScore)/sum(x$Coverage),sum(x$Coverage))) #Package plyr
#
#      colnames(meanScoreDepthTmp)  <- c("Depth", "AvgDepthScore", "TotalCoverage")
#      meanScoreDepthTmp$GivenScore <- rep(leftOutTest$GivenScore,nrow(meanScoreDepthTmp))
#      meanScoreDepthTmp$ID         <- rep(i,nrow(meanScoreDepthTmp))
#      meanScoreDepthTmp$Depth      <- as.numeric(meanScoreDepthTmp$Depth)
#      if(exists("meanScoreDepth")){
#        meanScoreDepth <- rbind(meanScoreDepth, meanScoreDepthTmp)
#      } else {
#        meanScoreDepth <- meanScoreDepthTmp
#      }
#    }
#  }
#  meanScoreDepth$Depth <- as.numeric(meanScoreDepth$Depth)
#  depthWeights         <- array(NA,length(unique(meanScoreDepth$Depth)) - 1,
#                          dimnames = list(unique(meanScoreDepth$Depth)[-1]))
#
#  for (j in unique(meanScoreDepth$Depth)[-1]){ #This works since depth 1 will allways be first
#    usedData <- meanScoreDepth[meanScoreDepth$Depth == j]
#    weight   <- sum(usedData$GivenScore*usedData$TotalCoverage)/
#                sum(usedData$AvgDepthScore*usedData$TotalCoverage)
#
#    error_function <- function(parameterInital){
#
#    }
#
#      # TODO LISTA UT VAFAN DU DSKA GÖRA MED DET HÄR NURÅ?
#  }
#  #TODO: Nu har du din data, dags att analysera
#}

## ----------------------------------------------------------------------------------------------------------------------
## -------------------------------------------- Predciction score functions ---------------------------------------------
## ----------------------------------------------------------------------------------------------------------------------

# predict_function_negativelog(allWineData)
# BRIEF: Predict a score for all wines dependent of the -log(frequency) of the specific combinations and words.
# ARGUMENTS:
# allWineData    = Data frame of all information from systembolaget
# RETURNS: (double) The predicted score of the wine. NA if no combinatiopns are present
predict_function_negativelog <- function(allWine){
  scoredWine   = allWine[!is.na(allWine$GivenScore),]
  scoredWine      = scoredWine[!is.na(scoredWine$smak), ]
  
  nrDataPoints <- calculate_nr_points_in_tree(scoredWine)
  wineTree     <- create_full_tree(scoredWine)

  allWine$NegLogPred <- sapply(allWine$smak, function(x)
    distance_function_negativelog(sort(unlist(strsplit(x,"\\."))),
      wineTree,
      nrDataPoints))

  return(allWine)

}

# create_combination_score_DF(reviewWords, wineTree, nrDataPoints)
# BRIEF: Predict a score dependent of the -log(frequency) of the specific combinations and words.
# ARGUMENTS:
# reviewWords = Array of key words (NOTE: THE WORDS ARE ASSUMED TO BE ORDERED ALPHABETICALY)
# wineTree    = Hashtree with all words in each combinations of scored.
# RETURNS: (double) The predicted score of the wine. NA if no combinatiopns are present
distance_function_negativelog <- function(reviewWords, wineTree, nrDataPoints) {
  if (is.na(reviewWords[1])) {
    return(NA)
  }
  reviewWords            <- reviewWords[reviewWords != ""]  #NOTE: Fulhack för någonstans lägger den in ".."
  currentWineCombination <- combination_of_length(reviewWords)
  nrRowCurrent           <- nrow(currentWineCombination)
  negLogFreqArray        <- array(NA,nrow(currentWineCombination))
  meanScoreArray         <- array(NA,nrow(currentWineCombination))

  for (i in 1:nrRowCurrent){
    currentCombination <- unlist(strsplit(currentWineCombination[i,1],"\t"))
    scoringArray       <- get_score_combination(wineTree,currentCombination)
    if (!is.na(scoringArray[1])) {
      meanScoreArray[i]  <- mean(scoringArray)
      negLogFreqArray[i] <- -log(length(scoringArray)/nrDataPoints)
    }
  }
  negLogFreqArray <- negLogFreqArray[!is.na(negLogFreqArray)]
  meanScoreArray  <- meanScoreArray[!is.na(meanScoreArray)]
  negLogFreqArray <- negLogFreqArray/sum(negLogFreqArray)

  if(length(meanScoreArray) != 0) {
    return(sum(meanScoreArray * negLogFreqArray))
  } else {
    return(NA)
  }
}

# create_combination_score_DF(reviewWords,wineTree)
# BRIEF: Matches all combinations of key words in rewiew to created wineTree
# ARGUMENTS:
# reviewWords = Array of key words (NOTE: THE WORDS ARE ASSUMED TO BE ORDERED ALPHABETICALY)
# wineTree    = the scored hashTree previously created
# RETURNS: CSV with predicted scores sorted to the number of matched words
create_combination_score_DF <- function(reviewWords,wineTree){
  combinationsDepthScoreCoverage <- as.data.frame(combination_of_length(reviewWords),
                                                  stringsAsFactors = F) # Get word combinations
  colnames(combinationsDepthScoreCoverage) <- c("KeyCombinations","Depth")
  scoreAndCoverage     <- sapply(combinationsDepthScoreCoverage$KeyCombinations,
                                 function(x) get_score_combination(wineTree,unlist(strsplit(x,"\t")))) # Get score and coverage of each combination

  combinationsDepthScoreCoverage$DepthScore <- scoreAndCoverage[2,] # Add score and coverage to data frame
  combinationsDepthScoreCoverage$Coverage   <- scoreAndCoverage[1,]
  combinationsDepthScoreCoverage = combinationsDepthScoreCoverage[
    !is.na(combinationsDepthScoreCoverage$DepthScore),] #Remove combinations without data

  return(combinationsDepthScoreCoverage)
}

# combination_of_length <- function(reviewWords)
# BRIEF: Creates all unique (ignoring order) combinations given word array and combination length
# ARGUMENTS:
# reviewWords = Array of key words (NOTE: THE WORDS ARE ASSUMED TO BE ORDERED ALPHABETICALY)
# RETURNS: Matrix with all combinations(column 1) and depth (column 2)
combination_of_length <- function(reviewWords){
  for(depth in 1:length(reviewWords)){
    combinations <- c()
    for (i in 1:(length(reviewWords)-depth+1)){
      combinations <- c(combinations,
                       unlist(combination_of_length_p(reviewWords[i:length(reviewWords)],depth)))
    }
    combinationDepthMatTmp <- matrix(NA,length(combinations),2)
    combinationDepthMatTmp[,1] <- combinations
    combinationDepthMatTmp[,2] <- rep(depth,length(combinations))
    if (!exists("combinationDepthMat")) {
      combinationDepthMat <- combinationDepthMatTmp
    } else {
      combinationDepthMat <- rbind(combinationDepthMatTmp,combinationDepthMat)
    }
  }
  return(combinationDepthMat)
}

# combination_of_length_p <- function(reviewWords,depth)
# BRIEF: Creates all unique (ignoring order) combinations given word array and combination length
# ARGUMENTS:
# reviewWords = Array of key words (NOTE: THE WORDS ARE ASSUMED TO BE ORDERED ALPHABETICALY)
# wineTree    = The number of words in each combinations.
# RETURNS: Array of strings containing word combinations.
combination_of_length_p <- function(reviewWords,depth) {
  for (i in 1:length(reviewWords)) {
    if (depth == 1) {
      return(reviewWords[i])
    } else {
      nextLevel <- sapply((i+1):(i+1+length(reviewWords)-depth), function(j)
        combination_of_length_p(reviewWords[j:length(reviewWords)], depth-1))
      #print(nextLevel)
      #print("Slut")
      return(sapply(nextLevel, function(x) paste(reviewWords[i],unlist(x),sep="\t")))
    }
  }
}

# get_score_combination <- function(depth,wineTree,combinationWords)
# BRIEF: Gets a predicted score for a combination of key word in score tree
# ARGUMENTS:
# wineTree         = current branch of tree being explored
# combinationWords = The array containing the words that we want to predict score for
# RETURNS: Array of predicted score for combination and depth explored
get_score_combination <- function(wineTree,combinationWords){
  if(length(combinationWords) == 0){
    return(wineTree[["value"]])
  } else if (!combinationWords[1] %in% ls(wineTree)) {
    return(c(NA))
  } else {
    #print(combinationWords)
    return(get_score_combination(
      wineTree[[combinationWords[1]]],
      combinationWords[-1]))
  }
}

# calculate_nr_points_in_tree(scoredWine)
# BRIEF: Calculates the number of datapoints in the tree
# ARGUMENTS:
# scoredWine = Data frame contaning all wines with given score
# RETURNS: number of datapoint in the wineTree
calculate_nr_points_in_tree <- function(scoredWine) {
  for(i in 1:nrow(scoredWine)){
    nrUniqueWords  <- length(unlist(strsplit(scoredWine[i,"smak"],"\\.")))
    combinationTmp <- sum(
      sapply(1:nrUniqueWords,
        function(x)  choose(nrUniqueWords,x)))
    if(exists("nrCombinations")) {
      nrCombinations = nrCombinations + combinationTmp
    } else {
      nrCombinations = combinationTmp
    }
  }
  return(nrCombinations)
}


## ----------------------------------------------------------------------------------------------------------------------
## ------------------------------------------ Scoring tree creation and update ------------------------------------------
## ----------------------------------------------------------------------------------------------------------------------

# create_full_treefunction(scoredWine)
# BRIEF: creates scoring hashTree
# ARGUMENTS:
# scoredWine = Data frame with data from scored wines
# RETURNS: hashTree updated with all combination score
create_full_tree <- function(scoredWine){
  wineTree <- new.env(hash = t,parent = emptyenv())

  for (i in 1:length(scoredWine$GivenScore)){
    wordList <- sort(unlist(unique(strsplit(scoredWine$smak[i], "\\."))))
    wordList <- wordList[wordList != ""]
    score    <- scoredWine$GivenScore[i]
    winetree <- update_tree(wineTree = wineTree,
                                setin = wordList,
                                score = score)
  }
  return(wineTree)
}

# update_tree <- function(setin, wineTree, score)
# BRIEF: inserts all unique combinations made from array to hashtree
# ARGUMENTS:
# setin    = the array to be converted (sorted tastes)
# wineTree = the hashTree for the combinatiosn to be added
# score    = the given score of the wine
# RETURNS: The hashTree updated with the new wine tastes.
update_tree <- function(setin, wineTree, score){
  for (i in 1:length(setin)){
    output= tree_unique_combinations(setin, i)
    for (j in 1:length(output)){
      wineTree = insert_combinations(wineTree,
                                     output[[j]], i, array(0,i), score)
    }
  }
  return(wineTree)
}


# tree_unique_prim <- function(step, idx, setin)
# BRIEF: Returns the unique combinations (without respect to position) as tree
# ARGUMENTS:
# depth       = the depth of the tree
# setin       = the array to be converted
# RETURNS: trees of all unique combinations of array values without respect to position
tree_unique_combinations <- function(setin, depth) {
  output = {}
  for (i in 1:(length(setin)-depth + 1)){
    output[i] = list(tree_unique_prim(depth, i, setin))
  }
  return(output)
}

# tree_unique_prim <- function(step, idx, setin)
# BRIEF: Returns the unique combinations (without respect to position) as tree with starting position at index
# ARGUMENTS:
# step        = the length of the depth left.
# idx         = index of array currently at.
# setin       = the array to be converted
# RETURNS: tree of all unique combinations of array values without respect to position starting with value at index
tree_unique_prim <- function(step, idx, setin){
  if(step == 1){
    return(list(setin[idx]))
  } else {
    out = list(setin[idx])
    k = 2
    for (i in (idx + 1):(length(setin)-step + 2)) {
      out[k] = list(tree_unique_prim(step-1, i,setin))
      k=k+1
    }
    return(out)
  }
}
# insert_combinations <- function(wineTree, combinations, pos, array, score)
# BRIEF: insert the array into the hashTree
# ARGUMENTS:
# wineTree     = the hashTree to be updated
# combinations = The taste combination to be inserted (array)
# pos          = at what part of the array we are worikng on (initially set to the length iof the combinations)
# array        = passes the previous values to the next position
# score        = the score of the wine
# RETURNS: the updated hashTree
insert_combinations <- function(wineTree, combinations, pos, array, score){
  if (pos == 1){
    for (i in 1:length(combinations)){
      array[length(array)] = combinations[i]
      array = unlist(array)
      tmp = tree_insert(wineTree,array,score)
      wineTree = tmp
    }
  } else {
    array[(length(array) - pos +1)] = combinations[1]
    for (i in 2:length(combinations)){
      tmp = insert_combinations(wineTree, combinations[[i]], (pos-1),array, score)
      wineTree = tmp
    }
  }
  return(wineTree)
}

# help function to insert_into_tree, sets set values.
# Adds "value" to taste array as the place to save or update value at level
#
tree_insert <- function(wineMap, tasteCombine, score){
  tasteCombine[(length(tasteCombine)+1)] = "value"
  return(insert_into_tree(wineMap, tasteCombine, 1, score))
}

# insert_into_tree <- function(wineMap, tasteCombine, i, score)
# BRIEF: creating a hash tree with names as keys
# ARGUMENTS:
# wineMap          = hash tree
# i                = keeps track of position of the taste combination array.
# tasteCombination = the taste combination array
# RETURNS: tree of all unique combinations of array values without respect to position starting with value at index
# the leafs contain the number of wines with this combination and the mean score.
insert_into_tree <- function(wineMap, tasteCombine, i, score){
  last   = F
  exists = T
  key    = tasteCombine[i]
  if (is.na(tasteCombine[i+1])){
    last = T
  }
  if (is.null(wineMap[[key]])){
    exists = F
  }

  if (exists){
    if (last){
      scoreArray     = wineMap[[key]]
      newArray       = c(scoreArray, score)
      wineMap[[key]] <- newArray
      return(wineMap)

    } else {
      wineMap[[key]] = insert_into_tree(wineMap[[key]],tasteCombine,(i+1),score)
      return(wineMap)
    }
  } else {
    if (last){
      newArray       = c(score)
      wineMap[[key]] <- newArray
      return(wineMap)

    } else {
      newLayer <- new.env(hash = t,parent = emptyenv())
      wineMap[[key]] <- insert_into_tree(newLayer,tasteCombine,(i+1),score)
      return(wineMap)
    }
  }
}

## ----------------------------------------------------------------------------------------------------------------------
## -------------------------------------------- Old Code --------------------------------------------
## ----------------------------------------------------------------------------------------------------------------------

#create_tree_from_array <- function(setin){
#  output = {}
#  for (i in 2:length(setin)){ #Creates a tree of combinations with depth 2:length(setin)
#    output[i-1] = create_tree_from_array_prim(setin,i,1)
#  }
#  return(output)
#}
#
#create_tree_from_array_prim <- function(setin,depth,idx){
#  if(depth == 1){
#    last_level = vector("list",length(setin)-idx)
#    for (i in idx+1:length(setin)){ #Sets the avaliable elemtes as the last level
#      last_level[i] = setin[i]
#    }
#    return(last_level)
#  } else {
#    count = 1
#    next_level = vector("list",length(setin)-depth+2)
#    next_level[count] = setin[idx]
#    for (i in (idx+1):(length(setin)-depth+2)){ #Gets remaning combinations from this point
#      count = count +1
#      next_level[count] = create_tree_from_array_prim(setin,depth-1,idx+1)
#    }
#    return(next_level)
#  }
#}
#
#
#get_score_prim <- function(wineTree, singleTree, depth, pos, levelscore){
#  for (j in length(singleTree)){
#
#  }
#
#}
#
#get_score_tree <- function(setin,wineTree) {
#  depth_score = {}
#  for (i in 2:length(setin)){
#    current_depth = tree_unique_combinations(setin, i)
#    for (j in 1:length(output)){
#      depth_score[j] = get_score_prim(wineTree, current_depth,i,array(0,i))
#    }
#  }
#  return(depth_score)
#}
