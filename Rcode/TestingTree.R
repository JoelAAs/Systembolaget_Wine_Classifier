file_wine = "./wine_database.csv"
file_wine_scores   = "./wine_scores.csv"

all_wine_data <- read.csv(file_wine, stringsAsFactors = F)
all_wine_data = all_wine_data[!duplicated(all_wine_data$Varnummer), ]
all_wine_scores <- read.csv(file_wine_scores, stringsAsFactors = F)
scoredWineData = all_wine_scores
allWineData = all_wine_data


all_wine = merge(x = all_wine_data, y = all_wine_scores, 
                 by = "Varnummer", all = TRUE)


all_wine_data = all_wine_data[!duplicated(all_wine_data$Namn), ]
all_wine_data = all_wine_data[!duplicated(all_wine_data$Varnummer), ]

scored_wine  <- all_wine[!is.na(all_wine$GivenScore),]
winetreeTest <- newLayer <- new.env(hash = t,parent = emptyenv())


for (i in 1:length(scored_wine$GivenScore)){
  word_list <- sort(unlist(unique(strsplit(scored_wine$smak[i], "\\."))))
  word_list <- word_list[word_list!=""]
  score     <- scored_wine$GivenScore[i]
  print(word_list)
  print(score)
  print("---------------------------------------------")
  winetreeTest <- update_tree(wineTree = winetreeTest,setin = word_list,score = score)  
}

# get_tree_score <- function(wineTree, setin)

# update_tree <- function(setin, wineTree, score)
# BRIEF: inserts all unique combinations made from array to hashtree
# ARGUMENTS:
# setin    = the array to be converted (sorted tastes)
# wineTree = the hashTree for the combinatiosn to be added
# score    = the given score of the wine
# RETURNS: The hashTree updated with the new wine tastes.
update_tree <- function(setin, wineTree, score){
  for (i in 2:length(setin)){
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
# score        = the score of the wine0
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