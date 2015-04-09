# create_combination_tree <- function(setin)
# BRIEF: Returns the unique combinations (without respect to position) as tree
# ARGUMENTS:
# setin       = the array to be converted
# RETURNS: trees of all unique combinations of array values without respect to position
# at depth of 2 to array length
create_combination_tree <- function(setin){
  output = {}
    k = 1
    for (i in 2:length(setin)){
      output[k] = list(tree_unique_combinations(setin, i))
      k = k + 1
    }
  return(output)
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

insert_into_tree <- function(wineMap, tasteCombine, i, score){
  last   = F
  exists = T
  key    = tasteCombine[i]
  if (is.null(tasteCombine[i+1])){
    last = T
  }
  if (is.null(wineMap[[key]])){
    exists = F
  }

  if (exists){
    if (last){
      scoreArray = wineMap[[key]]
      newArray   = c((scoreArray[1] + 1),
       ((scoreArray[1] * scoreArray[2] + score)/(scoreArray[1]+1)))
      wineMap[[key]] <- newarray
      return(wineMap)

    } else {
      wineMap[[key]] = insert_into_tree(wineMap[[key]],tasteCombine,(i+1),score)
      return(wineMap)
    }
  } else {
    if (last){
      newArray = c(1, score)
      wineMap[[key]] <- newArray
      return(wineMap)

    } else {
      newLayer <- new.env(hash = t,parent = emptyenv())
      wineMap[[key]] <- insert_into_tree(newLayer,tasteCombine,(i+1),score)
      return(wineMap)
    }
  }
}
