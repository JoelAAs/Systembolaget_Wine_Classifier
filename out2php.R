items = 20
ERR   = "ya input is shit ya'll!"
myWine <- read.csv("./wine_database.csv")
options(echo=TRUE) # if you want see commands in output file
args <- commandArgs(trailingOnly = TRUE)

start = items*as.numeric(args[1]) + 1
stop  = items*as.numeric(args[1]) + items

if(start > length(myWine[,1])){
  print(ERR)
} else if(length(args) == 1) {
  if(length(args)%%2 != 1)   {
    print(ERR)
  } else if(start > length(myWine[,1])){
    print(ERR)
  } else if (length(myWine[,1]) > stop) {
  print(myWine[c(start:stop),])
  } else {
  print(myWine[c(start:length(myWine[,1])),])
  }
} else {
  for (i in seq(2, length(args), by = 3)) {
    myWine<-filter_wine(args[i],args[i+1],args[i+2])
  }
  if(start > length(myWine[,1])){
    print(ERR)
  } else if (length(myWine[,1]) > stop) {
    print(myWine[c(start:stop),])
  } else {
    print(myWine[c(start:length(myWine[,1])),])
  }
}

myWine <- myWine[which(myWine[[args[i]]] == args[i+1]),]
# test comment
