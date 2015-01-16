get_class_wine <- function(invar) {
  all_word <- lapply(invar, function(x) x<-gsub("[0-9/%]+"        ,tolower(x),replacement = ""))
  all_word <- lapply(all_word, function(x) x<-gsub("samt ovriga druvsorter",x,replacement = ""))
  all_word <- lapply(all_word, function(x) x<-gsub("och"                   ,x,replacement = "\\."))
  all_word <- lapply(all_word, function(x) x<-gsub("samt"                  ,x,replacement = "\\."))
  all_word <- lapply(all_word, function(x) x<-gsub("huvudsakligen"         ,x,replacement = ""))
  all_word <- lapply(all_word, function(x) x<-gsub("\\."                   ,x,replacement = ""))
  all_word <- lapply(all_word, function(x) x<-gsub(" "                     ,x,replacement = ""))

return(all_word)
}