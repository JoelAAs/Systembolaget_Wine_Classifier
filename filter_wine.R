# 1. ARGS
# 2. Skapa lista över operationer
#   - STRING COLUMN VALUE BOOLEAN_OP
#   - INTERVAL COLUMN VALUE1 VALUE2
#   - NUMBER COULMN VALUE
#
# 4. loopa till slutet
#    While(COLUMN == PREV_COULMN && STRING)
#       Bygg regexp (c[i] = VALUE)
#  wine_table grepl regexp
#
filter_wine<-function(myWine_new,myWine){



}

arg2list<-function(args){
  i = 2
  args_list = {}
  ERR = "unkown input type:"
  string_table   <- data.frame(cname = character(0),
                    value = character(0), stringsAsFactors=F)
  interval_table <- data.frame(cname = character(0),
                    low = numeric(0), high = numeric(0), stringsAsFactors=F)
  number_table   <- data.frame(cname = character(0),
                    value = numeric(0), stringsAsFactors=F)

  while (!is.null(args[i]){
    if (args[i] == "STRING"){
      tmp <-c(args[i+1],args[i+2],args[i+3])
      string_table <- rbind(string_table,tmp)
      i = i + 3
    } else if (args[i] == "INTERVAL"){
      tmp <-c(args[i+1],args[i+2],args[i+3])
      interval_table <- rbind(interval_table,tmp)
      i = i + 3
    } else if (args[i] == "NUMBER"){
      tmp <-c(args[i+1],args[i+2],args[i+3])
      number_table <- rbind(number_table,tmp)
      i = i + 2
    } else {
      return(strcat(ERR,args[i]))
    }
  }


  args_list{1} = string_table
  args_list{2} = interval_table
  args_list{3} = number_table

}


string_filter<-function(myWine, regexp, cname){
  # här ska du splitta regexp på ;
  outWine <- myWine[which(myWine[grepl(regexp, myWine[cname])]),]
  return(outwine);
}

interval_filter<-function(myWine,cname,low,high){
  outWine <- myWine[which(myWine[cname]<high && myWine[cname]>low),]
  return(outwine);
}

number_filter<-function(myWine,cname,value){
  outWine <- myWine[which(myWine[cname] == value),]
  return(outwine);
}
