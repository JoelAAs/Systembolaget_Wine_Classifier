# packet
library(XML)
library(RCurl)
library(stringr)

# library(RSelenium)
# script
source.with.encoding('~/Rscripts/Webcrawler/get details.R', encoding='UTF-8')
#main
index = 0
page  = 1
k = 0
save_id <-data.frame(Year = character(0),Country = character(0),
                     Region  = character(0), Volume = character(0),
                     Price  = character(0), Article_number = character(0),
                     Name = character(0), fyllighet  = character(0),
                     stravhet = character(0), fruktsyra = character(0),
                     druva = character(0), smak = character(0), doft = character(0),
                     stringsAsFactors=F)

for (i in 1:1){
  html_str<-(paste("http://www.systembolaget.se/Sok-dryck/?searchview=All&varugrupp=R%C3%B6tt+vin&excludesvalues=False&groupfiltersheader=Default&filters=varugrupp%2c#sortfield=Default&sortdirection=Ascending&hitsoffset=",
                   index, '&page=', page,
                   "&searchview=All&varugrupp=R%C3%B6tt+vin&exkluderakosher=False&exkluderaretsinerat=False&groupfiltersheader=Default&filters=varugrupp,&action=resultlist",sep=""))
  
  ## test
#   RSelenium::startServer()
#   remDr <- remoteDriver()
#   remDr$open()
#   remDr$navigate(html_str)
#   tblSource <- remDr$executeScript("return tbls[0].outerHTML;")[[1]]
   ##
  index = index + 25
  page  = page  + 1
  tagrecode <- readHTMLTable(html_str)
  page_data <- as.data.frame(tagrecode)
  names (page_data) <- c("Info", "Year", "Country", "Region", "Volume","Price","unidef")
  page_data$Info <- as.character(page_data$Info)
  
  # GET ARTICLE NUMBER
  anr_rough <- strsplit(page_data$Info, "nr")
  page_data$Article_number <- lapply(anr_rough, function(x) x <- str_extract(as.character(strsplit(x[[2]], ")")[1]),"[0-9]+"))
  
  # GET APPORXIMATE NAME
  name_rough = strsplit(page_data$Info, "\n")
  page_data$Name <- lapply(name_rough, function(x) x<-str_split(x[1],"\r")[[1]][1])
  
  details_df<-data.frame(fyllighet = character(0), stravhet = character(0),
                         fruktsyra = character(0), druva = character(0),
                         smak = character(0), doft = character(0),
                         stringsAsFactors=F)

  for (j in 1:length(page_data$Article_number)) {
    k = k + 1
    print(paste(k, " bottles on wine on the wall"))
    detail_info <-get_details(page_data$Article_number[[j]])
    details_df <- rbind(details_df,detail_info)
  }
  
  page_data$Info<-NULL
  page_data$unidef<-NULL
  
  save_id_tmp <- cbind(page_data,details_df)
  save_id     <- rbind(save_id,save_id_tmp)
}

# 
# check$chainID <- lapply(list, function(x) x<-str_join(substr(x,1,1),collapse="."))
# http://www.systembolaget.se/Sok-dryck/?searchview=All&varugrupp=R%C3%B6tt+vin&excludesvalues=False&groupfiltersheader=Default&filters=varugrupp%2c#sortfield=Default&sortdirection=Ascending&hitsoffset=25&page=2&searchview=All&varugrupp=R%C3%B6tt+vin&exkluderakosher=False&exkluderaretsinerat=False&groupfiltersheader=Default&filters=varugrupp,&action=resultlist
# http://www.systembolaget.se/Sok-dryck/?searchview=All&varugrupp=R%C3%B6tt+vin&excludesvalues=False&groupfiltersheader=Default&filters=varugrupp%2c#sortfield=Default&sortdirection=Ascending&hitsoffset=50&page=3&searchview=All&varugrupp=R%C3%B6tt+vin&exkluderakosher=False&exkluderaretsinerat=False&groupfiltersheader=Default&filters=varugrupp,&action=resultlist
# http://www.systembolaget.se/Sok-dryck/?searchview=All&varugrupp=R%C3%B6tt+vin&excludesvalues=False&groupfiltersheader=Default&filters=varugrupp%2c#sortfield=Default&sortdirection=Ascending&hitsoffset=75&page=4&searchview=All&varugrupp=R%C3%B6tt+vin&exkluderakosher=False&exkluderaretsinerat=False&groupfiltersheader=Default&filters=varugrupp,&action=resultlist
# http://www.systembolaget.se/Sok-dryck/?searchview=All&varugrupp=R%C3%B6tt+vin&excludesvalues=False&groupfiltersheader=Default&filters=varugrupp%2c#sortfield=Default&sortdirection=Ascending&hitsoffset=25&page=2&searchview=All&varugrupp=R%C3%B6tt+vin&exkluderakosher=False&exkluderaretsinerat=False&groupfiltersheader=Default&filters=varugrupp,&action=resultlist