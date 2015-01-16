# packet
library(XML)
library(RCurl)
library(stringr)

# library(RSelenium)
# script

source('~/Rscripts/Webcrawler/Systembolaget_webcrawl/get details.R')
#source.with.encoding('~/Rscripts/Webcrawler/Systembolaget_webcrawl/get_details.R', encoding='UTF-8')
#mainindex = 0
page  = 1
k = 0
save_id <-data.frame(Year = character(0),Country = character(0),
                     Region  = character(0), Volume = character(0),
                     Price  = character(0), Article_number = character(0),
                     Name = character(0), fyllighet  = character(0),
                     stravhet = character(0), fruktsyra = character(0),
                     druva = character(0), smak = character(0), doft = character(0),
                     stringsAsFactors=F)
sys_xml<-read.csv('./Alla+Artiklar.csv',stringsAsFactors=F)

sys_xml.Rwine <- sys_xml[which(grepl('Rott vin', sys_xml$Varugrupp)),]

details_df<-data.frame(fyllighet = character(0), stravhet = character(0),
                       fruktsyra = character(0), druva = character(0),
                       smak = character(0), doft = character(0),
                       stringsAsFactors=F)

for (j in 1:length(sys_xml.Rwine$Varnummer)) {
  k = k + 1
  print(paste(k, " bottles on wine on the wall"))
  detail_info <-get_details(sys_xml.Rwine$Varnummer[[j]])
  details_df <- rbind(details_df,detail_info)
}

save_id_tmp <- cbind(sys_xml.Rwine,details_df)

write.csv(save_id_tmp, "./data_mine_wine.csv")
