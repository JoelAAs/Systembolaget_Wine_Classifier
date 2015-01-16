get_details <- function(ar_id) {
  one_url <- paste("http://www.systembolaget.se/Sok-dryck/Dryck/?artikelId=299029&varuNr=",ar_id,"&referringUrl=%2fglobalsok",sep="")
  tagrecode <- htmlTreeParse(one_url,useInternalNodes = T)
  root<-xmlRoot(tagrecode)
  info<-xmlValue(root[[3]][[2]])
  
  #Smak nyckelord
  smak_split <- str_split(info, "Smak \r")
  smak_out <- "inte testat"
  if (!(length(smak_split[[1]]) < 2) ){
    smak_split <- str_split(smak_split[[1]][2], "\r")
    smak_split <- gsub(pattern = ",",replacement = "",x = smak_split[[1]][1])
    smak_split <- gsub(pattern = "\\.",replacement = "",x = smak_split)
    smak_split <- str_split(smak_split[[1]][1], " ")[1]
    smak_split <- unlist(smak_split)
    smak_out   <- str_join(smak_split[nchar(smak_split) > 3], collapse = ".")
  }
  
  # BarrometervÃ¤rden
  fyllighet_split   <- str_split(info, "Fyllighet\r")
  stravhet_split    <- str_split(info, "Strävhet\r")
  fruktsyra_split <- str_split(info, "Fruktsyra\r")
  
  if (length(fyllighet_split[[1]]) < 3) {
    fyllighet_out <- "inte testat"
    stravhet_out  <- "inte testat"
    fruktsyra_out <- "inte testat"
  } else {
    fyllighet_split   <- str_split(fyllighet_split[[1]][3], "=")
    stravhet_split    <- str_split(stravhet_split[[1]][3], "=")
    fruktsyra_split <- str_split(fruktsyra_split[[1]][3], "=")
    
    fyllighet_out <- gsub(pattern = " ", x = substr(fyllighet_split[[1]][2], 2 , 3), replacement = "")
    stravhet_out  <- gsub(pattern = " ", x = substr(stravhet_split[[1]][2], 2 , 3), replacement = "")
    fruktsyra_out <- gsub(pattern = " ", x = substr(fruktsyra_split[[1]][2], 2 , 3), replacement = "")
  }
  
  #Druva
  druva_split <- str_split(info, "Råvaror \r")
  if (length(druva_split[[1]]) < 2) {
    druva_out <- "Ingen anged"
  } else {
    druva_split <- str_split(druva_split[[1]][2], "\r")
    druva_split <- gsub(pattern = ",",replacement = "",x = druva_split[[1]][1])
    druva_split <- gsub(pattern = "\\.",replacement = "",x = druva_split)
    druva_split <- str_split(druva_split, " ")[1]
    druva_split <- unlist(druva_split)
    druva_out   <- str_join(druva_split[nchar(druva_split) > 3], collapse = ".")    
  }
  
  
  #Doft
  doft_split <- str_split(info, "Doft \r")
  if (length(doft_split[[1]]) < 2) {
    doft_out <- "inte testat"
  } else {
    doft_split <- str_split(doft_split[[1]][2], "\r")
    doft_split <- gsub(pattern = ",",replacement = "",x = doft_split[[1]][1])
    doft_split <- gsub(pattern = "\\.",replacement = "",x = doft_split)
    doft_split <- str_split(doft_split[[1]][1], " ")[1]
    doft_split <- unlist(doft_split)
    doft_out   <- str_join(doft_split[nchar(doft_split) > 3], collapse = ".")
  }
  
  details_out<-data.frame(fyllighet = fyllighet_out, stravhet = stravhet_out,
                         fruktsyra = fruktsyra_out, druva = druva_out,
                         smak = smak_out, doft = doft_out,
                         stringsAsFactors=F)
  
  return(details_out)
  # observera varunummer
}