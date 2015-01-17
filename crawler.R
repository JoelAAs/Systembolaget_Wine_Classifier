# Include the required packets.
library(XML)
library(RCurl)
library(stringr)

# Include the configuration file, which contains pathnames needed.
source('./config.R')

# get_details(ar_id)
# BRIEF: Dowloads the information about the specified wine from Systembolaget.se
# ARGUMENTS:
# ar_id = The article id of the wine to download information for.
# RETURNS: A dataframe holding the information about the specified wine.
crawler_get_details <- function(ar_id) {

  one_url <- paste("http://www.systembolaget.se/Sok-dryck/Dryck/?artikelId=299029&varuNr=",ar_id,"&referringUrl=%2fglobalsok",sep="")
  tagrecode <- htmlTreeParse(one_url,useInternalNodes = T)
  root<-xmlRoot(tagrecode)
  info<-xmlValue(root[[3]][[2]])
  
  # Extract keywords related to taste.
  smak_split <- str_split(info, "Smak \r")
  smak_out <- "inte testat"
  if (!(length(smak_split[[1]]) < 2) ){
    smak_split <- str_split(tolower(smak_split[[1]][2]), "\r")
    smak_split <- gsub(pattern = ",",replacement = "",x = smak_split[[1]][1])
    smak_split <- gsub(pattern = "\\.",replacement = "",x = smak_split)
    smak_split <- str_split(smak_split[[1]][1], " ")[1]
    smak_split <- unlist(smak_split)
    smak_out   <- str_trim(str_join(smak_split[nchar(smak_split) > 3], 
                                    collapse = "."))

    # This is a temporary fix for a problem where the last character in the taste
    #  string is an unknown character.
    # TODO: FIXME FOR REAL
    smak_out <- substr(smak_out, 0, str_length(smak_out) - 1)
  }
  
  # Extract the clock symbol values.
  fyllighet_split <- str_split(info, "Fyllighet\r")
  stravhet_split  <- str_split(info, "Str.vhet\r")
  fruktsyra_split <- str_split(info, "Fruktsyra\r")
  
  if (length(fyllighet_split[[1]]) < 3) {
    fyllighet_out <- "inte testat"
    stravhet_out  <- "inte testat"
    fruktsyra_out <- "inte testat"
  } else {
    fyllighet_split <- str_split(fyllighet_split[[1]][3], "=")
    stravhet_split  <- str_split(stravhet_split[[1]][3], "=")
    fruktsyra_split <- str_split(fruktsyra_split[[1]][3], "=")
    
    fyllighet_out <- gsub(pattern = " ", x = 
                        substr(fyllighet_split[[1]][2], 2 , 3), replacement = "")
    stravhet_out  <- gsub(pattern = " ", x = 
                        substr(stravhet_split[[1]][2], 2 , 3), replacement = "")
    fruktsyra_out <- gsub(pattern = " ", x = 
                        substr(fruktsyra_split[[1]][2], 2 , 3), replacement = "")
  }
  
  # Extract information about the grape used.
  druva_split <- str_split(info, "R.varor \r")
  if (length(druva_split[[1]]) < 2) {
    druva_out <- "Ingen anged"
  } else {
    druva_split <- str_split(druva_split[[1]][2], "\r")
    druva_split <- gsub(pattern = ",",replacement = "",x = druva_split[[1]][1])
    druva_split <- gsub(pattern = "\\.",replacement = "",x = druva_split)
    druva_split <- str_split(druva_split, " ")[1]
    druva_split <- unlist(druva_split)
    druva_out   <- str_trim(str_join(druva_split[nchar(druva_split) > 3], 
                                        collapse = "."))
  }
  
  
  # Extract information about the smell of the wine.
  doft_split <- str_split(info, "Doft \r")
  if (length(doft_split[[1]]) < 2) {
    doft_out <- "inte testat"
  } else {
    doft_split <- str_split(doft_split[[1]][2], "\r")
    doft_split <- gsub(pattern = ",",replacement = "",x = doft_split[[1]][1])
    doft_split <- gsub(pattern = "\\.",replacement = "",x = doft_split)
    doft_split <- str_split(doft_split[[1]][1], " ")[1]
    doft_split <- unlist(doft_split)
    doft_out   <- str_trim(str_join(doft_split[nchar(doft_split) > 3], 
                                    collapse = "."))
  }
  
  details_out<-data.frame(fyllighet = fyllighet_out, stravhet = stravhet_out,
                         fruktsyra = fruktsyra_out, druva = druva_out,
                         smak = smak_out, doft = doft_out,
                         stringsAsFactors=F)
  
  return(details_out)

}

# crawler_create_wine_database(inputfile, outputfile)
# BRIEF: Goues through the input file for wines and downloads wine data from
#         Systembolaget.se. The resulting CSV file is stored as outputfile.
# ARGUMENTS: 
# inputfile  = The Alla+Varor.csv formatted file to read wine data from.
# outputfile = The location to store the produced CSV file in.
crawler_create_wine_database <- function(inputfile, outputfile) {

    # Read the input file into a dataframe.
    sys_xml <- read.csv(inputfile, stringsAsFactors = F)
    
    # Only pick out the red wines.
    sys_xml.Rwine <- sys_xml[which(grepl('Rott vin', sys_xml$Varugrupp)),]

    # Create a new dataframe for the details of a wine.
    details_df<-data.frame(fyllighet = character(0), stravhet = character(0),
                           fruktsyra = character(0), druva = character(0),
                           smak = character(0), doft = character(0),
                           stringsAsFactors=F)

    # Get the details of every wine in the input file.
    for (j in 1:length(sys_xml.Rwine$Varnummer)) {
      message(" -> ", j, " of ", length(sys_xml.Rwine$Varnummer), 
        " bottles of wine on the wall")
      detail_info <- crawler_get_details(sys_xml.Rwine$Varnummer[[j]])
      details_df <- rbind(details_df,detail_info)
    }

    # Join the details of the wine to the other information.
    save_id_tmp <- cbind(sys_xml.Rwine, details_df)

    # Write the output file.
    write.csv(save_id_tmp, output_file)

}

