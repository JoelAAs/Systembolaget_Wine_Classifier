# Include the required packets.
library(XML)
library(RCurl)
library(stringr)

# Include the configuration file, which contains pathnames needed.
source('./config.R')

# sanitize_taste_and_smell(instr)
# BRIEF: Removes unwanted terms from taste and smell strings.
# ARGUMENTS:
# instr = The string to sanitize
sanitize_taste_and_smell <- function(instr) {
  
    smak_str = gsub(", *", ".", instr)
    smak_str = gsub(" *med *", ".", smak_str)
    smak_str = gsub(" *och *", ".", smak_str)
    smak_str = gsub(" *inslag *av *", "", smak_str)
    smak_str = gsub(" *smak *", "", smak_str)
    smak_str = gsub(" *doft *", "", smak_str)
    smak_str = gsub("  +", " ", smak_str)

    smak_str = gsub("\u00F6", "o", smak_str)
    smak_str = gsub("\u00E4", "a", smak_str)
    smak_str = gsub("\u00E5", "a", smak_str)

    return(smak_str)

}

# get_details(ar_id)
# BRIEF: Dowloads the information about the specified wine from Systembolaget.se
# ARGUMENTS:
# ar_id = The article id of the wine to download information for.
# RETURNS: A dataframe holding the information about the specified wine.
crawler_get_details <- function(ar_id) {

  one_url <- paste("http://www.systembolaget.se/Sok-dryck/Dryck/?varuNr=",
                    ar_id, msep = "")
  tagrecode <- htmlTreeParse(one_url,useInternalNodes = T)
  root<-xmlRoot(tagrecode)
  info<-xmlValue(root[[3]][[2]])
  
  # Extract keywords related to taste.
  smak_split <- str_split(info, "Smak \r")
  smak_out <- "inte testat"
  if (!(length(smak_split[[1]]) < 2) ){
    smak_split <- str_split(tolower(smak_split[[1]][2]), "\r")

    # Create the smak_str string.
    smak_str = substr(smak_split[[1]][1], 0, str_length(smak_split[[1]][1]) - 2)
    smak_str = str_trim(smak_str)

    # Sanitation of the smak_str string.
    smak_out = sanitize_taste_and_smell(smak_str)
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
  
  # Extract information about the smell of the wine.
  doft_split <- str_split(info, "Doft \r")
  if (length(doft_split[[1]]) < 2) {
    doft_out <- "inte testat"
  } else {
    doft_split <- str_split(doft_split[[1]][2], "\r")  
    doft_str = substr(doft_split[[1]][1], 0, str_length(doft_split[[1]][1]) - 2)
    doft_str = str_trim(tolower(doft_str))

    # Sanitize the smell.
    doft_out = sanitize_taste_and_smell(doft_str)
  }
  
  details_out<-data.frame(fyllighet = fyllighet_out, stravhet = stravhet_out,
                         fruktsyra = fruktsyra_out, smak = smak_out, 
			 doft = doft_out, stringsAsFactors = F)
  
  return(details_out)

}

# crawler_create_wine_database(inputfile, outputfile)
# BRIEF: Goues through the input file for wines and downloads wine data from
#         Systembolaget.se. The resulting CSV file is stored as outputfile.
# ARGUMENTS: 
# inputfile  = The Alla+Varor.csv formatted file to read wine data from.
# outputfile = The location to store the produced CSV file in.
# maxnum     = The maximum number of wines to download. -1 for unlimited.
crawler_create_wine_database <- function(inputfile, outputfile, maxnum = -1) {

    # Read the input file into a dataframe.
    sys_xml <- read.csv(inputfile, stringsAsFactors = F)
    
    # Only pick out the red wines.
    sys_xml.Rwine <- sys_xml[which(grepl('Rott vin', sys_xml$Varugrupp)),]
    sys_xml.Rwine <- sys_xml.Rwine[which(grepl('FS', sys_xml.Rwine$Sortiment)),]

    # Create a new dataframe for the details of a wine.
    details_df<-data.frame(fyllighet = character(0), stravhet = character(0),
                           fruktsyra = character(0), druva = character(0),
                           smak = character(0), doft = character(0),
                           stringsAsFactors=F)

    # If no limit was placed, make sure we get the info for every wine.
    if(maxnum == -1) {
	maxnum = length(sys_xml.Rwine$Varnummer)
    }

    # Get the details of every wine in the input file.
    for (j in 1:maxnum) {
      message(" -> ", j, " of ", maxnum, " bottles of wine on the wall")
      detail_info <- crawler_get_details(str_trim(sys_xml.Rwine$Varnummer[[j]]))
      details_df <- rbind(details_df,detail_info)
    }

    # Join the details of the wine to the other information.
    save_id_tmp <- cbind(sys_xml.Rwine[1:maxnum,], details_df)

    # Write the output file.
    write.csv(save_id_tmp, outputfile)

}

