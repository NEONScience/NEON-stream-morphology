##############################################################################################
#' @title
#' formatPOCOutputs

#' @author
#' Rachel Krauss \email{rkrauss@battelleecology.org}

#' @description 
#' This script formats raw field data from the total stationPOC to be compatible with ArcGIS. 
#' Can be used for AIS Surveys as well, just remove mapCode and onlyDigits in ArcGIS.
#' 
#' @return
#' One .CSV file that is formatted for post-processing of survey data. 

# changelog and author contributions / copyrights
#   Rachel Krauss (2023-02-08)
#     original creation
##############################################################################################

# Load packages
library(tidyverse)
library(stringr)
options(digits=10)

# [USER INPUT REQUIRED] - Define raw data file location
input <- 'N:/Special/AIS Survey Work/D04/CUPE/D04_CUPE_20240821/Raw_Survey_Data/D04_CUPE_Raw_Survey_Data_20240821.csv'


#list all files in folder if correct file name is needed 
#list.files('N:/Science/AQU/Geomorphology_Survey_Data/D14/SYCA_2023/Workspace/')

# Read in the raw data file
rawData <- read.csv(input,
                    stringsAsFactors = F, sep=';')


# Format the data: remove unnecessary columns, add on new columns, add place holder data to assign the correct level of decimal places in Arc, re-order to match final output
formatted <- rawData %>%
  dplyr::select(NAME,E,N,H) %>%
  mutate(name = NAME,
         latitude = 1.234567,
         longitude = 1.234567,
         easting= 1.234567,
         northing = 1.234567,
         elevationM = 1.234,  #AIS surveys wants 3 decimal
         mapCode = "notReal") %>%
  dplyr::relocate(name,latitude,longitude,easting,northing,elevationM,mapCode)%>%
  dplyr::select(-c(NAME)) %>%
  arrange(E) #sort to make sure that the 0, 0, 1000 record is not first (no other decimal pts are then kept)


# Plot the survey for a quick check
plot(formatted$E, formatted$N)

# Add new column that only contains digits (only for GEO)
formatted$onlyDigits <- as.numeric(gsub("\\D", "", formatted$name))

# Fill in the mapcode field
# Only necessary for geomorphology surveys, skip to next line of code for AIS 
formatted <- formatted %>%
  mutate(mapCode = case_when(
    grepl("S1_BASE|S2_BASE",name) ~ "Sensor",
    grepl("USR_",name) ~ "Transect_USR",
    grepl("DSR_",name) ~ "Transect_DSR",
    grepl("DSC_",name) ~ "Transect_DSC",
    grepl("S1_",name) ~ "Transect_S1",
    grepl("S2_",name) ~ "Transect_S2",
    grepl("AP\\d",name) ~ paste0("Transect_AP",str_extract(name, "\\d+")),
    grepl("APL\\d",name) ~ paste0("Transect_AP",str_extract(name, "\\d+")),
    grepl("BM",name) ~ "Benchmark",
    grepl("BM\\d[_]C",name) ~ "Benchmark_Closure", ##double check that this works
    grepl("LEW",name) ~ "LEW",
    grepl("REW",name) ~ "REW",
    grepl("THL",name) ~ "THL",
    grepl("MCB",name) ~ "MCB",
    grepl("ISL",name) ~ "ISL",
    grepl("LWD",name) ~ "LWD Jam",
    grepl("WF",name) ~ "Waterfall",
    grepl("BEA",name) ~ "BEA",
    grepl("TRB",name) ~ "TRB",
    grepl("CUL",name) ~ "Culvert",
    grepl("MPD",name) ~ "Max Pool Depth",
    grepl("POT",name) ~ "Pool",
    grepl("RUT",name) ~ "Run",
    grepl("RIT",name) ~ "Riffle",
    grepl("SPT",name) ~ "Step-Pool",
    grepl("DDS|DUS",name) ~ "Dry Extent",
    grepl("SUB",name) ~ "SUB",
    grepl("SP_",name) ~ "Gauge",
    grepl("TP",name) ~ "Trimble",
    grepl("TS",name) ~ "Total Station",
    grepl("Marsh",name) ~ "Marsh"))
 

# Write the output to the workspace folder
outPath <- paste0(sub("Raw_Survey_Data.*$", "", input),"Workspace/", substr(basename(input),1,nchar(basename(input))-4),"_formatted.csv")

# Write out formatted file
write.csv(formatted, outPath, row.names = F)
