##############################################################################################
#' @title
#' addShapefileMetadata

#' @author
#' Rachel Krauss \email{rkrauss@battelleecology.org}

#' @description 
#' Adds metadata to shapefile outputs with standardized metadata that includes usage and protocol information.
#' This code will not run for external uses but provides transparency for what information is updated per survey.  

#' @return
#' Shapefiles with descriptive metadata 

# changelog and author contributions / copyrights
#   Rachel Krauss (2023-02-08)
#     original creation
##############################################################################################

# Load packages
library(XML)
library(xml2)
library(foreign)
library(stringr)

# Read in the all site directory.  This files contains information about each site including UTM zone, file location, and tags
siteDirectory<-read.csv('N:/Science/AQU/Geomorphology_Survey_Data/inputDirectory.csv', head = T, sep = ",", stringsAsFactors = F)

# Bring up list of surveyIDs
siteDirectory$surveyID

# Choose the survey Ids you want to run "XXXX_YYYY"
sitesToRun <- c("REDB_2022")

for (sitelist in 1:length(sitesToRun)){
  surveyID<- sitesToRun[sitelist]
  
  
  #variables from All.site.referencesheet
  surveyType <- siteDirectory$surveyType[which(siteDirectory$surveyID==surveyID)]
  utmzone<-siteDirectory$utmzone[which(siteDirectory$surveyID==surveyID)]
  siteCode<-siteDirectory$site[which(siteDirectory$surveyID==surveyID)]
  siteName<-siteDirectory$siteName[which(siteDirectory$surveyID==surveyID)]
  Domain <-siteDirectory$domain[which(siteDirectory$surveyID==surveyID)]
  State <-siteDirectory$State[which(siteDirectory$surveyID==surveyID)]
  outpath <-siteDirectory$outPathZip[which(siteDirectory$surveyID==surveyID)]
  
  if (surveyType == "geo"){
    # bring in the template
    template <- xmlParse("N:/Science/AQU/Geomorphology_Survey_Data/metadata_templates/geoMorph_metadata_template.shp.xml")
  } 
  
  if (surveyType == "bat"){
    # bring in the template
    template <- xmlParse("N:/Science/AQU/Bathymetry/Swanson/bath_metadata_template.shp.xml")  
  }
  
  # make it easier to access the different level nodes
  xmltop <- xmlRoot(template)  # see https://www.tutorialspoint.com/r/r_xml_files.htm 
  
  # create the date
  date <- format(Sys.Date(),format = "%B %d, %Y")
  
  if (surveyType == "geo"){
    # the part of the template we need to update is the description, located at the 2nd node, 6th subnode, 1 sub sub node
    xmlValue(xmltop[[2]][[6]][[1]]) <- paste("Geomorpholgy shapefiles are created from processed field data to show the size, shape, and location of the reach features. This layer is current as of ", 
                                             date, ". This file is in WGS1984, UTM Zone", utmzone, ".",
                                             sep = "")
    
    # update the tags
    xmlValue(xmltop[[2]][[8]][[9]]) <- siteCode
    xmlValue(xmltop[[2]][[8]][[10]]) <- siteName
    xmlValue(xmltop[[2]][[8]][[11]]) <- Domain
    xmlValue(xmltop[[2]][[8]][[12]]) <- State
  } 
  
  if (surveyType == "bat"){
    # the part of the template we need to update is the description, located at the 2nd node, 6th subnode, 1 sub sub node
    xmlValue(xmltop[[2]][[6]][[1]]) <- paste("Bathymetric maps are obtained using hydroacoustic (sonar) instrumentation and interfaced with differential global positioning system (DGPS) mounted on a boat. Hydroacoustics are utilized to detect the depth of a water body, sediment characteristics as well as the presence or absence, approximate abundance, distribution, size, and behavior of underwater biota. This layer is current as of ", 
                                             date, ". This file is in WGS1984, UTM Zone", utmzone, ".",
                                             sep = "")
    
    # update the tags
    xmlValue(xmltop[[2]][[8]][[6]]) <- siteCode
    xmlValue(xmltop[[2]][[8]][[7]]) <- siteName
    xmlValue(xmltop[[2]][[8]][[8]]) <- Domain
    xmlValue(xmltop[[2]][[8]][[9]]) <- State
  } 
  
 
  
  
  # Write the .xml file to the shapefiles
  
  # Creates a list of files that follow a particular pattern. 
  # try ".shp" if issues, watch out for extra files created
  thelist <- list.files(outpath, pattern = ".shp$")
  
  # Save the new template information to all the shapefiles in the outpath folder
  for (i in 1:length(thelist)){
    
    shapefile <- thelist[i]
    
    saveXML(template, paste(outpath,"/", shapefile, ".xml", sep = ''))
    
    
    print (paste(shapefile, " description tab has been updated", sep = ""))
  }
  
  if (surveyType == "geo"){
  # save the new template information to the habitat shapefile used for the habitat 
  habitatKMZPath <- paste(gsub('.{7}$', '', outpath), "/Processed_Survey_Data/", sep = "")
  
  habitatKMZ <- list.files(habitatKMZPath, pattern = "UnitsKMZ.*shp$")
  saveXML(template, paste(habitatKMZPath, habitatKMZ,".xml", sep = ''))
  
  print (paste(habitatKMZ, " description tab has been updated", sep = ""))
  }
  
  rm(list=ls()[! ls() %in% c("sitesToRun","siteDirectory")])
}
