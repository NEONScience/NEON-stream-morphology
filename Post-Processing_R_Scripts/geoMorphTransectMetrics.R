##############################################################################################
 
#geomorphTransectMetrics

#author: Nick Harrison, Principal Research Scientist, National Ecological Observatory Network 
#email{NHarrison@BattelleEcology.org}

#description 
#This script provides the following metrics based on data collected during the NEON geomorphology protocol:
  
#1. Bankfull widths for each stream transect where both right and left bankfull indicators were mapped.
#2. Average bankfull width of all transects mapped.
#3. Plots of each transects mapped.  Plot images are not read out as outputs.  

#The only input is a single file, DXX_SITE_surveyPts_YYYMMDD.csv, produced during ArcGIS processing.  This file contains raw (non post-processed) Level 0 northing, easting, and elevation data collected during the total station survey,  post-processed (real-world) lat/long/elevation values [those data are not used to calculate bankfull metrics due to potential for vertical and horizontal skewing during post-processing], and mapCodes that associate the transects with the mapped points [generated during post-processing].  


#Outputs are a single .CSV file that provides bankfull widths for each transect (where left and right bankfull indicators were mapped) and the average bankfull width of all transects.  If bankfull measurements (mapCodes ="RBF" or "LBF") are not available for a given transect, an "NA" value is returned.  

#changelog
#  N. Harrison (2017-10-18): Original creation, .R format.
#  N. Harrison (2017-12-18): Conversion to .RMD format.
#  N. Harrison (2018-02-17): Updated distance calculations to transform DistanceAdj/Easting distances to horizontal distances relative to set   
#                            reference point coordinates.
#  N. Harrison (2018-02-09): Added staff gauge calculations.
#  N. Harrison (2018-08-27): Updated script to import a new survey .csv file that contains raw easting, northing, and elevation coordinates.  Transects metrics are now calculated using raw values to avoid any potential transformation bias incorporated during post-processing.  Added calculations to offset discharge cross-section gauge height values in instances of negative stage transformations.   
#  N.Harrison (2020-02-28):  Removed gsub functionality to automatically select NorthStart, EastStart, and ReferenceDistance coordinates (it never really worked) - user must now select these points manually for each transect; added feature to transect plots where field bankfull calls are highlighted to validate bankfull width calculations.
#  N.Harrison (2020-03-31):  Created template script; added API functionality to download and read in data from the NEON Data Portal via neonUtilities package; removed staff gauge correction lines for discharge transects (this is now done in the hydrologic controls script); updated @param text
#  N.Harrison (2021-06-14):  Converted script from .RMD to .R format
# N. Harrison (2025-06-05):  Updated outputs and workflows to match new geo_transectBankfullWidths_in ingest  table

#########################################################################################################################

#Install packages (comment out after each is initially installed).
# install.packages("neonUtilities")
# install.packages("data.table")
# install.packages("calibrate")
# install.packages("rgdal")
# install.packages("ploty")

#Load packages
library(neonUtilities)
library(data.table)
library(calibrate)
library(rgdal)
library(plotly)

#Set variables.

#NEON Domain number (ex: D01).
domainID<-'D02' 

#Four-digit NEON site code (ex: HOPB).
siteID <- 'LEWI'  

#The end date of the geomorphology survey (YYYYMMDD).  
surveyDate<-'20241126'
eventID<-paste(siteID,substr(surveyDate, start=1, stop=6), sep=".")

#Stipulate 4-digit site code, underscore, and survey year (ex: HOPB_2017). 
surveyID<-paste(siteID,"_",as.numeric(substr(surveyDate, 1, 4)),sep="")

#Sets file path for survey on N drive.
surveyFilePath<-paste('N:/Science/AQU/Geomorphology_Survey_Data/',domainID,"/",domainID,"_",siteID,"_",surveyDate,"/",sep="")

#Set Working directory where files will be output.  
# wdir<-paste('N:/Science/AQU/Geomorphology_Survey_Data/',domainID,"/",domainID,"_",siteID,"_",surveyDate,"/Processed_Survey_Data",sep="")
# setwd(wdir)
# getwd()

############# Load data from N drive ##################################################################################################

#Set working directory where files will be output.  
#wdir<-paste('C:/Users/nharrison/Documents/GitHub/landWaterSoilIPT/streamMorpho/ScienceProcessingCode/R_Metrics',siteID,'Raw_Data',sep="/") 

#Queues a directory that contains file paths for each site per survey date.  
siteDirectory<-read.csv('N:/Science/AQU/Geomorphology_Survey_Data/inputDirectory.csv',head=T,sep=",",stringsAsFactors = F)

#Creates dataframe that contains survey data.
filePath <- siteDirectory$filePath[which(siteDirectory$surveyID==surveyID)]
surveyShapefileName <- siteDirectory$surveyShapefileName[which(siteDirectory$surveyID==surveyID)]
surveyPts <- readOGR(filePath,surveyShapefileName)
surveyPtsDF <- as.data.frame(surveyPts)

#surveyPtsDF <- read.csv(paste0(getwd(),"/",surveyShapefileName,".csv")) #backup plan if the .dbf path isn't working and the .csv file has already been created and placed in the processed_survey_data folder...

############# Load data from NEON API ##################################################################################################

#Run this chunk if you are not reading in data via the API and NOT from the internal NEON N drive.  
#If not connected to the NEON network, please use a VPN for this call.  

# #Creates dataframe of morphology data (morphology = DP4.00131.001).
# morphologyData <- loadByProduct(dpID="DP4.00131.001",
#                                 site=siteID,
#                                 startdate="2017-10-01", #set startDate to just before survey was conducted.
#                                 enddate="2017-11-05") #set endDate to just after survey was conducted.  
# 
# #Parses results file and delineates L0 and L4 web adressess for ECS download.
# geoResultsFile<-morphologyData$geo_resultsFile
# webAddressL0<-geoResultsFile$rawDataFilePath
# webAddressL4<-geoResultsFile$dataFilePath
# 
# #Downloads the L0 data from ECS with Google Chrome browser.
# browseURL(webAddressL0,browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
# 
# #Downloads the L4 data from ECS with Google Chrome browser.
# browseURL(webAddressL4,browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
# 
# #The raw data file is contained in the L0 zip, labeled "DXX_SITE_surveyPts_YYYMMDD.csv".  Unzip the folder and save this file somewhere on your local computer then read it in by editing the line below with the directory location.  
# surveyPtsDF<-read.csv('C:/Users/nharrison/Desktop/MAYF_surveyPts_20171103.csv',head=T,sep=",",stringsAsFactors = F)
# 
# #Parses the pebble count data.
# geoPebbleCount<-morphologyData$geo_pebbleCount #pebble count data was contained in the morphology app for older surveys ~pre-2018, for more recent surveys pebble count data is contained in the pebble count app.   
# 
# #Parses the survey field data.
# geoSurveyFieldData<-morphologyData$geo_surveyFieldData
# 
# #Parses the total station data.
# geoTotalStationData<-morphologyData$geo_totalStation
# 
# #Parses the readMe file.
# geoReadMe<-morphologyData$readme_00131
# 
# #Parses the list of variables.  
# geoVariables<-morphologyData$variables_00131

############# Creates dataframes for each mapped transect ##################################################################################################

#TRUE = the transect was mapped during the survey; FALSE = the transect was not mapped during the survey.

print(sort(unique(surveyPtsDF$mapCode)))

if("Transect_DSC1" %in% surveyPtsDF$mapCode){
  SurveyedDSC1<-TRUE
  dischargePointsXS1=subset(surveyPtsDF,mapCode=="Transect_DSC1")
  dischargePointsXS1<-dischargePointsXS1[
    with(dischargePointsXS1,order(dischargePointsXS1$N)), #if transect runs east-west, charge order to dischargePointsXS1$E
    ]
  rownames(dischargePointsXS1)<-seq(length=nrow(dischargePointsXS1))
}else {
  SurveyedDSC1<-FALSE
}

if("Transect_DSC" %in% surveyPtsDF$mapCode){
  SurveyedDSC1<-TRUE
  dischargePointsXS1=subset(surveyPtsDF,mapCode=="Transect_DSC")
  dischargePointsXS1<-dischargePointsXS1[
    with(dischargePointsXS1,order(dischargePointsXS1$N)), #if transect runs east-west, charge order to dischargePointsXS1$E
    ]
  rownames(dischargePointsXS1)<-seq(length=nrow(dischargePointsXS1))
}else {
  SurveyedDSC1<-FALSE
}

if("Transect_DSC2" %in% surveyPtsDF$mapCode){
  SurveyedDSC2<-TRUE
  dischargePointsXS2=subset(surveyPtsDF,mapCode=="Transect_DSC_OLD")
  dischargePointsXS2<-dischargePointsXS2[
    with(dischargePointsXS2,order(dischargePointsXS2$N)), #if transect runs east-west, charge order to dischargePointsXS2$E
    ]
  rownames(dischargePointsXS2)<-seq(length=nrow(dischargePointsXS2))
}else {
  SurveyedDSC2<-FALSE
}

if("Gauge" %in% surveyPtsDF$mapCode){
  SurveyedGauge<-TRUE
}else {
  SurveyedGauge<-FALSE
}

if("Gauge_Transect_DSC2" %in% surveyPtsDF$mapCode){
  SurveyedDSC2Gauge<-TRUE
}else {
  SurveyedDSC2Gauge<-FALSE
}

if("Transect_AP1" %in% surveyPtsDF$mapCode){
  SurveyedAP1<-TRUE
  AP1Points=subset(surveyPtsDF,mapCode=="Transect_AP1") 
  AP1Points<-AP1Points[
    with(AP1Points,order(AP1Points$N)), #if transect runs east-west, charge order to AP1Points$E
    ]
  rownames(AP1Points)<-seq(length=nrow(AP1Points))
}else {
  SurveyedAP1<-FALSE
}

if("Transect_AP2" %in% surveyPtsDF$mapCode){
  SurveyedAP2<-TRUE
  AP2Points=subset(surveyPtsDF,mapCode=="Transect_AP2") 
  AP2Points<-AP2Points[
    with(AP2Points,order(AP2Points$N)), #if transect runs east-west, charge order to AP2Points$E
    ]
  rownames(AP2Points)<-seq(length=nrow(AP2Points))
}else {
  SurveyedAP2<-FALSE
}

if("Transect_AP3" %in% surveyPtsDF$mapCode){
  SurveyedAP3<-TRUE
  AP3Points=subset(surveyPtsDF,mapCode=="Transect_AP3") 
  AP3Points<-AP3Points[
    with(AP3Points,order(AP3Points$N)), #if transect runs east-west, charge order to AP3Points$E
    ]
  rownames(AP3Points)<-seq(length=nrow(AP3Points))
}else {
  SurveyedAP3<-FALSE
}

if("Transect_AP4" %in% surveyPtsDF$mapCode){
  SurveyedAP4<-TRUE
  AP4Points=subset(surveyPtsDF,mapCode=="Transect_AP4") 
  AP4Points<-AP4Points[
    with(AP4Points,order(AP4Points$N)), #if transect runs east-west, charge order to AP4Points$E
    ]
  rownames(AP4Points)<-seq(length=nrow(AP4Points))
}else {
  SurveyedAP4<-FALSE
}

if("Transect_AP5" %in% surveyPtsDF$mapCode){
  SurveyedAP5<-TRUE
  AP5Points=subset(surveyPtsDF,mapCode=="Transect_AP5") 
  AP5Points<-AP5Points[
    with(AP5Points,order(AP5Points$N)), #if transect runs east-west, charge order to AP5Points$E
    ]
  rownames(AP5Points)<-seq(length=nrow(AP5Points))
}else {
  SurveyedAP5<-FALSE
}

if("Transect_AP6" %in% surveyPtsDF$mapCode){
  SurveyedAP6<-TRUE
  AP6Points=subset(surveyPtsDF,mapCode=="Transect_AP6") 
  AP6Points<-AP6Points[
    with(AP6Points,order(AP6Points$N)), #if transect runs east-west, charge order to AP6Points$E
    ]
  rownames(AP6Points)<-seq(length=nrow(AP6Points))
}else {
  SurveyedAP6<-FALSE
}

if("Transect_AP7" %in% surveyPtsDF$mapCode){
  SurveyedAP7<-TRUE
  AP7Points=subset(surveyPtsDF,mapCode=="Transect_AP7") 
  AP7Points<-AP7Points[
    with(AP7Points,order(AP7Points$N)), #if transect runs east-west, charge order to AP7Points$E
    ]
  rownames(AP7Points)<-seq(length=nrow(AP7Points))
}else {
  SurveyedAP7<-FALSE
}

if("Transect_AP8" %in% surveyPtsDF$mapCode){
  SurveyedAP8<-TRUE
  AP8Points=subset(surveyPtsDF,mapCode=="Transect_AP8") 
  AP8Points<-AP8Points[
    with(AP8Points,order(AP8Points$E)), #if transect runs east-west, charge order to AP8Points$E
    ]
  rownames(AP8Points)<-seq(length=nrow(AP8Points))
}else {
  SurveyedAP8<-FALSE
}

if("Transect_AP9" %in% surveyPtsDF$mapCode){
  SurveyedAP9<-TRUE
  AP9Points=subset(surveyPtsDF,mapCode=="Transect_AP9") 
  AP9Points<-AP9Points[
    with(AP9Points,order(AP9Points$N)), #if transect runs east-west, charge order to AP9Points$E
    ]
  rownames(AP9Points)<-seq(length=nrow(AP9Points))
}else {
  SurveyedAP9<-FALSE
}

if("Transect_AP10" %in% surveyPtsDF$mapCode){
  SurveyedAP10<-TRUE
  AP10Points=subset(surveyPtsDF,mapCode=="Transect_AP10") 
  AP10Points<-AP10Points[
    with(AP10Points,order(AP10Points$N)), #if transect runs east-west, charge order to AP10Points$E
    ]
  rownames(AP10Points)<-seq(length=nrow(AP10Points))
}else {
  SurveyedAP10<-FALSE
}

if("Transect_DSR" %in% surveyPtsDF$mapCode){
  SurveyedDSR<-TRUE
  DSRPoints=subset(surveyPtsDF,mapCode=="Transect_DSR") 
  DSRPoints<-DSRPoints[
    with(DSRPoints,order(DSRPoints$N)), #if transect runs east-west, charge order to DSRPoints$E
    ]
  rownames(DSRPoints)<-seq(length=nrow(DSRPoints))
}else {
  SurveyedDSR<-FALSE
}

if("Transect_USR" %in% surveyPtsDF$mapCode){
  SurveyedUSR<-TRUE
  USRPoints=subset(surveyPtsDF,mapCode=="Transect_USR") 
  USRPoints<-USRPoints[
    with(USRPoints,order(USRPoints$N)), #if transect runs east-west, charge order to USRPoints$E
    ]
  rownames(USRPoints)<-seq(length=nrow(USRPoints))
}else {
  SurveyedUSR<-FALSE
}

if("Transect_S1" %in% surveyPtsDF$mapCode){
  SurveyedS1<-TRUE
  S1Points=subset(surveyPtsDF,mapCode=="Transect_S1") 
  S1Points<-S1Points[
    with(S1Points,order(S1Points$N)), #if transect runs east-west, charge order to S1Points$E
    ]
  rownames(S1Points)<-seq(length=nrow(S1Points))
}else {
  SurveyedS1<-FALSE
}

if("Transect_S2" %in% surveyPtsDF$mapCode){
  SurveyedS2<-TRUE
  S2Points=subset(surveyPtsDF,mapCode=="Transect_S2") 
  S2Points<-S2Points[
    with(S2Points,order(S2Points$N)), #if transect runs east-west, charge order to S2Points$E
    ]
  rownames(S2Points)<-seq(length=nrow(S2Points))
}else {
  SurveyedS2<-FALSE
}

print(sort(unique(surveyPtsDF$mapCode)))

############# Set plot settings  ##################################################################################################

xAxisTitle1<-list(title="Distance (m)",zeroline=FALSE)
yAxisTitle1<-list(title="Elevation (m)",zeroline=FALSE)
font<-list(size=12,color='black')

############# Discharge XS1 transect  ##################################################################################################

if(SurveyedDSC1){
  
  DSCnorthEastStart<-42 #Manually select the row of the mapped point that is furthest along the left bank side of the channel
  DSCreference<-40 ##Manually select the row of the mapped point taken on a pin [preferably LB pin but RB works too]; if a pin shot is not available choose the mapped point furthest into the left bank side of the channel
  
  dischargeXS1NorthStart<-dischargePointsXS1$N[DSCnorthEastStart]
  dischargeXS1EastStart<-dischargePointsXS1$E[DSCnorthEastStart]
  
  #Assigns a raw Distance value to each point relative to the NorthStart and EastStart coordinates.
  for(i in 1:(length(dischargePointsXS1$name))){
    dischargeXS1PointN<-dischargePointsXS1$N[i]
    dischargeXS1PointE<-dischargePointsXS1$E[i]
    dischargePointsXS1$DistanceRaw[i]<-sqrt(((dischargeXS1PointN-dischargeXS1NorthStart)^2)+((dischargeXS1PointE-dischargeXS1EastStart)^2))
  }
  
  dischargeXS1ReferenceDistance<-dischargePointsXS1$DistanceRaw[DSCreference]
  
  #Sets Horizontal adjustment value based on reference point coordinate.  
  dischargeXS1HorizontalAdjust<-0-dischargeXS1ReferenceDistance
  
  #Transforms raw distance to adjusted distance based on reference distance point.
  for(i in 1:(length(dischargePointsXS1$name))){
    dischargePointsXS1$DistanceAdj[i]<-dischargePointsXS1$DistanceRaw[i]+dischargeXS1HorizontalAdjust
  }
  
  #Creates dataframe of bankfull shots mapped during the survey.  
  dsc1BankfullCalls<-data.frame(matrix(nrow=2,ncol=3))
  names(dsc1BankfullCalls)=c("name","DistanceAdj","H")
  dsc1BankfullCalls[1,1]<-dischargePointsXS1$name[grepl("LBF",dischargePointsXS1$name)]
  dsc1BankfullCalls[1,2]<-dischargePointsXS1$DistanceAdj[grepl("LBF",dischargePointsXS1$name)]
  dsc1BankfullCalls[1,3]<-dischargePointsXS1$H[grepl("LBF",dischargePointsXS1$name)]
  dsc1BankfullCalls[2,1]<-dischargePointsXS1$name[grepl("RBF",dischargePointsXS1$name)]
  dsc1BankfullCalls[2,2]<-dischargePointsXS1$DistanceAdj[grepl("RBF",dischargePointsXS1$name)]
  dsc1BankfullCalls[2,3]<-dischargePointsXS1$H[grepl("RBF",dischargePointsXS1$name)]
  
  #If the bankfull field calls are OK, calculates the bankfull width.
  if(SurveyedDSC1==TRUE){
    DSCXS1Bankfull<-abs((dischargePointsXS1$DistanceAdj[grepl("RBF",dischargePointsXS1$name)])-(dischargePointsXS1$DistanceAdj[grepl("LBF",dischargePointsXS1$name)]))
  } else {
    DSCXS1Bankfull<-"NA"
  }

  #Plot the cross-section profile, highlights the bankfull shots for QA.  
  plot_ly(data=dischargePointsXS1,x=~DistanceAdj, y=~H, name='Distance vs Elevation', type='scatter', mode='lines+markers', text=~name)%>%
    add_trace(data=dsc1BankfullCalls,x=~DistanceAdj,y=~H,name='Bankfull Field Calls',type='scatter',mode='markers',text=~name,marker=list(size=10,line=list(color='black',width=.5)))%>%
    layout(title = paste(siteID, "dischargePointsXS1",surveyDate), xaxis=xAxisTitle1, yaxis=yAxisTitle1)
  
} else {"There is no data associated with the 'Transect_DSC1' mapCode"}

############# Discharge XS2 transect  ##################################################################################################

if(SurveyedDSC2){
  
  #Manually select NorthStart and EastStart coordinates (this should be the mapped point furthest along the left bank side of the channel):
  dischargeXS2NorthStart<-dischargePointsXS2$N[1]
  dischargeXS2EastStart<-dischargePointsXS2$E[1]
  
  #Creates distance values for each point relative to the pin. 
  for(i in 1:(length(dischargePointsXS2$name))){
    dischargeXS2PointN<-dischargePointsXS2$N[i]
    dischargeXS2PointE<-dischargePointsXS2$E[i]
    dischargePointsXS2$DistanceRaw[i]<-sqrt(((dischargeXS2PointN-dischargeXS2NorthStart)^2)+((dischargeXS2PointE-dischargeXS2EastStart)^2))
  }
  
  #Manually select ReferenceDistance (this should be a mapped point on a pin [preferrably a left bank pin]; if a pin shot is not available choose the mapped point furthest into the left bank side of the channel):
  dischargeXS2ReferenceDistance<-dischargePointsXS2$DistanceRaw[1]
  
  #Sets Horizontal adjustment value based on reference point coordinate.  
  dischargeXS2HorizontalAdjust<-0-dischargeXS2ReferenceDistance
  
  #Transforms raw distance to adjusted distance based on reference distance point.
  for(i in 1:(length(dischargePointsXS2$name))){
    dischargePointsXS2$DistanceAdj[i]<-dischargePointsXS2$DistanceRaw[i]+dischargeXS2HorizontalAdjust
  }
  
  #Creates dataframe of bankfull shots mapped during the survey.  
  dsc2BankfullCalls<-data.frame(matrix(nrow=2,ncol=3))
  names(dsc2BankfullCalls)=c("name","DistanceAdj","H")
  dsc2BankfullCalls[1,1]<-dischargePointsXS2$name[grepl("LBF",dischargePointsXS2$name)]
  dsc2BankfullCalls[1,2]<-dischargePointsXS2$DistanceAdj[grepl("LBF",dischargePointsXS2$name)]
  dsc2BankfullCalls[1,3]<-dischargePointsXS2$H[grepl("LBF",dischargePointsXS2$name)]
  dsc2BankfullCalls[2,1]<-dischargePointsXS2$name[grepl("RBF",dischargePointsXS2$name)]
  dsc2BankfullCalls[2,2]<-dischargePointsXS2$DistanceAdj[grepl("RBF",dischargePointsXS2$name)]
  dsc2BankfullCalls[2,3]<-dischargePointsXS2$H[grepl("RBF",dischargePointsXS2$name)]


#Plot the cross-section profile, highlights the bankfull shots for QA.  

plot_ly(data=dischargePointsXS2,x=~DistanceAdj, y=~H, name='Distance vs Elevation', type='scatter', mode='lines+markers', text=~name)%>%
  add_trace(data=dsc2BankfullCalls,x=~DistanceAdj,y=~H,name='Bankfull Field Calls',type='scatter',mode='markers',text=~name,marker=list(size=10,line=list(color='black',width=.5)))%>%
  layout(title = paste(siteID, "dischargePointsXS2",surveyDate), xaxis=xAxisTitle1, yaxis=yAxisTitle1)

#If the bankfull field calls are OK, calculates the bankfull width.
if(SurveyedDSC2==TRUE){
  DSCXS2Bankfull<-abs((dischargePointsXS2$DistanceAdj[grepl("RBF",dischargePointsXS2$name)])-(dischargePointsXS2$DistanceAdj[grepl("LBF",dischargePointsXS2$name)]))
} else {
  DSCXS2Bankfull<-"NA"
}

} else {"There is no data associated with the 'Transect_DSC2' mapCode"}

############# AP1 transect  ##################################################################################################

if(SurveyedAP1){
  
  AP1northEastStart<-41 #Manually select the row of the mapped point that is furthest along the left bank side of the channel
  AP1reference<-31 ##Manually select the row of the mapped point taken on a pin [preferably LB pin but RB works too]; if a pin shot is not available choose the mapped point furthest into the left bank side of the channel
  
  AP1NorthStart<-AP1Points$N[AP1northEastStart]
  AP1EastStart<-AP1Points$E[AP1northEastStart]
  
  #Creates distance values for each point relative to the pin. 
  for(i in 1:(length(AP1Points$name))){
    AP1PointN<-AP1Points$N[i]
    AP1PointE<-AP1Points$E[i]
    AP1Points$DistanceRaw[i]<-sqrt(((AP1PointN-AP1NorthStart)^2)+((AP1PointE-AP1EastStart)^2))
  }
  
   AP1ReferenceDistance<-AP1Points$DistanceRaw[AP1reference]
  
  #Sets Horizontal adjustment value based on reference point coordinate.  
  AP1HorizontalAdjust<-0-AP1ReferenceDistance
  
  #Transforms raw distance to adjusted distance based on reference distance point.
  for(i in 1:(length(AP1Points$name))){
    AP1Points$DistanceAdj[i]<-AP1Points$DistanceRaw[i]+AP1HorizontalAdjust
  }
  
  #Creates dataframe of bankfull shots mapped during the survey.  
  AP1BankfullCalls<-data.frame(matrix(nrow=2,ncol=3))
  names(AP1BankfullCalls)=c("name","DistanceAdj","H")
  AP1BankfullCalls[1,1]<-AP1Points$name[grepl("LBF",AP1Points$name)]
  AP1BankfullCalls[1,2]<-AP1Points$DistanceAdj[grepl("LBF",AP1Points$name)]
  AP1BankfullCalls[1,3]<-AP1Points$H[grepl("LBF",AP1Points$name)]
  AP1BankfullCalls[2,1]<-AP1Points$name[grepl("RBF",AP1Points$name)]
  AP1BankfullCalls[2,2]<-AP1Points$DistanceAdj[grepl("RBF",AP1Points$name)]
  AP1BankfullCalls[2,3]<-AP1Points$H[grepl("RBF",AP1Points$name)]
  
  #Plot the cross-section profile, highlights the bankfull shots for QA.  
  plot_ly(data=AP1Points,x=~DistanceAdj, y=~H, name='Distance vs Elevation', type='scatter', mode='lines+markers', text=~name)%>%
    add_trace(data=AP1BankfullCalls,x=~DistanceAdj,y=~H,name='Bankfull Field Calls',type='scatter',mode='markers',text=~name,marker=list(size=10,line=list(color='black',width=.5)))%>%
    layout(title = paste(siteID, "AP1Points",surveyDate), xaxis=xAxisTitle1, yaxis=yAxisTitle1)
  
  #If the bankfull field calls are OK, calculates the bankfull width.
  AP1Bankfull<-abs((AP1Points$DistanceAdj[grepl("RBF",AP1Points$name)])-(AP1Points$DistanceAdj[grepl("LBF",AP1Points$name)]))
 
#If the bankfull field calls are OK, calculates the bankfull width.
if(SurveyedAP1==TRUE){
  AP1Bankfull<-abs((AP1Points$DistanceAdj[grepl("RBF",AP1Points$name)])-(AP1Points$DistanceAdj[grepl("LBF",AP1Points$name)]))
} else {
  AP1Bankfull<-"NA"
}

  #Plot the cross-section profile, highlights the bankfull shots for QA.  
  plot_ly(data=AP1Points,x=~DistanceAdj, y=~H, name='Distance vs Elevation', type='scatter', mode='lines+markers', text=~name)%>%
    add_trace(data=AP1BankfullCalls,x=~DistanceAdj,y=~H,name='Bankfull Field Calls',type='scatter',mode='markers',text=~name,marker=list(size=10,line=list(color='black',width=.5)))%>%
    layout(title = paste(siteID, "AP1Points",surveyDate), xaxis=xAxisTitle1, yaxis=yAxisTitle1)  
  
} else {"There is no data associated with the 'Transect_AP1' mapCode"}

############# AP2 transect  ##################################################################################################

if(SurveyedAP2){
  
  AP2northEastStart<-34 #Manually select the row of the mapped point that is furthest along the left bank side of the channel
  AP2reference<-31 ##Manually select the row of the mapped point taken on a pin [preferably LB pin but RB works too]; if a pin shot is not available choose the mapped point furthest into the left bank side of the channel
  
  AP2NorthStart<-AP2Points$N[AP2northEastStart]
  AP2EastStart<-AP2Points$E[AP2northEastStart]
  
  #Creates distance values for each point relative to the pin. 
  for(i in 1:(length(AP2Points$name))){
    AP2PointN<-AP2Points$N[i]
    AP2PointE<-AP2Points$E[i]
    AP2Points$DistanceRaw[i]<-sqrt(((AP2PointN-AP2NorthStart)^2)+((AP2PointE-AP2EastStart)^2))
  }
  
  AP2ReferenceDistance<-AP2Points$DistanceRaw[AP2reference]
  
  #Sets Horizontal adjustment value based on reference point coordinate.  
  AP2HorizontalAdjust<-0-AP2ReferenceDistance
  
  #Transforms raw distance to adjusted distance based on reference distance point.
  for(i in 1:(length(AP2Points$name))){
    AP2Points$DistanceAdj[i]<-AP2Points$DistanceRaw[i]+AP2HorizontalAdjust
  }
  
  #Creates dataframe of bankfull shots mapped during the survey.  
  AP2BankfullCalls<-data.frame(matrix(nrow=2,ncol=3))
  names(AP2BankfullCalls)=c("name","DistanceAdj","H")
  AP2BankfullCalls[1,1]<-AP2Points$name[grepl("LBF",AP2Points$name)]
  AP2BankfullCalls[1,2]<-AP2Points$DistanceAdj[grepl("LBF",AP2Points$name)]
  AP2BankfullCalls[1,3]<-AP2Points$H[grepl("LBF",AP2Points$name)]
  AP2BankfullCalls[2,1]<-AP2Points$name[grepl("RBF",AP2Points$name)]
  AP2BankfullCalls[2,2]<-AP2Points$DistanceAdj[grepl("RBF",AP2Points$name)]
  AP2BankfullCalls[2,3]<-AP2Points$H[grepl("RBF",AP2Points$name)]
  
#If the bankfull field calls are OK, calculates the bankfull width.
if(SurveyedAP2==TRUE){
  AP2Bankfull<-abs((AP2Points$DistanceAdj[grepl("RBF",AP2Points$name)])-(AP2Points$DistanceAdj[grepl("LBF",AP2Points$name)]))
} else {
  AP2Bankfull<-"NA"
}

  #Plot the cross-section profile, highlights the bankfull shots for QA.  
  plot_ly(data=AP2Points,x=~DistanceAdj, y=~H, name='Distance vs Elevation', type='scatter', mode='lines+markers', text=~name)%>%
    add_trace(data=AP2BankfullCalls,x=~DistanceAdj,y=~H,name='Bankfull Field Calls',type='scatter',mode='markers',text=~name,marker=list(size=10,line=list(color='black',width=.5)))%>%
    layout(title = paste(siteID, "AP2Points",surveyDate), xaxis=xAxisTitle1, yaxis=yAxisTitle1)
  
} else {"There is no data associated with the 'Transect_AP2' mapCode"}

############# AP3 transect  ##################################################################################################

if(SurveyedAP3){
  
  AP3northEastStart<-31 #Manually select the row of the mapped point that is furthest along the left bank side of the channel
  AP3reference<-1 ##Manually select the row of the mapped point taken on a pin [preferably LB pin but RB works too]; if a pin shot is not available choose the mapped point furthest into the left bank side of the channel
  
  AP3NorthStart<-AP3Points$N[AP3northEastStart]
  AP3EastStart<-AP3Points$E[AP3northEastStart]
  
  #Creates distance values for each point relative to the pin. 
  for(i in 1:(length(AP3Points$name))){
    AP3PointN<-AP3Points$N[i]
    AP3PointE<-AP3Points$E[i]
    AP3Points$DistanceRaw[i]<-sqrt(((AP3PointN-AP3NorthStart)^2)+((AP3PointE-AP3EastStart)^2))
  }
  
  AP3ReferenceDistance<-AP3Points$DistanceRaw[AP3reference]
  
  #Sets Horizontal adjustment value based on reference point coordinate.  
  AP3HorizontalAdjust<-0-AP3ReferenceDistance
  
  #Transforms raw distance to adjusted distance based on reference distance point.
  for(i in 1:(length(AP3Points$name))){
    AP3Points$DistanceAdj[i]<-AP3Points$DistanceRaw[i]+AP3HorizontalAdjust
  }
  
  #Creates dataframe of bankfull shots mapped during the survey.  
  AP3BankfullCalls<-data.frame(matrix(nrow=2,ncol=3))
  names(AP3BankfullCalls)=c("name","DistanceAdj","H")
  AP3BankfullCalls[1,1]<-AP3Points$name[grepl("LBF",AP3Points$name)]
  AP3BankfullCalls[1,2]<-AP3Points$DistanceAdj[grepl("LBF",AP3Points$name)]
  AP3BankfullCalls[1,3]<-AP3Points$H[grepl("LBF",AP3Points$name)]
  AP3BankfullCalls[2,1]<-AP3Points$name[grepl("RBF",AP3Points$name)]
  AP3BankfullCalls[2,2]<-AP3Points$DistanceAdj[grepl("RBF",AP3Points$name)]
  AP3BankfullCalls[2,3]<-AP3Points$H[grepl("RBF",AP3Points$name)]

#If the bankfull field calls are OK, calculates the bankfull width.
if(SurveyedAP3==TRUE){
  AP3Bankfull<-abs((AP3Points$DistanceAdj[grepl("RBF",AP3Points$name)])-(AP3Points$DistanceAdj[grepl("LBF",AP3Points$name)]))
} else {
  AP3Bankfull<-"NA"
}

  #Plot the cross-section profile, highlights the bankfull shots for QA.  
  plot_ly(data=AP3Points,x=~DistanceAdj, y=~H, name='Distance vs Elevation', type='scatter', mode='lines+markers', text=~name)%>%
    add_trace(data=AP3BankfullCalls,x=~DistanceAdj,y=~H,name='Bankfull Field Calls',type='scatter',mode='markers',text=~name,marker=list(size=10,line=list(color='black',width=.5)))%>%
    layout(title = paste(siteID, "AP3Points",surveyDate), xaxis=xAxisTitle1, yaxis=yAxisTitle1)  
  
} else {"There is no data associated with the 'Transect_AP3' mapCode"}

############# AP4 transect  ##################################################################################################

if(SurveyedAP4){
  
  AP4northEastStart<-47 #Manually select the row of the mapped point that is furthest along the left bank side of the channel
  AP4reference<-35 ##Manually select the row of the mapped point taken on a pin [preferably LB pin but RB works too]; if a pin shot is not available choose the mapped point furthest into the left bank side of the channel
  
  AP4NorthStart<-AP4Points$N[AP4northEastStart]
  AP4EastStart<-AP4Points$E[AP4northEastStart]
  
  #Creates distance values for each point relative to the pin. 
  for(i in 1:(length(AP4Points$name))){
    AP4PointN<-AP4Points$N[i]
    AP4PointE<-AP4Points$E[i]
    AP4Points$DistanceRaw[i]<-sqrt(((AP4PointN-AP4NorthStart)^2)+((AP4PointE-AP4EastStart)^2))
  }
  
  AP4ReferenceDistance<-AP4Points$DistanceRaw[AP4reference]
  
  #Sets Horizontal adjustment value based on reference point coordinate.  
  AP4HorizontalAdjust<-0-AP4ReferenceDistance
  
  #Transforms raw distance to adjusted distance based on reference distance point.
  for(i in 1:(length(AP4Points$name))){
    AP4Points$DistanceAdj[i]<-AP4Points$DistanceRaw[i]+AP4HorizontalAdjust
  }
  
  #Creates dataframe of bankfull shots mapped during the survey.  
  AP4BankfullCalls<-data.frame(matrix(nrow=2,ncol=3))
  names(AP4BankfullCalls)=c("name","DistanceAdj","H")
  AP4BankfullCalls[1,1]<-AP4Points$name[grepl("LBF",AP4Points$name)]
  AP4BankfullCalls[1,2]<-AP4Points$DistanceAdj[grepl("LBF",AP4Points$name)]
  AP4BankfullCalls[1,3]<-AP4Points$H[grepl("LBF",AP4Points$name)]
  AP4BankfullCalls[2,1]<-AP4Points$name[grepl("RBF",AP4Points$name)]
  AP4BankfullCalls[2,2]<-AP4Points$DistanceAdj[grepl("RBF",AP4Points$name)]
  AP4BankfullCalls[2,3]<-AP4Points$H[grepl("RBF",AP4Points$name)]

#If the bankfull field calls are OK, calculates the bankfull width.
if(SurveyedAP4==TRUE){
  AP4Bankfull<-abs((AP4Points$DistanceAdj[grepl("RBF",AP4Points$name)])-(AP4Points$DistanceAdj[grepl("LBF",AP4Points$name)]))
} else {
  AP4Bankfull<-"NA"
}

  #Plot the cross-section profile, highlights the bankfull shots for QA.  
  plot_ly(data=AP4Points,x=~DistanceAdj, y=~H, name='Distance vs Elevation', type='scatter', mode='lines+markers', text=~name)%>%
    add_trace(data=AP4BankfullCalls,x=~DistanceAdj,y=~H,name='Bankfull Field Calls',type='scatter',mode='markers',text=~name,marker=list(size=10,line=list(color='black',width=.5)))%>%
    layout(title = paste(siteID, "AP4Points",surveyDate), xaxis=xAxisTitle1, yaxis=yAxisTitle1)  
  
} else {"There is no data associated with the 'Transect_AP4' mapCode"}

############# AP5 transect  ##################################################################################################

if(SurveyedAP5){
  
  AP5northEastStart<-1 #Manually select the row of the mapped point that is furthest along the left bank side of the channel
  AP5reference<-3 ##Manually select the row of the mapped point taken on a pin [preferably LB pin but RB works too]; if a pin shot is not available choose the mapped point furthest into the left bank side of the channel
  
  AP5NorthStart<-AP5Points$N[AP5northEastStart]
  AP5EastStart<-AP5Points$E[AP5northEastStart]
  
  #Creates distance values for each point relative to the pin. 
  for(i in 1:(length(AP5Points$name))){
    AP5PointN<-AP5Points$N[i]
    AP5PointE<-AP5Points$E[i]
    AP5Points$DistanceRaw[i]<-sqrt(((AP5PointN-AP5NorthStart)^2)+((AP5PointE-AP5EastStart)^2))
  }
  
 AP5ReferenceDistance<-AP5Points$DistanceRaw[AP5reference]
  
  #Sets Horizontal adjustment value based on reference point coordinate.  
  AP5HorizontalAdjust<-0-AP5ReferenceDistance
  
  #Transforms raw distance to adjusted distance based on reference distance point.
  for(i in 1:(length(AP5Points$name))){
    AP5Points$DistanceAdj[i]<-AP5Points$DistanceRaw[i]+AP5HorizontalAdjust
  }
  
  #Creates dataframe of bankfull shots mapped during the survey.  
  AP5BankfullCalls<-data.frame(matrix(nrow=2,ncol=3))
  names(AP5BankfullCalls)=c("name","DistanceAdj","H")
  AP5BankfullCalls[1,1]<-AP5Points$name[grepl("LBF",AP5Points$name)]
  AP5BankfullCalls[1,2]<-AP5Points$DistanceAdj[grepl("LBF",AP5Points$name)]
  AP5BankfullCalls[1,3]<-AP5Points$H[grepl("LBF",AP5Points$name)]
  AP5BankfullCalls[2,1]<-AP5Points$name[grepl("RBF",AP5Points$name)]
  AP5BankfullCalls[2,2]<-AP5Points$DistanceAdj[grepl("RBF",AP5Points$name)]
  AP5BankfullCalls[2,3]<-AP5Points$H[grepl("RBF",AP5Points$name)]
  
  #If the bankfull field calls are OK, calculates the bankfull width.
  if(SurveyedAP5==TRUE){
    AP5Bankfull<-abs((AP5Points$DistanceAdj[grepl("RBF",AP5Points$name)])-(AP5Points$DistanceAdj[grepl("LBF",AP5Points$name)]))
  } else {
    AP5Bankfull<-"NA"
  }
  
  #Plot the cross-section profile, highlights the bankfull shots for QA.  
  plot_ly(data=AP5Points,x=~DistanceAdj, y=~H, name='Distance vs Elevation', type='scatter', mode='lines+markers', text=~name)%>%
    add_trace(data=AP5BankfullCalls,x=~DistanceAdj,y=~H,name='Bankfull Field Calls',type='scatter',mode='markers',text=~name,marker=list(size=10,line=list(color='black',width=.5)))%>%
    layout(title = paste(siteID, "AP5Points",surveyDate), xaxis=xAxisTitle1, yaxis=yAxisTitle1)
  
} else {"There is no data associated with the 'Transect_AP5' mapCode"}

############# AP6 transect  ##################################################################################################

if(SurveyedAP6){
  
  AP6northEastStart<-1 #Manually select the row of the mapped point that is furthest along the left bank side of the channel
  AP6reference<-8 ##Manually select the row of the mapped point taken on a pin [preferably LB pin but RB works too]; if a pin shot is not available choose the mapped point furthest into the left bank side of the channel
  
  AP6NorthStart<-AP6Points$N[AP6northEastStart]
  AP6EastStart<-AP6Points$E[AP6northEastStart]
  
  #Creates distance values for each point relative to the pin. 
  for(i in 1:(length(AP6Points$name))){
    AP6PointN<-AP6Points$N[i]
    AP6PointE<-AP6Points$E[i]
    AP6Points$DistanceRaw[i]<-sqrt(((AP6PointN-AP6NorthStart)^2)+((AP6PointE-AP6EastStart)^2))
  }
  
  AP6ReferenceDistance<-AP6Points$DistanceRaw[AP6reference]
  
  #Sets Horizontal adjustment value based on reference point coordinate.  
  AP6HorizontalAdjust<-0-AP6ReferenceDistance
  
  #Transforms raw distance to adjusted distance based on reference distance point.
  for(i in 1:(length(AP6Points$name))){
    AP6Points$DistanceAdj[i]<-AP6Points$DistanceRaw[i]+AP6HorizontalAdjust
  }
  
  #Creates dataframe of bankfull shots mapped during the survey.  
  AP6BankfullCalls<-data.frame(matrix(nrow=2,ncol=3))
  names(AP6BankfullCalls)=c("name","DistanceAdj","H")
  AP6BankfullCalls[1,1]<-AP6Points$name[grepl("LBF",AP6Points$name)]
  AP6BankfullCalls[1,2]<-AP6Points$DistanceAdj[grepl("LBF",AP6Points$name)]
  AP6BankfullCalls[1,3]<-AP6Points$H[grepl("LBF",AP6Points$name)]
  AP6BankfullCalls[2,1]<-AP6Points$name[grepl("RBF",AP6Points$name)]
  AP6BankfullCalls[2,2]<-AP6Points$DistanceAdj[grepl("RBF",AP6Points$name)]
  AP6BankfullCalls[2,3]<-AP6Points$H[grepl("RBF",AP6Points$name)]
  
#If the bankfull field calls are OK, calculates the bankfull width.
if(SurveyedAP6==TRUE){
  AP6Bankfull<-abs((AP6Points$DistanceAdj[grepl("RBF",AP6Points$name)])-(AP6Points$DistanceAdj[grepl("LBF",AP6Points$name)]))
} else {
  AP6Bankfull<-"NA"
}

  #Plot the cross-section profile, highlights the bankfull shots for QA.  
  plot_ly(data=AP6Points,x=~DistanceAdj, y=~H, name='Distance vs Elevation', type='scatter', mode='lines+markers', text=~name)%>%
    add_trace(data=AP6BankfullCalls,x=~DistanceAdj,y=~H,name='Bankfull Field Calls',type='scatter',mode='markers',text=~name,marker=list(size=10,line=list(color='black',width=.5)))%>%
    layout(title = paste(siteID, "AP6Points",surveyDate), xaxis=xAxisTitle1, yaxis=yAxisTitle1)  
  
} else {"There is no data associated with the 'Transect_AP6' mapCode"}

############# AP7 transect  ##################################################################################################

if(SurveyedAP7){
  
  AP7northEastStart<-1 #Manually select the row of the mapped point that is furthest along the left bank side of the channel
  AP7reference<-3 ##Manually select the row of the mapped point taken on a pin [preferably LB pin but RB works too]; if a pin shot is not available choose the mapped point furthest into the left bank side of the channel
  
  AP7NorthStart<-AP7Points$N[AP7northEastStart]
  AP7EastStart<-AP7Points$E[AP7northEastStart]
  
  #Creates distance values for each point relative to the pin. 
  for(i in 1:(length(AP7Points$name))){
    AP7PointN<-AP7Points$N[i]
    AP7PointE<-AP7Points$E[i]
    AP7Points$DistanceRaw[i]<-sqrt(((AP7PointN-AP7NorthStart)^2)+((AP7PointE-AP7EastStart)^2))
  }
  
  AP7ReferenceDistance<-AP7Points$DistanceRaw[AP7reference]
  
  #Sets Horizontal adjustment value based on reference point coordinate.  
  AP7HorizontalAdjust<-0-AP7ReferenceDistance
  
  #Transforms raw distance to adjusted distance based on reference distance point.
  for(i in 1:(length(AP7Points$name))){
    AP7Points$DistanceAdj[i]<-AP7Points$DistanceRaw[i]+AP7HorizontalAdjust
  }
  
  #Creates dataframe of bankfull shots mapped during the survey.  
  AP7BankfullCalls<-data.frame(matrix(nrow=2,ncol=3))
  names(AP7BankfullCalls)=c("name","DistanceAdj","H")
  AP7BankfullCalls[1,1]<-AP7Points$name[grepl("LBF",AP7Points$name)]
  AP7BankfullCalls[1,2]<-AP7Points$DistanceAdj[grepl("LBF",AP7Points$name)]
  AP7BankfullCalls[1,3]<-AP7Points$H[grepl("LBF",AP7Points$name)]
  AP7BankfullCalls[2,1]<-AP7Points$name[grepl("RBF",AP7Points$name)]
  AP7BankfullCalls[2,2]<-AP7Points$DistanceAdj[grepl("RBF",AP7Points$name)]
  AP7BankfullCalls[2,3]<-AP7Points$H[grepl("RBF",AP7Points$name)]
  
#If the bankfull field calls are OK, calculates the bankfull width.
if(SurveyedAP7==TRUE){
  AP7Bankfull<-abs((AP7Points$DistanceAdj[grepl("RBF",AP7Points$name)])-(AP7Points$DistanceAdj[grepl("LBF",AP7Points$name)]))
} else {
  AP7Bankfull<-"NA"
}

  #Plot the cross-section profile, highlights the bankfull shots for QA.  
  plot_ly(data=AP7Points,x=~DistanceAdj, y=~H, name='Distance vs Elevation', type='scatter', mode='lines+markers', text=~name)%>%
    add_trace(data=AP7BankfullCalls,x=~DistanceAdj,y=~H,name='Bankfull Field Calls',type='scatter',mode='markers',text=~name,marker=list(size=10,line=list(color='black',width=.5)))%>%
    layout(title = paste(siteID, "AP7Points",surveyDate), xaxis=xAxisTitle1, yaxis=yAxisTitle1)
  
} else {"There is no data associated with the 'Transect_AP7' mapCode"}

############# AP8 transect  ##################################################################################################

if(SurveyedAP8){
  
  AP8northEastStart<-41 #Manually select the row of the mapped point that is furthest along the left bank side of the channel
  AP8reference<-38 ##Manually select the row of the mapped point taken on a pin [preferably LB pin but RB works too]; if a pin shot is not available choose the mapped point furthest into the left bank side of the channel
  
  AP8NorthStart<-AP8Points$N[AP8northEastStart]
  AP8EastStart<-AP8Points$E[AP8northEastStart]
  
  #Creates distance values for each point relative to the pin. 
  for(i in 1:(length(AP8Points$name))){
    AP8PointN<-AP8Points$N[i]
    AP8PointE<-AP8Points$E[i]
    AP8Points$DistanceRaw[i]<-sqrt(((AP8PointN-AP8NorthStart)^2)+((AP8PointE-AP8EastStart)^2))
  }
  
  AP8ReferenceDistance<-AP8Points$DistanceRaw[AP8reference]
  
  #Sets Horizontal adjustment value based on reference point coordinate.  
  AP8HorizontalAdjust<-0-AP8ReferenceDistance
  
  #Transforms raw distance to adjusted distance based on reference distance point.
  for(i in 1:(length(AP8Points$name))){
    AP8Points$DistanceAdj[i]<-AP8Points$DistanceRaw[i]+AP8HorizontalAdjust
  }
  
  #Creates dataframe of bankfull shots mapped during the survey.  
  AP8BankfullCalls<-data.frame(matrix(nrow=2,ncol=3))
  names(AP8BankfullCalls)=c("name","DistanceAdj","H")
  AP8BankfullCalls[1,1]<-AP8Points$name[grepl("LBF",AP8Points$name)]
  AP8BankfullCalls[1,2]<-AP8Points$DistanceAdj[grepl("LBF",AP8Points$name)]
  AP8BankfullCalls[1,3]<-AP8Points$H[grepl("LBF",AP8Points$name)]
  AP8BankfullCalls[2,1]<-AP8Points$name[grepl("RBF",AP8Points$name)]
  AP8BankfullCalls[2,2]<-AP8Points$DistanceAdj[grepl("RBF",AP8Points$name)]
  AP8BankfullCalls[2,3]<-AP8Points$H[grepl("RBF",AP8Points$name)]
  
#If the bankfull field calls are OK, calculates the bankfull width.
if(SurveyedAP8==TRUE){
  AP8Bankfull<-abs((AP8Points$DistanceAdj[grepl("RBF",AP8Points$name)])-(AP8Points$DistanceAdj[grepl("LBF",AP8Points$name)]))
} else {
  AP8Bankfull<-"NA"
}

  #Plot the cross-section profile, highlights the bankfull shots for QA.  
  plot_ly(data=AP8Points,x=~DistanceAdj, y=~H, name='Distance vs Elevation', type='scatter', mode='lines+markers', text=~name)%>%
    add_trace(data=AP8BankfullCalls,x=~DistanceAdj,y=~H,name='Bankfull Field Calls',type='scatter',mode='markers',text=~name,marker=list(size=10,line=list(color='black',width=.5)))%>%
    layout(title = paste(siteID, "AP8Points",surveyDate), xaxis=xAxisTitle1, yaxis=yAxisTitle1)
  
} else {"There is no data associated with the 'Transect_AP8' mapCode"}

############# AP9 transect  ##################################################################################################

if(SurveyedAP9){
  
  AP9northEastStart<-62 #Manually select the row of the mapped point that is furthest along the left bank side of the channel
  AP9reference<-11 ##Manually select the row of the mapped point taken on a pin [preferably LB pin but RB works too]; if a pin shot is not available choose the mapped point furthest into the left bank side of the channel
  
  AP9NorthStart<-AP9Points$N[AP9northEastStart]
  AP9EastStart<-AP9Points$E[AP9northEastStart]
  
  #Creates distance values for each point relative to the pin. 
  for(i in 1:(length(AP9Points$name))){
    AP9PointN<-AP9Points$N[i]
    AP9PointE<-AP9Points$E[i]
    AP9Points$DistanceRaw[i]<-sqrt(((AP9PointN-AP9NorthStart)^2)+((AP9PointE-AP9EastStart)^2))
  }
  
  AP9ReferenceDistance<-AP9Points$DistanceRaw[AP9reference]
  
  #Sets Horizontal adjustment value based on reference point coordinate.  
  AP9HorizontalAdjust<-0-AP9ReferenceDistance
  
  #Transforms raw distance to adjusted distance based on reference distance point.
  for(i in 1:(length(AP9Points$name))){
    AP9Points$DistanceAdj[i]<-AP9Points$DistanceRaw[i]+AP9HorizontalAdjust
  }
  
  #Creates dataframe of bankfull shots mapped during the survey.  
  AP9BankfullCalls<-data.frame(matrix(nrow=2,ncol=3))
  names(AP9BankfullCalls)=c("name","DistanceAdj","H")
  AP9BankfullCalls[1,1]<-AP9Points$name[grepl("LBF",AP9Points$name)]
  AP9BankfullCalls[1,2]<-AP9Points$DistanceAdj[grepl("LBF",AP9Points$name)]
  AP9BankfullCalls[1,3]<-AP9Points$H[grepl("LBF",AP9Points$name)]
  AP9BankfullCalls[2,1]<-AP9Points$name[grepl("RBF",AP9Points$name)]
  AP9BankfullCalls[2,2]<-AP9Points$DistanceAdj[grepl("RBF",AP9Points$name)]
  AP9BankfullCalls[2,3]<-AP9Points$H[grepl("RBF",AP9Points$name)]
  
#If the bankfull field calls are OK, calculates the bankfull width.
if(SurveyedAP9==TRUE){
  AP9Bankfull<-abs((AP9Points$DistanceAdj[grepl("RBF",AP9Points$name)])-(AP9Points$DistanceAdj[grepl("LBF",AP9Points$name)]))
} else {
  AP9Bankfull<-"NA"
}

  #Plot the cross-section profile, highlights the bankfull shots for QA.  
  plot_ly(data=AP9Points,x=~DistanceAdj, y=~H, name='Distance vs Elevation', type='scatter', mode='lines+markers', text=~name)%>%
    add_trace(data=AP9BankfullCalls,x=~DistanceAdj,y=~H,name='Bankfull Field Calls',type='scatter',mode='markers',text=~name,marker=list(size=10,line=list(color='black',width=.5)))%>%
    layout(title = paste(siteID, "AP9Points",surveyDate), xaxis=xAxisTitle1, yaxis=yAxisTitle1)
  
} else {"There is no data associated with the 'Transect_AP9' mapCode"}


############# AP10 transect  ##################################################################################################

if(SurveyedAP10){
  
  AP10northEastStart<-1 #Manually select the row of the mapped point that is furthest along the left bank side of the channel
  AP10reference<-28 ##Manually select the row of the mapped point taken on a pin [preferably LB pin but RB works too]; if a pin shot is not available choose the mapped point furthest into the left bank side of the channel
  
  AP10NorthStart<-AP10Points$N[AP10northEastStart]
  AP10EastStart<-AP10Points$E[AP10northEastStart]
  
  #Creates distance values for each point relative to the pin. 
  for(i in 1:(length(AP10Points$name))){
    AP10PointN<-AP10Points$N[i]
    AP10PointE<-AP10Points$E[i]
    AP10Points$DistanceRaw[i]<-sqrt(((AP10PointN-AP10NorthStart)^2)+((AP10PointE-AP10EastStart)^2))
  }
  
  AP10ReferenceDistance<-AP10Points$DistanceRaw[AP10reference]
  
  #Sets Horizontal adjustment value based on reference point coordinate.  
  AP10HorizontalAdjust<-0-AP10ReferenceDistance
  
  #Transforms raw distance to adjusted distance based on reference distance point.
  for(i in 1:(length(AP10Points$name))){
    AP10Points$DistanceAdj[i]<-AP10Points$DistanceRaw[i]+AP10HorizontalAdjust
  }
  
  #Creates dataframe of bankfull shots mapped during the survey.  
  AP10BankfullCalls<-data.frame(matrix(nrow=2,ncol=3))
  names(AP10BankfullCalls)=c("name","DistanceAdj","H")
  AP10BankfullCalls[1,1]<-AP10Points$name[grepl("LBF",AP10Points$name)]
  AP10BankfullCalls[1,2]<-AP10Points$DistanceAdj[grepl("LBF",AP10Points$name)]
  AP10BankfullCalls[1,3]<-AP10Points$H[grepl("LBF",AP10Points$name)]
  AP10BankfullCalls[2,1]<-AP10Points$name[grepl("RBF",AP10Points$name)]
  AP10BankfullCalls[2,2]<-AP10Points$DistanceAdj[grepl("RBF",AP10Points$name)]
  AP10BankfullCalls[2,3]<-AP10Points$H[grepl("RBF",AP10Points$name)]

#If the bankfull field calls are OK, calculates the bankfull width.
if(SurveyedAP10==TRUE){
  AP10Bankfull<-abs((AP10Points$DistanceAdj[grepl("RBF",AP10Points$name)])-(AP10Points$DistanceAdj[grepl("LBF",AP10Points$name)]))
  
  #Plot the cross-section profile, highlights the bankfull shots for QA.  
  plot_ly(data=AP10Points,x=~DistanceAdj, y=~H, name='Distance vs Elevation', type='scatter', mode='lines+markers', text=~name)%>%
    add_trace(data=AP10BankfullCalls,x=~DistanceAdj,y=~H,name='Bankfull Field Calls',type='scatter',mode='markers',text=~name,marker=list(size=10,line=list(color='black',width=.5)))%>%
    layout(title = paste(siteID, "AP10Points",surveyDate), xaxis=xAxisTitle1, yaxis=yAxisTitle1)
  
} else {
  AP10Bankfull<-"NA"
}

} else {"There is no data associated with the 'Transect_AP10' mapCode"}

############# S1 transect  ##################################################################################################

if(SurveyedS1){
  
  S1northEastStart<-44 #Manually select the row of the mapped point that is furthest along the left bank side of the channel
  S1reference<-44 ##Manually select the row of the mapped point taken on a pin [preferably LB pin but RB works too]; if a pin shot is not available choose the mapped point furthest into the left bank side of the channel
  
  S1NorthStart<-S1Points$N[S1northEastStart]
  S1EastStart<-S1Points$E[S1northEastStart]
  
  #Creates distance values for each point relative to the pin. 
  for(i in 1:(length(S1Points$name))){
    S1PointN<-S1Points$N[i]
    S1PointE<-S1Points$E[i]
    S1Points$DistanceRaw[i]<-sqrt(((S1PointN-S1NorthStart)^2)+((S1PointE-S1EastStart)^2))
  }
  
  S1ReferenceDistance<-S1Points$DistanceRaw[S1reference]
  
  #Sets Horizontal adjustment value based on reference point coordinate.  
  S1HorizontalAdjust<-0-S1ReferenceDistance
  
  #Transforms raw distance to adjusted distance based on reference distance point.
  for(i in 1:(length(S1Points$name))){
    S1Points$DistanceAdj[i]<-S1Points$DistanceRaw[i]+S1HorizontalAdjust
  }
  
  #Creates dataframe of bankfull shots mapped during the survey.  
  S1BankfullCalls<-data.frame(matrix(nrow=2,ncol=3))
  names(S1BankfullCalls)=c("name","DistanceAdj","H")
  S1BankfullCalls[1,1]<-S1Points$name[grepl("LBF",S1Points$name)]
  S1BankfullCalls[1,2]<-S1Points$DistanceAdj[grepl("LBF",S1Points$name)]
  S1BankfullCalls[1,3]<-S1Points$H[grepl("LBF",S1Points$name)]
  S1BankfullCalls[2,1]<-S1Points$name[grepl("RBF",S1Points$name)]
  S1BankfullCalls[2,2]<-S1Points$DistanceAdj[grepl("RBF",S1Points$name)]
  S1BankfullCalls[2,3]<-S1Points$H[grepl("RBF",S1Points$name)]

#If the bankfull field calls are OK, calculates the bankfull width.
if(SurveyedS1==TRUE){
  S1Bankfull<-abs((S1Points$DistanceAdj[grepl("RBF",S1Points$name)])-(S1Points$DistanceAdj[grepl("LBF",S1Points$name)]))
} else {
  S1Bankfull<-"NA"
}

  #Plot the cross-section profile, highlights the bankfull shots for QA.  
  plot_ly(data=S1Points,x=~DistanceAdj, y=~H, name='Distance vs Elevation', type='scatter', mode='lines+markers', text=~name)%>%
    add_trace(data=S1BankfullCalls,x=~DistanceAdj,y=~H,name='Bankfull Field Calls',type='scatter',mode='markers',text=~name,marker=list(size=10,line=list(color='black',width=.5)))%>%
    layout(title = paste(siteID, "S1Points",surveyDate), xaxis=xAxisTitle1, yaxis=yAxisTitle1)
  
  
} else {"There is no data associated with the 'Transect_S1' mapCode"}

############# S2 transect  ##################################################################################################

if(SurveyedS2){
  
  S2northEastStart<-39 #Manually select the row of the mapped point that is furthest along the left bank side of the channel
  S2reference<-3 #Manually select the row of the mapped point taken on a pin [preferably LB pin but RB works too]; if a pin shot is not available choose the mapped point furthest into the left bank side of the channel
  
  S2NorthStart<-S2Points$N[S2northEastStart]
  S2EastStart<-S2Points$E[S2northEastStart]
  
  #Creates distance values for each point relative to the pin. 
  for(i in 1:(length(S2Points$name))){
    S2PointN<-S2Points$N[i]
    S2PointE<-S2Points$E[i]
    S2Points$DistanceRaw[i]<-sqrt(((S2PointN-S2NorthStart)^2)+((S2PointE-S2EastStart)^2))
  }
  
  S2ReferenceDistance<-S2Points$DistanceRaw[S2reference]
  
  #Sets Horizontal adjustment value based on reference point coordinate.  
  S2HorizontalAdjust<-0-S2ReferenceDistance
  
  #Transforms raw distance to adjusted distance based on reference distance point.
  for(i in 1:(length(S2Points$name))){
    S2Points$DistanceAdj[i]<-S2Points$DistanceRaw[i]+S2HorizontalAdjust
  }
  
  #Creates dataframe of bankfull shots mapped during the survey.  
  S2BankfullCalls<-data.frame(matrix(nrow=2,ncol=3))
  names(S2BankfullCalls)=c("name","DistanceAdj","H")
  S2BankfullCalls[1,1]<-S2Points$name[grepl("LBF",S2Points$name)]
  S2BankfullCalls[1,2]<-S2Points$DistanceAdj[grepl("LBF",S2Points$name)]
  S2BankfullCalls[1,3]<-S2Points$H[grepl("LBF",S2Points$name)]
  S2BankfullCalls[2,1]<-S2Points$name[grepl("RBF",S2Points$name)]
  S2BankfullCalls[2,2]<-S2Points$DistanceAdj[grepl("RBF",S2Points$name)]
  S2BankfullCalls[2,3]<-S2Points$H[grepl("RBF",S2Points$name)]
  
#If the bankfull field calls are OK, calculates the bankfull width.
if(SurveyedS2==TRUE){
  S2Bankfull<-abs((S2Points$DistanceAdj[grepl("RBF",S2Points$name)])-(S2Points$DistanceAdj[grepl("LBF",S2Points$name)]))
} else {
  S2Bankfull<-"NA"
}

  #Plot the cross-section profile, highlights the bankfull shots for QA.  
  plot_ly(data=S2Points,x=~DistanceAdj, y=~H, name='Distance vs Elevation', type='scatter', mode='lines+markers', text=~name)%>%
    add_trace(data=S2BankfullCalls,x=~DistanceAdj,y=~H,name='Bankfull Field Calls',type='scatter',mode='markers',text=~name,marker=list(size=10,line=list(color='black',width=.5)))%>%
    layout(title = paste(siteID, "S2Points",surveyDate), xaxis=xAxisTitle1, yaxis=yAxisTitle1)
  
} else {"There is no data associated with the 'Transect_S2' mapCode"}

############# USR transect  ##################################################################################################

if(SurveyedUSR){
  
  USRnorthEastStart<-35 #Manually select the row of the mapped point that is furthest along the left bank side of the channel
  USRreference<-33 #Manually select the row of the mapped point taken on a pin [preferably LB pin but RB works too]; if a pin shot is not available choose the mapped point furthest into the left bank side of the channel
  
  USRNorthStart<-USRPoints$N[USRnorthEastStart]
  USREastStart<-USRPoints$E[USRnorthEastStart]
  
  #Creates distance values for each point relative to the pin. 
  for(i in 1:(length(USRPoints$name))){
    USRPointN<-USRPoints$N[i]
    USRPointE<-USRPoints$E[i]
    USRPoints$DistanceRaw[i]<-sqrt(((USRPointN-USRNorthStart)^2)+((USRPointE-USREastStart)^2))
  }
  
  USRReferenceDistance<-USRPoints$DistanceRaw[USRreference]
  
  #Sets Horizontal adjustment value based on reference point coordinate.  
  USRHorizontalAdjust<-0-USRReferenceDistance
  
  #Transforms raw distance to adjusted distance based on reference distance point.
  for(i in 1:(length(USRPoints$name))){
    USRPoints$DistanceAdj[i]<-USRPoints$DistanceRaw[i]+USRHorizontalAdjust
  }
  
  #Creates dataframe of bankfull shots mapped during the survey.  
  USRBankfullCalls<-data.frame(matrix(nrow=2,ncol=3))
  names(USRBankfullCalls)=c("name","DistanceAdj","H")
  USRBankfullCalls[1,1]<-USRPoints$name[grepl("LBF",USRPoints$name)]
  USRBankfullCalls[1,2]<-USRPoints$DistanceAdj[grepl("LBF",USRPoints$name)]
  USRBankfullCalls[1,3]<-USRPoints$H[grepl("LBF",USRPoints$name)]
  USRBankfullCalls[2,1]<-USRPoints$name[grepl("RBF",USRPoints$name)]
  USRBankfullCalls[2,2]<-USRPoints$DistanceAdj[grepl("RBF",USRPoints$name)]
  USRBankfullCalls[2,3]<-USRPoints$H[grepl("RBF",USRPoints$name)]
  
#If the bankfull field calls are OK, calculates the bankfull width.
if(SurveyedUSR==TRUE){
  USRBankfull<-abs((USRPoints$DistanceAdj[grepl("RBF",USRPoints$name)])-(USRPoints$DistanceAdj[grepl("LBF",USRPoints$name)]))
} else {
  USRBankfull<-"NA"
}

  #Plot the cross-section profile, highlights the bankfull shots for QA.  
  plot_ly(data=USRPoints,x=~DistanceAdj, y=~H, name='Distance vs Elevation', type='scatter', mode='lines+markers', text=~name)%>%
    add_trace(data=USRBankfullCalls,x=~DistanceAdj,y=~H,name='Bankfull Field Calls',type='scatter',mode='markers',text=~name,marker=list(size=10,line=list(color='black',width=.5)))%>%
    layout(title = paste(siteID, "USRPoints",surveyDate), xaxis=xAxisTitle1, yaxis=yAxisTitle1)
  
} else {"There is no data associated with the 'Transect_USR' mapCode"}

############# DSR transect  ##################################################################################################

if(SurveyedDSR){
  
  DSRnorthEastStart<-38 #Manually select the row of the mapped point that is furthest along the left bank side of the channel
  DSRreference<-34 #Manually select the row of the mapped point taken on a pin [preferably LB pin but RB works too]; if a pin shot is not available choose the mapped point furthest into the left bank side of the channel
  
  DSRNorthStart<-DSRPoints$N[DSRnorthEastStart]
  DSREastStart<-DSRPoints$E[DSRnorthEastStart]
  
  #Creates distance values for each point relative to the pin. 
  for(i in 1:(length(DSRPoints$name))){
    DSRPointN<-DSRPoints$N[i]
    DSRPointE<-DSRPoints$E[i]
    DSRPoints$DistanceRaw[i]<-sqrt(((DSRPointN-DSRNorthStart)^2)+((DSRPointE-DSREastStart)^2))
  }
  
 DSRReferenceDistance<-DSRPoints$DistanceRaw[DSRreference]
  
  #Sets Horizontal adjustment value based on reference point coordinate.  
  DSRHorizontalAdjust<-0-DSRReferenceDistance
  
  #Transforms raw distance to adjusted distance based on reference distance point.
  for(i in 1:(length(DSRPoints$name))){
    DSRPoints$DistanceAdj[i]<-DSRPoints$DistanceRaw[i]+DSRHorizontalAdjust
  }
  
  #Creates dataframe of bankfull shots mapped during the survey.  
  DSRBankfullCalls<-data.frame(matrix(nrow=2,ncol=3))
  names(DSRBankfullCalls)=c("name","DistanceAdj","H")
  DSRBankfullCalls[1,1]<-DSRPoints$name[grepl("LBF",DSRPoints$name)]
  DSRBankfullCalls[1,2]<-DSRPoints$DistanceAdj[grepl("LBF",DSRPoints$name)]
  DSRBankfullCalls[1,3]<-DSRPoints$H[grepl("LBF",DSRPoints$name)]
  DSRBankfullCalls[2,1]<-DSRPoints$name[grepl("RBF",DSRPoints$name)]
  DSRBankfullCalls[2,2]<-DSRPoints$DistanceAdj[grepl("RBF",DSRPoints$name)]
  DSRBankfullCalls[2,3]<-DSRPoints$H[grepl("RBF",DSRPoints$name)]
  
#If the bankfull field calls are OK, calculates the bankfull width.  Had to edit for LBF typo.
if(SurveyedDSR==TRUE){
  DSRBankfull<-abs((DSRPoints$DistanceAdj[grepl("RBF",DSRPoints$name)])-(DSRPoints$DistanceAdj[grepl("LBF",DSRPoints$name)]))
} else {
  DSRBankfull<-"NA"
}

  #Plot the cross-section profile, highlights the bankfull shots for QA.  
  plot_ly(data=DSRPoints,x=~DistanceAdj, y=~H, name='Distance vs Elevation', type='scatter', mode='lines+markers', text=~name)%>%
    add_trace(data=DSRBankfullCalls,x=~DistanceAdj,y=~H,name='Bankfull Field Calls',type='scatter',mode='markers',text=~name,marker=list(size=10,line=list(color='black',width=.5)))%>%
    layout(title = paste(siteID, "DSRPoints",surveyDate), xaxis=xAxisTitle1, yaxis=yAxisTitle1)
  
} else {"There is no data associated with the 'Transect_DSR' mapCode"}

############# Create BF width and datOut dataframes  ##################################################################################################

transectBankfullWidths<-data.frame(matrix(nrow=1,ncol=16))
names(transectBankfullWidths)=c("Transect_AP1","Transect_AP2","Transect_AP3","Transect_AP4","Transect_AP5","Transect_AP6","Transect_AP7","Transect_AP8","Transect_AP9","Transect_AP10","Transect_USR","Transect_DSR","Transect_S1","Transect_S2","Transect_DSC","Transect_DSC2")

if(SurveyedAP1){
  transectBankfullWidths$Transect_AP1<-as.numeric(AP1Bankfull)
}else{transectBankfullWidths$Transect_AP1<-NA}

if(SurveyedAP2){
  transectBankfullWidths$Transect_AP2<-as.numeric(AP2Bankfull)
}else{transectBankfullWidths$Transect_AP2<-NA}

if(SurveyedAP3){
  transectBankfullWidths$Transect_AP3<-as.numeric(AP3Bankfull)
}else{transectBankfullWidths$Transect_AP3<-NA}

if(SurveyedAP4){
  transectBankfullWidths$Transect_AP4<-as.numeric(AP4Bankfull)
}else{transectBankfullWidths$Transect_AP4<-NA}

if(SurveyedAP5){
  transectBankfullWidths$Transect_AP5<-as.numeric(AP5Bankfull)
}else{transectBankfullWidths$Transect_AP5<-NA}

if(SurveyedAP6){
  transectBankfullWidths$Transect_AP6<-as.numeric(AP6Bankfull)
}else{transectBankfullWidths$Transect_AP6<-NA}

if(SurveyedAP7){
  transectBankfullWidths$Transect_AP7<-as.numeric(AP7Bankfull)
}else{transectBankfullWidths$Transect_AP7<-NA}

if(SurveyedAP8){
  transectBankfullWidths$Transect_AP8<-as.numeric(AP8Bankfull)
}else{transectBankfullWidths$Transect_AP8<-NA}

if(SurveyedAP9){
  transectBankfullWidths$Transect_AP9<-as.numeric(AP9Bankfull)
}else{transectBankfullWidths$Transect_AP9<-NA}

if(SurveyedAP10){
  transectBankfullWidths$Transect_AP10<-as.numeric(AP10Bankfull)
}else{transectBankfullWidths$Transect_AP10<-NA}

if(SurveyedUSR){
  transectBankfullWidths$Transect_USR<-as.numeric(USRBankfull)
}else{transectBankfullWidths$Transect_USR<-NA}

if(SurveyedDSR){
  transectBankfullWidths$Transect_DSR<-as.numeric(DSRBankfull)
}else{transectBankfullWidths$Transect_DSR<-NA}

if(SurveyedS1){
  transectBankfullWidths$Transect_S1<-as.numeric(S1Bankfull)
}else{transectBankfullWidths$Transect_S1<-NA}

if(SurveyedS2){
  transectBankfullWidths$Transect_S2<-as.numeric(S2Bankfull)
}else{transectBankfullWidths$Transect_S2<-NA}

if(SurveyedDSC1){
  transectBankfullWidths$Transect_DSC<-as.numeric(DSCXS1Bankfull)
}else{transectBankfullWidths$Transect_DSC<-NA}

if(SurveyedDSC2){
  transectBankfullWidths$Transect_DSC2<-as.numeric(DSCXS2Bankfull)
}else{transectBankfullWidths$Transect_DSC2<-NA}

############################# Read and write to temporary tables  ############################################################################################################################

#Reads in geoSummaryTable and geo_transectBankfullWidths from N drive. 
geoSummaryTable <- read.csv(paste(surveyFilePath,'Ingest_Tables/',domainID,'_',siteID,'_geo_surveySummary_',surveyDate,'.csv',sep=""))
geoTransectBankfullWidths<-read.csv(paste(surveyFilePath,'Ingest_Tables/',domainID,'_',siteID,'_geo_transectBankfullWidths_',surveyDate,'.csv',sep=""))
      
#extract endDate from GEO summary table
surveyEndDate<-geoSummaryTable$endDate

print(surveyEndDate)
print(surveyDate)

#removes NA's from transectBankfullWidths DF
transectBankfullWidths<- transectBankfullWidths[ , colSums(is.na(transectBankfullWidths))==0]

#Averages all bankfull width values.
meanBankfullWidth<-round(sum(transectBankfullWidths[1,])/ncol(transectBankfullWidths),3)
print(meanBankfullWidth)

#Adds BF width value to geoSummaryTable and writes the DF back to the N Drive.
geoSummaryTable$meanBankfullWidth<-meanBankfullWidth
write.csv(geoSummaryTable,paste(surveyFilePath,"Ingest_Tables/",domainID,"_",siteID,"_geo_surveySummary_",surveyDate,".csv",sep=""),row.names=F)

#adds data to geoTransectBankfullWidths DF.
i<-ncol(transectBankfullWidths)
geoTransectBankfullWidths[i, ]<-NA

geoTransectBankfullWidths$locationID<-siteID
geoTransectBankfullWidths$startDate<-geoSummaryTable$startDate
geoTransectBankfullWidths$endDate<-geoSummaryTable$endDate
geoTransectBankfullWidths$eventID<-geoSummaryTable$eventID  
geoTransectBankfullWidths$transectID<-colnames(transectBankfullWidths)
geoTransectBankfullWidths$bankfullWidth<-as.numeric(c(transectBankfullWidths[1,]))
geoTransectBankfullWidths$bankfullWidth<-round(geoTransectBankfullWidths$bankfullWidth,3)
geoTransectBankfullWidths$processedSurveyVersion<-geoTransectBankfullWidths$processedSurveyVersion[1]

print(nrow(geoTransectBankfullWidths)) #ensure that there are 8 bankfull widths

#Checks that there are 8 BF width in the dataOut dataframe. 
if(length(which(!is.na(geoTransectBankfullWidths$bankfullWidth)))==8){
  print("Success!  There are 8 bankfull width values contained in the dataOut dataframe!")  
} else {
  warning("There are NOT 8 bankfull width values contained in the dataOut dataframe!")
}

#Plots bankfull width data per transect as a final check.  
plot_ly(data=geoTransectBankfullWidths,x=~transectID, y=~bankfullWidth, name='Bankfull Widths', type='bar', text=~transectID,marker=(list(color='orange',line=(list(color='black',width=1)))))%>%
  layout(title = paste(siteID, "Bankfull Widths",surveyDate))

#writes out geoTransectBankfullWidths DF to N drive.
write.csv(geoTransectBankfullWidths,paste(surveyFilePath,"Ingest_Tables/",domainID,"_",siteID,"_geo_transectBankfullWidths_",surveyDate,".csv",sep=""),row.names=F)

#Save the global environment as an .rda file.
save.image(file = paste("N:/Science/AQU/Geomorphology_Survey_Data/",domainID,"/",domainID,"_",siteID,"_",surveyDate,"/",siteID,"_",surveyDate,"_geo_transectBankfullWidths_in.RData",sep=""))



















############################# Create dataOut dataframe ############################################################################################################################

#Creates an output dataframe that lists bankfull widths for all transects.
dataOut<-data.frame(matrix(nrow=sum(!is.na(transectBankfullWidths[1,])),ncol=9))

names(dataOut)=c("uid", "locationID", "startDate", "endDate", "eventID", "transectID", "bankfullWidth", "processedSurveyVersion", "dataQF")

dataOut$uid<-"" #leave blank
dataOut$locationID<-siteID
dataOut$startDate<-surveyEndDate
dataOut$endDate<-surveyEndDate
dataOut$eventID<-eventID
dataOut$transectID<-colnames(transectBankfullWidths) 
dataOut$bankfullWidth<-round(as.numeric(paste(transectBankfullWidths[1,])),3)
dataOut$processedSurveyVersion<-"vG" #current post-processing version is vG 
dataOut$dataQF<-"" #leave blank

#Checks that there are 8 BF width in the dataOut dataframe. 
if(length(which(!is.na(dataOut$bankfullWidth)))==8){
  print("Success!  There are 8 bankfull width values contained in the dataOut dataframe!")  
} else {
  warning("There are NOT 8 bankfull width values contained in the dataOut dataframe!")
}

#Plots bankfull width data per transect as a final check.  
plot_ly(data=dataOut,x=~transectID, y=~bankfullWidth, name='Bankfull Widths', type='bar', text=~transectID,marker=(list(color='orange',line=(list(color='black',width=1)))))%>%
  layout(title = paste(siteID, "Bankfull Widths",surveyDate))

#Append dataOut df to newSurveyTransectBankfullWidths table 
newSurveyTransectBankfullWidths<-rbind(newSurveyTransectBankfullWidths,dataOut)

#write newSurveyTransectBankfullWidths back out (temporary location)
write.csv(newSurveyTransectBankfullWidths,'N:/Science/AQU/Geomorphology_Survey_Data/newIngestOutputs/Geo_tablesToIngest/newSurveyTransectBankfullWidths.csv',row.names=F)

#Writes out dataOut dataframe to working directory.  
write.csv(dataOut,paste(wdir,"/",domainID,"_",siteID,"_geo_transectBankfullWidths_in_",surveyDate,".csv",sep=""),row.names=F)

#Save the global environment as an .rda file.
save.image(file = paste("N:/Science/AQU/Geomorphology_Survey_Data/",domainID,"/",domainID,"_",siteID,"_",surveyDate,"/",siteID,"_",surveyDate,"_geo_transectBankfullWidths_in.RData",sep=""))

# #Writes out dataOut dataframe to ECS L4 directory for data upload.  
# ECSdir<-paste('N:/Science/AQU/Geomorphology_Survey_Data/',domainID,"/",domainID,"_",siteID,"_",surveyDate,"/ECS/L4",sep="")
# write.csv(dataOut,paste(ECSdir,"/",domainID,"_",siteID,"_geo_transectBankfullWidths_in_",surveyDate,".csv",sep=""),row.names=F)


