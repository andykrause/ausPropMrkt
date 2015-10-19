################################################################################
#                                                                              #
#  Data Analysis Code for the Price to Rent Ratio Model comparison Study       #
#                                                                              #
################################################################################

### Preliminary Commands -------------------------------------------------------

  ## Set parameters

  reBuildData <- FALSE     # Enter TRUE if you wish to rebuild data from raw

 ## Load Libraries

  library(plyr)
  library(dplyr)
  library(ggplot2)
  library(reshape2)
  library(stringr)
  library(maptools)
  library(sp)
  library(rgeos)

 ## Source Files

  # File containing function for working with prr and APM data
  source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                'master/prrFunctions.R'))

 ## Set the path to the data

  dataPath <- "C:/Dropbox/Australia Data/ausPropData/melData/"
  subGeoFile <- 'Vic_Suburbs.shp'
  lgaGeoFile <- 'Vic_LGAs.shp'
  sla1GeoFile <- 'Vic_SLA1.shp'
  postGeoFile <- 'Vic_PostCodes.shp'

### Read in and prepare data ---------------------------------------------------------------
  
 ## Read in transaction Data  
  
  # Conditional depending on reBuild
  if(reBuildData){
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/akModelComparisonAnalysis/prmcDataPrep.R'))
  } else {
    load(paste0(dataPath, 'prrWrkspc.RData'))
  }
  
 ## Load in Geographical Data
  
  subShp <- readShapePoly(paste0(dataPath, subGeoFile))
  lgaShp <- readShapePoly(paste0(dataPath, lgaGeoFile))
  sla1Shp <- readShapePoly(paste0(dataPath, sla1GeoFile))
  postCodeShp <- readShapePoly(paste0(dataPath, postGeoFile))

### Median method approach
  
 ## Global Analysis
  
  mmGlobY <- prrMedMethod(allTrans)
  mmGlobQ <- prrMedMethod(allTrans, 'transQtr')
  
  mmGlobBUY <- prrMedMethod(allTrans, 'transYear', TRUE)
  
  

### Cross regression comparison method -----------------------------------------

 ## Set the specification (formula)

  regSpec <- log(transValue) ~ as.factor(postCode) + as.factor(transQtr) + 
                log(AreaSize) +Bedrooms + Baths + ssInteg + ssChoice

 ## Estimate models and make new predictions: Global by Use

  # For houses
  houseResults <- prrCrossReg(regSpec, 
                            subset(allTrans, transType == 'sale' &
                                             PropertyType == 'House' & 
                                             YT_house_postCode == 1),
                            subset(allTrans, transType == 'rent' &
                                             PropertyType == 'House' & 
                                             YT_house_postCode == 1),
                            verbose=TRUE)
  
  # For Units
  unitResults <- prrCrossReg(regSpec, 
                           subset(allTrans, transType == 'sale' &
                                    PropertyType == 'Unit' & 
                                    YT_unit_postCode == 1),
                           subset(allTrans, transType == 'rent' &
                                    PropertyType == 'Unit' & 
                                    YT_unit_postCode == 1),
                           verbose=TRUE)

 ## Calculate the ratio

  # Extract vales
  crmValues <- rbind(houseResults$results, unitResults$results)

  # Calculate the ratio
  crmValues$prRatio <- crmValues$Price / (crmValues$Rent * 52 / 12)

  # Add Ratio to full dataset 
  allTrans$prRatio <- crmValues$prRatio[match(allTrans$UID, crmValues$UID)]
  xTrans <- subset(allTrans, !is.na(prRatio)) 
       
### Output raw results ---------------------------------------------------------
  
  write.csv(xTrans, paste0(dataPath, 'rawResults.csv'), row.names=FALSE)
    
### Break down results by dimensions -------------------------------------------
  
 ## Global
  
  # Basic
  glob <- prrTrender(list('transYear', 'transQtr', 'transMonth'), xData=xTrans)
  globY <- glob$transYear
  globQ <- glob$transQtr
  
  # By Use
  globBU <- prrTrender(list('transYear', 'transQtr', 'transMonth'),
                       xData=xTrans, byUse=TRUE)
  globBUY <- globBU$transYear
  globBUQ <- globBU$transQtr
  
  # By use weighted
  globBUW <- prrTrender(list('transYear', 'transQtr', 'transMonth'),
                        xData=xTrans, byUse=TRUE, weighted=TRUE)
  globBUWY <- globBUW$transYear
  globBUWQ <- globBUW$transQtr
  
  
 ## All lgas 
  
  # Basic
  lgaY <- prrTrender('transYear', xData=xTrans, geog='lga', geogName='all')
  lgaQ <- prrTrender('transQtr', xData=xTrans, geog='lga', geogName='all')
  
  # By Use
  lgaYU <- prrTrender('transYear', xData=xTrans, geog='lga', 
                      geogName='all', byUse=TRUE)
  lgaQU <- prrTrender('transQtr', xData=xTrans, geog='lga', 
                      geogName='all', byUse=TRUE)
  
  # By use weighted
  lgaYUW <- prrTrender('transYear', xData=xTrans, geog='lga',
                       geogName='all', byUse=TRUE, weighted=TRUE)
  lgaQUW <- prrTrender('transQtr', xData=xTrans, geog='lga',
                       geogName='all', byUse=TRUE, weighted=TRUE)
  
 ## All SLA1s 
  
  # Basic
  slaY <- prrTrender('transYear', xData=xTrans, geog='sla1', geogName='all')
  slaQ <- prrTrender('transQtr', xData=xTrans, geog='sla1', geogName='all')
  
  # By Use
  slaYU <- prrTrender('transYear', xData=xTrans, geog='sla1', 
                      geogName='all', byUse=TRUE)
  slaQU <- prrTrender('transQtr', xData=xTrans, geog='sla1', 
                      geogName='all', byUse=TRUE)
  
  # By use weighted
  slaYUW <- prrTrender('transYear', xData=xTrans, geog='sla1',
                       geogName='all', byUse=TRUE, weighted=TRUE)
  slaQUW <- prrTrender('transQtr', xData=xTrans, geog='sla1',
                       geogName='all', byUse=TRUE, weighted=TRUE)
  
  ## All PostCodes 
  
  # Basic
  pcY <- prrTrender('transYear', xData=xTrans, geog='postCode', geogName='all')
  pcQ <- prrTrender('transQtr', xData=xTrans, geog='postCode', geogName='all')
  
  # By Use
  pcYU <- prrTrender('transYear', xData=xTrans, geog='postCode', 
                      geogName='all', byUse=TRUE)
  pcQU <- prrTrender('transQtr', xData=xTrans, geog='postCode', 
                      geogName='all', byUse=TRUE)
  
  # By use weighted
  pcYUW <- prrTrender('transYear', xData=xTrans, geog='postCode',
                       geogName='all', byUse=TRUE, weighted=TRUE)
  pcQUW <- prrTrender('transQtr', xData=xTrans, geog='postCode',
                       geogName='all', byUse=TRUE, weighted=TRUE)
  
  ## All Suburbs
  
  # Basic
  subY <- prrTrender('transYear', xData=xTrans, geog='suburb', geogName='all')
  subQ <- prrTrender('transQtr', xData=xTrans, geog='suburb', geogName='all')
  
  # By Use
  subYU <- prrTrender('transYear', xData=xTrans, geog='suburb', 
                     geogName='all', byUse=TRUE)
  subQU <- prrTrender('transQtr', xData=xTrans, geog='suburb', 
                     geogName='all', byUse=TRUE)
  
  # By use weighted
  subYUW <- prrTrender('transYear', xData=xTrans, geog='suburb',
                      geogName='all', byUse=TRUE, weighted=TRUE)
  subQUW <- prrTrender('transQtr', xData=xTrans, geog='suburb',
                      geogName='all', byUse=TRUE, weighted=TRUE)

### Save Workspace -------------------------------------------------------------
  
  save.image(paste0(dataPath, 'prrWrkspc.RData'))
  save(globY, globQ, globBUY, globBUQ, globBUWY, globBUWQ, 
      lgaY, lgaQ, lgaYU, lgaQU, lgaYUW, lgaQUW,
      slaY, slaQ, slaYU, slaQU, slaYUW, slaQUW,
      pcY, pcQ, pcYU, pcQU, pcYUW, pcQUW,
      subY, subQ, subYU, subQU, subYUW, subQUW,
      subShp, lgaShp, sla1Shp, postCodeShp, xTrans,
      file = paste0(dataPath, 'plotObjs.rData'))
  
### Visualize Results ----------------------------------------------------------  
  
  
  ggplot(prrObj, aes(x=time, y=value)) + geom_line()
  
  
  
  
  
  
  
  
  
  
  
  
  