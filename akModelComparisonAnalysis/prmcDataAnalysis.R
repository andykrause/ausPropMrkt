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
  
  # File containing function for analyzing data
  source(paste0('https://raw.githubusercontent.com/andykrause/',
                'dataAnalysisTools/master/stShardFunctions.R'))

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
    load(paste0(dataPath, 'cleanData.RData'))
  }
  
 ## Load in Geographical Data
  
  subShp <- readShapePoly(paste0(dataPath, subGeoFile))
  lgaShp <- readShapePoly(paste0(dataPath, lgaGeoFile))
  sla1Shp <- readShapePoly(paste0(dataPath, sla1GeoFile))
  postCodeShp <- readShapePoly(paste0(dataPath, postGeoFile))

### Median method approach -----------------------------------------------------
  
 ## Metro Analysis

  # Metro 
  mmMetYields <- aryStsGeoWrap(stsData = allTrans,
                             metric=c('transValue', 'transValue'),
                             spaceField='all', timeField='transQtr',
                             defDim='time', stsLimit=3, 
                             calcs=list(median='median'))
  
  # Metro by Use
  mmMetYieldsH <- aryStsGeoWrap(allTrans[allTrans$PropertyType == 'House',],
                               metric=c('transValue', 'transValue'),
                               spaceField='all', timeField='transQtr',
                               defDim='time', stsLimit=3, 
                               calcs=list(median='median'))
  
  mmMetYieldsU <- aryStsGeoWrap(allTrans[allTrans$PropertyType == 'Unit',],
                               metric=c('transValue', 'transValue'),
                               spaceField='all', timeField='transQtr',
                               defDim='time', stsLimit=3, 
                               calcs=list(median='median'))
  
  
 ## At LGA Level
  
  # All Uses
  mmLgaYields <- aryStsGeoWrap(stsData = allTrans,
                               metric=c('transValue', 'transValue'),
                               spaceField='lga', timeField='transQtr',
                               defDim='time', stsLimit=3, 
                               calcs=list(median='median'))
  
  # By Use
  mmLgaYieldsH <- aryStsGeoWrap(allTrans[allTrans$PropertyType == 'House', ],
                               metric=c('transValue', 'transValue'),
                               spaceField='lga', timeField='transQtr',
                               defDim='time', stsLimit=3, 
                               calcs=list(median='median'))
  
  mmLgaYieldsU <- aryStsGeoWrap(allTrans[allTrans$PropertyType == 'Unit', ],
                                metric=c('transValue', 'transValue'),
                                spaceField='lga', timeField='transQtr',
                                defDim='time', stsLimit=3, 
                                calcs=list(median='median'))
  
 ## At SLA1 Level
  
  # All Uses  
  mmSlaYields <- aryStsGeoWrap(stsData = allTrans,
                               metric=c('transValue', 'transValue'),
                               spaceField='sla1', timeField='transQtr',
                               defDim='time', stsLimit=3, 
                               calcs=list(median='median'))
  
  # By Use
  mmSlaYieldsH <- aryStsGeoWrap(allTrans[allTrans$PropertyType == 'House', ],
                                metric=c('transValue', 'transValue'),
                                spaceField='sla1', timeField='transQtr',
                                defDim='time', stsLimit=3, 
                                calcs=list(median='median'))
  
  mmSlaYieldsU <- aryStsGeoWrap(allTrans[allTrans$PropertyType == 'Unit', ],
                                metric=c('transValue', 'transValue'),
                                spaceField='sla1', timeField='transQtr',
                                defDim='time', stsLimit=3, 
                                calcs=list(median='median'))
  
 ## At Suburb Level
  
  # All Uses  
  mmSuburbYields <- aryStsGeoWrap(stsData = allTrans,
                                  metric=c('transValue', 'transValue'),
                                  spaceField='suburb', timeField='transQtr',
                                  defDim='time', stsLimit=3, 
                                  calcs=list(median='median'))
  
  # By Use
  mmSuburbYieldsH <- aryStsGeoWrap(allTrans[allTrans$PropertyType == 'House', ],
                                   metric=c('transValue', 'transValue'),
                                   spaceField='suburb', timeField='transQtr',
                                   defDim='time', stsLimit=3, 
                                   calcs=list(median='median'))

  mmSuburbYieldsU <- aryStsGeoWrap(allTrans[allTrans$PropertyType == 'Unit', ],
                                   metric=c('transValue', 'transValue'),
                                   spaceField='suburb', timeField='transQtr',
                                   defDim='time', stsLimit=3, 
                                   calcs=list(median='median'))
  
  ## At PostCode Level
  
  # All Uses   
  mmPCYields <- aryStsGeoWrap(stsData = allTrans,
                              metric=c('transValue', 'transValue'),
                              spaceField='postCode', timeField='transQtr',
                              defDim='time', stsLimit=3, 
                              calcs=list(median='median'))
  
  # by Use   
  mmPCYieldsM <- aryStsGeoWrap(allTrans[allTrans$PropertyType == 'House', ],
                               metric=c('transValue', 'transValue'),
                               spaceField='postCode', timeField='transQtr',
                               defDim='time', stsLimit=3, 
                               calcs=list(median='median'))
  # All Uses   
  mmPCYieldsU <- aryStsGeoWrap(allTrans[allTrans$PropertyType == 'Unit', ],
                               metric=c('transValue', 'transValue'),
                               spaceField='postCode', timeField='transQtr',
                               defDim='time', stsLimit=3, 
                               calcs=list(median='median'))
  
  
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
  crmValues$yield <- (crmValues$Rent * 52) / crmValues$Price

  # Add Ratio to full dataset 
  allTrans$yield <- crmValues$yield[match(allTrans$UID, crmValues$UID)]
  xTrans <- subset(allTrans, !is.na(yield)) 
       
### Output raw results ---------------------------------------------------------
  
  write.csv(xTrans, paste0(dataPath, 'rawResults.csv'), row.names=FALSE)
    
### Break down results by dimensions -------------------------------------------
  
 ## Metro
  
  # Metro 
  crMetYields <- spaceTimeShard(stsData = xTrans,
                               metric=c('yield'),
                               spaceField='all', timeField='transQtr',
                               defDim='time', stsLimit=3, 
                               calcs=list(median='median'))
  
  # By Use
  crMetYieldsH <- spaceTimeShard(xTrans[xTrans$PropertyType == 'House', ],
                                metric=c('yield'),
                                spaceField='all', timeField='transQtr',
                                defDim='time', stsLimit=3, 
                                calcs=list(median='median'))

  crMetYieldsU <- spaceTimeShard(xTrans[xTrans$PropertyType == 'Unit', ],
                                metric=c('yield'),
                                spaceField='all', timeField='transQtr',
                                defDim='time', stsLimit=3, 
                                calcs=list(median='median'))
 ## LGA
  
  crLgaYields <- spaceTimeShard(stsData = xTrans,
                                metric=c('yield'),
                                spaceField='lga', timeField='transQtr',
                                defDim='time', stsLimit=3, 
                                calcs=list(median='median'))
  
  # By Use
  crLgaYieldsH <- spaceTimeShard(xTrans[xTrans$PropertyType == 'House', ],
                                 metric=c('yield'),
                                 spaceField='lga', timeField='transQtr',
                                 defDim='time', stsLimit=3, 
                                 calcs=list(median='median'))
  
  crLgaYieldsU <- spaceTimeShard(xTrans[xTrans$PropertyType == 'Unit', ],
                                 metric=c('yield'),
                                 spaceField='lga', timeField='transQtr',
                                 defDim='time', stsLimit=3, 
                                 calcs=list(median='median')) 
  
  ## SLA1
  
  crSlaYields <- spaceTimeShard(stsData = xTrans,
                                metric=c('yield'),
                                spaceField='sla1', timeField='transQtr',
                                defDim='time', stsLimit=3, 
                                calcs=list(median='median'))
  
  # By Use
  crSlaYieldsH <- spaceTimeShard(xTrans[xTrans$PropertyType == 'House', ],
                                 metric=c('yield'),
                                 spaceField='sla1', timeField='transQtr',
                                 defDim='time', stsLimit=3, 
                                 calcs=list(median='median'))
  
  crSlaYieldsU <- spaceTimeShard(xTrans[xTrans$PropertyType == 'Unit', ],
                                 metric=c('yield'),
                                 spaceField='sla1', timeField='transQtr',
                                 defDim='time', stsLimit=3, 
                                 calcs=list(median='median'))

  ## Suburb
  
  crSuburbYields <- spaceTimeShard(stsData = xTrans,
                                metric=c('yield'),
                                spaceField='suburb', timeField='transQtr',
                                defDim='time', stsLimit=3, 
                                calcs=list(median='median'))
  
  # By Use
  crSuburbYieldsH <- spaceTimeShard(xTrans[xTrans$PropertyType == 'House', ],
                                 metric=c('yield'),
                                 spaceField='suburb', timeField='transQtr',
                                 defDim='time', stsLimit=3, 
                                 calcs=list(median='median'))
  
  crSuburbYieldsU <- spaceTimeShard(xTrans[xTrans$PropertyType == 'Unit', ],
                                 metric=c('yield'),
                                 spaceField='suburb', timeField='transQtr',
                                 defDim='time', stsLimit=3, 
                                 calcs=list(median='median'))
  
  ## post code
  
  crPCYields <- spaceTimeShard(stsData = xTrans,
                                metric=c('yield'),
                                spaceField='postCode', timeField='transQtr',
                                defDim='time', stsLimit=3, 
                                calcs=list(median='median'))
  
  # By Use
  crPCYieldsH <- spaceTimeShard(xTrans[xTrans$PropertyType == 'House', ],
                                 metric=c('yield'),
                                 spaceField='postCode', timeField='transQtr',
                                 defDim='time', stsLimit=3, 
                                 calcs=list(median='median'))
  
  crPCYieldsU <- spaceTimeShard(xTrans[xTrans$PropertyType == 'Unit', ],
                                 metric=c('yield'),
                                 spaceField='postCode', timeField='transQtr',
                                 defDim='time', stsLimit=3, 
                                 calcs=list(median='median'))
  
### Direct Match ---------------------------------------------------------------
  
 ## Create data
  
  dmData <- arySaleRentMatch(sales=allTrans[allTrans$transType=='sale',], 
                             rentals=allTrans[allTrans$transType=='rent',],
                             matchField='AddressID', saleField='transValue',
                             rentField='transValue', timeField='transQtr')
  
  
 ## All Metro  
  dmMetYields <- spaceTimeShard(stsData = dmData,
                                metric=c('saleYield'),
                                spaceField='all', timeField='saleTime',
                                defDim='time', stsLimit=3, 
                                calcs=list(median='median'))
  
  # By Use   
  dmMetYieldsH <- spaceTimeShard(dmData[dmData$PropertyType == 'House',],
                                metric=c('saleYield'),
                                spaceField='all', timeField='saleTime',
                                defDim='time', stsLimit=3, 
                                calcs=list(median='median'))
  
  dmMetYieldsU <- spaceTimeShard(dmData[dmData$PropertyType == 'Unit',],
                                metric=c('saleYield'),
                                spaceField='all', timeField='saleTime',
                                defDim='time', stsLimit=3, 
                                calcs=list(median='median'))
  
 ## LGA  
  
  dmLgaYields <- spaceTimeShard(stsData = dmData,
                                metric=c('saleYield'),
                                spaceField='lga', timeField='saleTime',
                                defDim='time', stsLimit=3, 
                                calcs=list(median='median'))
  
  # By Use   
  dmLgaYieldsH <- spaceTimeShard(dmData[dmData$PropertyType == 'House',],
                                 metric=c('saleYield'),
                                 spaceField='lga', timeField='saleTime',
                                 defDim='time', stsLimit=3, 
                                 calcs=list(median='median'))
  
  dmLgaYieldsU <- spaceTimeShard(dmData[dmData$PropertyType == 'Unit',],
                                 metric=c('saleYield'),
                                 spaceField='lga', timeField='saleTime',
                                 defDim='time', stsLimit=3, 
                                 calcs=list(median='median'))
  
  ## SLA1 
  
  dmSlaYields <- spaceTimeShard(stsData = dmData,
                                metric=c('saleYield'),
                                spaceField='sla1', timeField='saleTime',
                                defDim='time', stsLimit=3, 
                                calcs=list(median='median'))
  
  # By Use   
  dmSlaYieldsH <- spaceTimeShard(dmData[dmData$PropertyType == 'House',],
                                 metric=c('saleYield'),
                                 spaceField='sla1', timeField='saleTime',
                                 defDim='time', stsLimit=3, 
                                 calcs=list(median='median'))
  
  dmSlaYieldsU <- spaceTimeShard(dmData[dmData$PropertyType == 'Unit',],
                                 metric=c('saleYield'),
                                 spaceField='sla1', timeField='saleTime',
                                 defDim='time', stsLimit=3, 
                                 calcs=list(median='median')) 

  ## Suburb
  
  dmSubYields <- spaceTimeShard(stsData = dmData,
                                metric=c('saleYield'),
                                spaceField='suburb', timeField='saleTime',
                                defDim='time', stsLimit=3, 
                                calcs=list(median='median'))
  
  # By Use   
  dmSubYieldsH <- spaceTimeShard(dmData[dmData$PropertyType == 'House',],
                                 metric=c('saleYield'),
                                 spaceField='suburb', timeField='saleTime',
                                 defDim='time', stsLimit=3, 
                                 calcs=list(median='median'))
  
  dmSubYieldsU <- spaceTimeShard(dmData[dmData$PropertyType == 'Unit',],
                                 metric=c('saleYield'),
                                 spaceField='suburb', timeField='saleTime',
                                 defDim='time', stsLimit=3, 
                                 calcs=list(median='median')) 
  
  ## PostCode 
  
  dmPCYields <- spaceTimeShard(stsData = dmData,
                                metric=c('saleYield'),
                                spaceField='postCode', timeField='saleTime',
                                defDim='time', stsLimit=3, 
                                calcs=list(median='median'))
  
  # By Use   
  dmPCYieldsH <- spaceTimeShard(dmData[dmData$PropertyType == 'House',],
                                 metric=c('saleYield'),
                                 spaceField='postCode', timeField='saleTime',
                                 defDim='time', stsLimit=3, 
                                 calcs=list(median='median'))
  
  dmPCYieldsU <- spaceTimeShard(dmData[dmData$PropertyType == 'Unit',],
                                 metric=c('saleYield'),
                                 spaceField='postCode', timeField='saleTime',
                                 defDim='time', stsLimit=3, 
                                 calcs=list(median='median')) 
  
### Clean up workspace and save ------------------------------------------------
  
  rm(crmValues)
    
  save.image(paste0(dataPath, 'analysisResults.RData'))

  
  
  
  
  
  