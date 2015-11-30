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

  #dataPath <- "D:/Data/Research/priceRentMethComp/"
  dataPath <- "C:/Dropbox/Australia Data/ausPropData/melData/"
  
### Read in and prepare data ---------------------------------------------------------------
  
 ## Read in transaction Data  
  
  # Conditional depending on reBuild
  if(reBuildData){
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/akModelComparisonAnalysis/prmcDataPrep.R'))
  } else {
    load(paste0(dataPath, 'cleanData.RData'))
  }
  
################################################################################  
### Calculate ratios

### Median method approach -----------------------------------------------------
  
 ## Metro Analysis

  # Metro 
  mmMetro <- aryStsGeoWrap(stsData = allTrans,
                           metric=c('transValue', 'transValue'),
                           spaceField='all', timeField='transQtr',
                           defDim='time', stsLimit=3, 
                           calcs=list(median='median'))
  
  # Metro by Use
  mmMetroH <- aryStsGeoWrap(allTrans[allTrans$PropertyType == 'House',],
                            metric=c('transValue', 'transValue'),
                            spaceField='all', timeField='transQtr',
                            defDim='time', stsLimit=3, 
                            calcs=list(median='median'))
  
  mmMetroU <- aryStsGeoWrap(allTrans[allTrans$PropertyType == 'Unit',],
                            metric=c('transValue', 'transValue'),
                            spaceField='all', timeField='transQtr',
                            defDim='time', stsLimit=3, 
                            calcs=list(median='median'))
  
 ## At LGA Level
  
  # All Uses
  mmLga<- aryStsGeoWrap(stsData = allTrans,
                        metric=c('transValue', 'transValue'),
                        spaceField='lga', timeField='transQtr',
                        defDim='time', stsLimit=3, 
                        calcs=list(median='median'))
  
  # By Use
  mmLgaH <- aryStsGeoWrap(allTrans[allTrans$PropertyType == 'House', ],
                          metric=c('transValue', 'transValue'),
                          spaceField='lga', timeField='transQtr',
                          defDim='time', stsLimit=3, 
                          calcs=list(median='median'))
  
  mmLgaU <- aryStsGeoWrap(allTrans[allTrans$PropertyType == 'Unit', ],
                          metric=c('transValue', 'transValue'),
                          spaceField='lga', timeField='transQtr',
                          defDim='time', stsLimit=3, 
                          calcs=list(median='median'))
  
 ## At SLA1 Level
  
  # All Uses  
  mmSla <- aryStsGeoWrap(stsData = allTrans,
                         metric=c('transValue', 'transValue'),
                         spaceField='sla1', timeField='transQtr',
                         defDim='time', stsLimit=3, 
                         calcs=list(median='median'))
  
  # By Use
  mmSlaH <- aryStsGeoWrap(allTrans[allTrans$PropertyType == 'House', ],
                          metric=c('transValue', 'transValue'),
                          spaceField='sla1', timeField='transQtr',
                          defDim='time', stsLimit=3, 
                          calcs=list(median='median'))
  
  mmSlaU <- aryStsGeoWrap(allTrans[allTrans$PropertyType == 'Unit', ],
                          metric=c('transValue', 'transValue'),
                          spaceField='sla1', timeField='transQtr',
                          defDim='time', stsLimit=3, 
                          calcs=list(median='median'))
  
 ## At Suburb Level
  
  # All Uses  
  mmSuburb <- aryStsGeoWrap(stsData = allTrans,
                            metric=c('transValue', 'transValue'),
                            spaceField='suburb', timeField='transQtr',
                            defDim='time', stsLimit=3, 
                            calcs=list(median='median'))
  
  # By Use
  mmSuburbH <- aryStsGeoWrap(allTrans[allTrans$PropertyType == 'House', ],
                             metric=c('transValue', 'transValue'),
                             spaceField='suburb', timeField='transQtr',
                             defDim='time', stsLimit=3, 
                             calcs=list(median='median'))

  mmSuburbU <- aryStsGeoWrap(allTrans[allTrans$PropertyType == 'Unit', ],
                             metric=c('transValue', 'transValue'),
                             spaceField='suburb', timeField='transQtr',
                             defDim='time', stsLimit=3, 
                             calcs=list(median='median'))
  
  ## At PostCode Level
  
  # All Uses   
  mmPostcode <- aryStsGeoWrap(stsData = allTrans,
                              metric=c('transValue', 'transValue'),
                              spaceField='postCode', timeField='transQtr',
                              defDim='time', stsLimit=3, 
                              calcs=list(median='median'))
  
  # by Use   
  mmPostcodeH <- aryStsGeoWrap(allTrans[allTrans$PropertyType == 'House', ],
                               metric=c('transValue', 'transValue'),
                               spaceField='postCode', timeField='transQtr',
                               defDim='time', stsLimit=3, 
                               calcs=list(median='median'))
  # All Uses   
  mmPostcodeU <- aryStsGeoWrap(allTrans[allTrans$PropertyType == 'Unit', ],
                               metric=c('transValue', 'transValue'),
                               spaceField='postCode', timeField='transQtr',
                               defDim='time', stsLimit=3, 
                               calcs=list(median='median'))
  
### Impute regression comparison method -----------------------------------------

 ## Set the specification (formula)

  regSpecH <- log(transValue) ~ as.factor(postCode) + as.factor(transQtr) + 
                 log(AreaSize) +Bedrooms + Baths + ssInteg + ssChoice
  
  regSpecU <- log(transValue) ~ as.factor(postCode) + as.factor(transQtr) + 
    + Bedrooms + Baths + ssInteg + ssChoice
  
 ## Estimate models and make new predictions: Global by Use

  # For houses
  houseResults <- prrImputeReg(regSpecH, 
                            subset(allTrans, transType == 'sale' &
                                             PropertyType == 'House' & 
                                             QT_house_postCode == 1),
                            subset(allTrans, transType == 'rent' &
                                             PropertyType == 'House' & 
                                             QT_house_postCode == 1),
                            verbose=TRUE)
  
  # For Units
  unitResults <- prrImputeReg(regSpecU, 
                           subset(allTrans, transType == 'sale' &
                                    PropertyType == 'Unit' & 
                                    QT_unit_postCode == 1),
                           subset(allTrans, transType == 'rent' &
                                    PropertyType == 'Unit' & 
                                    QT_unit_postCode == 1),
                           verbose=TRUE)

 ## Calculate the ratio

  # Extract vales
  irValues <- rbind(houseResults$results, unitResults$results)

  # Calculate the ratio
  irValues$yield <- (irValues$Rent * 52) / irValues$Price

  # Add Ratio to full dataset 
  allTrans$yield <- irValues$yield[match(allTrans$UID, irValues$UID)]
  xTrans <- subset(allTrans, !is.na(yield)) 
       
### Output raw results ---------------------------------------------------------
  
  write.csv(xTrans, paste0(dataPath, 'rawResults.csv'), row.names=FALSE)
    
### Break down results by dimensions -------------------------------------------
  
 ## Metro
  
  # Metro 
  irMetro <- spaceTimeShard(stsData = xTrans,
                            metric=c('yield'),
                            spaceField='all', timeField='transQtr',
                            defDim='time', stsLimit=3, 
                            calcs=list(median='median'))
  
  # By Use
  irMetroH <- spaceTimeShard(xTrans[xTrans$PropertyType == 'House', ],
                             metric=c('yield'),
                             spaceField='all', timeField='transQtr',
                             defDim='time', stsLimit=3, 
                             calcs=list(median='median'))

  irMetroU <- spaceTimeShard(xTrans[xTrans$PropertyType == 'Unit', ],
                             metric=c('yield'),
                             spaceField='all', timeField='transQtr',
                             defDim='time', stsLimit=3, 
                             calcs=list(median='median'))
 ## LGA
  
  irLga <- spaceTimeShard(stsData = xTrans,
                          metric=c('yield'),
                          spaceField='lga', timeField='transQtr',
                          defDim='time', stsLimit=3, 
                          calcs=list(median='median'))
  
  # By Use
  irLgaH <- spaceTimeShard(xTrans[xTrans$PropertyType == 'House', ],
                           metric=c('yield'),
                           spaceField='lga', timeField='transQtr',
                           defDim='time', stsLimit=3, 
                           calcs=list(median='median'))
  
  irLgaU <- spaceTimeShard(xTrans[xTrans$PropertyType == 'Unit', ],
                           metric=c('yield'),
                           spaceField='lga', timeField='transQtr',
                           defDim='time', stsLimit=3, 
                           calcs=list(median='median')) 
  
  ## SLA1
  
  irSla <- spaceTimeShard(stsData = xTrans,
                          metric=c('yield'),
                          spaceField='sla1', timeField='transQtr',
                          defDim='time', stsLimit=3, 
                          calcs=list(median='median'))
  
  # By Use
  irSlaH <- spaceTimeShard(xTrans[xTrans$PropertyType == 'House', ],
                           metric=c('yield'),
                           spaceField='sla1', timeField='transQtr',
                           defDim='time', stsLimit=3, 
                           calcs=list(median='median'))
  
  irSlaU <- spaceTimeShard(xTrans[xTrans$PropertyType == 'Unit', ],
                           metric=c('yield'),
                           spaceField='sla1', timeField='transQtr',
                           defDim='time', stsLimit=3, 
                           calcs=list(median='median'))

  ## Suburb
  
  irSuburb <- spaceTimeShard(stsData = xTrans,
                             metric=c('yield'),
                             spaceField='suburb', timeField='transQtr',
                             defDim='time', stsLimit=3, 
                             calcs=list(median='median'))
  
  # By Use
  irSuburbH <- spaceTimeShard(xTrans[xTrans$PropertyType == 'House', ],
                              metric=c('yield'),
                              spaceField='suburb', timeField='transQtr',
                              defDim='time', stsLimit=3, 
                              calcs=list(median='median'))
  
  irSuburbU <- spaceTimeShard(xTrans[xTrans$PropertyType == 'Unit', ],
                              metric=c('yield'),
                              spaceField='suburb', timeField='transQtr',
                              defDim='time', stsLimit=3, 
                              calcs=list(median='median'))
  
  ## post code
  
  irPostcode <- spaceTimeShard(stsData = xTrans,
                               metric=c('yield'),
                               spaceField='postCode', timeField='transQtr',
                               defDim='time', stsLimit=3, 
                               calcs=list(median='median'))
  
  # By Use
  irPostcodeH <- spaceTimeShard(xTrans[xTrans$PropertyType == 'House', ],
                                metric=c('yield'),
                                spaceField='postCode', timeField='transQtr',
                                defDim='time', stsLimit=3, 
                                calcs=list(median='median'))
  
  irPostcodeU <- spaceTimeShard(xTrans[xTrans$PropertyType == 'Unit', ],
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
  dmMetro <- spaceTimeShard(stsData = dmData,
                            metric=c('saleYield'),
                            spaceField='all', timeField='saleTime',
                            defDim='time', stsLimit=3, 
                            calcs=list(median='median'))
  
  # By Use   
  dmMetroH <- spaceTimeShard(dmData[dmData$PropertyType == 'House',],
                             metric=c('saleYield'),
                             spaceField='all', timeField='saleTime',
                             defDim='time', stsLimit=3, 
                             calcs=list(median='median'))
  
  dmMetroU <- spaceTimeShard(dmData[dmData$PropertyType == 'Unit',],
                             metric=c('saleYield'),
                             spaceField='all', timeField='saleTime',
                             defDim='time', stsLimit=3, 
                             calcs=list(median='median'))
  
 ## LGA  
  
  dmLga <- spaceTimeShard(stsData = dmData,
                          metric=c('saleYield'),
                          spaceField='lga', timeField='saleTime',
                          defDim='time', stsLimit=3, 
                          calcs=list(median='median'))
  
  # By Use   
  dmLgaH <- spaceTimeShard(dmData[dmData$PropertyType == 'House',],
                           metric=c('saleYield'),
                           spaceField='lga', timeField='saleTime',
                           defDim='time', stsLimit=3, 
                           calcs=list(median='median'))
  
  dmLgaU <- spaceTimeShard(dmData[dmData$PropertyType == 'Unit',],
                           metric=c('saleYield'),
                           spaceField='lga', timeField='saleTime',
                           defDim='time', stsLimit=3, 
                           calcs=list(median='median'))
  
  ## SLA1 
  
  dmSla <- spaceTimeShard(stsData = dmData,
                          metric=c('saleYield'),
                          spaceField='sla1', timeField='saleTime',
                          defDim='time', stsLimit=3, 
                          calcs=list(median='median'))
  
  # By Use   
  dmSlaH <- spaceTimeShard(dmData[dmData$PropertyType == 'House',],
                           metric=c('saleYield'),
                           spaceField='sla1', timeField='saleTime',
                           defDim='time', stsLimit=3, 
                           calcs=list(median='median'))
  
  dmSlaU <- spaceTimeShard(dmData[dmData$PropertyType == 'Unit',],
                           metric=c('saleYield'),
                           spaceField='sla1', timeField='saleTime',
                           defDim='time', stsLimit=3, 
                           calcs=list(median='median')) 

  ## Suburb
  
  dmSuburb <- spaceTimeShard(stsData = dmData,
                             metric=c('saleYield'),
                             spaceField='suburb', timeField='saleTime',
                             defDim='time', stsLimit=3, 
                             calcs=list(median='median'))
  
  # By Use   
  dmSuburbH <- spaceTimeShard(dmData[dmData$PropertyType == 'House',],
                              metric=c('saleYield'),
                              spaceField='suburb', timeField='saleTime',
                              defDim='time', stsLimit=3, 
                              calcs=list(median='median'))
  
  dmSuburbU <- spaceTimeShard(dmData[dmData$PropertyType == 'Unit',],
                              metric=c('saleYield'),
                              spaceField='suburb', timeField='saleTime',
                              defDim='time', stsLimit=3, 
                              calcs=list(median='median')) 
  
  ## PostCode 
  
  dmPostcode <- spaceTimeShard(stsData = dmData,
                               metric=c('saleYield'),
                               spaceField='postCode', timeField='saleTime',
                               defDim='time', stsLimit=3, 
                               calcs=list(median='median'))
  
  # By Use   
  dmPostcodeH <- spaceTimeShard(dmData[dmData$PropertyType == 'House',],
                                metric=c('saleYield'),
                                spaceField='postCode', timeField='saleTime',
                                defDim='time', stsLimit=3, 
                                calcs=list(median='median'))
  
  dmPostcodeU <- spaceTimeShard(dmData[dmData$PropertyType == 'Unit',],
                                metric=c('saleYield'),
                                spaceField='postCode', timeField='saleTime',
                                defDim='time', stsLimit=3, 
                                calcs=list(median='median')) 

### Extract a price index from the regression models ---------------------------
  
 ## Get original regression model coefficients  
  
  houseModel <- as.data.frame(houseResults$saleModel$coef)
  unitModel <- as.data.frame(unitResults$saleModel$coef)
  
 ## Extract only the time coefficients & add qtr 1 value of 0  
  
  houseTimeCoefs <- c(0, houseModel$Estimate[grep('transQtr', 
                                                  rownames(houseModel))])
  
  unitTimeCoefs <- c(0, unitModel$Estimate[grep('transQtr', 
                                                rownames(unitModel))])
  
 ## Convert into an index of change
  
  houseIndex <- c(0, (houseTimeCoefs[-1] - 
                        houseTimeCoefs[-length(houseTimeCoefs)]))
  unitIndex <- c(0, (unitTimeCoefs[-1] - 
                       unitTimeCoefs[-length(unitTimeCoefs)]))
  
 ## Convert to a combined index
  
  allIndex <- (houseIndex + unitIndex) / 2
  
 ## Make into a list
  
  indexList <- list(all = allIndex,
                    house = houseIndex,
                    unit = unitIndex)
  
### Build into 5 basic data object by geography --------------------------------
  
 ## Metro
  
  metroList <- list(mm=list(all=mmMetro, house=mmMetroH, unit=mmMetroU),
                    ir=list(all=irMetro, house=irMetroH, unit=irMetroU),
                    dm=list(all=dmMetro, house=dmMetroH, unit=dmMetroU))

  metroData <- aryAggrGeoData(metroList, indexList)
  
 ## LGA  
  
  lgaList <- list(mm=list(all=mmLga, house=mmLgaH, unit=mmLgaU),
                  ir=list(all=irLga, house=irLgaH, unit=irLgaU),
                  dm=list(all=dmLga, house=dmLgaH, unit=dmLgaU))
  
  lgaData <- aryAggrGeoData(lgaList, indexList, geoSplit=TRUE)
  
 ## SLA
  
  slaList <- list(mm=list(all=mmSla, house=mmSlaH, unit=mmSlaU),
                  ir=list(all=irSla, house=irSlaH, unit=irSlaU),
                  dm=list(all=dmSla, house=dmSlaH, unit=dmSlaU))
  
  slaData <- aryAggrGeoData(slaList, indexList, geoSplit=TRUE)
  
 ## Postcode  
  
  postcodeList <- list(mm=list(all=mmPostcode, house=mmPostcodeH, unit=mmPostcodeU),
                  ir=list(all=irPostcode, house=irPostcodeH, unit=irPostcodeU),
                  dm=list(all=dmPostcode, house=dmPostcodeH, unit=dmPostcodeU))
  
  postcodeData <- aryAggrGeoData(postcodeList, indexList, geoSplit=TRUE)
  
 ## Suburb  
  
  suburbList <- list(mm=list(all=mmSuburb, house=mmSuburbH, unit=mmSuburbU),
                  ir=list(all=irSuburb, house=irSuburbH, unit=irSuburbU),
                  dm=list(all=dmSuburb, house=dmSuburbH, unit=dmSuburbU))
  
  suburbData <- aryAggrGeoData(suburbList, indexList, geoSplit=TRUE)
  
### Clean up workspace and save ------------------------------------------------
  
  save(metroList, metroData, lgaList, lgaData, slaList, slaData,
       postcodeList, postcodeData, suburbList, suburbData,
       file=paste0(dataPath, 'analysisResults.RData'))

  