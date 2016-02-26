##########################################################################################
#                                                                                        #
#  Data Preparation functions for APM data                                               #
#                                                                                        #
##########################################################################################

### Function to load necessary APM data ----------------------------------------
  
loadAPMData <- function(dataPath,               # Location of raw data
                        saleFile,               # File name/loc of sales
                        rentFile,               # File name/loc of rents
                        geoFiles=list(),        # List of shapefiles
                        offline=FALSE,          # Are you offline??
                        verbose=FALSE           # Show progress?
                        )
{
  
  ### Example Function Call --------------------------------------------------------------
  
  if(F){
    
    loadAPMData(dataPath="C:/data/research/priceRentMethComp/",
                saleFile='transData/sales10_15.csv',
                rentFile='transData/rents10_15.csv',
                geoFiles=list(suburb='shapefiles/Vic_Suburbs.shp',
                              lga='shapefiles/Vic_LGAs.shp',
                              sla1='shapefiles/Vic_SLA1.shp',
                              postcode='shapefiles/Vic_PostCodes.shp',
                              ssFile='spatialData/allSS.csv'),
                offline=TRUE,
                verbose=TRUE)
  }
  
  ### Preliminary Commands ---------------------------------------------------------------
  
  if(verbose) cat('Loading Libraries and Source Files\n')
  
  ## Load Libraries
  
  library(plyr)
  library(dplyr)
  library(reshape2)
  library(stringr)
  library(maptools)
  library(sp)
  library(rgeos)
  
  ## Source Files
  
  # File containing function for working with prr and APM data
  if(offline){
    source('c:/Code/research/ausPropMrkt/prrFunctions.R')
    source('c:/Code/research/ausPropMrkt/apmDataOptions.R')
    
  } else {
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/prrFunctions.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/apmDataOptions.R'))
  }
  
  ### Read in raw data -------------------------------------------------------------------
  
  if(verbose) cat('Loading Data\n')
  
  ## Read in Data  
  
  # Sales data
  if(verbose) cat('...Sales Data\n')
  assign('rawSales', read.csv(paste0(dataPath, saleFile), 
                              stringsAsFactors = FALSE),
         envir=.GlobalEnv)
  if(verbose) cat('...Rental Data\n')
  assign('rawRents', read.csv(paste0(dataPath, rentFile), 
                              stringsAsFactors = FALSE),
         envir=.GlobalEnv)
  
  if(verbose) cat('...SS Data\n')
  assign('ssData', read.csv(paste0(dataPath, geoFiles$ssFile), 
                     stringsAsFactors = FALSE),
         envir=.GlobalEnv)
  
  if(verbose) cat('...Geographic Shapefiles\n')
  subShp <- readShapePoly(paste0(dataPath, geoFiles$suburb))
  lgaShp <- readShapePoly(paste0(dataPath, geoFiles$lga))
  sla1Shp <- readShapePoly(paste0(dataPath, geoFiles$sla1))
  postCodeShp <-  readShapePoly(paste0(dataPath, geoFiles$postcode))
  assign('geoShapes', list(suburb=subShp,
                           lga=lgaShp,
                           sla1=sla1Shp,
                           postcode=postCodeShp), envir=.GlobalEnv)
  
}


### Function to integrate data  ----------------------------------------------------------  

buildAPMData <- function(rawRents,              # Raw rental data
                         rawSales,              # Raw sale data
                         ssData,                # space syntax data
                         geoShapes,             # geographic shape files
                         verbose=FALSE          # Show Progress?
                         )
{
 
 ## Example function call
  
  if(F){
    allTrans <- buildAPMData(rawRents, rawSales, ssData, geoShapes,
                             verbose=TRUE)
  }
  
  if(verbose) cat('Building Data\n')
  
 ## Create UniqueID
  
  if(verbose) cat('...Assign Unique Ids\n')
  rawSales$UID <- paste0('sale', 1:nrow(rawSales))
  rawRents$UID <- paste0('rental', 1:nrow(rawRents))
  
 ## Create conforming fields regarding transaction times and values

  # Fix date formats
  if(verbose) cat('...Fix Date Values\n')
  rawSales$transDate <- fixAPMDates(rawSales$FinalResultEventDate)
  rawRents$transDate <- fixAPMDates(rawRents$EventDate)

  # Build new column for transaction Value
  if(verbose) cat('...Fix Transaction Values\n')
  rawSales$transValue <- as.numeric(rawSales$FinalResultEventPrice)
  rawRents$transValue <- as.numeric(rawRents$EventPrice)

  # Set transaction Type   
  rawSales$transType <- 'sale'  
  rawRents$transType <- 'rent'

 ## Fix missing lat long

  if(verbose) cat('...Fix Missing Lat/Long Values\n')

  # ID missing
  sXY <- which(is.na(rawSales$Property_Latitude) | 
               is.na(rawSales$Property_Longitude))
  rXY <- which(is.na(rawRents$Property_Latitude) | 
               is.na(rawRents$Property_Longitude))

  # Fix, if possible 
  rawSales$Property_Latitude[sXY] <- rawSales$Street_Centroid_Latitude[sXY]
  rawSales$Property_Longitude[sXY] <- rawSales$Street_Centroid_Longitude[sXY]
  rawRents$Property_Latitude[rXY] <- rawRents$Street_Centroid_Latitude[rXY]
  rawRents$Property_Longitude[rXY] <- rawRents$Street_Centroid_Longitude[rXY]

 ## Limit both datasets to a standard field list  

  if(verbose) cat('...Trimming Fields\n')

  # Combine and clip fields to make cleaning easier
  allTrans <- rbind(rawSales[ ,apmOptions$rawColumnList], 
                    rawRents[ ,apmOptions$rawColumnList])

 ## Add additional time information

  if(verbose) cat('...Adding Additional Time Data\n')

  # Add a yearly variable
  allTrans$transYear <- as.numeric(substr(allTrans$transDate, 1, 4))

  # Add a month
  allTrans$transMonth <- (((12 * (allTrans$transYear - apmOptions$startYear)) + 
                             as.numeric(substr(allTrans$transDate, 6, 7))) - 
                              (apmOptions$startMonth - 1))

  # Add a days count
  allTrans$transDays <- 1+ (as.numeric(allTrans$transDate - 
                                       as.Date(paste0(apmOptions$startYear, '-', 
                                                      apmOptions$startMonth, '-01'))))

  # Add a quarter count
  allTrans$transQtr <- ((allTrans$transMonth - 1) %/% 3) + 1

 ## Fix NA Fields

  if(verbose) cat('...Fixing NA Fields\n')
  naFields <- apmOptions$naFields
  for(naF in 1:length(naFields)){
    naX <- which(is.na(allTrans[ ,naFields[[naF]]]))
    allTrans[naX, naFields[[naF]]] <- 0
  }

  ##  Check for and remove duplicates

  if(verbose) cat('...Check and Remove Duplicates\n')

  # Create a unique ID
  allTrans$dUID <- paste0(allTrans$AddressID,"..", allTrans$transDate, "..", 
                        allTrans$transType)

  # Keep only those not duplicated
  allTrans <- subset(allTrans, !duplicated(dUID))

  # Remove the UID field
  allTrans$dUID <- NULL

  ## Add Spatial Information

  if(verbose) cat('Adding Spatial Information\n')

  # Remove Missing lat/long
  allTrans <- subset(allTrans, !is.na(Property_Latitude) & 
                       !is.na(Property_Longitude))

  # Create a spatial points data frame
  allSP <- SpatialPointsDataFrame(coords=cbind(allTrans$Property_Longitude,
                                               allTrans$Property_Latitude),
                                  data=allTrans)

  # Add PostCodes
  if(verbose) cat('...Adding Postcodes\n')
  spJoin <- over(allSP, geoShapes$postcode)
  allSP@data$postCode <- as.character(spJoin$POA_2006)

  # Add Suburbs
  if(verbose) cat('...Adding Suburbs\n')
  spJoin <- over(allSP, geoShapes$suburb)
  allSP@data$suburb <- as.character(spJoin$NAME_2006)

  # correct error in names
  allSP@data$suburb <- gsub(' - Bal', '', allSP@data$suburb)

  # Add SLA1
  if(verbose) cat('...Adding Sla1s\n')
  spJoin <- over(allSP, geoShapes$sla1)
  allSP@data$sla1 <- as.character(spJoin$SLA_NAME11)

  # Add LGA
  if(verbose) cat('...Adding LGAs\n')
  spJoin <- over(allSP, geoShapes$lga)
  allSP@data$lga <- as.character(spJoin$LGA_NAME11)

  ## Convert back to regular data.frame

  allTrans <- allSP@data

  ## Add SS Measures

  if(verbose) cat('...Adding SS data\n')

  # Build Single SS File
  ssTrim <- ssData[, c('AddressID', apmOptions$ssFields)]
  names(ssTrim)[2:3] <- c('ssChoice', 'ssInteg')

  # Add Measure to trans
  allTrans$ssChoice <- ssTrim$ssChoice[match(allTrans$AddressID,
                                             ssTrim$AddressID)]
  allTrans$ssInteg <- ssTrim$ssInteg[match(allTrans$AddressID, 
                                           ssTrim$AddressID)]

 ## Return values  
  
  return(allTrans)
}


### Function to clean data  --------------------------------------------------------------  

cleanAPMData <- function(apmDataObj,            # transaction data from buildAPMData()
                         verbose=FALSE){


 ## Example function call
  
  if(F){
    allTrans <- cleanAPMData(apmDataObj=allTrans, verbose=TRUE)
  }
  
  if(verbose) cat('Cleaning Data\n')

 ## Remove all non house and units

  # Create separate labels
  if(verbose) cat('...Creating Property Type Labels\n')
  apmDataObj$Terrace <- ifelse(apmDataObj$PropertyType == 'Terrace', 1, 0)
  apmDataObj$Townhouse <- ifelse(apmDataObj$PropertyType == 'Townhouse', 1, 0)
  apmDataObj$Studio <- ifelse(apmDataObj$PropertyType == 'Studio', 1, 0)
  apmDataObj$Duplex <- ifelse(apmDataObj$PropertyType == 'Duplex', 1, 0)
  apmDataObj$Villa <- ifelse(apmDataObj$PropertyType == 'Villa', 1, 0)

  # Convert to House or Unit
  apmDataObj$PropertyType[apmDataObj$PropertyTypes %in% 
                          apmOptions$houseTypes] <- 'House'
  apmDataObj$PropertyType[apmDataObj$PropertyTypes %in% 
                          apmOptions$unitTypes] <- 'Unit'
  
  # Limit to houses or units
  if(verbose) cat('...Limiting to Houses and Units\n')
  apmDataObj <- subset(apmDataObj, PropertyType == 'House' |
                       PropertyType == 'Unit')

  ## Removing missing values  

  if(verbose) cat('...Removing Observations with missing data\n')

  for(ij in 1:length(apmOptions$reqFields)){
    idX <- which(names(apmDataObj) == apmOptions$reqFields[[ij]])
    apmDataObj <- subset(apmDataObj, !is.na(apmDataObj[ ,idX]))
  }

 ## Remove suspect values

  if(verbose) cat('...Removing Observation with suspect data values\n')

  # Set limits
  areaLimits <- c(40, 25000)
  bathLimits <- c(1, 8)
  bedLimits <- c(0, 1, 8)  # apt limit, then home limit, then all limit
  rentLimits <- c(125, 2500)
  saleLimits <- c(150000, 4000000)

  # Remove by characteristic
  apmDataObj <- subset(apmDataObj, AreaSize >= apmOptions$areaLimits$min & 
                       AreaSize <= apmOptions$areaLimits$max)
  apmDataObj <- subset(apmDataObj, Baths >= apmOptions$bathLimits$min & 
                       Baths <= apmOptions$bathLimits$max)

  apmDataObjU <- subset(apmDataObj, PropertyType == 'Unit' & 
                         Bedrooms >= apmOptions$bedLimits$unitMin &
                          Bedrooms <= apmOptions$bedLimits$unitMax)
  apmDataObjH <- subset(apmDataObj, PropertyType == 'House' & 
                         Bedrooms >= apmOptions$bedLimits$houseMin &
                          Bedrooms <= apmOptions$bedLimits$houseMax)
  apmDataObj <- rbind(apmDataObjH, apmDataObjU)

  # Split back out
  xSales <- subset(apmDataObj, transType == 'sale')
  xRentals <- subset(apmDataObj, transType == 'rent')

  # Remove by suspect trans value
  xSales <- subset(xSales, transValue >= apmOptions$saleLimits$min & 
                     transValue <= apmOptions$saleLimits$max)
  xRentals <- subset(xRentals, transValue >= apmOptions$rentLimits$min & 
                       transValue <= apmOptions$rentLimits$max)

 ## Return combined object
  
  return(rbind(xSales, xRentals))
 
}

### Function that sets the limits for # of obs by geo by time ---------------------------- 

setAPMGeoThres <- function(apmDataObj,           # transaction data from cleanAPMData()
                           geoShapes,            # geographic shape files
                           verbose=FALSE         # Show progress
                           )
{
  
 ## Example function call
  
  if(F){
    allTrans <- setAPMGeoThres(allTrans, geoShapes, verbose=TRUE) 
  }
  
 ## Limit shapefiles
  
  if(verbose) cat('...Limiting Geography Files to Extent of Data\n')
  
  # Suburbs
  studySuburbs <- geoShapes$suburb[(which(geoShapes$suburb@data$NAME_2006 %in% 
                                     names(table(allTrans$suburb)))), ]
  
  # Post Codes
  studyPostCodes <- geoShapes$postcode[(which(geoShapes$postcode@data$POA_2006 %in% 
                                         names(table(allTrans$postCode)))), ]
  
  # SLA1
  studySLA1s <- geoShapes$sla1[(which(geoShapes$sla1@data$SLA_NAME11 %in% 
                                 names(table(allTrans$sla1)))), ]
  
  # LGAs
  studyLGAs <- geoShapes$lga[(which(geoShapes$lga@data$LGA_NAME11 %in% 
                               names(table(allTrans$lga)))), ]
  
  # Combine into a single list
  studyShapes <- list(suburb=studySuburbs,
                      postcode=studyPostCodes,
                      lga=studyLGAs,
                      sla1=studySLA1s)

 ## Determine which geographies meet which time thresholds
  
  if(verbose) cat('...Adding Sample Size Threshold Designators\n')
  
  # Yearly threshold
  yearThres <- mapply(prrGeoLimit, 
                      locField=c('postCode', 'sla1', 'suburb', 'lga'), 
                      MoreArgs=list(timeField='transYear',
                                    transData=apmDataObj,
                                    geoTempLimit=apmOptions$geoTempLimit))
  
  names(yearThres) <- paste0(rep("YT_"),
                             rep(c('both', 'house', 'unit', 'either'), 4),
                             rep("_", 16),
                             c(rep('postCode',4), rep('sla1',4),
                               rep('suburb',4), rep('lga', 4)))
  
  # Quarterly threshold
  qtrThres <- mapply(prrGeoLimit, 
                     locField=c('postCode', 'sla1', 'suburb', 'lga'), 
                     MoreArgs=list(timeField='transQtr',
                                   transData=apmDataObj,
                                   geoTempLimit=apmOptions$geoTempLimit))
  
  names(qtrThres) <- paste0(rep("QT_"),
                            rep(c('both', 'house', 'unit', 'either'), 4),
                            rep("_", 16),
                            c(rep('postCode',4), rep('sla1',4),
                              rep('suburb',4), rep('lga', 4)))
  
 ## Add designators to transactions
  
  # Yearly thresholds
  apmDataObj <- prrApplyThres(yearThres[1:4], apmDataObj, 'YT', 'postCode')
  apmDataObj <- prrApplyThres(yearThres[5:8], apmDataObj, 'YT', 'sla1')
  apmDataObj <- prrApplyThres(yearThres[9:12], apmDataObj, 'YT', 'suburb')
  apmDataObj <- prrApplyThres(yearThres[13:16], apmDataObj, 'YT', 'lga')
  
  # Quarterly thresholds
  apmDataObj <- prrApplyThres(qtrThres[1:4], apmDataObj, 'QT', 'postCode')
  apmDataObj <- prrApplyThres(qtrThres[5:8], apmDataObj, 'QT', 'sla1')
  apmDataObj <- prrApplyThres(qtrThres[9:12], apmDataObj, 'QT', 'suburb')
  apmDataObj <- prrApplyThres(qtrThres[13:16], apmDataObj, 'QT', 'lga')
  
 ## Return values
  
  return(apmDataObj)
}

