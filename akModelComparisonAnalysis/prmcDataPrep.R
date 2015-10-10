################################################################################
#                                                                              #
#  Data Preparation Code for the Price to Rent Ratio Model comparison Study    #
#                                                                              #
################################################################################

### Preliminary Commands -------------------------------------------------------

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

 ## Set the path to the raw data

  dataPath <- "C:/Dropbox/Australia Data/ausPropData/melData/"
  saleFile <- 'sales10_15.csv'
  rentFile <- 'rents10_15.csv'
  subGeoFile <- 'Vic_Suburbs.shp'
  lgaGeoFile <- 'Vic_LGAs.shp'
  sla1GeoFile <- 'Vic_SLA1.shp'
  postGeoFile <- 'Vic_PostCodes.shp'
  
### Read in raw data -----------------------------------------------------------

  ## Read in Data  

  rawSales <- read.csv(paste0(dataPath, saleFile), stringsAsFactors = FALSE)
  rawRents <- read.csv(paste0(dataPath, rentFile), stringsAsFactors = FALSE)
  subShp <- readShapePoly(paste0(dataPath, subGeoFile))
  lgaShp <- readShapePoly(paste0(dataPath, lgaGeoFile))
  sla1Shp <- readShapePoly(paste0(dataPath, sla1GeoFile))
  postCodeShp <- readShapePoly(paste0(dataPath, postGeoFile))

### DATA MANAGEMENT ------------------------------------------------------------  

 ## Create UniqueID
  rawSales$UID <- paste0('sale', 1:nrow(rawSales))
  rawRents$UID <- paste0('rental', 1:nrow(rawRentals))
  
 ## Create conforming fields regarding transaction times and values

  # Fix date formats
  rawSales$transDate <- fixAPMDates(rawSales$FinalResultEventDate)
  rawRents$transDate <- fixAPMDates(rawRents$EventDate)

  # Build new column for transaction Value
  rawSales$transValue <- as.numeric(rawSales$FinalResultEventPrice)
  rawRents$transValue <- as.numeric(rawRents$EventPrice)

  # Set transaction Type   
  rawSales$transType <- 'sale'  
  rawRents$transType <- 'rent'

 ## Limit both datasets to a standard field list  

  # Set list
  columnList <- c('UID', 'GeographicalID', 'EventID', 'AddressID', 'FlatNumber', 
                  'transDate', 'transValue', 'transType',
                  'PropertyType', 'Property_Latitude', 'Property_Longitude',
                  'AreaSize', 'Bedrooms', 'Baths', 'Parking','HasFireplace',
                  'HasPool', 'HasGarage', 'HasAirConditioning')

  # Combine and clip fields to make cleaning easier
  allTrans <- rbind(rawSales[ ,columnList], rawRents[ ,columnList])

 ## Add additional time information

  # Add a yearly variable
  allTrans$transYear <- as.numeric(substr(allTrans$transDate, 1, 4))

  # Add a month
  allTrans$transMonth <- ((12 * (allTrans$transYear - 2010)) + 
                            as.numeric(substr(allTrans$transDate, 6, 7))) - 5
    
  # Add a days count
  allTrans$transDays <- (as.numeric(allTrans$transDate - 
                                      (min(allTrans$transDate) - 1)))

  # Add a quarter count
  allTrans$transQtr <- (allTrans$transDays %/% 91.25) + 1
  allTrans$transQtr[allTrans$transQtr == 21] <- 20

  # Remove Missing lat/long
  allTrans <- subset(allTrans, !is.na(Property_Latitude) & 
                       !is.na(Property_Longitude))
  
 ## Fix NA Fields
  naFields <- list('HasPool', 'HasGarage', 'HasAirConditioning', 'HasFireplace')
  for(naF in 1:length(naFields)){
    naX <- which(is.na(allTrans[ ,naFields[[naF]]]))
    allTrans[naX, naFields[[naF]]] <- 0
  }
  
 ##  Check for and remove duplicates
  
  # Create a unique ID
  allTrans$UID <- paste0(allTrans$AddressID,"..", allTrans$transDate, "..", 
                         allTrans$transType)
  
  # Keep only those not duplicated
  allTrans <- subset(allTrans, !duplicated(UID))
  
  # Remove the UID field
  allTrans$UID <- NULL
  
 ## Add Spatial Information
  
  # Create a spatial points data frame
  allSP <- SpatialPointsDataFrame(coords=cbind(allTrans$Property_Longitude,
                                               allTrans$Property_Latitude),
                                  data=allTrans)
  
  # Add PostCodes
  spJoin <- over(allSP, postCodeShp)
  allSP@data$postCode <- as.character(spJoin$POA_2006)

  # Add Suburbs
  spJoin <- over(allSP, subShp)
  allSP@data$suburb <- as.character(spJoin$NAME_2006)
  
  # Add SLA1
  spJoin <- over(allSP, sla1Shp)
  allSP@data$sla1 <- as.character(spJoin$SLA_NAME11)
  
  # Add LGA
  spJoin <- over(allSP, lgaShp)
  allSP@data$lga <- as.character(spJoin$LGA_NAME11)
  
 ## Convert back to regular data.frame
  
  allTrans <- allSP@data

 ## Clean up memory
  rm(rawRents); rm(rawSales); rm(spJoin); rm(allSP); gc()

### DATA CLEANING --------------------------------------------------------------  
  
 ## Removing missing values  

  # Missing transaction values
  allTrans <- subset(allTrans, !is.na(transValue))
  allTrans <- subset(allTrans, transValue  != 0)

  # Missing home characteristics
  allTrans <- subset(allTrans, !is.na(AreaSize))
  allTrans <- subset(allTrans, !is.na(Bedrooms))
  allTrans <- subset(allTrans, !is.na(Baths))

 ## Remove suspect values
  
  # Set limits
  areaLimits <- c(40, 25000)
  bedLimits <- c(1, 8)
  bathLimits <- c(1, 8)
  rentLimits <- c(125, 2500)
  saleLimits <- c(150000, 4000000)
  
  # Remove by characteristic
  allTrans <- subset(allTrans, AreaSize >= areaLimits[1] & 
                       AreaSize <= areaLimits[2])
  allTrans <- subset(allTrans, Bedrooms >= bedLimits[1] & 
                       Bedrooms <= bedLimits[2])
  allTrans <- subset(allTrans, Baths >= bathLimits[1] & 
                       Baths <= bathLimits[2])
  
  # Split back out
  xSales <- subset(allTrans, transType == 'sale')
  xRentals <- subset(allTrans, transType == 'rent')
  
  # Remove by suspect trans value
  xSales <- subset(xSales, transValue >= saleLimits[1] & 
                     transValue <= saleLimits[2])
  xRentals <- subset(xRentals, transValue >= rentLimits[1] & 
                     transValue <= rentLimits[2])
  
  # Combine and clean up memory
  allTrans <- rbind(xSales, xRentals)
  rm(xSales); rm(xRentals); gc()
  
 ## Limit geography shapefiles to transaction extent  

  # Suburbs
  studySuburbs <- subShp[(which(subShp@data$NAME_2006 %in% 
                            names(table(allTrans$suburb)))), ]
  
  # Post Codes
  studyPostCodes <- postCodeShp[(which(postCodeShp@data$POA_2006 %in% 
                                  names(table(allTrans$postCode)))), ]
  
  # SLA1
  studySLA1s <- sla1Shp[(which(sla1Shp@data$SLA_NAME11 %in% 
                                    names(table(allTrans$sla1)))), ]
  
  # LGAs
  studyLGAs <- lgaShp[(which(lgaShp@data$LGA_NAME11 %in% 
                                    names(table(allTrans$lga)))), ]
  
  
 ## Determine which geographies meet which time thresholds
  
  # Yearly threshold
  yearThres <- mapply(prrGeoLimit, 
                      locField=c('postCode', 'sla1', 'suburb', 'lga'), 
                      MoreArgs=list(timeField='transYear',
                                     transData=allTrans,
                                     geoTempLimit=3))

  names(yearThres) <- paste0(rep("YT_"),
                             rep(c('both', 'house', 'unit', 'either'), 4),
                             rep("_", 16),
                             c(rep('postCode',4), rep('sla1',4),
                               rep('suburb',4), rep('lga', 4)))
  
  # Quarterly threshold
  qtrThres <- mapply(prrGeoLimit, 
                     locField=c('postCode', 'sla1', 'suburb', 'lga'), 
                     MoreArgs=list(timeField='transQtr',
                                   transData=allTrans,
                                   geoTempLimit=3))
  
  names(qtrThres) <- paste0(rep("QT_"),
                             rep(c('both', 'house', 'unit', 'either'), 4),
                             rep("_", 16),
                             c(rep('postCode',4), rep('sla1',4),
                               rep('suburb',4), rep('lga', 4)))
  

 ## Add designators to transactions
  
  # Yearly thresholds
  allTrans <- applyThres(yearThres[1:4], allTrans, 'YT', 'postCode')
  allTrans <- applyThres(yearThres[5:8], allTrans, 'YT', 'sla1')
  allTrans <- applyThres(yearThres[9:12], allTrans, 'YT', 'suburb')
  allTrans <- applyThres(yearThres[13:16], allTrans, 'YT', 'lga')
  
  # Quarterly thresholds
  allTrans <- applyThres(qtrThres[1:4], allTrans, 'QT', 'postCode')
  allTrans <- applyThres(qtrThres[5:8], allTrans, 'QT', 'sla1')
  allTrans <- applyThres(qtrThres[9:12], allTrans, 'QT', 'suburb')
  allTrans <- applyThres(qtrThres[13:16], allTrans, 'QT', 'lga')
  
 ## TODO:  Save for combining neighboring localities potential

### Write out workspace and .csv
  
 ## Write workspace

  save.image(paste0(dataPath, 'prrWrkspc.RData'))
             
 ## Write .csv
 
  write.csv(allTrans, paste0(dataPath, 'cleanData.csv'), row.names=F)

################################################################################
################################################################################
 