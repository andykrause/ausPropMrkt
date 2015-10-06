
### Preliminary Commands ---------------------------------------------------------------------------

 ## Load Libraries

  library(plyr)
  library(dplyr)
  library(ggplot2)
  library(reshape2)
  library(stringr)

 ## Source Files

  # File containing function for working with prr and APM data
  source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                 'master/prrFunctions.R'))

 ## Set the path to the raw data
  
  dataPath <- "C:/Dropbox/Australia Data/ausPropData/melData/"
  saleFile <- 'sales10_15.csv'
  rentFile <- 'rents10_15.csv'

### Read in raw data ----------------------------------------------------------------------------------
  
 ## Read in Data  
  
  rawSales <- read.csv(paste0(dataPath, saleFile),
                       stringsAsFactors = FALSE)
  rawRents <- read.csv(paste0(dataPath, rentFile),
                       stringsAsFactors = FALSE)

### Clean Data (TEMPORARY PROCESS TO BE REPLACED BY MORE FORMAL ONE LATER) -------------------------

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
  columnList <- c('GeographicalID', 'EventID', 'AddressID', 'FlatNumber', 'Suburb', 'Postcode',
                'transDate', 'transValue', 'transType', 'PropertyType', 'Property_Latitude',
                'Property_Longitude', 'AreaSize', 'Bedrooms', 'Baths', 'Parking','HasFireplace',
                'HasPool', 'HasGarage', 'HasAirConditioning')
  
  # Clip data to set columns
  sales <- rawSales[ ,columnList]
  rentals <- rawRents[ ,columnList]
  
  # Combine to make cleaning easier
  allTrans <- rbind(sales, rentals)
  
 ## Add additional time information
  
  # Add a yearly variable
  allTrans$transYear <- as.numeric(substr(allTrans$transDate, 1, 4))
  
  # Add a days count
  allTrans$transDays <- as.numeric(allTrans$transDate - (min(allTrans$transDate) - 1))
  
  # Add a quarter count
  allTrans$transQtr <- (allTrans$transDays %/% 91.25) + 1
  
 ## Removing missing values  
  
  # Missing transaction values
  allTrans <- subset(allTrans, !is.na(transValue))
  allTrans <- subset(allTrans, transValue  != 0)
  
  # Missing home characteristics
  allTrans <- subset(allTrans, !is.na(AreaSize))
  allTrans <- subset(allTrans, !is.na(Bedrooms))
  allTrans <- subset(allTrans, !is.na(Baths))
  
  # Missing lat/long
  allTrans <- subset(allTrans, !is.na(Property_Latitude) & !is.na(Property_Longitude))
  
 ## Remove suspect values
  
  # Set limits
  areaLimits <- c(40, 25000)
  bedLimits <- c(1, 8)
  bathLimits <- c(1, 8)
  rentLimits <- c(125, 2500)
  saleLimits <- c(150000, 4000000)

  # Remove by characteristic
  allTrans <- subset(allTrans, AreaSize >= areaLimits[1] & AreaSize <= areaLimits[2])
  allTrans <- subset(allTrans, Bedrooms >= bedLimits[1] & Bedrooms <= bedLimits[2])
  allTrans <- subset(allTrans, Baths >= bathLimits[1] & Baths <= bathLimits[2])
  
  # Split back out
  xSales <- subset(allTrans, transType == 'sale')
  xRentals <- subset(allTrans, transType == 'rent')
  
 ## Limit by geographic density
  
  # Set minimum number of transactions required in each geo area (currently suburbs)
  geoTempLimit <- 3 # Must have at least three transactions per year
  
  # Split Sales by use
  houseSales <- subset(xSales, PropertyType == 'House' & transType == 'sale')
  unitSales <- subset(xSales, PropertyType == 'Unit' & transType == 'sale')
  houseRentals <- subset(xRentals, PropertyType == 'House' & transType == 'rent')
  unitRentals <- subset(xRentals, PropertyType == 'Unit' & transType == 'rent')
  
  # Determine which suburbs meet criteria for each
  saleHTable <- table(houseSales$Suburb, houseSales$transYear)
  shKeep <- which(apply(saleHTable, 1, min) >= geoTempLimit)
  shGeo <- rownames(saleHTable[shKeep, ])
  saleUTable <- table(unitSales$Suburb, unitSales$transYear)
  suKeep <- which(apply(saleUTable, 1, min) >= geoTempLimit)
  suGeo <- rownames(saleUTable[suKeep, ])
  rentHTable <- table(houseRentals$Suburb, houseRentals$transYear)
  rhKeep <- which(apply(rentHTable, 1, min) >= geoTempLimit)
  rhGeo <- rownames(rentHTable[rhKeep, ])
  rentUTable <- table(unitRentals$Suburb, unitRentals$transYear)
  ruKeep <- which(apply(rentUTable, 1, min) >= geoTempLimit)
  ruGeo <- rownames(rentUTable[ruKeep, ])
  allUGeo <- intersect(intersect(intersect(shGeo, suGeo), rhGeo), ruGeo)
  
  # Limit sales and rents to these suburbs
  xSales <- xSales[xSales$Suburb %in% allUGeo, ]
  xRentals <- xRentals[xRentals$Suburb %in% allUGeo, ]
  
### Develop the cross regression comparison method -------------------------------------------------
  
 ## Set the specification (formula)
  
  regSpec <- log(transValue) ~ log(AreaSize) + Bedrooms + Baths + as.factor(Suburb) +
    as.factor(transQtr)
  
 ## Estimate models and make new predictions
  
  houseResults <- prrCrossReg(regSpec, 
                              subset(xSales, PropertyType == 'House'),
                              subset(xRentals, PropertyType == 'House'),
                              verbose=TRUE)
  
  unitResults <- prrCrossReg(regSpec, 
                             subset(xSales, PropertyType == 'Unit'),
                             subset(xRentals, PropertyType == 'Unit'),
                             verbose=TRUE)
  
 ## Calculate the ratio
  
  # Extract vales
  allValues <- rbind(houseResults$allData, unitResults$allData)
  
  # Calculate the ratio
  allValues$prRatio <- allValues$Price / (allValues$Rent * 52 / 12)

### Clean up memory
  
  rm(rentals); rm(sales); rm(xSales); rm(xRentals)
  gc()
  
    
### Analyzing the prRatios ------------------------------------------------------------------------
  
 ## Global analysis by year, qtr and days(^-1)

  globTrends <- prrMakeTrends(allValues)
  
 ## Global Trends by property type
  
  houseTrends <- prrMakeTrends(allValues[allValues$PropertyType == 'House',])
  unitTrends <- prrMakeTrends(allValues[allValues$PropertyType == 'Unit',])
  
 ## Trends by Suburb
  
  # Calculate trends by suburbs
  subTrends <- lapply(allUGeo, prrWrapper, xData=allValues)
  names(subTrends) <- allUGeo
   
  # Strip out yearly data
  subYears <- lapply(subTrends, function(x) x[1])
  subYears <- matrix(unlist(subYears), ncol=6, byrow=TRUE)
  subYears <- as.data.frame(subYears)
  rownames(subYears) <- allUGeo
  names(subYears) <- 2010:2015

 ## Trends by use by suburb
  
  # Calculate trends by suburbs by use
  subHouseTrends <- lapply(allUGeo, prrWrapper,
                           xData=allValues[allValues$PropertyType == 'House', ])
  names(subHouseTrends) <- allUGeo
  
  subUnitTrends <- lapply(allUGeo, prrWrapper, 
                          xData=allValues[allValues$PropertyType == 'Unit', ])
  names(subUnitTrends) <- allUGeo

  # Strip out yearly data
  subHouseYears <- lapply(subHouseTrends, function(x) x[1])
  subHouseYears <- matrix(unlist(subHouseYears), ncol=6, byrow=TRUE)
  subHouseYears <- as.data.frame(subHouseYears)
  rownames(subHouseYears) <- allUGeo
  
  subUnitYears <- lapply(subUnitTrends, function(x) x[1])
  subUnitYears <- matrix(unlist(subUnitYears), ncol=6, byrow=TRUE)
  subUnitYears <- as.data.frame(subUnitYears)
  rownames(subUnitYears) <- allUGeo
  
### Make some plots --------------------------------------------------------------------------------  

 ## By year (Global)  
  
  # Build data
  globGG <- melt(globTrends$year)
  names(globGG) <- c('Year', "PRR")
  globGGQ <- melt(globTrends$qtr)
  names(globGGQ) <- c('Quarter', "PRR")
  globGGD <- melt(globTrends$days)
  names(globGGD) <- c('Days_10', "PRR")

  # Plot
  ggplot(globGG, aes(x=Year, y=PRR)) + 
    geom_line(size=3) +
    ggtitle('Price to Rent Ratios in Melbourne\n Cross Regression Method')
  
  ggplot(globGGQ, aes(x=Quarter, y=PRR)) + 
    geom_line(size=3) +
    ggtitle('Price to Rent Ratios in Melbourne\n Cross Regression Method')
  
  ggplot(globGGD, aes(x=Days_10, y=PRR)) + 
    geom_line(size=3) +
    ggtitle('Price to Rent Ratios in Melbourne\n Cross Regression Method')
  
 ## By Use by Year
  
  # Build data
  typeGG <- melt(c(houseTrends$year, unitTrends$year))
  names(typeGG) <- c("PRR")
  typeGG$Year <- rep(2010:2015, 2)
  typeGG$Type <- c(rep('House', 6), rep('Unit', 6))
  typeGGQ <- melt(c(houseTrends$qtr, unitTrends$qtr))
  names(typeGGQ) <- c("PRR")
  typeGGQ$Qtr <- rep(1:21, 2)
  typeGGQ$Type <- c(rep('House', 21), rep('Unit', 21))
  typeGGD <- melt(c(houseTrends$days, unitTrends$days))
  names(typeGGD) <- c("PRR")
  typeGGD$Days_10 <- rep(1:184, 2)
  typeGGD$Type <- c(rep('House', 184), rep('Unit', 184))
  
  # Plot
  ggplot(typeGG, aes(x=Year, y=PRR, colour=Type)) + 
    geom_line(size=3) +
    ggtitle('Price to Rent Ratios in Melbourne\n Cross Regression Method')
  
  ggplot(typeGGQ, aes(x=Qtr, y=PRR, colour=Type)) + 
    geom_line(size=3) +
    ggtitle('Price to Rent Ratios in Melbourne\n Cross Regression Method')
  
  ggplot(typeGGD, aes(x=Days_10, y=PRR, colour=Type)) + 
    geom_line(size=2) +
    ggtitle('Price to Rent Ratios in Melbourne\n Cross Regression Method')
  
 ## By suburb by Year
  subYears$Suburb <- allUGeo
  subGG <- melt(subYears, id.vars='Suburb')
  names(subGG) <- c('Suburb', 'Year', 'PRR')
  
  ggplot(subGG, aes(x=Year, y=PRR, group=Suburb)) + 
    geom_line(size=.5) +
    ggtitle('Price to Rent Ratios in Melbourne by Suburb\n Cross Regression Method') +
    theme(legend.position='none')
  
 ## By sub by type by year
  subHouseYears$Suburb <- allUGeo
  subUnitYears$Suburb <- allUGeo
  diffYears <- subHouseYears[,1:6] - subUnitYears[,1:6]
  diffYears$Suburb <- allUGeo
  subHGG <- melt(subHouseYears, id.vars='Suburb')
  subUGG <- melt(subUnitYears, id.vars='Suburb')
  subDGG <- melt(diffYears, id.vars='Suburb')
  names(subHGG) <- names(subUGG) <-  names(subDGG) <- c('Suburb', 'Year', 'PRR')
  
  ggplot(subHGG, aes(x=Year, y=PRR, group=Suburb)) + 
    geom_line(size=.5) +
    ggtitle('Price to Rent Ratios for Houses in Melbourne by Suburb\n Cross Regression Method') +
    theme(legend.position='none')
  
  ggplot(subUGG, aes(x=Year, y=PRR, group=Suburb)) + 
    geom_line(size=.5) +
    ggtitle('Price to Rent Ratios for Units in Melbourne by Suburb\n Cross Regression Method') +
    theme(legend.position='none')
  
  ggplot(subDGG, aes(x=Year, y=PRR, group=Suburb)) + 
    geom_line(size=.5) +
    ggtitle('Difference between PPRs for Houses and Units in Melbourne by Suburb\n Cross Regression Method') +
    theme(legend.position='none')
  
  ### Export necessary objects to a .rData workspace for Shiny page --------------
  
  save(globGG, globGGD, globGGQ, subDGG, subGG, subHGG, subHouseYears,
       subUGG, subUnitYears, subYears, typeGG, typeGGD, typeGGQ, 
       globTrends, houseTrends,unitTrends, subHouseTrends, subUnitTrends,
       subTrends,
       file = paste0(dataPath, 'prr.rData'))
