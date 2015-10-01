
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

### Read in raw data ----------------------------------------------------------------------------------
  
 ## Read in Data  
  
  rawSales <- read.csv(paste0(dataPath, 'sales10_15.csv'),
                       stringsAsFactors = FALSE)
  rawRents <- read.csv(paste0(dataPath, 'rents10_15.csv'),
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
  geoLimit <- 10
  
  # Determine which suburbs meet criteria for each
  salesGeo <- names(table(xSales$Suburb)[table(xSales$Suburb) >= geoLimit])
  rentGeo <- names(table(xRentals$Suburb)[table(xRentals$Suburb) >= geoLimit])
  
  # Determine which suburbs meet criteria in both sales and rents
  allGeo <- intersect(salesGeo, rentGeo)
  
  # Limit sales and rents to these suburbs
  xSales <- xSales[xSales$Suburb %in% allGeo, ]
  xRentals <- xRentals[xRentals$Suburb %in% allGeo, ]
  
### Develop the cross regression comparison method -------------------------------------------------
  
 ## Set the specification (formula)
  
  regSpec <- log(transValue) ~ log(AreaSize) + Bedrooms + Baths + as.factor(Suburb) +
    as.factor(transQtr)
  
 ## Estimate models and make new predictions
  
  xResults <- prrCrossReg(regSpec, xSales, xRentals, verbose=TRUE)
  
 ## Calculate the ratio
  
  # Extract vales
  allValues <- xResults$allData
  
  # Calculate the ratio
  allValues$prRatio <- allValues$Price / (allValues$Rent * 52 / 12)
  
  ### Next up:  Analysing the ratios
  
