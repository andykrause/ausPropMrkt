################################################################################
#                                                                              #
#  Predictive Accuracy Analysis, PRR Model comparison Study                    #
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
  library(grid)

 ## Source Files

  # File containing function for working with prr and APM data
  source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
              'master/prrFunctions.R'))

### Load Data ------------------------------------------------------------------  

 ## Set the path to the data

  dataPath <- "C:/Dropbox/Australia Data/ausPropData/melData/"

 ## Load in saved workspace

  load(paste0(dataPath, 'cleanData.RData'))

 ## Load the calculated yield data
  
  yieldData <- read.csv(paste0(dataPath, 'rawresults.csv'))
  
 ## Load in the additional spatial variables
    
  spatVar <- read.csv(paste0(dataPath, 'prrSpatialVariables.csv'), header=T)

### Prepare data -------------------------------------------------------------------------
  
 ## Convert spatial distances to meters
  
  # Set conversion factor
  convFactor <- 92000
  
  # Make conversions
  for(cI in 2:ncol(spatVar)){
    spatVar[ ,cI] <- spatVar[ ,cI] * convFactor
  }

 ## Join spatial variables to yieldData
  
  yieldData <- merge(yieldData, spatVar, by='AddressID')
  
 ## Create Distance Categories
  
  # Set breaks
  distBreaks <- c(0,250, 500, 1000, 2000, 4000, Inf)
  
  # Create new variables with the breaks
  yieldData$train <- as.numeric(cut(yieldData$trainDist, breaks=distBreaks))
  yieldData$bus <- as.numeric(cut(yieldData$busDist, breaks=distBreaks))
  yieldData$tram <- as.numeric(cut(yieldData$tramDist, breaks=distBreaks))
  yieldData$seven <- as.numeric(cut(yieldData$seven11Dist, breaks=distBreaks))
  
 ## Data cleaning
  
  # Remove very high/low yields
  yieldData <- subset(yieldData, yield > .01 & yield < .09)
  
  # Remove unnecessary fields
  kill <- grep('QT', names(yieldData))
  yieldData <- yieldData[ ,-kill]
  kill <- grep('YT', names(yieldData))
  yieldData <- yieldData[ ,-kill]

 ## Split yield data into units and houses
  
  unitYields <- subset(yieldData, PropertyType == 'Unit')
  houseYields <- subset(yieldData, PropertyType == 'House')

### Build basic models looking into trains and trams -------------------------------------  
  
 ## Set the base specification
  
  baseSpec <- as.formula(yield ~ as.factor(transQtr) + as.factor(suburb) + 
                          as.factor(Bedrooms) + HasGarage + HasPool + 
                           HasAirConditioning)
  
 ## build a base model
  
  baseModel <- lm(baseSpec, data=yieldData)
  
  unitModel <- lm(baseSpec, data=unitYields)
  houseModel <- lm(baseSpec, data=houseYields)
  
 ## Add in spatial factors (ring model)
  
  unitSpat <- lm(update(baseSpec, . ~ . + as.factor(train) + as.factor(tram)),
                 data=unitYields)
  houseSpat <- lm(update(baseSpec, . ~ . + as.factor(train) + as.factor(tram)),
                   data=houseYields)
  

 ## Split by sale/lease
  SunitSpat <- lm(update(baseSpec, . ~ . + as.factor(train) + as.factor(tram) + transValue),
                 data=unitYields[unitYields$transType == 'sale',])
  RunitSpat <- lm(update(baseSpec, . ~ . + as.factor(train) + as.factor(tram) + transValue),
                 data=unitYields[unitYields$transType == 'rent',])
  
  
  
  
  