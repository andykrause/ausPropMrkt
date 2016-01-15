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

  dataPath <- "C:/data/research/priceRentMethComp/"

 ## Load in saved workspace

  load(paste0(dataPath, 'cleanData.RData'))
  
 ## Load the calculated yield data
  
  yieldData <- read.csv(paste0(dataPath, 'rawresults.csv'))
  
 ## Load in the additional spatial variables
    
  spatVar <- read.csv(paste0(dataPath, 'spatialData/prrSpatialVariables.csv'), header=T)

 ## Load in suburb designations
  
  subDes <- read.csv('c:/data/aus/vic/geographic/melbourne/suburbDesignations.csv',
                     header=T)
  
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
  
 ## Join suburb designation
  
  yieldData <- merge(yieldData, subDes, by='suburb')
  
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
  yieldData <- subset(yieldData, yield >= .01 & yield <= .09)
  
  # Remove unnecessary fields
  kill <- grep('QT', names(yieldData))
  yieldData <- yieldData[ ,-kill]
  kill <- grep('YT', names(yieldData))
  yieldData <- yieldData[ ,-kill]

 ## Split yield data into units and houses
  
  unitYields <- subset(yieldData, PropertyType == 'Unit')
  houseYields <- subset(yieldData, PropertyType == 'House')
  
 ## Create a 'recent' dataset
  
  yieldData1415 <- subset(yieldData, transQtr >= 17)
  unitYields1415 <- subset(unitYields, transQtr >= 17)
  houseYields1415 <- subset(houseYields, transQtr >= 17)
  
 ## Split dataset by suburbs
  
  unitsInner1415 <- subset(unitYields1415, location == 'Inner')
  unitsMiddle1415 <- subset(unitYields1415, location == 'Middle')
  unitsOuter1415 <- subset(unitYields1415, location == 'Outer')
  houseInner1415 <- subset(houseYields1415, location == 'Inner')
  houseMiddle1415 <- subset(houseYields1415, location == 'Middle')
  houseOuter1415 <- subset(houseYields1415, location == 'Outer')
  
### Build basic models looking into trains and trams -------------------------------------  
  
 ## Set the base specifications
  
  unitSpec <- as.formula(yield ~ as.factor(transQtr) + as.factor(suburb) + 
                          as.factor(Bedrooms) + HasGarage + HasPool + 
                           HasAirConditioning)
  
  houseSpec <- as.formula(yield ~ as.factor(transQtr) + as.factor(suburb) + 
                           as.factor(Bedrooms) + HasGarage + HasPool + 
                            HasAirConditioning + AreaSize)
  
 ## Build basic model (naive to microspatial impacts)

  unitModel <- lm(unitSpec, data=unitYields1415)
  houseModel <- lm(houseSpec, data=houseYields1415)
  
 ## Add in spatial factors
  
  # Linear  
  unitModel1415.lin <- lm(update(unitSpec, . ~ . + trainDist + tramDist),
                      data=unitYields1415)
  houseModel1415.lin <- lm(update(houseSpec, . ~ . + trainDist + tramDist), 
                       data=houseYields1415)
  
  # Log
  unitModel1415.log <- lm(update(unitSpec, . ~ . + log(trainDist) + log(tramDist)),
                           data=unitYields1415)
  houseModel1415.log <- lm(update(houseSpec, . ~ . + log(trainDist) + log(tramDist)), 
                            data=houseYields1415)
   
  # Ring
  unitModel1415.ring <- lm(update(unitSpec, . ~ . + as.factor(train) + as.factor(tram)),
                          data=unitYields1415)
  houseModel1415.ring <- lm(update(houseSpec, . ~ . + as.factor(train) + as.factor(tram)), 
                           data=houseYields1415)
  
  # Save for spline
  
 ## Check for spatial heterogeneity
  
  # Inner Suburbs
  unitInner1415.lin <- lm(update(unitSpec, . ~ . + trainDist + tramDist),
                          data=unitYields1415[unitYields1415$location == 'Inner',])
  
  houseInner1415.lin <- lm(update(unitSpec, . ~ . + trainDist + tramDist),
                            data=houseYields1415[houseYields1415$location == 'Inner',])
  # Middle Suburbs
  unitMiddle1415.lin <- lm(update(unitSpec, . ~ . + trainDist + tramDist),
                          data=unitYields1415[unitYields1415$location == 'Middle',])
  
  houseMiddle1415.lin <- lm(update(unitSpec, . ~ . + trainDist + tramDist),
                           data=houseYields1415[houseYields1415$location == 'Middle',])
  
  # Outer Suburbs
  unitOuter1415.lin <- lm(update(unitSpec, . ~ . + trainDist + tramDist),
                          data=unitYields1415[unitYields1415$location == 'Outer',])
  
  houseOuter1415.lin <- lm(update(unitSpec, . ~ . + trainDist + tramDist),
                           data=houseYields1415[houseYields1415$location == 'Outer',])
  
  
  
  
  
  