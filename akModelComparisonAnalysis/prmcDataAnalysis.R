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

### Read in data ---------------------------------------------------------------
  
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
