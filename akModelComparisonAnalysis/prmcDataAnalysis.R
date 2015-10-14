################################################################################
#                                                                              #
#  Data Analysis Code for the Price to Rent Ratio Model comparison Study       #
#                                                                              #
################################################################################

### Preliminary Commands -------------------------------------------------------

  ## Set parameters

  reBuildData <- FALSE     # Enter TRUE if you wish to rebuild data from raw
  testCRMSens <- FALSE     # Test the sensitivity of the result to alternate
                             # regression models?
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

### Cross regression comparison method -----------------------------------------

 ## Set the specification (formula)

 regSpec <- log(transValue) ~ log(AreaSize) + Bedrooms + Baths + 
    as.factor(postCode) + as.factor(transQtr) +

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

 ## Calculate sensitivity to the model
  
 if(testCRMSpec){
  
  # Specify two alternative models
  
  altSpec1 <- log(transValue) ~ Bedrooms + ssInteg + ssChoice +
     as.factor(transMonth) 
  
  altSpec2 <- log(transValue) ~ log(AreaSize) + Bedrooms + Baths + 
    HasPool + HasGarage + HasAirConditioning + HasFireplace +
    as.factor(postCode) + as.factor(transMonth) + ssChoice + ssInteg
  
  # For houses
  houseResultsA1 <- prrCrossReg(altSpec1, 
                              subset(allTrans, transType == 'sale' &
                                       PropertyType == 'House' & 
                                       YT_house_postCode == 1),
                              subset(allTrans, transType == 'rent' &
                                       PropertyType == 'House' & 
                                       YT_house_postCode == 1),
                              verbose=TRUE)
  houseResultsA2 <- prrCrossReg(altSpec2, 
                              subset(allTrans, transType == 'sale' &
                                       PropertyType == 'House' & 
                                       YT_house_postCode == 1),
                              subset(allTrans, transType == 'rent' &
                                       PropertyType == 'House' & 
                                       YT_house_postCode == 1),
                              verbose=TRUE)
  
  # For Units
  unitResultsA1 <- prrCrossReg(altSpec1, 
                             subset(allTrans, transType == 'sale' &
                                      PropertyType == 'Unit' & 
                                      YT_unit_postCode == 1),
                             subset(allTrans, transType == 'rent' &
                                      PropertyType == 'Unit' & 
                                      YT_unit_postCode == 1),
                             verbose=TRUE)
  unitResultsA2 <- prrCrossReg(altSpec2, 
                             subset(allTrans, transType == 'sale' &
                                      PropertyType == 'Unit' & 
                                      YT_unit_postCode == 1),
                             subset(allTrans, transType == 'rent' &
                                      PropertyType == 'Unit' & 
                                      YT_unit_postCode == 1),
                             verbose=TRUE)
  
  # Extract vales
  crmAltValues1 <- rbind(houseResultsA1$results, unitResultsA1$results)
  crmAltValues2 <- rbind(houseResultsA2$results, unitResultsA2$results)
  
  # Calculate the ratio
  crmAltValues1$prRatio <- crmAltValues1$Price / (crmAltValues1$Rent * 52 / 12)
  crmAltValues2$prRatio <- crmAltValues2$Price / (crmAltValues2$Rent * 52 / 12)
  
  a1Diff <- crmValues$prRatio - crmAltValues1$prRatio
  a2Diff <- crmValues$prRatio - crmAltValues2$prRatio
  
  summary(a1Diff)
  summary(a2Diff)
  
 ## Make Comparisons
  
  # Add values to full dataset
  allTrans$prRatio <- crmValues$prRatio[match(allTrans$UID, crmValues$UID)]
  xTrans <- subset(allTrans, !is.na(prRatio))
  xTrans$pr1 <- crmAltValues1$prRatio[match(xTrans$UID, crmAltValues1$UID)]
  xTrans$pr2 <- crmAltValues2$prRatio[match(xTrans$UID, crmAltValues2$UID)]
  
  # Make Comparison plots
  plot(tapply(xTrans$prRatio, xTrans$transQtr, median), type='l', col=2, lwd=3,
       xlab='Quarter', ylab='PRR')
  lines(tapply(xTrans$pr1, xTrans$transQtr, median), type='l', col=1, lwd=1)
  lines(tapply(xTrans$pr2, xTrans$transQtr, median), type='l', col=4, lwd=1)
  legend('bottomleft', c("Main", 'Alt1', 'Alt2'), col=c(2,1,4), lwd=c(3,1,1))
  
 ## Clean up memory
  rm(unitResultsA1, unitResultsA2, houseResultsA1, houseResultsA2)
  rm(crmAltValues1, crmAltValues2); gc()
  
 } else {
 
   allTrans$prRatio <- crmValues$prRatio[match(allTrans$UID, crmValues$UID)]
   xTrans <- subset(allTrans, !is.na(prRatio)) 
       
 }

### Output raw results ---------------------------------------------------------
  
  write.csv(xTrans, paste0(dataPath, 'rawResults.csv'), row.names=FALSE)
    
### Break down results by dimensions -------------------------------------------
  
  ## Global analysis by year, qtr and days(^-1)
  
  glob <- lapply(list('transYear', 'transQtr', 'transMonth'),
                 prrMakeTrends, byUse=FALSE, xData=xTrans)
  
  ## Global Trends by property type
  
  globUse <- lapply(list('transYear', 'transQtr', 'transMonth'),
                    prrMakeTrends, byUse=TRUE, xData=xTrans)
  globUseW <- lapply(list('transYear', 'transQtr', 'transMonth'),
                    prrMakeTrends, byUse=TRUE, xData=xTrans, weighted=TRUE)
  
  ## By Suburb
  
   subYear <- mapply(prrMakeTrends, timeField='transYear',  
             geogName=names(table(xTrans$suburb[xTrans$YT_either_suburb==1])), 
             MoreArgs=list(xData = xTrans[xTrans$YT_either_suburb==1,],
                           geog='suburb'))
  
   subYearU <- mapply(prrMakeTrends, timeField='transYear',  
                      geogName=names(table(xTrans$suburb[xTrans$YT_both_suburb==1])), 
                      MoreArgs=list(xData = xTrans[xTrans$YT_both_suburb==1,],
                                    geog='suburb', byUse=TRUE))
   