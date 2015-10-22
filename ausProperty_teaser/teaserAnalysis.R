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
  source('/Users/gaschwanden/Documents/R_Workspace/ausPropMrkt/prrFunctions.R')

 ## Set the path to the data

  dataPath <- "/Users/gaschwanden/Documents/R_Workspace/ausPropMrkt"
  subGeoFile <- 'Vic_Suburbs.shp'
  lgaGeoFile <- 'Vic_LGAs.shp'
  sla1GeoFile <- 'Vic_SLA1.shp'
  postGeoFile <- 'Vic_PostCodes.shp'

### Read in and prepare data ---------------------------------------------------------------
  
 ## Read in transaction Data  
  
  # Conditional depending on reBuild
  if(reBuildData){
    source(paste0('/Users/gaschwanden/Documents/R_Workspace/ausPropMrkt/prmcDataPrep.R'))
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
    as.factor(postCode) + as.factor(transQtr) + ssInteg + ssChoice

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
  
 if(testCRMSens){
  
  # Specify two alternative models
  
  altSpec1 <- log(transValue) ~ Bedrooms + ssInteg + ssChoice +
     as.factor(transMonth) 
  
  altSpec2 <- log(transValue) ~ log(AreaSize) + Bedrooms + Baths + 
    HasPool + HasGarage + HasAirConditioning + HasFireplace +
    as.factor(postCode) + as.factor(transMonth) 
  
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
  
 ## Global
  
  # Basic
  glob <- prrTrender(list('transYear', 'transQtr', 'transMonth'), xData=xTrans)
  globY <- glob$transYear
  globQ <- glob$transQtr
  
  # By Use
  globBU <- prrTrender(list('transYear', 'transQtr', 'transMonth'),
                       xData=xTrans, byUse=TRUE)
  globBUY <- globBU$transYear
  globBUQ <- globBU$transQtr
  
  # By use weighted
  globBUW <- prrTrender(list('transYear', 'transQtr', 'transMonth'),
                        xData=xTrans, byUse=TRUE, weighted=TRUE)
  globBUWY <- globBUW$transYear
  globBUWQ <- globBUW$transQtr
  
  
 ## All lgas 
  
  # Basic
 lgaY <- prrTrender('transYear', xData=xTrans, geog='lga', geogName='all')
 lgaQ <- prrTrender('transQtr', xData=xTrans, geog='lga', geogName='all')
  
  # By Use
 lgaYU <- prrTrender('transYear', xData=xTrans, geog='lga', 
                      geogName='all', byUse=TRUE)
 lgaQU <- prrTrender('transQtr', xData=xTrans, geog='lga', 
                      geogName='all', byUse=TRUE)
  
  # By use weighted
 lgaYUW <- prrTrender('transYear', xData=xTrans, geog='lga',
                       geogName='all', byUse=TRUE, weighted=TRUE)
 lgaQUW <- prrTrender('transQtr', xData=xTrans, geog='lga',
                       geogName='all', byUse=TRUE, weighted=TRUE)
  
 ## All SLA1s 
  
  # Basic
  slaY <- prrTrender('transYear', xData=xTrans, geog='sla1', geogName='all')
  slaQ <- prrTrender('transQtr', xData=xTrans, geog='sla1', geogName='all')
  
  # By Use
  slaYU <- prrTrender('transYear', xData=xTrans, geog='sla1', 
                      geogName='all', byUse=TRUE)
  slaQU <- prrTrender('transQtr', xData=xTrans, geog='sla1', 
                      geogName='all', byUse=TRUE)
  
  # By use weighted
  slaYUW <- prrTrender('transYear', xData=xTrans, geog='sla1',
                       geogName='all', byUse=TRUE, weighted=TRUE)
  slaQUW <- prrTrender('transQtr', xData=xTrans, geog='sla1',
                       geogName='all', byUse=TRUE, weighted=TRUE)
  
  ## All PostCodes 
  
  # Basic
  pcY <- prrTrender('transYear', xData=xTrans, geog='postCode', geogName='all')
  pcQ <- prrTrender('transQtr', xData=xTrans, geog='postCode', geogName='all')
  
  # By Use
  pcYU <- prrTrender('transYear', xData=xTrans, geog='postCode', 
                      geogName='all', byUse=TRUE)
  pcQU <- prrTrender('transQtr', xData=xTrans, geog='postCode', 
                      geogName='all', byUse=TRUE)
  
  # By use weighted
  pcYUW <- prrTrender('transYear', xData=xTrans, geog='postCode',
                       geogName='all', byUse=TRUE, weighted=TRUE)
  pcQUW <- prrTrender('transQtr', xData=xTrans, geog='postCode',
                       geogName='all', byUse=TRUE, weighted=TRUE)
  
  ## All Suburbs
  
  # Basic
  subY <- prrTrender('transYear', xData=xTrans, geog='suburb', geogName='all')
  subQ <- prrTrender('transQtr', xData=xTrans, geog='suburb', geogName='all')
  
  # By Use
  subYU <- prrTrender('transYear', xData=xTrans, geog='suburb', 
                     geogName='all', byUse=TRUE)
  subQU <- prrTrender('transQtr', xData=xTrans, geog='suburb', 
                     geogName='all', byUse=TRUE)
  
  # By use weighted
  subYUW <- prrTrender('transYear', xData=xTrans, geog='suburb',
                      geogName='all', byUse=TRUE, weighted=TRUE)
  subQUW <- prrTrender('transQtr', xData=xTrans, geog='suburb',
                      geogName='all', byUse=TRUE, weighted=TRUE)

### Save Workspace -------------------------------------------------------------
  
  save.image(paste0(dataPath, 'prrWrkspc.RData'))
  save(globY, globQ, globBUY, globBUQ, globBUWY, globBUWQ, 
      lgaY, lgaQ, lgaYU, lgaQU, lgaYUW, lgaQUW,
      slaY, slaQ, slaYU, slaQU, slaYUW, slaQUW,
      pcY, pcQ, pcYU, pcQU, pcYUW, pcQUW,
      subY, subQ, subYU, subQU, subYUW, subQUW,
      subShp, lgaShp, sla1Shp, postCodeShp,
      file = paste0(dataPath, 'plotObjs.rData'))
  
### Visualize Results ----------------------------------------------------------  
  
  
  ggplot(prrObj, aes(x=time, y=value)) + geom_line()
  
  
  
  
  
  
  
  
  
  
  
  
  