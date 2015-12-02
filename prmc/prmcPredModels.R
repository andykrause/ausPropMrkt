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

  #dataPath <- "C:/Dropbox/Australia Data/ausPropData/melData/"
  dataPath <- "D:/data/research/priceRentMethComp/"

 ## Load in saved workspace
  
  load(paste0(dataPath, 'analysisResults.RData'))

### Create and extract necessary data ------------------------------------------
  
 ## Yield indexes  
  
  metroYields <- prrGetYields(metroData)
  lgaYields <- prrGetYields(lgaData)
  slaYields <- prrGetYields(slaData)
  postcodeYields <- prrGetYields(postcodeData)
  suburbYields <- prrGetYields(suburbData)

  # Direct match data  
  dmData$uID <- 1:nrow(dmData)

### Estimate prediction errors -------------------------------------------------  
  
 ## Estimate raw prediction errors

  # Metro Level
  metroRes <- prrPredModelWrap(dmData, metroYields)
  metroResU <- prrPredModelWrap(dmData, metroYields, byUse=TRUE)
  
  # LGA Level
  lgaRes <- prrPredModelWrap(dmData, lgaYields, byGeog=TRUE, geoField='lga')
  lgaResU <- prrPredModelWrap(dmData, lgaYields, byGeog=TRUE, 
                              geoField='lga', byUse=TRUE)
  
  # Suburb Level
  suburbRes <- prrPredModelWrap(dmData, suburbYields, byGeog=TRUE, 
                                geoField='suburb')
  suburbResU <- prrPredModelWrap(dmData, suburbYields, byGeog=TRUE, 
                                 geoField='suburb', byUse=TRUE)

 ## Attached data to the matched dataset
  
  # Metro Level
  dmData$mMed <- metroRes$median$error[match(dmData$uID, metroRes$median$uID)]
  dmData$mMedU <- metroResU$median$error[match(dmData$uID, metroResU$median$uID)]
  dmData$mImp <- metroRes$impute$error[match(dmData$uID, metroRes$impute$uID)]
  dmData$mImpU <- metroResU$impute$error[match(dmData$uID, metroResU$impute$uID)]
  dmData$mMat <- metroRes$match$error[match(dmData$uID, metroRes$match$uID)]
  dmData$mMatU <- metroResU$match$error[match(dmData$uID, metroResU$match$uID)]

  # LGA Level
  dmData$lMed <- lgaRes$median$error[match(dmData$uID, lgaRes$median$uID)]
  dmData$lMedU <- lgaResU$median$error[match(dmData$uID, lgaResU$median$uID)]
  dmData$lImp <- lgaRes$impute$error[match(dmData$uID, lgaRes$impute$uID)]
  dmData$lImpU <- lgaResU$impute$error[match(dmData$uID, lgaResU$impute$uID)]
  dmData$lMat <- lgaRes$match$error[match(dmData$uID, lgaRes$match$uID)]
  dmData$lMatU <- lgaResU$match$error[match(dmData$uID, lgaResU$match$uID)]

  # Suburb Level
  dmData$sMed <- suburbRes$median$error[match(dmData$uID, suburbRes$median$uID)]
  dmData$sMedU <- suburbResU$median$error[match(dmData$uID, suburbResU$median$uID)]
  dmData$sImp <- suburbRes$impute$error[match(dmData$uID, suburbRes$impute$uID)]
  dmData$sImpU <- suburbResU$impute$error[match(dmData$uID, suburbResU$impute$uID)]
  dmData$sMat <- suburbRes$match$error[match(dmData$uID, suburbRes$match$uID)]
  dmData$sMatU <- suburbResU$match$error[match(dmData$uID, suburbResU$match$uID)]

 ## Convert to absolute values
  
  absPredResults <- lapply(dmData[ ,(which(colnames(dmData) == 'mMed'):
                                      which(colnames(dmData) == 'sMatU'))], abs)

 ## Calculate the median absolute error  
  
  # Make calc
  absPredMed <- lapply(absPredResults, median, na.rm=TRUE)

  # Convert into a table
  absPredMed <- as.matrix(unlist(absPredMed))
  predTable <- data.frame(metro=absPredMed[1:6],
                          lga=absPredMed[7:12],
                          suburb=absPredMed[13:18])
  rownames(predTable) <- c('Median', 'Median by Use', 'Impute', 'Impute by Use',
                           'Match', 'Match by Use')

 ## Calculate the hit rate
  
  # Build function to count 'non-hits'
  countNA <- function(x){length(which(is.na(x)))/length(x)}

  # Make calc
  hitRate <- lapply(absPredResults, countNA)

  # Convert into a table
  hitRate <- 1 - as.matrix(unlist(hitRate))
  hrTable <- data.frame(metro=hitRate[1:6],
                        lga=hitRate[7:12],
                        suburb=hitRate[13:18])
  rownames(hrTable) <- c('Median', 'Median by Use', 'Impute', 'Impute by Use',
                         'Match', 'Match by Use')

### Save workspace -------------------------------------------------------------

  save(dmData, predTable, hrTable, 
       file=paste0(dataPath, 'predModelResults.RData'))
  


