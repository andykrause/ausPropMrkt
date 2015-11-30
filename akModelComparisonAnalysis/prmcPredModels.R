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

# File containing function for analyzing data
source(paste0('https://raw.githubusercontent.com/andykrause/',
              'dataAnalysisTools/master/stShardFunctions.R'))


### Load Data ------------------------------------------------------------------  

 ## Set the path to the data

  dataPath <- "C:/Dropbox/Australia Data/ausPropData/melData/"

 ## Load in saved workspace

  load(paste0(dataPath, 'vizResults.RData'))

### Create the yield indexes
  
prrGetYields <- function(prrObj){
    
    mixYields <- prrObj$mix$comp
    
    mix <- list(median=subset(mixYields, method=='Median'),
                impute=subset(mixYields, method=='Impute'),
                match=subset(mixYields, method=='Match'))
    
    houseYields <- subset(prrObj$use$comp, use=='House')
    
    house <- list(median=subset(houseYields, method=='Median'),
                  impute=subset(houseYields, method=='Impute'),
                  match=subset(houseYields, method=='Match'))
    
    unitYields <- subset(prrObj$use$comp, use=='Unit')
    
    unit <- list(median=subset(unitYields, method=='Median'),
                 impute=subset(unitYields, method=='Impute'),
                 match=subset(unitYields, method=='Match'))
    
    return(list(mix=mix,
                house=house,
                unit=unit))
    
}
  
metroYields <- prrGetYields(metroData)
lgaYields <- prrGetYields(lgaData)
slaYields <- prrGetYields(slaData)
postcodeYields <- prrGetYields(postcodeData)
suburbYields <- prrGetYields(suburbData)

dmData <- subset(dmData, PropertyType == 'House' | PropertyType == 'Unit')
dmData$uID <- 1:nrow(dmData)

metroRes <- ebmWrap(dmData, metroYields)
metroResU <- ebmWrap(dmData, metroYields, byUse=TRUE)
lgaRes <- ebmWrap(dmData, lgaYields, byGeog=TRUE, geoField='lga')
lgaResU <- ebmWrap(dmData, lgaYields, byGeog=TRUE, geoField='lga', byUse=TRUE)
suburbRes <- ebmWrap(dmData, suburbYields, byGeog=TRUE, geoField='suburb')
suburbResU <- ebmWrap(dmData, suburbYields, byGeog=TRUE, geoField='suburb',
                      byUse=TRUE)


dmData$mMed <- metroRes$median$error[match(dmData$uID, metroRes$median$uID)]
dmData$mMedU <- metroResU$median$error[match(dmData$uID, metroResU$median$uID)]
dmData$mImp <- metroRes$impute$error[match(dmData$uID, metroRes$impute$uID)]
dmData$mImpU <- metroResU$impute$error[match(dmData$uID, metroResU$impute$uID)]
dmData$mMat <- metroRes$match$error[match(dmData$uID, metroRes$match$uID)]
dmData$mMatU <- metroResU$match$error[match(dmData$uID, metroResU$match$uID)]

dmData$lMed <- lgaRes$median$error[match(dmData$uID, lgaRes$median$uID)]
dmData$lMedU <- lgaResU$median$error[match(dmData$uID, lgaResU$median$uID)]
dmData$lImp <- lgaRes$impute$error[match(dmData$uID, lgaRes$impute$uID)]
dmData$lImpU <- lgaResU$impute$error[match(dmData$uID, lgaResU$impute$uID)]
dmData$lMat <- lgaRes$match$error[match(dmData$uID, lgaRes$match$uID)]
dmData$lMatU <- lgaResU$match$error[match(dmData$uID, lgaResU$match$uID)]

dmData$sMed <- suburbRes$median$error[match(dmData$uID, suburbRes$median$uID)]
dmData$sMedU <- suburbResU$median$error[match(dmData$uID, suburbResU$median$uID)]
dmData$sImp <- suburbRes$impute$error[match(dmData$uID, suburbRes$impute$uID)]
dmData$sImpU <- suburbResU$impute$error[match(dmData$uID, suburbResU$impute$uID)]
dmData$sMat <- suburbRes$match$error[match(dmData$uID, suburbRes$match$uID)]
dmData$sMatU <- suburbResU$match$error[match(dmData$uID, suburbResU$match$uID)]

xRes <- lapply(dmData[,18:35], abs)

xMed <- lapply(xRes, median, na.rm=TRUE)
xxMed <- as.matrix(unlist(xMed))

yMed <- data.frame(metro=xxMed[1:6],
                   lga=xxMed[7:12],
                   suburb=xxMed[13:18])
rownames(yMed) <- c('Median', 'Median by Use', 'Impute', 'Impute by Use',
                    'Match', 'Match by Use')

countNA <- function(x){length(which(is.na(x)))/length(x)}
xHR <- lapply(xRes, countNA)

xxHR <- as.matrix(unlist(xHR))

yHR <- data.frame(metro=xxHR[1:6],
                   lga=xxHR[7:12],
                   suburb=xxHR[13:18])
yHR <- 1- yHR
rownames(yHR) <- c('Median', 'Median by Use', 'Impute', 'Impute by Use',
                    'Match', 'Match by Use')







