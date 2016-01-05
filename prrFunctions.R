##########################################################################################
#                                                                                        #
#  Suite of functions for analyzing price to rent ratios in Australia                    #
#                                                                                        #
##########################################################################################

### Function to convert various APM date structures into R date structure ----------------

fixAPMDates <- function(xDates      # Vector of dates to be fixed
                        )
  {
  
 ## Set required libraries
  
  require(stringr)

 ## Break down dates
  
  # Remove Time
  xDates <- gsub(" 0:00", "", xDates)
  
  # Find location of slashes
  sLoc <- matrix(unlist(str_locate_all(xDates, '/')), ncol=4, byrow=TRUE)[,1:2]
  
  # Correct Days
  days <- as.numeric(substr(xDates, 1, sLoc[ ,1] - 1))
  days <- ifelse(days < 10, paste0('0', days), as.character(days))
    
  # Correct Months
  months <- as.numeric(substr(xDates, sLoc[ ,1] + 1, sLoc[ ,2] - 1))
  months <- ifelse(months < 10, paste0('0', months), as.character(months))
  
  # Correct years
  years <- as.numeric(substr(xDates, sLoc[ ,2] + 1, 50))
  years <- ifelse(years < 2000, paste0('20', years), as.character(years))

 ## Recombine into R date format  
    
  newDates <- as.Date(paste0(days, '/' , months, '/', years), "%d/%m/%Y")

 ## Return Values  
  
  return(newDates)
}

### Regression function that creates imputed rent and sales values -----------------------

prrImputeReg <- function(formula,               # LM regression formula
                         saleData,              # Data containing sales
                         rentData,              # Data containing rentals
                         verbose = FALSE        # Show progress?
                         ){
  
 ## Estimate models and make new predictions
  
  # Esimate models
  if(verbose) cat('Estimating sale and rent models\n')
  saleModel <- lm(formula, data=saleData)
  rentModel <- lm(formula, data=rentData)
  
  # Make predictions of imputed values
  if(verbose) cat('Imputing values\n')
  impPrice <- exp(predict(saleModel, newdata=rentData))
  impRent <- exp(predict(rentModel, newdata=saleData))
  
  # Apply cross values
  if(verbose) cat('Stacking observed and imputed values\n')
  saleData$Price <- saleData$transValue
  rentData$Price <- impPrice
  saleData$Rent <- impRent
  rentData$Rent <- rentData$transValue
  
  # Combine data back together
  if(verbose) cat('Merging data\n')
  allData <- rbind(saleData, rentData)

  ## Extract model information
  saleModelInfo <- list(coef=summary(saleModel)$coefficients,
                        r2=summary(saleModel)$r.squared,
                        sigma=summary(saleModel)$r.squared,
                        resid=summary(saleModel)$residuals)
  rentModelInfo <- list(coef=summary(rentModel)$coefficients,
                        r2=summary(rentModel)$r.squared,
                        sigma=summary(rentModel)$r.squared,
                        resid=summary(rentModel)$residuals)
  
  
  
 ## Return values
  return(list(results = allData[ ,c('UID', 'Price', 'Rent')],
              saleModel = saleModelInfo,
              rentModel = rentModelInfo))
         
}

### Function to determine which geo areas meet use and time criteria ---------------------

prrGeoLimit <- function(transData,               # Dataframe of trans data
                        locField = 'locName',    # Field containing location
                        timeField = 'transYear', # Field containing time
                        geoTempLimit = 3         # Min trans per use/time/loc
){  
  
  # Split transactions by use
  houseSales <- subset(transData, PropertyType == 'House' &
                         transType == 'sale')
  unitSales <- subset(transData, PropertyType == 'Unit' & 
                        transType == 'sale')
  houseRentals <- subset(transData, PropertyType == 'House' & 
                           transType == 'rent')
  unitRentals <- subset(transData, PropertyType == 'Unit' & 
                          transType == 'rent')
  
  # Determine which suburbs meet criteria for each
  saleHTable <- table(houseSales[,locField], houseSales[,timeField])
  shKeep <- which(apply(saleHTable, 1, min) >= geoTempLimit)
  shGeo <- rownames(saleHTable[shKeep, ])
  saleUTable <- table(unitSales[,locField], unitSales[,timeField])
  suKeep <- which(apply(saleUTable, 1, min) >= geoTempLimit)
  suGeo <- rownames(saleUTable[suKeep, ])
  rentHTable <- table(houseRentals[,locField], houseRentals[,timeField])
  rhKeep <- which(apply(rentHTable, 1, min) >= geoTempLimit)
  rhGeo <- rownames(rentHTable[rhKeep, ])
  rentUTable <- table(unitRentals[,locField], unitRentals[,timeField])
  ruKeep <- which(apply(rentUTable, 1, min) >= geoTempLimit)
  ruGeo <- rownames(rentUTable[ruKeep, ])
  bothGeo <- intersect(intersect(intersect(shGeo, suGeo), rhGeo), ruGeo)
  houseGeo <- intersect(shGeo,rhGeo)
  unitGeo <- intersect(suGeo, ruGeo)
  eitherGeo <- union(houseGeo, unitGeo)
  
  # Create tables
  return(list(bothGeo = bothGeo,
              houseGeo = houseGeo,
              unitGeo = unitGeo,
              eitherGeo = eitherGeo))  
}

### Apply the threshold designations across all transactions -----------------------------

prrApplyThres <- function(thresData,       # Threshold data object from prrGeoLimit
                          transData,       # Set of transaction data
                          timePrefix='YT', # Which time was used YT or QT
                          geo="postCode"   # Which geo to use (one at a time)
                          ){
  
  # Pull out single designations
  both <- ifelse(transData[,geo] %in% thresData[[1]],1,0)
  house <- ifelse(transData[,geo] %in% thresData[[2]],1,0)
  unit <- ifelse(transData[,geo] %in% thresData[[3]],1,0)
  either <- ifelse(transData[,geo] %in% thresData[[4]],1,0)
  
  # Combine them
  all <- as.data.frame(cbind(both, house, unit, either))
  
  # Rename
  names(all) <- paste0(timePrefix, "_", names(all), "_",geo)
  
  # Add to existing transactions
  return(cbind(transData, all))
}

##########################################################################################
### New Australia Rent Yield Functions (works with stShard operations) -------------------

prrStsGeoWrap <- function(stsData,                    # Observation data frame
                          metrics,                    # Field(s) to calculate on
                          spaceField,                 # Field with space variable or 'all'
                          timeField,                  # Field with time variable
                          defDim='time',              # 'Space' or 'Time' as dimension
                          stsLimit,                   # Nbr of obs required per shard
                          calcs=list(median='median') # Type of calculation to do
                          ){
  
  ## Source stshard functions if not already done
  if(!exists('spaceTimeShard')) {
    source(paste0('https://raw.githubusercontent.com/andykrause/',
                  'dataAnalysisTools/master/stShardFunctions.R'))
  }
  
  
  ## Calculate prices and rents
  
  xPrice <- spaceTimeShard(stsData[stsData$transType == 'sale', ],
                           metric=metrics[1], spaceField=spaceField,
                           timeField=timeField, defDim=defDim, 
                           stsLimit=stsLimit, calcs=calcs)
  
  xRent <- spaceTimeShard(stsData[stsData$transType == 'rent', ],
                          metric=metrics[2], spaceField=spaceField,
                          timeField=timeField, defDim=defDim, 
                          stsLimit=stsLimit, calcs=calcs)
  
  ## If not doing all spatial areas
  
  if(spaceField != 'all'){
  
  # Trim results to geographies with both prices and rents  
    okNames <- intersect(names(xPrice[[2]]), names(xRent[[2]]))
    geoPrices <- xPrice[[4]]
    geoPrices <- geoPrices[geoPrices$spaceName %in% okNames,]
    geoRents <- xRent[[4]]
    geoRents <- geoRents[geoRents$spaceName %in% okNames,]
  
  # Convert to exportable table
    geoTable <- data.frame(timeName=geoPrices$timeName,
                           spaceName=geoPrices$spaceName,
                           price=geoPrices$median,
                           rent=geoRents$median,
                           yield=(geoRents$median * 52) / geoPrices$median)
  } else {
    
    geoTable <- data.frame(timeName=xPrice$stsDF$timeName,
                           spaceName='all',
                           price=xPrice$stsDF$median,
                           rent=xRent$stsDF$median,
                           yield=((xRent$stsDF$median * 52) / 
                             xPrice$stsDF$median))
    
  }
    
  ## Export data  
  
  return(list(stsDF=geoTable,
              priceStsTable=xPrice$stTable,
              rentStsTable=xRent$stTable))
}

### Function to compare price and rent on only matched properties ------------------------

prrSaleRentMatch <- function(sales,               # Data.frame of sales
                             rentals,             # Data.frame of rentals
                             matchField = 'ID',   # Field containing matching ID
                             saleField = 'Price', # Field containing sale price
                             rentField = 'Rent',  # Field containing rent 
                             timeField = 'Year'   # Field containing time breakdown
){

  ## Matching sales to rentals
  
  # Remove NAs in matchField
  xSales <- subset(sales, !is.na(sales[matchField]))
  xRentals <- subset(rentals, !is.na(rentals[matchField]))

  # Sort to order
  xSales <- xSales[order(xSales[,matchField]),]
  xRentals <- xRentals[order(xRentals[,matchField]),]
  
  # Extract matching field
  sMatch <- xSales[ ,matchField]
  rMatch <- xRentals[ ,matchField]
  
  # Perform cross match identification
  mSales <- xSales[!is.na(match(sMatch, rMatch)), ]
  mRentals <- xRentals[!is.na(match(rMatch, sMatch)), ]
  
  # Make the match
  mTrans <- merge(mSales[, c(matchField, saleField, timeField)],
                  mRentals[, c(matchField, rentField, timeField)],
                  by=matchField)
  
  # Rename Match Fields
  names(mTrans) <- c(matchField, 'saleValue', 'saleTime', 'rentValue', 'rentTime')
  
  ## Make time adjustments to matched transactions
  
  # Create the rent index
  rentTrend <- as.numeric(tapply(mTrans$rentValue, mTrans$rentTime, median))
  rentIndex <- rentTrend / rentTrend[1]
  
  # Create the sale index
  saleTrend <- as.numeric(tapply(mTrans$saleValue, mTrans$saleTime, median))
  saleIndex <- saleTrend / saleTrend[1]
  
  # Make the adjustments to the rentals
  rentAdj <- (rentIndex[as.numeric(as.factor(mTrans$saleTime))] /
                rentIndex[as.numeric(as.factor(mTrans$rentTime))])
  mTrans$adjRent <- mTrans$rentValue * rentAdj
  
  # Make the adjustments to the sales
  saleAdj <- (saleIndex[as.numeric(as.factor(mTrans$rentTime))] /
                saleIndex[as.numeric(as.factor(mTrans$saleTime))])
  mTrans$adjSale <- mTrans$saleValue * saleAdj
  
  # Calc Yields
  mTrans$saleYield <- (mTrans$adjRent * 52) / mTrans$saleValue
  mTrans$rentYield <- (mTrans$rentValue * 52) / mTrans$adjSale
  
 ## Add Location variables
  
  mTrans$lga <- xSales$lga[match(mTrans$AddressID, xSales$AddressID)]
  mTrans$sla1 <- xSales$sla1[match(mTrans$AddressID, xSales$AddressID)]
  mTrans$suburb <- xSales$suburb[match(mTrans$AddressID, xSales$AddressID)]
  mTrans$postCode <- xSales$postCode[match(mTrans$AddressID, xSales$AddressID)]
  mTrans$latitude <- xSales$Property_Latitude[match(mTrans$AddressID, 
                                                    xSales$AddressID)]
  mTrans$longitude <- xSales$Property_Longitude[match(mTrans$AddressID, 
                                                      xSales$AddressID)]
  mTrans$PropertyType <- xSales$PropertyType[match(mTrans$AddressID, 
                                                   xSales$AddressID)]

  ## Return Values    
  return(mTrans)  
}  

## Function to aggregate data by geography -----------------------------------------------

prrAggrGeoData <- function(geoList,         # List of sharded results (9 total)
                           indexList,       # List of price indexes (all, house, unit)
                           geoSplit=FALSE   # Using a geo smaller than all area?
){
  
  
  ## Build Mixed unweighted
    
  mixData <- prrAggrMethData(mmObj=geoList$mm$all,
                             irObj=geoList$ir$all,
                             dmObj=geoList$dm$all,
                             pIndex=indexList$all,
                             geoSplit=geoSplit)
  
  ## Build Mixed weighted
  
  if(!geoSplit){
    
    mixDataWgt <- mixData
    
  } else {
    
    mixDataWgt <- prrAggrMethData(mmObj=geoList$mm$all,
                                  irObj=geoList$ir$all,
                                  dmObj=geoList$dm$all,
                                  pIndex=indexList$all,
                                  geoSplit=geoSplit,
                                  wgt=TRUE)
  }
  
  ## Build Unit specific
  
  # Calculate separate dataset for house and units
  houseData <- prrAggrMethData(mmObj=geoList$mm$house,
                               irObj=geoList$ir$house,
                               dmObj=geoList$dm$house,
                               pIndex=indexList$house,
                               geoSplit=geoSplit)
  unitData <- prrAggrMethData(mmObj=geoList$mm$unit,
                              irObj=geoList$ir$unit,
                              dmObj=geoList$dm$unit,
                              pIndex=indexList$unit,
                              geoSplit=geoSplit)
  
  # Add labels
  houseData$comp$use <- 'House'
  houseData$diff$use <- 'House'
  unitData$comp$use <- 'Unit'
  unitData$diff$use <- 'Unit'
  
  # combine
  useData <- list(comp=rbind(houseData$comp,
                             unitData$comp),
                  diff=rbind(houseData$diff,
                             unitData$diff))
  
  ## Build unit specific weighted
  
  if(!geoSplit){
    
    useWgt <- prrWeightUses(houseData, unitData, geoList, 
                            pIndex=indexList$all, 
                            geoSplit=geoSplit) 
  } else {
    
   
   # Calculate separate dataset for house and units
  houseDataW <- prrAggrMethData(mmObj=geoList$mm$house,
                                irObj=geoList$ir$house,
                                dmObj=geoList$dm$house,
                                pIndex=indexList$house,
                                geoSplit=geoSplit,
                                wgt=TRUE)
  unitDataW <- prrAggrMethData(mmObj=geoList$mm$unit,
                               irObj=geoList$ir$unit,
                               dmObj=geoList$dm$unit,
                               pIndex=indexList$unit,
                               geoSplit=geoSplit,
                               wgt=TRUE)
  
  # Add labels
  houseDataW$comp$use <- 'House'
  houseDataW$diff$use <- 'House'
  unitDataW$comp$use <- 'Unit'
  unitDataW$diff$use <- 'Unit'
  
  # Calculate use specific data
  useWgt <- prrWeightUses(houseDataW, unitDataW, geoList, 
                          pIndex=indexList$all, geoSplit=TRUE)  
  }
  
  return(list(mix=mixData,
              mixWgt=mixDataWgt,
              use=useData,
              useWgt=useWgt))
  
}

## Function to aggregate different method's data -----------------------------------------

prrAggrMethData <- function(mmObj,          # Med Meth obj from prrStsGeoWrap
                            irObj,          # Cross Reg obj from spaceTimeShard()
                            dmObj,          # Match obj from spaceTimeShard()
                            pIndex,         # A price index at the time scale
                            wgt=FALSE,      # weight based observations
                            geoSplit=FALSE  # is not all geo areas
){  
  
  ## Isolate the correct data from each object
  
#   if(geoSplit){
    mmDF <- mmObj$stsDF[,c('timeName', 'spaceName', 'yield')]
#   } else {
#     mmDF <- mmObj$stsDF[ ,c('timeName', 'yield')]
#     mmDF$spaceName <- 'all'
#   }
    
  irDF <- irObj$stsDF
  dmDF <- dmObj$stsDF
  names(irDF)[2] <- names(dmDF)[2] <- 'yield'
  
  ## Determine the spatial areas that exist in all objects  
  
  # Extract space from each
  mmGeo <- levels(mmDF$spaceName)
  irGeo <- levels(as.factor(irDF$spaceName))
  dmGeo <- levels(as.factor(dmDF$spaceName))
  
  # Determine intersect and limit to that
  allGeo <- intersect(intersect(mmGeo, irGeo),dmGeo)
  mmDF <- subset(mmDF, mmDF$spaceName %in% allGeo)
  irDF <- subset(irDF, irDF$spaceName %in% allGeo)
  dmDF <- subset(dmDF, dmDF$spaceName %in% allGeo)
  
  ## Extract counting parameters
  
  oLng <- nrow(mmDF)
  tLng <- length(unique(mmDF$timeName))
  
  ## Build the comparison data set
  
  # if not weighted
  if(!wgt){
    
    comData <- rbind(mmDF, irDF, dmDF)
    comData$method <- c(rep('Median', oLng), rep('Impute', oLng),
                        rep('Match', oLng))
    
  } else {
    
    # Weights Tables
    mmPrice <- mmObj$priceStsTable[rownames(mmObj$priceStsTable) %in% allGeo,]
    mmRent <- mmObj$rentStsTable[rownames(mmObj$rentStsTable) %in% allGeo,]
    mmWgts <- prrConvStsTables(mmPrice + mmRent, allGeo)$wgts
    irWgts <- prrConvStsTables(irObj$stTable, allGeo)$wgts
    dmWgts <- prrConvStsTables(dmObj$stTable, allGeo)$wgts
    
    # Add weights to DF 
    mmDF$wgt <- mmWgts[match(mmDF$spaceName, names(mmWgts))]  
    irDF$wgt <- irWgts[match(irDF$spaceName, names(irWgts))]  
    dmDF$wgt <- dmWgts[match(dmDF$spaceName, names(dmWgts))]  
    
    # calc weighted yield components
    mmDF$wYield <- mmDF$yield * mmDF$wgt
    irDF$wYield <- irDF$yield * irDF$wgt
    dmDF$wYield <- dmDF$yield * dmDF$wgt
    
    # sum to weighted yield
    mmYields <- tapply(mmDF$wYield, mmDF$timeName, sum)
    irYields <- tapply(irDF$wYield, irDF$timeName, sum)
    dmYields <- tapply(dmDF$wYield, dmDF$timeName, sum)
    
    # combine together for weighted data
    comData <- data.frame(timeName=rep(1:tLng, 3),
                          spaceName=rep('all', 3*tLng),
                          yield = c(mmYields, irYields, dmYields),
                          method=c(rep('Median', tLng), rep('Impute', tLng),
                                   rep('Match', tLng)))
    
    # create the dif data inputs
    mmDF <- comData[comData$method == 'Median', ]
    irDF <- comData[comData$method == 'Impute', ]
    dmDF <- comData[comData$method == 'Match', ]
    oLng <- tLng
    
  }
  
  # Re-orders levels
  comData$method <- factor(comData$method,
                           levels=c('Median', 'Impute', 'Match'))
  
  ## Build the differences dataset
  
  difData <- rbind(mmDF, irDF, dmDF)
  difData$pIndex <- rep(pIndex, 3)
  difData$yield <- NULL
  difData$method <- c(rep('Impute - Median', oLng),
                      rep('Match - Median', oLng),
                      rep('Match - Impute', oLng))
  difData$dif <- c(irDF$yield - mmDF$yield,
                   dmDF$yield - mmDF$yield,
                   dmDF$yield - irDF$yield)
  
  # Re-order levels
  difData$method <- factor(difData$method,
                           levels=c('Impute - Median', 'Match - Median',
                                    'Match - Impute'))
  
  ## Return data
  return(list(comp = comData,
              diff = difData))
}

### Function that calculates locations count weights -------------------------------------

prrConvStsTables <- function(stTable,       # stsTable from stsSharder
                             allGeo         # List of OK geographies
){
  
  stTable <- as.data.frame(stTable)
  
  stTable <- stTable[rownames(stTable) %in% allGeo, ]
  
  stSums <- rowSums(stTable)
  
  stWgts <- stSums / sum(stSums)
  
  return(list(sums=stSums,
              wgts=stWgts))
}

### Function that combines and weights house and unit results ----------------------------

prrWeightUses <- function(hDataW,       # House wgt data from prrAggrMethData
                          uDataW,       # unit wgt data from prrAggrMethData
                          geoList,      # Full geolist from  prrAggrGeoData
                          pIndex,       # Price time index
                          geoSplit=F    # is not all areas?
                               
){  
  
  ## Calculate house vs unit weights
  if(geoSplit){
    
  # Find acceptable geos
    mmHPrice <- geoList$mm$house$priceStsTable
    mmHRent <- geoList$mm$house$rentStsTable
    mmUPrice <- geoList$mm$unit$priceStsTable
    mmURent <- geoList$mm$unit$rentStsTable
  
    irHTable <- geoList$ir$house$stTable
    irUTable <- geoList$ir$unit$stTable

    dmHTable <- geoList$dm$house$stTable
    dmUTable <- geoList$dm$unit$stTable
  
  # Combine all GeoNames  
    allGeos <- c(rownames(mmHPrice), rownames(mmHRent), rownames(mmUPrice), 
                rownames(mmURent), rownames(irHTable), rownames(irUTable),
                rownames(dmHTable), rownames(dmUTable))
    geoNames <- names(table(allGeos))
    geoTable <- as.numeric(table(allGeos))
    
  # Choose those present in all situations  
    allGeos <- geoNames[which(geoTable == 8)]
    mmHPrice <- mmHPrice[rownames(mmHPrice) %in% allGeos, ]
    mmHRent <- mmHRent[rownames(mmHRent) %in% allGeos, ]
    mmUPrice <- mmUPrice[rownames(mmUPrice) %in% allGeos, ]
    mmURent <- mmURent[rownames(mmURent) %in% allGeos, ]
    irHTable <- irHTable[rownames(irHTable) %in% allGeos, ]
    irUTable <- irUTable[rownames(irUTable) %in% allGeos, ]
    dmHTable <- dmHTable[rownames(dmHTable) %in% allGeos, ]
    dmUTable <- dmUTable[rownames(dmUTable) %in% allGeos, ]
  
  } else {
    
  # Or select all geos if !geoSplit  
    mmHPrice <- geoList$mm$house$priceStsTable
    mmHRent <- geoList$mm$house$rentStsTable
    mmUPrice <- geoList$mm$unit$priceStsTable
    mmURent <- geoList$mm$unit$rentStsTable
    
    irHTable <- geoList$ir$house$stTable
    irUTable <- geoList$ir$unit$stTable
    
    dmHTable <- geoList$dm$house$stTable
    dmUTable <- geoList$dm$unit$stTable
  }
  
  # Median method
  
  mmHwgt <- (sum(mmHPrice + mmHRent)) / (sum(mmHPrice + mmHRent) +
                                           sum(mmUPrice + mmURent))
  mmUwgt <- 1-mmHwgt
  
  # Impute method
  irHwgt <- sum(irHTable) / (sum(irHTable + irUTable))  
  irUwgt <- 1-irHwgt
  
  # Match Method
  dmHwgt <- sum(dmHTable) / (sum(dmHTable + dmUTable))  
  dmUwgt <- 1-dmHwgt
  
  # Combine weights
  hWgts <- c(rep(mmHwgt, 20), rep(irHwgt, 20), rep(dmHwgt, 20))
  uWgts <- c(rep(mmUwgt, 20), rep(irUwgt, 20), rep(dmUwgt, 20))
  
  ## Build comp data
  
  compData <- hDataW$comp
  compData$yield <- ((hDataW$comp$yield * hWgts) + 
                       (uDataW$comp$yield * uWgts))
  compData$use <- NULL
  
  # Re-order levels
  compData$method <- factor(compData$method,
                            levels=c('Median', 'Impute', 'Match'))
  
  ## Build difference data
  
  difData <- compData
  oLng <- 20
  difData$pIndex <- rep(pIndex, 3)
  difData$yield <- NULL
  difData$method <- c(rep('Impute - Median', oLng),
                      rep('Match - Median', oLng),
                      rep('Match - Impute', oLng))
  mmDF <- subset(compData, method=='Median')
  irDF <- subset(compData, method=='Impute')
  dmDF <- subset(compData, method=='Match')
  difData$dif <- c(irDF$yield - mmDF$yield,
                   dmDF$yield - mmDF$yield,
                   dmDF$yield - irDF$yield)
  
  # Re-order levels
  difData$method <- factor(difData$method,
                           levels=c('Impute - Median', 'Match - Median',
                                    'Match - Impute'))
  
  ## Return values  
  
  return(list(comp=compData,
              diff=difData))    
}

### Extract yields from a prrObject ------------------------------------------------------

prrGetYields <- function(prrObj     # Object from the prrAggrGeoData()
                         ){
  # Extract mixed yields
  mixYields <- prrObj$mix$comp
  
  # conver to a list
  mix <- list(median=subset(mixYields, method=='Median'),
              impute=subset(mixYields, method=='Impute'),
              match=subset(mixYields, method=='Match'))
  
  # Extract house only yields
  houseYields <- subset(prrObj$use$comp, use=='House')
  
  # Convert to a list
  house <- list(median=subset(houseYields, method=='Median'),
                impute=subset(houseYields, method=='Impute'),
                match=subset(houseYields, method=='Match'))
  
  # Extract unit only yields
  unitYields <- subset(prrObj$use$comp, use=='Unit')
  
  # Convert to a list
  unit <- list(median=subset(unitYields, method=='Median'),
               impute=subset(unitYields, method=='Impute'),
               match=subset(unitYields, method=='Match'))
  
  # Return values
  return(list(mix=mix,
              house=house,
              unit=unit))
  
}

### Wrapper to calculate the predictive accuracty of various yield trends ----------------

prrPredModelWrap <- function(dmData,              # matched dataset
                             yieldData,           # yield data from prrGetYields()
                             byUse=FALSE,         # Calculate by use?
                             byGeog=FALSE,        # Calculate by Geography
                             geoField=NULL        # Which field is the geog name in?
                             ){
  
  # Small helper function to count number of observations
  geoCount <- function(x){nrow(rbind.fill(x))}
  
  # Error catch if no geoField is specified but it is needed
  if(byGeog & is.null(geoField)) return(cat('Must specify geoField'))
  
 ## If calculating by use  
  
  if(byUse){
    
    # Subset house and unit data
    hData <- subset(dmData, PropertyType == 'House')
    uData <- subset(dmData, PropertyType == 'Unit')
    
    # if calculating by Geography
    if(byGeog){
    
    ## Calculate for Houses
      
      # Get geography names and extract necessary data
      geoListH <- levels(yieldData$house$median$spaceName)
      geoDataH <- lapply(geoListH, prrExtractGeoData, xData=hData, 
                         geoField=geoField)
      
      # Identify non-empty dataset (by geog)
      hCount <- unlist(lapply(geoDataH, nrow))
      idH <- which(hCount > 0)
      
      # Extract necessary geographic yield information
      geoYieldsH <- lapply(geoListH, prrExtractGeoYields, yieldData=yieldData$house)
      
      # Identify non-empty dataset (by geog)
      idYH <- lapply(geoYieldsH, geoCount)
      idYH <- which(idYH > 0)
      
      # Select those that meet both criteria
      idH <- intersect(idH, idYH)
      
      # Trim data to those that are not empty
      geoDataH <- geoDataH[idH]
      geoYieldsH <- geoYieldsH[idH]
      
      # Estimate the prediction error  
      geoH <- mapply(prrErrorByMethod, mData=geoDataH, yieldData=geoYieldsH)
      
      # Combine and rename results
      hResults <- rbind.fill(geoH)
      hResults$use <- 'house'
      hResults$geog <- geoField
      
    ## Calculate for units  
      
      # Geo geography names and extract necessary data
      geoListU <- levels(yieldData$unit$median$spaceName)
      geoDataU <- lapply(geoListU, prrExtractGeoData, xData=uData, 
                         geoField=geoField)
      
      # Identify non-empty datasets (by geog)
      uCount <- unlist(lapply(geoDataU, nrow))
      idU <- which(uCount > 0)
      
      # Extract necessary geographic yield information
      geoYieldsU <- lapply(geoListU, prrExtractGeoYields, yieldData=yieldData$unit)
      
      # Identify non-empty dataset (by geog)
      idYU <- lapply(geoYieldsU, geoCount)
      idYU <- which(idYU > 0)
      
      # Select those that meet both criteria
      idU <- intersect(idU, idYU)
      
      # Trim data to those that are not empty
      geoDataU <- geoDataU[idU]
      geoYieldsU <- geoYieldsU[idU]
      
      # Estimate the prediction error  
      geoU <- mapply(prrErrorByMethod, mData=geoDataU, yieldData=geoYieldsU)
      
      # Combine and rename results
      uResults <- rbind.fill(geoU)
      uResults$use <- 'unit'
      uResults$geog <- geoField
      
    ## Merge house and unit results
      
      xResults <- rbind(hResults, uResults)
      
    } else {
      
     ## if by Use by not geog  
      
      # Calculate errors for houses
      hResults <- prrErrorByMethod(hData, yieldData$house)
      hResults <- rbind.fill(hResults)
      hResults$use <- 'house'
      
      # Calculate errors for units
      uResults <- prrErrorByMethod(uData, yieldData$unit)
      uResults <- rbind.fill(uResults)
      uResults$use <- 'unit'
      
      # Combine results
      xResults <- rbind(hResults, uResults)
      xResults$geog <- 'all'
    }
  } else {
    
  ## if not by Use by by Geography
    
    if(byGeog){
      
      # Extract relevant geography names
      geoList <- levels(yieldData$mix$median$spaceName)
      
      # Extract geographic base data
      geoData <- lapply(geoList, prrExtractGeoData, xData=dmData, 
                        geoField=geoField)
      
      # Extract yield trends
      geoYields <- lapply(geoList, prrExtractGeoYields, yieldData=yieldData$mix)
      
      # Calculate all errors
      geo <- mapply(prrErrorByMethod, mData=geoData, yieldData=geoYields)
      xResults <- rbind.fill(geo)
      xResults$geog <- geoField
      
    } else {
      
  ## If not by use and not by Geography
      
      # Calculate error results
      xResults <- prrErrorByMethod(dmData, yieldData$mix)
      xResults <- rbind.fill(xResults)
      xResults$geog <- 'all'
    }
    xResults$use <- 'mix'
  }
  
  
  # Return Values  
  return(list(median=subset(xResults, method=='median'),
              impute=subset(xResults, method=='impute'),
              match=subset(xResults, method=='match')))  
}

### Helper function to extract geographic from a given dataset ---------------------------

prrExtractGeoData <- function(geoName,      # Specific geographic name
                              xData,        # dataset 
                              geoField      # Field containing geographic names
                              ){
  
  # ID and extract field
  gData <- xData[ ,geoField]
  
  # Label matching rows
  idx <- which(gData == geoName)
  
  # Return Values
  return(xData[idx, ])
  
}

### Helper function to extract the three yield types for a given geography ---------------

prrExtractGeoYields <- function(geoName,      # A specific geographic name 
                                yieldData     # yield data from prrGetYields()$mix
                                ){
  
  # Extract yields
  gMedian <- subset(yieldData$median, spaceName==geoName)
  gImpute <- subset(yieldData$impute, spaceName==geoName)
  gMatch <- subset(yieldData$match, spaceName==geoName)
  
  # Return Values
  return(list(median=gMedian,
              impute=gImpute,
              match=gMatch))
  
}

### Wrapper to spread the error calcs over all three methods -----------------------------

prrErrorByMethod <- function(mData,       # Matched dataset
                             yieldData    # List of yield data from prrGetYields
                             ){
  
  # Calc error for median method
  medianError <- prrCalcPredError(mData, yieldData$median)
  medianError$method='median'
  
  # Calc error for imputation method
  imputeError <- prrCalcPredError(mData, yieldData$impute)
  imputeError$method='impute'
  
  # calc error for matching method
  matchError <- prrCalcPredError(mData, yieldData$match)
  matchError$method='match'
  
  # Return Values
  return(list(median=medianError,
              impute=imputeError,
              match=matchError))
  
}

### calculate the predictive error -------------------------------------------------------

prrCalcPredError <- function(mData,     # Dataset of matched sales and rentals 
                             yData      # Timeseries of yield estimates
                             ){
  
  # Subset into sales and rentals based on which observation is first
  sData <- subset(mData, saleTime <= rentTime)
  rData <- subset(mData, saleTime > rentTime)
  sData$tType <- 'sale'
  rData$tType <- 'rent'
  
  # Add the yield information at the time of the first transaction
  sData$pYield <- yData$yield[match(sData$rentTime, yData$timeName)]  
  rData$pYield <- yData$yield[match(rData$saleTime, yData$timeName)]  
  
  # Predict rental value of sales and the error
  sData$pValue <- ((sData$adjSale * sData$pYield) / 52) 
  sData$error <- (sData$rentValue - sData$pValue) / sData$rentValue

  # Precict sale value of rentals and the error    
  rData$pValue <- (rData$adjRent * 52) / rData$pYield
  rData$error <- (rData$saleValue - rData$pValue) / rData$saleValue

## TODO:  Make time adjustments  
  
  # Merge data together  
  xData <- rbind(sData[,c('uID', 'tType', 'error')],
                 rData[,c('uID', 'tType', 'error')])
  
  # Return values
  return(xData)
}



