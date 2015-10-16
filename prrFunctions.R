####################################################################################################
### Below here is where I'm working on building a master function that will analyse the data
### with all methods possible and across all geographic levels desired
### WORK IN PROGRESS 
####################################################################################################

### Function to compare price and rent on only matched properties ----------------------------------

prrDirectCompare <- function(sales,               # Data.frame of sales
                             rentals,             # Data.frame of rentals
                             matchField = 'ID',   # Field containing matching ID
                             saleField = 'Price', # Field containing sale price
                             rentField = 'Rent',  # Field containing rent 
                             timeField = 'Year'   # Field containing time breakdown
                             ){
  
 ## Example of how to use function
  
  if(F){
    prr <- prrDirectCompare(sales=Sales, rentals=Rentals,
                            matchField='AddressID', saleField='Price',
                            rentField='Rent', timeField='Year')  
  }
  
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
  names(mTrans) <- c(matchField, saleField, 'saleTime', rentField, 'rentTime')
  
 ## Make time adjustments to matched transactions
  
  # Create the rent index
  rentTrend <- as.numeric(tapply(mTrans[,rentField], mTrans$rentTime, median))
  rentIndex <- rentTrend / rentTrend[1]
  
  # Create the sale index
  saleTrend <- as.numeric(tapply(mTrans[,saleField], mTrans$saleTime, median))
  saleIndex <- saleTrend / saleTrend[1]
  
  # Make the adjustments to the rentals
  rentAdj <- (rentIndex[as.numeric(as.factor(mTrans$saleTime))] /
                rentIndex[as.numeric(as.factor(mTrans$rentTime))])
  mTrans$adjRent <- mTrans[,rentField] * rentAdj
  
  # Make the adjustments to the sales
  saleAdj <- (saleIndex[as.numeric(as.factor(mTrans$rentTime))] /
                saleIndex[as.numeric(as.factor(mTrans$saleTime))])
  mTrans$adjSale <- mTrans[,saleField] * saleAdj
  
 ## Compute Ratios
  
  # Compute observation level ratios
  sAnchRatio <- mTrans[,saleField] / (mTrans$adjRent * 52 / 12)
  rAnchRatio <- mTrans$adjSale / (mTrans[,rentField] * 52 / 12)
  mAnchRatio <- (sAnchRatio + rAnchRatio) / 2
  
  # Calculate the median per time period
  sAnchIndex <- tapply(sAnchRatio, mTrans$saleTime, median)
  rAnchIndex <- tapply(rAnchRatio, mTrans$rentTime, median)
  mAnchIndex <- (sAnchIndex + rAnchIndex) / 2
  
 ## Return Values    
  return(list(matchTrans = mTrans,
              rawRatios = list(saleBased = sAnchRatio,
                               rentBased = rAnchRatio,
                               mixBased = mAnchRatio),
              prrIndex = list(saleBased = sAnchIndex,
                              rentBased = rAnchIndex,
                              mixBased = mAnchIndex),
              timeIndex = list(saleIndex = saleIndex,
                               rentIndex = rentIndex)))  
}  

### Function to convert various APM date structures into R date structure --------------------------

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

### Regression function that creates imputed rent and sales values -------------------------------

prrCrossReg <- function(formula,               # LM regression formula
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

### Function to determine which geo areas meet use and time criteria -----------

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

### Apply the threshold designations across all transactions ------------------

applyThres <- function(thresData,       # Threshold data object from prrGeoLimit
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


if(F){
  
  # Examples of use
  
  # Global
  glob <- prrTrender(list('transYear', 'transQtr', 'transMonth'), xData=xTrans)
  
  # Global by Use
  globBU <- prrTrender(list('transYear', 'transQtr', 'transMonth'),
                       xData=xTrans, byUse=TRUE)
  
  # glob by use weighted
  globBUW <- prrTrender(list('transYear', 'transQtr', 'transMonth'),
                        xData=xTrans, byUse=TRUE, weighted=TRUE)
  
  ## All LGAs at qtr level
  lgaQ <- prrTrender(list('transQtr'), xData=xTrans, geog='lga',
                     geogName='all')
  
  ## All LGAs at qtr level by Use
  lgaQU <- prrTrender(list('transQtr'), xData=xTrans, geog='lga',
                      geogName='all', byUse=TRUE)
  
  
  ## All GLS at qtr level by use weighted
  lgaQUW <- prrTrender(list('transQtr'), xData=xTrans, geog='lga',
                       geogName='all', byUse=TRUE, weighted=TRUE)
  
}

### Wrapper function for handling the prrMakeTrends() --------------------------

prrTrender <- function(timeFields,       # List of time fields to use
                       xData,            # Dataset including time and PRR ratios
                       byUse=FALSE,      # Calculate by use?
                       geog=NULL,        # WHich geog level to divide by
                       geogName=NULL,    # which geog areas to use
                       weighted=FALSE    # provide single, weighted output
){
  
  ## Remove months if by geography
  
  if(!is.null(geog)) timeFields <- timeFields[timeFields!='transMonth']
  if(length(timeFields) == 0) return(cat('Cannot select only months',
                                         ' when analysing at sub metro level'))
  if(!is.null(geogName)){
    if(geogName == 'all' & length(timeFields) > 1){
      return(cat('Can only select one time field when analyzing',
                 ' multiple geographies'))
    } 
  }
  
  ## Main call to the prrMakeTrends() function
  
  # If not doing all geogNames
  if(is.null(geogName) || geogName != 'all'){
    
    # If more than one geogName
    if(!is.null(geogName) && length(geogName) > 1) {
      return(cat('You cannot select more than one geography.',
                 ' Use "all" here if you want more than one'))  
    } 
    
    # Just One or none geogName
    else 
    {
      tempData <- lapply(timeFields, prrMakeTrends, byUse=byUse, xData=xData,
                         geog=geog, geogName=geogName, weighted=weighted)
      names(tempData) <- timeFields
    }
  } 
  
  ## If doing all geognames in a geog
  else
  {
    
    ## Call a function to get correct data
    geogFilter <- prrFilterData(geog=geog, timeField=timeFields,
                                byUse=byUse)
    
    tempData <- prrGeogMkr(geogFilter, xData=xData, byUse=byUse, geog=geog, 
                           timeFields=timeFields, weighted=weighted)
    
  } 
  
  ## Convert existing list into data frames  
  
  xDFs <- lapply(tempData, prrToDF)
  
  ## Convert dataframes into tidy datasets    
  
  tidyPRR <- lapply(xDFs, prrMelt)
  tidyCount <- lapply(xDFs, prrMelt, exclField='prr')
  
  ## Combine all into a list 
  
  # Set up blank list
  allRes <- list()
  
  # Loop though and assign to spots
  for(i in 1:length(tempData)){
    allRes[[i]] <- list(data=xDFs[[i]],
                        tidyPRR=tidyPRR[[i]],
                        tidyCount=tidyCount[[i]])
  }
  
  # Name list items
  names(allRes) <- names(tempData)
  
  ## Return object
  
  return(allRes)
}

### Function to build PRR trends by different temporal breakdowns --------------

prrMakeTrends <- function(timeField,        # Time field to analyse
                          xData,            # Dataset w/ time and PRR ratios
                          byUse=FALSE,      # Calculate by use
                          geog=NULL,        # Which geog level to divide by
                          geogName=NULL,    # Which geog areas to use
                          weighted=FALSE    # provide single, weighted output?
){
  
  ## Select particular geography
  
  if(!is.null(geog)){
    xData <- xData[xData[,geog]==geogName, ]
  }
  
  ## If calculating by Use
  
  if(byUse){
    
    # Divide into use
    hTemp <- xData[xData$PropertyType == 'House', timeField]
    uTemp <- xData[xData$PropertyType == 'Unit', timeField]
    
    # Calculate count per category
    hCount <- tapply(xData$prRatio[xData$PropertyType == 'House'],
                     hTemp, length)
    uCount <- tapply(xData$prRatio[xData$PropertyType == 'Unit'],
                     uTemp, length)
    
    # Calculate median PRR per category
    hRes <- tapply(xData$prRatio[xData$PropertyType == 'House'],
                   hTemp, median)
    uRes <- tapply(xData$prRatio[xData$PropertyType == 'Unit'],
                   uTemp, median)
    
    ## If weighting by category
    
    if(weighted){
      
      # Make weighted sums
      hh <- hCount * hRes
      uu <- uCount * uRes
      
      # Calculate weighted values
      xx <- (hh+uu) / (hCount + uCount)
      
      # Return results
      return(list(allCount = hCount + uCount,
                  allResults = xx))
    }
    else
    {
      
      # Return unweighted results
      return(list(houseCount = hCount,
                  houseResults = hRes,
                  unitCount = uCount,
                  unitResults = uRes))
      
    }   
    
  } 
  
  ## If not by use
  else 
  {
    
    # Extract temporal scale
    temporalScale <- xData[,timeField]
    
    # Calculate count
    xCount <- tapply(xData$prRatio, temporalScale, length)
    
    # Calculate median PRR
    xRes <- tapply(xData$prRatio, temporalScale, median)
    
    # Return not by use results
    return(list(allCount = xCount, allResults = xRes))
  }
}

### Function to convert prrObj into simple data frame --------------------------

prrToDF <- function(prrObj       ## prrObj[[x]] from prrMakeTrends() 
){
  
  ## If obj is not by Use
  
  if(length(prrObj) < 4){
    
    #Combine columns
    dfTemp <- as.data.frame(cbind(as.numeric(unlist(prrObj[1])),
                                  as.numeric(unlist(prrObj[2]))))
    names(dfTemp) <- c('count', 'prr')
    
    # Add Time variable
    dfTemp$time <- names(prrObj$allCount)
    
    # Add Type Variable
    dfTemp$type <- 'both'
    
    
  } else {
    
    # Combine Columns
    hTemp <- as.data.frame(cbind(as.numeric(unlist(prrObj[1])),
                                 as.numeric(unlist(prrObj[2]))))
    names(hTemp) <- c('count', 'prr')
    uTemp <- as.data.frame(cbind(as.numeric(unlist(prrObj[3])),
                                 as.numeric(unlist(prrObj[4]))))
    names(uTemp) <- c('count', 'prr')
    
    # Add Time variables
    hTemp$time <- names(prrObj$houseCount)
    uTemp$time <- names(prrObj$unitCount)
    
    # Add Type variable
    hTemp$type <- 'house'
    uTemp$type <- 'unit'
    
    # Combine into single DF
    dfTemp <- rbind(hTemp, uTemp)
  }
  
  ## Return Values (re-order fields)
  return(dfTemp[ ,c('type', 'time', 'count', 'prr')])
}


### Function to melt DFs into tidy data (for ggplot) ---------------------------

prrMelt <- function(prrObj,                  # Base prrObj after prrToDF()
                    exclField='count'        # Field to ignore 'count' | 'prr'
){
  
  # Set libraries
  library(reshape2)
  
  # Define the value field
  valueField <- ifelse(exclField == 'count', 'prr', 'count')
  
  # Remove field to ignore
  xCut <- which(names(prrObj) == exclField)
  xObj <- prrObj[ ,-xCut]
  
  # Determine tidy IDs
  idNames <- names(xObj)[names(xObj) != valueField]
  
  # Melt to a tidy dataset
  meltObj <- melt(xObj, id=idNames)
  
  ## Return values
  return(meltObj)
  
}

### Function that determine correct data field based on arguments --------------

prrFilterData <- function(geog,
                          timeField,
                          byUse){
  fieldName <- paste0(ifelse(timeField=='transYear', 'YT', 'QT'),
                      "_",
                      ifelse(byUse, 'both', 'either'),
                      "_",
                      geog)
  return(fieldName)
}

### Function that handles mulitple geography situations ------------------------

prrGeogMkr <- function(geogFilter, xData, byUse, geog, timeFields,
                       weighted){
  
  ## Extract correct data
  
  # Find and extract geog filter field
  geoField <- xData[names(xData) == geogFilter]
  geoData <- xData[which(geoField == 1), ]
  
  # Get acceptable list of geographies
  geoTable <- table(geoData[geog])
  okGeos <- names(geoTable)[geoTable > 0]
  
  ## Run the trend maker
  
  geoTrends <- mapply(prrMakeTrends, timeField = timeFields, geogName=okGeos,
                      MoreArgs=list(byUse=byUse, xData=geoData,
                                    geog=geog, weighted=weighted))
  
  ## Turn into data frames
  
  if(byUse == FALSE | weighted == TRUE){
    
    iRes <- list()
    for(iR in 1:(length(geoTrends) / 2)){
      iRes[[iR]] <- list(allCount = geoTrends[[(iR * 2) - 1]],
                         allResults = geoTrends[[iR * 2]])
    } # Ends iR Loop
  } 
  else 
  {
    
    iRes <- list()
    for(iR in 1:(length(geoTrends) / 4)){
      iRes[[iR]] <- list(houseCount = geoTrends[[(iR * 4) - 3]],
                         houseResults = geoTrends[[(iR * 4) - 2]],
                         unitCount = geoTrends[[(iR * 4) - 1]],
                         unitResults = geoTrends[[iR * 4]])
    } # Ends jk loop
  } # Ends if/else
  
  names(iRes) <- okGeos
  return(iRes)
}
