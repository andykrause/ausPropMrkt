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
  
 ## Compute Ratios
  
  # Compute observation level ratios
  sAnchRatio <- mTrans$saleValue / (mTrans$adjRent * 52)
  rAnchRatio <- mTrans$adjSale / (mTrans$rentValue * 52)
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
  
  ## Find Max
  lMax <- length(table(xData[,timeField]))
  
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
    
    if(length(hCount) != lMax || length(uCount) != lMax){
      return(list(allCount = 0,
                  allResults = 0))
    }
    
    # Calculate median PRR per category
    hRes <- tapply(xData$prRatio[xData$PropertyType == 'House'],
                   hTemp, median)
    uRes <- tapply(xData$prRatio[xData$PropertyType == 'Unit'],
                   uTemp, median)
    
    # Check to make sure both have enough
    testH <- unlist(lapply(hRes, function(x) length(unlist(x))))
    testU <- unlist(lapply(uRes, function(x) length(unlist(x))))
    hRes <- hRes[testH == max(testH)]
    uRes <- uRes[testU == max(testU)]
    
    ## If weighting by category
    
    if(weighted){
      
      # Make weighted sums
      hh <- hCount * hRes
      uu <- uCount * uRes
      
      # Calculate weighted values
      xx <- (hh + uu) / (hCount + uCount)
      
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

  # if long geotrends
    if(length(geoTrends) > length(okGeos)){
      iRes <- list()
      for(iR in 1:(length(geoTrends) / 4)){
       iRes[[iR]] <- list(houseCount = geoTrends[[(iR * 4) - 3]],
                          houseResults = geoTrends[[(iR * 4) - 2]],
                          unitCount = geoTrends[[(iR * 4) - 1]],
                          unitResults = geoTrends[[iR * 4]])
     } # Ends jk loop
   } # Ends if/else
   else
   {
     iRes <- geoTrends
   }
 } #ends if(byUse == FALSE....)
  
 ## Fix up names
  
  names(iRes) <- okGeos
  testL <- unlist(lapply(iRes, function(x) length(unlist(x))))
  iRes <- iRes[testL == max(testL)]
  return(iRes)
}



### Function that finds the proper object given various parameters -------------

prrFindObj <- function(geoType,             # all, lga, sla1, postCode, suburb
                       timeType,            # year or qtr
                       useType,             # Split by use (comb or use)
                       wgtType,             # Weighted or not (yes or no)
                       valType,             # Data type Count or PRR
                       geoName=NULL         # Specific Geography Name
)
{
  
  ## Test for proper match between geoType and geoName
  
  if(geoType != 'metro' & 
     (is.null(geoName) || geoName == "")) return(cat('You must select a',
                                                    'specific geography name.'))
  
  ## Define the large object table  
  
  objTable <- data.frame(name = c('globY', 'globQ', 'globBUY', 'globBUQ',
                                  'globBUWY', 'globBUWQ', 'lgaY', 'lgaQ',
                                  'lgaYU', 'lgaQU', 'lgaYUW', 'lgaQUW', 'slaY',
                                  'slaQ', 'slaYU', 'slaQU', 'slaYUW', 'slaQUW',
                                  'pcY', 'pcQ', 'pcYU', 'pcQU', 'pcYUW', 
                                  'pcQUW', 'subY', 'subQ', 'subYU', 'subQU', 
                                  'subYUW', 'subQUW'),
                         geoType = c(rep('metro', 6), rep('lga', 6), 
                                     rep('sla1', 6), rep('postCode', 6), 
                                     rep('suburb', 6)),
                         timeType = rep(c('year', 'qtr'), 15),
                         useType = rep(c('comb', 'comb', 'use', 'use', 
                                         'use', 'use'), 5),
                         wgtType = rep(c(FALSE, FALSE, FALSE, FALSE, 
                                         TRUE, TRUE), 5))
  
  ## Find the actual object name  
  
  objName <- objTable$name[objTable$geoType == geoType & 
                             objTable$timeType == timeType &
                             objTable$useType == useType & 
                             objTable$wgtType == wgtType]
  objName <- as.character(objName)
  
 ## Extract out the proper table from the object  
  
  # If not found 
  if(length(objName) == 0) return('notFound')
  
  # If global geography
  if(geoType == 'metro'){
    if(valType == 'Count'){
      obj <- get(objName)$tidyCount
    } else {
      obj <- get(objName)$tidyPRR
    }
  }
  
  # If specific geography
  else 
  {
    objX <- get(objName)
    geoX <- which(names(objX) == geoName)
    if(length(geoX) == 0) return(cat('Geography not found, Check name'))
    objY <- objX[[geoX]]
    if(valType == 'Count'){
      obj <- objY$tidyCount
    } else {
      obj <- objY$tidyPRR
    }
  }
  
  # Return the specific object  
  
  #return(obj)
  return(obj)
}

### General plotting function --------------------------------------------------     

prrTimePlot <- function(prrObj,
                        lineSize=2
){
  
  # Determine if by use or not  
  useGroup <- ifelse(length(table(prrObj$type) > 1), TRUE, FALSE)
  
  # if by use  
  if(useGroup){
    plotObj <- ggplot(prrObj, aes(x=as.numeric(time), y=value, colour=type)) +
      geom_line(size=lineSize) +
      labs(y='Price to Rent Ratio', x='Month from June 2010') + 
      theme(legend.position='bottom')
    
  }
  
  # If not by use  
  else 
  {
    plotObj <- ggplot(prrObj, aes(x=as.numeric(time), y=value)) +
      geom_line(size=lineSize)
  }
  
  # Return plot obj  
  return(plotObj)
}

### Median method approach -----------------------------------------------------

prrMedMethod <- function(transData,              # transaction dataset
                         timeField='transYear',  # timefield to use
                         byUse = FALSE ,         #Split by use
                         wgtd = FALSE            # Weight by use
                         )
  {
  
  # Separate sales and rents
  sData <- transData[transData$transType == 'sale', ]
  rData <- transData[transData$transType == 'rent', ]
  
  # Extract time field
  sTime <- sData[ ,timeField]
  rTime <- rData[ ,timeField]
  
  if(!byUse){
    
    # Calculate price trends
    sMed <- tapply(sData$transValue, sTime, median)
    rMed <- tapply(rData$transValue, rTime, median)
    prrCount <- tapply(transData, transData[,timeField], length)
    prr <- sMed/(rMed * 52)
    
    return(list(allCount=prrCount,
                allResults=prr))
    
  } else {
    
    sh <- which(sData$PropertyType == 'House')
    rh <- which(rData$PropertyType == 'House')
    su <- which(sData$PropertyType == 'Unit')
    ru <- which(rData$PropertyType == 'Unit')
    
    # Extract time field
    shMed <- tapply(sData$transValue[sh], sTime[sh], median)
    rhMed <- tapply(rData$transValue[rh], sTime[rh], median)
    suMed <- tapply(sData$transValue[su], sTime[su], median)
    ruMed <- tapply(rData$transValue[ru], sTime[ru], median)
    
    shCount <- tapply(sData$transValue[sh], sTime[sh], length)
    rhCount <- tapply(rData$transValue[rh], sTime[rh], length)
    suCount <- tapply(sData$transValue[su], sTime[su], length)
    ruCount <- tapply(rData$transValue[ru], sTime[ru], length)
    
    if(wgtd){
 
      shTemp <- shMed * shCount
      rhTemp <- rhMed * rhCount
      suTemp <- suMed * suCount
      ruTemp <- ruMed * ruCount
      
      hTemp <- (shMed/(rhMed*52)) * (length(c(sh,rh)))
      uTemp <- (suMed/(ruMed*52)) * (length(c(su,ru)))
      prr <- (hTemp + uTemp) / length(c(sh,rh,su,ru))
      prrCount <- tapply(transData, transData[,timeField], length)
      
      return(list(allCount=prrCount,
                  allResults=prr))
      
    } else {
      return(list(houseCount=shCount+rhCount,
                  houseResults = shMed/(rhMed * 52),
                  unitCount=suCount + ruCount,
                  unitResults=suMed/(ruMed * 52))) 
    
    }
  }  
    
  return(prr)
  
}


prrGeoMed <- function(geoName, geog, transData,
                      timeField='transYear', byUse=FALSE, wgtd=FALSE){
  
  geoField <- transData[,geog]
  geoX <- which(geoField == geoName)
  geoData <- transData[geoX, ]
  
  prr <- prrMedMethod(geoData, timeField=timeField, byUse=byUse, wgtd=wgtd)
  return(prr)
}

prrGeoWrap <- function(geog, transData, timeField, byUse, wgtd){
  
  geoFilter <- prrFilterData(geog=geog, timeField=timeField, byUse=byUse)
  idX <- which(transData[,geoFilter] == 1)
  xData <- transData[idX, ]
  geoTable <- table(xData[,geog])
  okGeos <- names(geoTable[geoTable > 1])
  
  qq<-lapply(okGeos, prrGeoMed, geog=geog, transData=xData, timeField=timeField,
             byUse=byUse, wgtd=wgtd)
  
}

### Function that converts a tidy data object to a data.frame ------------------

prrTidyToDF <- function(tidyObj    # Tidy data object
){
  
  # Set up blank objects / extract names
  newList <- list()
  newNames <- names(tidyObj)
  
  
  # Cycle through and pull out data and names
  for(ij in 1:length(tidyObj)){
    newList[[ij]] <- tidyObj[[ij]]$tidyPRR
    newList[[ij]]$geoName <- newNames[ij]
  }
  
  # Return Value
  return(newList)
} 

### Plot for comparing all metro to individual geographies ---------------------  

geoCompPlot <- function(geoPRR,         # prrObj with geog specific data
                        metroPRR,       # object with all metro data
                        geog,                 # Geography level
                        timeField='transYear' # time field
){
  
  ## create data
  
  # geog specific data frame
  geoDF <- rbind.fill(prrTidyToDF(geoPRR))
  geoDF$scale <- geog
  
  # general (metro) data 
  gq <- metroPRR$tidyPRR
  gq$geoName='metro'
  gq$scale='all'
  
  # Combine
  geoDFx <- rbind(geoDF, gq)
  geoDFx$value <- 1/geoDFx$value
  
  # Set location of metro data
  if(timeField=='transYear'){
    gcLoc <- ((nrow(geoDFx)-5):(nrow(geoDFx)))
  } else {
    gcLoc <- ((nrow(geoDFx)-19):(nrow(geoDFx)))
  }
  
  ## Build plot
  
  gcPlot <- ggplot(geoDFx, 
                   aes(x=as.numeric(time), y=value, 
                       group=geoName, colour=scale)) + 
    geom_line(size=.1, colour='gray40') +
    theme(panel.background = element_rect(colour='black', fill='black'),
          panel.grid.major=element_line(colour='gray20'),
          panel.grid.minor=element_blank()) +
    xlab("") + 
    ylab("Gross ROI (1 / PRR)") +
    theme(legend.position='none') +
    geom_line(data=geoDFx[gcLoc,], aes(x=as.numeric(time),
                                       y=value), colour='orange', size=2) +
    scale_x_continuous(breaks=seq(2,18,4), labels=2011:2015) +
    scale_y_continuous(limits=c(.02, .07),
                       breaks=seq(.025, .07, .005), 
                       labels=paste0(format(100*(
                         seq(.025, .07, .005)),
                         nsmall=1), "%")) +
    theme(plot.background=element_rect(fill='gray10'),
          axis.title.y=element_text(colour='white'),
          legend.background=element_rect(fill='gray10'),
          legend.key=element_rect(fill='gray10', color='gray10'),
          legend.text=element_text(color='white'))
  
  ## Return Value  
  return(gcPlot)  
} 

################################################################################
### New Australia Rent Yield Functions (works with stShard operations) ---------

aryStsGeoWrap <- function(stsData, metrics, spaceField, timeField,
                          defDim, stsLimit, calcs){
  
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
                           price=xPrice$stsDF$median,
                           rent=xRent$stsDF$median,
                           yield=((xRent$stsDF$median * 52) / 
                             xPrice$stsDF$median))
    
  }
    
  ## Export data  
  
  return(list(stsDF=geoTable,
              priceStsTable=xPrice$stTable,
              rentStsTable=xPrice$stTable))
}

### Function to compare price and rent on only matched properties ----------------------------------

arySaleRentMatch <- function(sales,               # Data.frame of sales
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

### Function for aggregating data from the three methods (med, imp, match)------

aryAggrMethData <- function(mmObj,       # Med Meth obj from aryStsGeoWrap
                            crObj,       # Cross Reg obj from spaceTimeShard()
                            dmObj,       # Match obj from spaceTimeShard()
                            pIndex       # A price index at the time scale
                            ){  
  
 ## Isolate the correct data from each object
  
  mmObj <- mmObj[,c('timeName', 'spaceName', 'yield')]
  crObj <- crObj$stsDF
  dmObj <- dmObj$stsDF
  names(crObj)[2] <- names(dmObj)[2] <- 'yield'
  
 ## Determine the spatial areas that exist in all objects  
  
  # Extract space from each
  mmGeo <- levels(mmObj$spaceName)
  crGeo <- levels(as.factor(crObj$spaceName))
  dmGeo <- levels(as.factor(dmObj$spaceName))
  
  # Determine intersect and limit to that
  allGeo <- intersect(intersect(mmGeo, crGeo),dmGeo)
  mmObj <- subset(mmObj, mmObj$spaceName %in% allGeo)
  crObj <- subset(crObj, crObj$spaceName %in% allGeo)
  dmObj <- subset(dmObj, dmObj$spaceName %in% allGeo)
  
 ## Extract counting parameters
  
  oLng <- nrow(mmObj)
  tLng <- length(unique(mmObj$timeName))

 ## Build the comparison data set
  
  comData <- rbind(mmObj, crObj, dmObj)
  comData$method <- c(rep('Median', oLng), rep('Impute', oLng),
                      rep('Match', oLng))
  
  # Re-orders levels
  comData$method <- factor(comData$method,
                           levels=c('Median', 'Impute', 'Match'))
  
 ## Build the differences dataset
  
  difData <- rbind(mmObj, crObj, dmObj)
  difData$pIndex <- rep(pIndex, 3)
  difData$yield <- NULL
  difData$method <- c(rep('Impute - Median', oLng),
                      rep('Match - Median', oLng),
                      rep('Match - Impute', oLng))
  difData$dif <- c(crObj$yield - mmObj$yield,
                   dmObj$yield - mmObj$yield,
                   dmObj$yield - crObj$yield)
  
  # Re-order levels
  difData$method <- factor(difData$method,
                           levels=c('Impute - Median', 'Match - Median',
                                    'Match - Impute'))
  
 ## Build the Median of each dataset  
  
  # Set identifiers
  medX <- which(comData$method == 'Median')
  impX <- which(comData$method == 'Impute')
  matX <- which(comData$method == 'Match')
  
  # Create dataset
  comMed <- data.frame(timeName = rep(1:tLng, 3),
                       method = c(rep('Median', tLng),
                                  rep('Impute', tLng),
                                  rep('Match', tLng)),
                       spaceName = rep('Median', tLng * 3),
                       yield = c(as.numeric(tapply(comData$yield[medX], 
                                                   comData$timeName[medX], 
                                                   median)),
                                 as.numeric(tapply(comData$yield[impX], 
                                                   comData$timeName[impX], 
                                                   median)),
                                 as.numeric(tapply(comData$yield[matX], 
                                                   comData$timeName[matX], 
                                                   median))))
  # Re-order the factors
  comMed$method <- factor(comMed$method,
                           levels=c('Median', 'Impute', 'Match'))
  
 ## Build the median of the difference dataset
  
  # Set identifiers
  medXX <- which(comMed$method == 'Median')
  impXX <- which(comMed$method == 'Impute')
  matXX <- which(comMed$method == 'Match')
  
  # Combine Data
  difMed <- comMed
  difMed$pIndex <- rep(pIndex, 3)
  difMed$yield <- NULL
  difMed$method <- c(rep('1. Impute - Median', tLng),
                     rep('2. Match - Median', tLng),
                     rep('3. Match - Impute', tLng))
  difMed$dif <- c(comMed$yield[impXX] - comMed$yield[medXX],
                  comMed$yield[matXX] - comMed$yield[medXX],
                  comMed$yield[matXX] - comMed$yield[impXX])
  
  # Re-order levels
  difMed$method <- factor(difMed$method,
                           levels=c('Impute - Median', 'Match - Median',
                                    'Match - Impute'))
  
  
  ## Return data
  return(list(comp = comData,
              diff = difData,
              compMed = comMed,
              diffMed = difMed))
}


