##########################################################################################
#                                                                                        #
#  Suite of functions for analyzing price to rent ratios with APM data                   #
#                                                                                        #
##########################################################################################

##########################################################################################
# Functions for creating price and rent indexes at each geography                        #
##########################################################################################

### Base function for creating time index from a set of reg. coefficients  ---------------

apmIndexEngine <- function(ref.coefs,                     
                           time.field='transQtr'      
                           )
{
  
  # ARGUMENTS
  #
  # reg.coefs = full set of coefficients from the regression model
  # time.field = field containing the time breakdown
  
 ## Turn Coefficients into a data.frame
  
  coefs.df <- as.data.frame(ref.coefs) 
  names(coefs.df)[1] <- 'Estimate'
  
 ## Extract relavent coefs and turn into a series  
  
  coefs.time <- c(0, coefs.df$Estimate[grep(time.field, rownames(coefs.df))])
  
 ## Convert to an index, base 100  
  
  coefs.index <- 100 * (1 + (exp(c(0, coefs.time[-1])) - 1))
  
 ## Return Values  
  
  return(coefs.index)
}

### Geo Unit Wrapper to calc house, unit, sale and rent indexes across a given geo -------

apmGeoIndUnitWrap <- function(geo.value, 
                              geo.field, 
                              trans.data, 
                              time.field='transQtr',
                              verbose=FALSE
                              )
  {
  
  # ARGUMENTS
  #
  # geo.value = a particular geographic identifier
  # geo.field = field containing the geo.value
  # trans.data = transaction data
  # time.field = field containing the time period analyzed
  
 ## Fix equations by removing spatial fixed effects
  
  house.eq <- apmOptions$houseEquation
  unit.eq <- apmOptions$houseEquation
  unit.eq <- update(unit.eq, . ~ . -as.factor(postCode))
  house.eq <- update(house.eq, . ~ . -as.factor(postCode))
  
 ## Calculate the required length to cover all periods
  
  req.length <- length(levels(as.factor(trans.data[, time.field])))
  
 ## Isolate data geographically
  
  if(verbose) cat('Isolating data for: ', geo.value, '\n')
  
  # If global take all data
  if(geo.field == 'Global') {
    geo.data <- trans.data
  } else {
    
  # Otherwise limit to that specific geography  
    geo.data <- trans.data[trans.data[, geo.field] == geo.value, ]
  }
  
 ## Split into house and units
  
  if(verbose) cat('Split into house and units\n')
  
  house.data <- geo.data[geo.data$PropertyType == 'House', ]
  unit.data <- geo.data[geo.data$PropertyType == 'Unit', ]
  
  ## Calculate indexes for Houses   
  
  if(verbose) cat('Calculate house price and rent indexes\n')
  
  if(nrow(house.data) > 60){
    
    house.sales <- subset(house.data, transType == 'sale')
    house.rents <- subset(house.data, transType == 'rent')
    
    # House Sales
    if(nrow(house.sales) > 60){
      
      hs.model <- lm(house.eq, data=house.sales)
      hs.index <- apmIndexEngine(hs.model$coef, time.field=time.field)
      hs.raw <- ((hs.index / 100) * 
                   median(house.sales$transValue[house.sales[, time.field] == 1]))
    } else {
      
      hs.index <- 'NA'
      hs.raw <- 'NA'
    
    }
    
    # House Rents
    if(nrow(house.rents) > 60){
      
      hr.model <- lm(house.eq, data=house.rents)
      hr.index <- apmIndexEngine(hr.model$coef, time.field=time.field)
      hr.raw <- ((hr.index / 100) * 
                   median(house.rents$transValue[house.rents[, time.field] == 1]))
    } else {
      
      hr.index <- 'NA'
      hr.raw <- 'NA'
      
    }
    
  } else {
    
    hs.index <- 'NA'
    hr.index <- 'NA'
    hs.raw <- 'NA'
    hr.raw <- 'NA'
    
  }
  
  ## Calculate indexes for Units  
  
  if(nrow(unit.data) > 60){
    
    unit.sales <- subset(unit.data, transType == 'sale')
    unit.rents <- subset(unit.data, transType == 'rent')
    
    # Unit Sales
    if(nrow(unit.sales) > 60){
      
      us.model <- lm(unit.eq, data=unit.sales)
      us.index <- apmIndexEngine(us.model$coef, time.field=time.field)
      us.raw <- ((us.index / 100) * 
                   median(unit.sales$transValue[unit.sales[, time.field] == 1]))
    } else {
      
      us.index <- 'NA'
      us.raw <- 'NA'
    
    }
    
    # Unit Rents
    if(nrow(unit.rents) > 60){
      ur.model <- lm(unit.eq, data=unit.rents)
      ur.index <- apmIndexEngine(ur.model$coef, time.field=time.field)
      ur.raw <- ((ur.index / 100) * 
                   median(unit.rents$transValue[unit.rents[, time.field] == 1]))
    } else {
    
      ur.index <- 'NA'
      ur.raw <- 'NA'
    
    }
  } else {
    
    us.index <- 'NA'
    ur.index <- 'NA'
    us.raw <- 'NA'
    ur.raw <- 'NA'
    
  }
  
 ## Calculate indexes  
#   
#   # Units
#   if(length(us.index) == req.length & length(ur.index) == req.length){
#     unit.yields <- (ur.index * 52) / us.index
#   } else {
#     unit.yields <- 0
#   }
#   
#   # Houses
#   if(length(hs.index) == req.length & length(hr.index) == req.length){
#     house.yields <- (hr.index * 52) / hs.index
#   } else {
#     house.yields <- 0
#   }
#   
#   # All
#   if(house.yields[1] != 0 & unit.yields[1] != 0){
#     all.yields <- (unit.yields + house.yields) / 2
#   } else {
#     all.yields <- 0
#   }
  
  ## Return values
  
  return(list(house.sale=hs.index,
              house.rent=hr.index,
              unit.sale=us.index,
              unit.rent=ur.index,
              raw=list(house.sale=hs.raw,
                       house.rent=hr.raw,
                       unit.sale=us.raw,
                       unit.rent=ur.raw)))
}

### Geo Level Wrapper to calc h, u, s and r indexs for all areas in a geo level ----------

apmGeoIndLevelWrap <- function(geo.field,
                               trans.data, 
                               time.field='transQtr',
                               verbose=FALSE
                               )
  {
  
  # ARGUMENTS
  #
  # geo.field = geogrphic field for which to calculate all of the indexes
  # trans.data = transaction data
  # time.field = field containing the time period analyzed
  
 ## Get the list of geographies to use
  
  if(verbose) cat('Getting list of geographic areas\n')
  geo.list <- levels(as.factor(trans.data[, geo.field]))
  
 ## Apply geo index method across all
  
  if(verbose) cat('Calculating Indexes across all geographies\n')
  ind.list <- lapply(geo.list, FUN=apmGeoIndUnitWrap, trans.data=trans.data,
                     geo.field=geo.field, verbose=FALSE)
  
  ## name list items
  
  names(ind.list) <- geo.list
  
  ## return values
  
  return(ind.list)
  
}    


### Area Level Wrapper to calc indexes across all different geographic levels ------------

apmAreaIndLevelWrap <- function(trans.data, 
                                geo.levels=c('Global', 'lga', 'sla1', 
                                             'suburb', 'postCode'),
                                verbose=FALSE
                                )
  {
  
  # ARGUMENTS
  #
  # trans.data = transaction data
  # geo.levels = various levels at which to perform the analysis
  
 ## Prep data and objects
  
  results.list <- list()
  idL <- 1
  
 ## Global analysis
  
  if('Global' %in% geo.levels){
    
    if(verbose) cat('Estimating all area index method\n')
    global.ind <- apmGeoIndUnitWrap('Global', 'Global', trans.data, verbose=FALSE)
    results.list[[idL]] <- global.ind
    idL <- idL + 1
  
  }
  
  ## LGA area analysis
  
  if('lga' %in% geo.levels){
  
    if(verbose) cat('Estimating lga level index method\n')
    lga.ind <- apmGeoIndLevelWrap('lga', trans.data)
    results.list[[idL]] <- lga.ind
    idL <- idL + 1
  
  }
  
  ## SLA1 area analysis
  
  if('sla1' %in% geo.levels){
  
    if(verbose) cat('Estimating sla1 level index method\n')
    sla1.ind <- apmGeoIndLevelWrap('sla1', trans.data)
    results.list[[idL]] <- sla1.ind
    idL <- idL + 1
  
  }
  
  ## Suburb area analysis
  
  if('suburb' %in% geo.levels){
  
    if(verbose) cat('Estimating suburb level index method\n')
    suburb.ind <- apmGeoIndLevelWrap('suburb', trans.data)
    results.list[[idL]] <- suburb.ind
    idL <- idL + 1
  
  }
  
  ## Post code area analysis  
  
  if('postCode' %in% geo.levels){
    
    if(verbose) cat('Estimating postcode level index method\n')
    postcode.ind <- apmGeoIndLevelWrap('postCode', trans.data)
    results.list[[idL]] <- postcode.ind
    idL <- idL + 1
  
  }
  
  ## Fix names and return values  
  names(results.list) <- geo.levels
  
  return(results.list)
  
}  


##########################################################################################
# Functions for imputing obs. level prices and rents                                     #
##########################################################################################

### Run the impute regression on a set of sale and rental data ---------------------------

apmImpRegress <- function(transData,             # Transaction data (sales and rentals)
                          reg.spec,              # REgression specification
                          verbose = FALSE        # Show progress?
){
  
  ## Split data
  
  if(verbose) cat('...Separating sales and rentals\n')
  saleData <- subset(transData, transType == 'sale')
  rentData <- subset(transData, transType == 'rent')
  
  ## Estimate models and make new predictions
  
  # Esimate models
  if(verbose) cat('......Estimating sale and rent models\n')
  saleModel <- lm(reg.spec, data=saleData)
  rentModel <- lm(reg.spec, data=rentData)
  
  # Make predictions of imputed values
  if(verbose) cat('......Imputing values\n')
  impPrice <- exp(predict(saleModel, newdata=rentData))
  impRent <- exp(predict(rentModel, newdata=saleData))
  
 ## Building data compatability 
  
  if(verbose) cat('......Stacking observed and imputed values\n')
  
  saleData$Price <- saleData$transValue
  saleData$impPrice <- round(exp(saleModel$fitted.values), 0)
  saleData$Rent <- rep(0, nrow(saleData))
  saleData$impRent <- impRent
  
  rentData$Price <- rep(0, nrow(rentData))
  rentData$impPrice <- impPrice
  rentData$Rent <- rentData$transValue
  rentData$impRent <- round(exp(rentModel$fitted.values), 0)
  
  # Combine data back together
  if(verbose) cat('......Merging data\n')
  allData <- rbind(saleData, rentData)
  
  ## Extract model information
  saleModelInfo <- list(coef=summary(saleModel)$coefficients,
                        r2=summary(saleModel)$r.squared,
                        sigma=summary(saleModel)$r.squared,
                        resid=summary(saleModel)$residuals,
                        baseValue=median(saleData$transValue[which(as.factor(
                          saleData$transQtr) == 1)]))
  
  rentModelInfo <- list(coef=summary(rentModel)$coefficients,
                        r2=summary(rentModel)$r.squared,
                        sigma=summary(rentModel)$r.squared,
                        resid=summary(rentModel)$residuals,
                        baseValue=median(rentData$transValue[which(as.factor(
                          rentData$transQtr) == 1)]))

  ## Return values
  return(list(results = allData[ ,c('UID', 'Price', 'impPrice', 'Rent', 'impRent')],
              saleModel = saleModelInfo,
              rentModel = rentModelInfo))
  
}

### Assign the impute regression yields back to the transaction data ---------------------

apmAssignIRYields <- function(transData,            # apmDataObj
                              houseRegRes,          # Results of house reg.
                              unitRegRes            # Results of unit reg.
)
{
  
  # Extract values
  irValues <- rbind(houseRegRes$results, unitRegRes$results)
  
  # Calculate the ratio
  irValues$impYield <- (irValues$impRent * 52) / irValues$impPrice
  
  # Calculate the mixed ratios
  idS <- which(irValues$Rent == 0)
  idR <- which(irValues$Price == 0)
  
  irValues$impSaleYield <- 0
  irValues$impSaleYield[idS] <- (irValues$impRent[idS] * 52) / irValues$Price[idS]
  irValues$impRentYield <- 0
  irValues$impRentYield[idR] <- (irValues$Rent[idR] * 52) / irValues$impPrice[idR]
  
  # Add Ratio to full dataset 
  transData$impYield <- irValues$impYield[match(transData$UID, irValues$UID)]
  transData$impSaleYield <- irValues$impSaleYield[match(transData$UID, irValues$UID)]
  transData$impRentYield <- irValues$impRentYield[match(transData$UID, irValues$UID)]
  
  # Remove those with missing values
  xTrans <- subset(transData, !is.na(impYield)) 
  
 ## Return  transactions  
  
  return(xTrans)
}  

### Create a set of indexes based on impute regression results ---------------------------

apmCreateIndexes <- function(impHouse,           # House results from impute regression
                             impUnit             # Unit results from impute regression
)
{
  
  ## Build sales indexes
  
  saleIndex <- list(house = 1 + apmMakeIndex(impHouse$saleModel$coef),
                    unit = 1 + apmMakeIndex(impUnit$saleModel$coef))
  saleIndex$all <- (saleIndex$house + saleIndex$unit) / 2
  saleIndex$houseValue <- saleIndex$house * impHouse$saleModel$baseValue
  saleIndex$unitValue <- saleIndex$unit * impUnit$saleModel$baseValue
  
  ## Build Rent Indexes
  
  rentIndex <- list(house = 1 + apmMakeIndex(impHouse$rentModel$coef),
                    unit = 1 + apmMakeIndex(impUnit$rentModel$coef))
  rentIndex$all <- (rentIndex$house + rentIndex$unit) / 2  
  rentIndex$houseValue <- rentIndex$house * impHouse$rentModel$baseValue
  rentIndex$unitValue <- rentIndex$unit * impUnit$rentModel$baseValue
  
  ## Return values
  
  return(list(rent = rentIndex,
              sale = saleIndex))
}


### Function to compare price and rent on only matched properties ------------------------

apmSaleRentMatch <- function(transData,           # Transaction data
                             indexObj,            # Index obj from apmCreateIndexes()
                             matchField = 'ID',   # Field containing matching ID
                             saleField = 'Price', # Field containing sale price
                             rentField = 'Rent',  # Field containing rent 
                             timeField = 'Year'   # Field containing time breakdown
){
  
  ## Split into sales and rentals
  
  sales <- transData[transData$transType == 'sale',]
  rentals <- transData[transData$transType == 'rent',]
  
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
  mTrans <- merge(mSales[, c(matchField, 'PropertyType', 'UID', saleField, timeField)],
                  mRentals[, c(matchField, 'UID', rentField, timeField)],
                  by=matchField)
  
  # Rename Match Fields
  names(mTrans) <- c(matchField, 'PropertyType', 'saleID', 'saleValue', 'saleTime', 
                     'rentID', 'rentValue', 'rentTime')
  
  ## Make time adjustments to matched transactions

  mTrans <- apmTimeAdjMatches(transData=mTrans,
                              indexObj=indexObj)

  # Calc Yields
  mTrans$dmSaleYield <- (mTrans$adjRent * 52) / mTrans$saleValue
  mTrans$dmRentYield <- (mTrans$rentValue * 52) / mTrans$adjSale
  mTrans$dmYield <- (mTrans$dmSaleYield + mTrans$dmRentYield) / 2
  
  # Add information on time between transactions
  mTrans$timeGap <- mTrans$rentTime - mTrans$saleTime

 ## Return Values    
  
  return(mTrans)  
}  

### Time adjusts the direct matches based on a given set of indexes ----------------------

apmTimeAdjMatches <- function(transData,          # Transaction data
                              indexObj            # Index obj from apmCreateIndexes()
)
{
  
  ## Split by use
  
  houseTrans <- transData[transData$PropertyType == "House", ]
  unitTrans <- transData[transData$PropertyType == "Unit", ]
  
  # Make adjustment to houses 
  houseSaleAdj <- (indexObj$sale$house[as.numeric(as.factor(houseTrans$rentTime))] /
                     indexObj$sale$house[as.numeric(as.factor(houseTrans$saleTime))])
  houseTrans$adjSale <- houseTrans$saleValue * houseSaleAdj
  
  houseRentAdj <- (indexObj$rent$house[as.numeric(as.factor(houseTrans$saleTime))] /
                     indexObj$rent$house[as.numeric(as.factor(houseTrans$rentTime))])
  houseTrans$adjRent <- houseTrans$rentValue * houseRentAdj
  
  # Make adjustment to units 
  unitSaleAdj <- (indexObj$sale$unit[as.numeric(as.factor(unitTrans$rentTime))] /
                    indexObj$sale$unit[as.numeric(as.factor(unitTrans$saleTime))])
  unitTrans$adjSale <- unitTrans$saleValue * unitSaleAdj
  
  unitRentAdj <- (indexObj$rent$unit[as.numeric(as.factor(unitTrans$saleTime))] /
                    indexObj$sale$unit[as.numeric(as.factor(unitTrans$rentTime))])
  unitTrans$adjRent <- unitTrans$rentValue * unitRentAdj
  
  ## Merge back together
  
  mTrans <- rbind(houseTrans, unitTrans)
  
  ## Return Data  
  
  return(mTrans)
  
}

### Swap yield information from imputations and matching methods -------------------------

apmYieldSwap <- function(transData,       # Transaction data
                         matchData        # Set of matched data from apmSaleRentMatch()
)
{
  
  ##  Split into sales and rent sets
  
  transSales <- transData[transData$transType == 'sale', ]
  transRents <- transData[transData$transType == 'rent', ]
  
  ## Apply the imputed yields to the match data  
  
  matchData$impSaleYield <- transSales$impSaleYield[match(matchData$saleID,
                                                          transSales$UID)]
  matchData$impRentYield <- transRents$impRentYield[match(matchData$rentID,
                                                          transRents$UID)]
  matchData$impSaleYieldX <- transSales$impYield[match(matchData$saleID,
                                                       transSales$UID)]
  matchData$impRentYieldX <- transRents$impYield[match(matchData$rentID,
                                                       transRents$UID)]
  
  ## Resolve situations where properties have more than one matched yield  
  
  matchSales <- tapply2DF(xData=matchData$dmSaleYield, byField=matchData$saleID, 
                          xFunc=median)
  matchRents <- tapply2DF(xData=matchData$dmRentYield, byField=matchData$rentID, 
                          xFunc=median) 
  
  ## Add matched yields to the transaction data  
  
  transSales$dmSaleYield <- matchSales$Var[match(transSales$UID, matchSales$ID)]
  transSales$dmRentYield <- NA
  transRents$dmRentYield <- matchRents$Var[match(transRents$UID, matchRents$ID)]
  transRents$dmSaleYield <- NA
  
  ## Recombine transdata
  
  return(list(transData=rbind(transSales, transRents),
              matchData=matchData))
  
}  

## Compare the differences between the matched observations and all observations ---------

apmCompareSamples <- function(clean.trans)
{  
  
  ## Create a match trans showing only those observations used in the match data  
  
  matchTrans <- cleanTrans[!is.na(cleanTrans$dmSaleYield) | 
                             !is.na(cleanTrans$dmRentYield),]
  
  ## De trend sales prices and rents of all transactions
  
  # Create separate data sets
  c.unit.rent <- subset(cleanTrans, transType=='rent' & PropertyType == 'Unit')
  c.house.rent <- subset(cleanTrans, transType=='rent' & PropertyType == 'House')
  c.unit.sale <- subset(cleanTrans, transType=='sale' & PropertyType == 'Unit')
  c.house.sale <- subset(cleanTrans, transType=='sale' & PropertyType == 'House')
  
  # Create an index for detrending
  unitrentsTrender <- tapply(c.unit.rent$transValue, c.unit.rent$transQtr, median)
  unitrentsTrender <- data.frame(qtr=1:20,
                                 adj=1/(unitrentsTrender/unitrentsTrender[1]))
  unitsalesTrender <- tapply(c.unit.sale$transValue, c.unit.sale$transQtr, median)
  unitsalesTrender <- data.frame(qtr=1:20,
                                 adj=1/(unitsalesTrender/unitsalesTrender[1]))
  houserentsTrender <- tapply(c.house.rent$transValue, c.house.rent$transQtr, median)
  houserentsTrender <- data.frame(qtr=1:20,
                                  adj=1/(houserentsTrender/houserentsTrender[1]))
  housesalesTrender <- tapply(c.house.sale$transValue, c.house.sale$transQtr, median)
  housesalesTrender <- data.frame(qtr=1:20,
                                  adj=1/(housesalesTrender/housesalesTrender[1]))
  
  # Apply detrending adjustments
  c.unit.rent$xValue <- (c.unit.rent$transValue * 
                           unitrentsTrender$adj[match(c.unit.rent$transQtr,
                                                      unitrentsTrender$qtr)])
  c.house.rent$xValue <- (c.house.rent$transValue * 
                            houserentsTrender$adj[match(c.house.rent$transQtr,
                                                        houserentsTrender$qtr)])
  c.unit.sale$xValue <- (c.unit.sale$transValue * 
                           unitsalesTrender$adj[match(c.unit.sale$transQtr,
                                                      unitsalesTrender$qtr)])
  c.house.sale$xValue <- (c.house.sale$transValue * 
                            housesalesTrender$adj[match(c.house.sale$transQtr,
                                                        housesalesTrender$qtr)])
  
  ## Detrend sales and rents of matched trans
  
  # Subset data
  m.unit.rent <- subset(matchTrans, transType=='rent' & PropertyType == 'Unit')
  m.house.rent <- subset(matchTrans, transType=='rent' & PropertyType == 'House')
  m.unit.sale <- subset(matchTrans, transType=='sale' & PropertyType == 'Unit')
  m.house.sale <- subset(matchTrans, transType=='sale' & PropertyType == 'House')
  
  # Detrend values
  m.unit.rent$xValue <- (m.unit.rent$transValue * 
                           unitrentsTrender$adj[match(m.unit.rent$transQtr,
                                                      unitrentsTrender$qtr)])
  m.house.rent$xValue <- (m.house.rent$transValue * 
                            houserentsTrender$adj[match(m.house.rent$transQtr,
                                                        houserentsTrender$qtr)])
  m.unit.sale$xValue <- (m.unit.sale$transValue * 
                           unitsalesTrender$adj[match(m.unit.sale$transQtr,
                                                      unitsalesTrender$qtr)])
  m.house.sale$xValue <- (m.house.sale$transValue * 
                            housesalesTrender$adj[match(m.house.sale$transQtr,
                                                        housesalesTrender$qtr)])
  
  ## Calculate differences in the de-trended values  
  
  ur <- t.test(c.unit.rent$xValue, m.unit.rent$xValue)
  us <- t.test(c.unit.sale$xValue, m.unit.sale$xValue)
  hr <- t.test(c.house.rent$xValue, m.house.rent$xValue)
  hs <- t.test(c.house.sale$xValue, m.house.sale$xValue)
  
  ## Return Values
  return(list(match.trans = matchTrans,
              house.sale=list(all=c.house.sale,
                              match=m.house.sale),
              unit.sale=list(all=c.unit.sale,
                             match=m.unit.sale),
              house.rent=list(all=c.house.rent,
                              match=m.house.rent),
              unit.rent=list(all=c.unit.rent,
                             match=m.unit.rent),
              house.sale.test = hs,
              unit.sale.test = us,
              house.rent.test = hr,
              unit.rent.test = ur))
  
}










medianMethodWrap <- function(cleanData,            # clean trans data (apmDataObj) 
                             verbose=FALSE
                             )
  {
  
  if(verbose) cat('Calculating yields with median method\n')

  ## Metro Level

  if(verbose) cat('...At Metro level\n')
  mmMetro <- prrStsGeoWrap(stsData = cleanData,
                           metric=c('transValue', 'transValue'),
                           spaceField='all', timeField='transQtr',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))

  # Metro by Use
  mmMetroH <- prrStsGeoWrap(cleanData[cleanData$PropertyType == 'House',],
                            metric=c('transValue', 'transValue'),
                            spaceField='all', timeField='transQtr',
                            defDim='time', stsLimit=apmOptions$geoTempLimit, 
                            calcs=list(median='median'))

  mmMetroU <- prrStsGeoWrap(cleanData[cleanData$PropertyType == 'Unit',],
                            metric=c('transValue', 'transValue'),
                            spaceField='all', timeField='transQtr',
                            defDim='time', stsLimit=apmOptions$geoTempLimit, 
                            calcs=list(median='median'))

 ## At LGA Level

  if(verbose) cat('...At LGA level\n')
  # All Uses
  mmLga <- prrStsGeoWrap(stsData = cleanData,
                        metric=c('transValue', 'transValue'),
                        spaceField='lga', timeField='transQtr',
                        defDim='time', stsLimit=apmOptions$geoTempLimit, 
                        calcs=list(median='median'))

  # By Use
  mmLgaH <- prrStsGeoWrap(cleanData[cleanData$PropertyType == 'House', ],
                          metric=c('transValue', 'transValue'),
                          spaceField='lga', timeField='transQtr',
                          defDim='time', stsLimit=apmOptions$geoTempLimit, 
                          calcs=list(median='median'))

  mmLgaU <- prrStsGeoWrap(cleanData[cleanData$PropertyType == 'Unit', ],
                          metric=c('transValue', 'transValue'),
                          spaceField='lga', timeField='transQtr',
                          defDim='time', stsLimit=apmOptions$geoTempLimit, 
                          calcs=list(median='median'))

 ## At SLA1 Level
 
  if(verbose) cat('...At SLA1 level\n')

  # All Uses  
  mmSla <- prrStsGeoWrap(stsData = cleanData,
                         metric=c('transValue', 'transValue'),
                         spaceField='sla1', timeField='transQtr',
                         defDim='time', stsLimit=apmOptions$geoTempLimit, 
                         calcs=list(median='median'))

  # By Use
  mmSlaH <- prrStsGeoWrap(cleanData[cleanData$PropertyType == 'House', ],
                          metric=c('transValue', 'transValue'),
                          spaceField='sla1', timeField='transQtr',
                          defDim='time', stsLimit=apmOptions$geoTempLimit, 
                          calcs=list(median='median'))

  mmSlaU <- prrStsGeoWrap(cleanData[cleanData$PropertyType == 'Unit', ],
                          metric=c('transValue', 'transValue'),
                          spaceField='sla1', timeField='transQtr',
                          defDim='time', stsLimit=apmOptions$geoTempLimit, 
                          calcs=list(median='median'))

 ## At Suburb Level

  if(verbose) cat('...At Suburb level\n')

  # All Uses  
  mmSuburb <- prrStsGeoWrap(stsData = cleanData,
                            metric=c('transValue', 'transValue'),
                            spaceField='suburb', timeField='transQtr',
                            defDim='time', stsLimit=apmOptions$geoTempLimit, 
                            calcs=list(median='median'))

  # By Use
  mmSuburbH <- prrStsGeoWrap(cleanData[cleanData$PropertyType == 'House', ],
                             metric=c('transValue', 'transValue'),
                             spaceField='suburb', timeField='transQtr',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))

  mmSuburbU <- prrStsGeoWrap(cleanData[cleanData$PropertyType == 'Unit', ],
                             metric=c('transValue', 'transValue'),
                             spaceField='suburb', timeField='transQtr',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))

 ## At PostCode Level

  if(verbose) cat('...At Post Code Level\n')

  # All Uses   
  mmPostcode <- prrStsGeoWrap(stsData = cleanData,
                              metric=c('transValue', 'transValue'),
                              spaceField='postCode', timeField='transQtr',
                              defDim='time', stsLimit=apmOptions$geoTempLimit, 
                              calcs=list(median='median'))

  # by Use   
  mmPostcodeH <- prrStsGeoWrap(cleanData[cleanData$PropertyType == 'House', ],
                               metric=c('transValue', 'transValue'),
                               spaceField='postCode', timeField='transQtr',
                               defDim='time', stsLimit=apmOptions$geoTempLimit, 
                               calcs=list(median='median'))
  # All Uses   
  mmPostcodeU <- prrStsGeoWrap(cleanData[cleanData$PropertyType == 'Unit', ],
                               metric=c('transValue', 'transValue'),
                               spaceField='postCode', timeField='transQtr',
                               defDim='time', stsLimit=apmOptions$geoTempLimit, 
                               calcs=list(median='median'))

 ## Combine Results  
  
  mmResults <- list(metro=list(all=mmMetro, house=mmMetroH, unit=mmMetroU),
                    lga=list(all=mmLga, house=mmLgaH, unit=mmLgaU),
                    sla=list(all=mmSla, house=mmSlaH, unit=mmSlaU),
                    suburb=list(all=mmSuburb, house=mmSuburbH, unit=mmSuburbU),
                    postcode=list(all=mmPostcode, house=mmPostcodeH, unit=mmPostcodeU))

 ## Return Results

  return(mmResults)
}

imputeMethodWrap <- function(cleanData,            # Clean trans data (apmDataObj)
                             verbose=FALSE
                             )
{

### Break down results by dimensions -------------------------------------------
  
  ## Metro
  
  if(verbose) cat('...Analyze at Metro Level\n')
  
  # Metro 
  irMetro <- spaceTimeShard(stsData = cleanData,
                            metric=c('impYield'),
                            spaceField='all', timeField='transQtr',
                            defDim='time', stsLimit=apmOptions$geoTempLimit, 
                            calcs=list(median='median'))
  
  # By Use
  irMetroH <- spaceTimeShard(cleanData[cleanData$PropertyType == 'House', ],
                             metric=c('impYield'),
                             spaceField='all', timeField='transQtr',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  
  irMetroU <- spaceTimeShard(cleanData[cleanData$PropertyType == 'Unit', ],
                             metric=c('impYield'),
                             spaceField='all', timeField='transQtr',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  ## LGA
  
  if(verbose) cat('...Analyze at LGA Level\n')
  
  irLga <- spaceTimeShard(stsData = cleanData,
                          metric=c('impYield'),
                          spaceField='lga', timeField='transQtr',
                          defDim='time', stsLimit=apmOptions$geoTempLimit, 
                          calcs=list(median='median'))
  
  # By Use
  irLgaH <- spaceTimeShard(cleanData[cleanData$PropertyType == 'House', ],
                           metric=c('impYield'),
                           spaceField='lga', timeField='transQtr',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  irLgaU <- spaceTimeShard(cleanData[cleanData$PropertyType == 'Unit', ],
                           metric=c('impYield'),
                           spaceField='lga', timeField='transQtr',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median')) 
  
  ## SLA1
  
  if(verbose) cat('...Analyze at SLA Level\n')
  
  irSla <- spaceTimeShard(stsData = cleanData,
                          metric=c('impYield'),
                          spaceField='sla1', timeField='transQtr',
                          defDim='time', stsLimit=apmOptions$geoTempLimit, 
                          calcs=list(median='median'))
  
  # By Use
  irSlaH <- spaceTimeShard(cleanData[cleanData$PropertyType == 'House', ],
                           metric=c('impYield'),
                           spaceField='sla1', timeField='transQtr',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  irSlaU <- spaceTimeShard(cleanData[cleanData$PropertyType == 'Unit', ],
                           metric=c('impYield'),
                           spaceField='sla1', timeField='transQtr',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  ## Suburb
  
  if(verbose) cat('...Analyze at Suburb Level\n')
  
  irSuburb <- spaceTimeShard(stsData = cleanData,
                             metric=c('impYield'),
                             spaceField='suburb', timeField='transQtr',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  
  # By Use
  irSuburbH <- spaceTimeShard(cleanData[cleanData$PropertyType == 'House', ],
                              metric=c('impYield'),
                              spaceField='suburb', timeField='transQtr',
                              defDim='time', stsLimit=apmOptions$geoTempLimit, 
                              calcs=list(median='median'))
  
  irSuburbU <- spaceTimeShard(cleanData[cleanData$PropertyType == 'Unit', ],
                              metric=c('impYield'),
                              spaceField='suburb', timeField='transQtr',
                              defDim='time', stsLimit=apmOptions$geoTempLimit, 
                              calcs=list(median='median'))
  
  ## post code
  
  if(verbose) cat('...Analyze at Postcode Level\n')
  
  irPostcode <- spaceTimeShard(stsData = cleanData,
                               metric=c('impYield'),
                               spaceField='postCode', timeField='transQtr',
                               defDim='time', stsLimit=apmOptions$geoTempLimit, 
                               calcs=list(median='median'))
  
  # By Use
  irPostcodeH <- spaceTimeShard(cleanData[cleanData$PropertyType == 'House', ],
                                metric=c('impYield'),
                                spaceField='postCode', timeField='transQtr',
                                defDim='time', stsLimit=apmOptions$geoTempLimit, 
                                calcs=list(median='median'))
  
  irPostcodeU <- spaceTimeShard(cleanData[cleanData$PropertyType == 'Unit', ],
                                metric=c('impYield'),
                                spaceField='postCode', timeField='transQtr',
                                defDim='time', stsLimit=apmOptions$geoTempLimit, 
                                calcs=list(median='median')) 
  
  ## Combine Results  
  
  irResults <- list(metro=list(all=irMetro, house=irMetroH, unit=irMetroU),
                    lga=list(all=irLga, house=irLgaH, unit=irLgaU),
                    sla=list(all=irSla, house=irSlaH, unit=irSlaU),
                    suburb=list(all=irSuburb, house=irSuburbH, unit=irSuburbU),
                    postcode=list(all=irPostcode, house=irPostcodeH, unit=irPostcodeU))
  
  ## Return Results
  
  return(irResults)  
  
}

### Function that assigns the imputed yields to the correct observations -------



### Function for applying match method to all properties -----------------------

matchMethodWrap <- function(matchData,             # Matched data object
                            verbose=FALSE)
{
  
  if(verbose) cat('Calculating yields with match method\n')
  
  ## All Metro  
  
  if(verbose) cat('...Analyze at Metro Level\n')
  
  dmMetro <- spaceTimeShard(stsData = matchData,
                            metric=c('saleYield'),
                            spaceField='all', timeField='saleTime',
                            defDim='time', stsLimit=apmOptions$geoTempLimit, 
                            calcs=list(median='median'))
  
  # By Use   
  dmMetroH <- spaceTimeShard(matchData[matchData$PropertyType == 'House',],
                             metric=c('saleYield'),
                             spaceField='all', timeField='saleTime',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  
  dmMetroU <- spaceTimeShard(matchData[matchData$PropertyType == 'Unit',],
                             metric=c('saleYield'),
                             spaceField='all', timeField='saleTime',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  
  ## LGA  
  
  if(verbose) cat('...Analyze at LGA Level\n')
  
  dmLga <- spaceTimeShard(stsData = matchData,
                          metric=c('saleYield'),
                          spaceField='lga', timeField='saleTime',
                          defDim='time', stsLimit=apmOptions$geoTempLimit, 
                          calcs=list(median='median'))
  
  # By Use   
  dmLgaH <- spaceTimeShard(matchData[matchData$PropertyType == 'House',],
                           metric=c('saleYield'),
                           spaceField='lga', timeField='saleTime',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  dmLgaU <- spaceTimeShard(matchData[matchData$PropertyType == 'Unit',],
                           metric=c('saleYield'),
                           spaceField='lga', timeField='saleTime',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  ## SLA1 
  
  if(verbose) cat('...Analyze at SLA1 Level\n')
  
  dmSla <- spaceTimeShard(stsData = matchData,
                          metric=c('saleYield'),
                          spaceField='sla1', timeField='saleTime',
                          defDim='time', stsLimit=apmOptions$geoTempLimit, 
                          calcs=list(median='median'))
  
  # By Use   
  dmSlaH <- spaceTimeShard(matchData[matchData$PropertyType == 'House',],
                           metric=c('saleYield'),
                           spaceField='sla1', timeField='saleTime',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  dmSlaU <- spaceTimeShard(matchData[matchData$PropertyType == 'Unit',],
                           metric=c('saleYield'),
                           spaceField='sla1', timeField='saleTime',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median')) 
  
  ## Suburb
  
  if(verbose) cat('...Analyze at Suburb Level\n')
  
  dmSuburb <- spaceTimeShard(stsData = matchData,
                             metric=c('saleYield'),
                             spaceField='suburb', timeField='saleTime',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  
  # By Use   
  dmSuburbH <- spaceTimeShard(matchData[matchData$PropertyType == 'House',],
                              metric=c('saleYield'),
                              spaceField='suburb', timeField='saleTime',
                              defDim='time', stsLimit=apmOptions$geoTempLimit, 
                              calcs=list(median='median'))
  
  dmSuburbU <- spaceTimeShard(matchData[matchData$PropertyType == 'Unit',],
                              metric=c('saleYield'),
                              spaceField='suburb', timeField='saleTime',
                              defDim='time', stsLimit=apmOptions$geoTempLimit, 
                              calcs=list(median='median')) 
  
  ## PostCode 
  
  if(verbose) cat('...Analyze at Postcode Level\n')
  
  dmPostcode <- spaceTimeShard(stsData = matchData,
                               metric=c('saleYield'),
                               spaceField='postCode', timeField='saleTime',
                               defDim='time', stsLimit=apmOptions$geoTempLimit, 
                               calcs=list(median='median'))
  
  # By Use   
  dmPostcodeH <- spaceTimeShard(matchData[matchData$PropertyType == 'House',],
                                metric=c('saleYield'),
                                spaceField='postCode', timeField='saleTime',
                                defDim='time', stsLimit=apmOptions$geoTempLimit, 
                                calcs=list(median='median'))
  
  dmPostcodeU <- spaceTimeShard(matchData[matchData$PropertyType == 'Unit',],
                                metric=c('saleYield'),
                                spaceField='postCode', timeField='saleTime',
                                defDim='time', stsLimit=apmOptions$geoTempLimit, 
                                calcs=list(median='median'))  
  
  ## Combine Results  
  
  dmResults <- list(metro=list(all=dmMetro, house=dmMetroH, unit=dmMetroU),
                    lga=list(all=dmLga, house=dmLgaH, unit=dmLgaU),
                    sla=list(all=dmSla, house=dmSlaH, unit=dmSlaU),
                    suburb=list(all=dmSuburb, house=dmSuburbH, unit=dmSuburbU),
                    postcode=list(all=dmPostcode, house=dmPostcodeH, unit=dmPostcodeU))
  
  ## Return Results
  
  return(dmResults) 
}  

### Function to convert data from method based to geography based --------------

apmConvertToGeo <- function(mmRes, irRes, dmRes, indexList){
  
  ## Metro Level
  
  metroList <- list(mm=list(all=mmRes$metro$all, house=mmRes$metro$house,
                            unit=mmRes$metro$unit),
                    ir=list(all=irRes$metro$all, house=irRes$metro$house,
                            unit=irRes$metro$unit),
                    dm=list(all=dmRes$metro$all, house=dmRes$metro$house,
                            unit=dmRes$metro$unit))
  
  metroData <- prrAggrGeoData(metroList, indexList)
  
  ## LGA Level
  
  lgaList <- list(mm=list(all=mmRes$lga$all, house=mmRes$lga$house,
                          unit=mmRes$lga$unit),
                  ir=list(all=irRes$lga$all, house=irRes$lga$house,
                          unit=irRes$lga$unit),
                  dm=list(all=dmRes$lga$all, house=dmRes$lga$house,
                          unit=dmRes$lga$unit))
  
  lgaData <- prrAggrGeoData(lgaList, indexList, geoSplit=TRUE)
  
  ## SLA1 Level
  
  slaList <- list(mm=list(all=mmRes$sla$all, house=mmRes$sla$house,
                          unit=mmRes$sla$unit),
                  ir=list(all=irRes$sla$all, house=irRes$sla$house,
                          unit=irRes$sla$unit),
                  dm=list(all=dmRes$sla$all, house=dmRes$sla$house,
                          unit=dmRes$sla$unit))
  
  slaData <- prrAggrGeoData(slaList, indexList, geoSplit=TRUE)
  
  ## suburb Level
  
  suburbList <- list(mm=list(all=mmRes$suburb$all, house=mmRes$suburb$house,
                             unit=mmRes$suburb$unit),
                     ir=list(all=irRes$suburb$all, house=irRes$suburb$house,
                             unit=irRes$suburb$unit),
                     dm=list(all=dmRes$suburb$all, house=dmRes$suburb$house,
                             unit=dmRes$suburb$unit))
  
  suburbData <- prrAggrGeoData(suburbList, indexList, geoSplit=TRUE)
  
  ## postcode Level
  
  postcodeList <- list(mm=list(all=mmRes$postcode$all, house=mmRes$postcode$house,
                               unit=mmRes$postcode$unit),
                       ir=list(all=irRes$postcode$all, house=irRes$postcode$house,
                               unit=irRes$postcode$unit),
                       dm=list(all=dmRes$postcode$all, house=dmRes$postcode$house,
                               unit=dmRes$postcode$unit))
  
  postcodeData <- prrAggrGeoData(postcodeList, indexList, geoSplit=TRUE)
  
  ## Return Results
  
  return(list(metroData=metroData,
              lgaData=lgaData,
              slaData=slaData,
              suburbData=suburbData,
              postcodeData=postcodeData))
  
}



