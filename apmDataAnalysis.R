##########################################################################################
#                                                                                        #
#  Suite of functions for analyzing price to rent ratios with APM data                   #
#                                                                                        #
##########################################################################################














##########################################################################################
# Functions for imputing obs. level prices and rents                                     #
##########################################################################################

### Run the impute regression on a set of sale and rental data ---------------------------

apmHedImpute <- function(trans.data,             # Transaction data (sales and rentals)
                         reg.spec,              # REgression specification
                         verbose = FALSE        # Show progress?
){
  
  # ARGUMENTS
  #
  # trans.data = transaction data with sales and rentals
  # regression specification to be used
  # verbose = show progress?
  
 ## Split data
  
  if(verbose) cat('...Separating sales and rentals\n')
  sale.data <- subset(trans.data, transType == 'sale')
  rent.data <- subset(trans.data, transType == 'rent')
  
  ## Estimate models and make new predictions
  
  # Esimate models
  if(verbose) cat('......Estimating sale and rent models\n')
  sale.model <- lm(reg.spec, data=sale.data)
  rent.model <- lm(reg.spec, data=rent.data)
  
  # Make predictions of imputed values
  if(verbose) cat('......Imputing values\n')
  himp.price <- exp(predict(sale.model, newdata=rent.data))
  himp.rent <- exp(predict(rent.model, newdata=sale.data))
  
 ## Building data compatability 
  
  if(verbose) cat('......Stacking observed and imputed values\n')
  
  sale.data$Price <- sale.data$transValue
  sale.data$himp.price <- round(exp(sale.model$fitted.values), 0)
  sale.data$Rent <- rep(0, nrow(sale.data))
  sale.data$himp.rent <- himp.rent
  
  rent.data$Price <- rep(0, nrow(rent.data))
  rent.data$himp.price <- himp.price
  rent.data$Rent <- rent.data$transValue
  rent.data$himp.rent <- round(exp(rent.model$fitted.values), 0)
  
  # Combine data back together
  if(verbose) cat('......Merging data\n')
  all.data <- rbind(sale.data, rent.data)
  
 ## Extract model information
  
  sale.model.info <- list(coef=summary(sale.model)$coefficients,
                          r2=summary(sale.model)$r.squared,
                          sigma=summary(sale.model)$r.squared,
                          resid=summary(sale.model)$residuals,
                          baseValue=median(sale.data$transValue[which(as.factor(
                            sale.data$transQtr) == 1)]))
  
  rent.model.info <- list(coef=summary(rent.model)$coefficients,
                          r2=summary(rent.model)$r.squared,
                          sigma=summary(rent.model)$r.squared,
                          resid=summary(rent.model)$residuals,
                          baseValue=median(rent.data$transValue[which(as.factor(
                            rent.data$transQtr) == 1)]))

 ## Return values
  
  return(list(results = all.data[ ,c('UID', 'Price', 'himp.price', 'Rent', 'himp.rent')],
              sale.model = sale.model.info,
              rent.model = rent.model.info))
  
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
  irValues$impYield <- (irValues$himp.rent * 52) / irValues$himp.price
  
  # Calculate the mixed ratios
  idS <- which(irValues$Rent == 0)
  idR <- which(irValues$Price == 0)
  
  irValues$impSaleYield <- 0
  irValues$impSaleYield[idS] <- (irValues$himp.rent[idS] * 52) / irValues$Price[idS]
  irValues$impRentYield <- 0
  irValues$impRentYield[idR] <- (irValues$Rent[idR] * 52) / irValues$himp.price[idR]
  
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
                             indexObj,            # Index obj from apmAreaIndLevelWrap()
                             index.geo = 'Global',# Level at which to use adj Indexes
                             matchField = 'ID',   # Field containing matching ID
                             saleField = 'Price', # Field containing sale price
                             rentField = 'Rent',  # Field containing rent 
                             timeField = 'Year'   # Field containing time breakdown
){
  
 ## Deal with a global index.geo
  if(index.geo == 'Global') transData$Global <- 0
  
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
  mTrans <- merge(mSales[, c(matchField, 'PropertyType', 'lga', 'sla1','suburb',
                             'postCode', 'UID', saleField, timeField)],
                  mRentals[, c(matchField, 'UID', rentField, timeField)],
                  by=matchField)
  
  # Rename Match Fields
  names(mTrans) <- c(matchField, 'PropertyType', 'lga', 'sla1', 'suburb', 'postCode',
                     'saleID', 'saleValue', 'saleTime', 'rentID', 'rentValue', 'rentTime')
  
  ## Make time adjustments to matched transactions
  
  mTrans <- apmTimeAdjGeoWrap(transData=mTrans,
                              indexObj=indexObj,
                              index.geo=index.geo)
  
  # Calc Yields
  mTrans$dmSaleYield <- (mTrans$adjRent * 52) / mTrans$saleValue
  mTrans$dmRentYield <- (mTrans$rentValue * 52) / mTrans$adjSale
  mTrans$dmYield <- (mTrans$dmSaleYield + mTrans$dmRentYield) / 2
  
  # Add information on time between transactions
  mTrans$timeGap <- mTrans$rentTime - mTrans$saleTime
  
  ## Return Values    
  
  return(mTrans)  
} 

apmTimeAdjGeoWrap <- function(transData,
                              indexObj,
                              index.geo
){
  
 ## List of Geographies  
  
  if(index.geo != 'Global'){
    geo.list <- levels(as.factor(transData[,index.geo]))
  } else {
    geo.list <- 'Global'
  }
 
 ## Extract the relevant index objects
  
  idObj <- which(names(indexObj) == index.geo)
  index.obj <- indexObj[[idObj]]
  
 ## Make the matches
  
  match.list <- lapply(geo.list, FUN=apmTimeAdjMatches, transData=transData,
                       indexObj=index.obj, index.geo=index.geo)
  

 ## Convert to data frame
  
  match.data <- rbind.fill(match.list)
  
 ## Return values  
  
  return(match.data)  
}

### Time adjusts the direct matches based on a given set of indexes ----------------------

apmTimeAdjMatches <- function(transData,          # Transaction data
                              indexObj,           # Index obj from apmCreateIndexes()
                              index.geo,          # Geography to time adjust at
                              geo.value=NULL      # Specific Geo Value
                              )
{
  
  if(!is.null(geo.value)) transData <- transData[transData[,index.geo] == geo.value, ]
  
  idO <- which(names(indexObj) == geo.value)
  indexObj <- indexObj[[idO]]
  
 ## Split by use
  
  houseTrans <- transData[transData$PropertyType == "House", ]
  unitTrans <- transData[transData$PropertyType == "Unit", ]
  
  # Make adjustment to houses
  if(nrow(houseTrans) > 0 & indexObj$house.sale[1] != "NA" &
                                 indexObj$house.rent[1] != 'NA'){
    houseSaleAdj <- (indexObj$house.sale[as.numeric(as.factor(houseTrans$rentTime))] /
                       indexObj$house.sale[as.numeric(as.factor(houseTrans$saleTime))])
    houseTrans$adjSale <- houseTrans$saleValue * houseSaleAdj
  
    houseRentAdj <- (indexObj$house.rent[as.numeric(as.factor(houseTrans$saleTime))] /
                     indexObj$house.rent[as.numeric(as.factor(houseTrans$rentTime))])
    houseTrans$adjRent <- houseTrans$rentValue * houseRentAdj
  } else {
    houseTrans <- NULL
  }
  
  # Make adjustment to units 
  if(nrow(unitTrans) & indexObj$unit.sale[1] != "NA" &
                            indexObj$unit.rent[1] != 'NA'){
    unitSaleAdj <- (indexObj$unit.sale[as.numeric(as.factor(unitTrans$rentTime))] /
                      indexObj$unit.sale[as.numeric(as.factor(unitTrans$saleTime))])
    unitTrans$adjSale <- unitTrans$saleValue * unitSaleAdj
  
    unitRentAdj <- (indexObj$unit.rent[as.numeric(as.factor(unitTrans$saleTime))] /
                      indexObj$unit.rent[as.numeric(as.factor(unitTrans$rentTime))])
    unitTrans$adjRent <- unitTrans$rentValue * unitRentAdj
  } else {
    unitTrans <- NULL
  }
  
  ## Merge back together
  
  if(!is.null(houseTrans) && !is.null(unitTrans)){
    mTrans <- rbind(houseTrans, unitTrans)
  }
  if(!is.null(houseTrans) & is.null(unitTrans)){
    mTrans <- houseTrans
  }  
  if(is.null(houseTrans) & !is.null(unitTrans)){
    mTrans <- unitTrans
  }  
  if(is.null(houseTrans) & is.null(unitTrans)){
    mTrans <- NULL
  }  
  
  
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
                            metric=c('dmYield'),
                            spaceField='all', timeField='saleTime',
                            defDim='time', stsLimit=apmOptions$geoTempLimit, 
                            calcs=list(median='median'))
  
  # By Use   
  dmMetroH <- spaceTimeShard(matchData[matchData$PropertyType == 'House',],
                             metric=c('dmYield'),
                             spaceField='all', timeField='saleTime',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  
  dmMetroU <- spaceTimeShard(matchData[matchData$PropertyType == 'Unit',],
                             metric=c('dmYield'),
                             spaceField='all', timeField='saleTime',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  
  ## LGA  
  
  if(verbose) cat('...Analyze at LGA Level\n')
  
  dmLga <- spaceTimeShard(stsData = matchData,
                          metric=c('dmYield'),
                          spaceField='lga', timeField='saleTime',
                          defDim='time', stsLimit=apmOptions$geoTempLimit, 
                          calcs=list(median='median'))
  
  # By Use   
  dmLgaH <- spaceTimeShard(matchData[matchData$PropertyType == 'House',],
                           metric=c('dmYield'),
                           spaceField='lga', timeField='saleTime',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  dmLgaU <- spaceTimeShard(matchData[matchData$PropertyType == 'Unit',],
                           metric=c('dmYield'),
                           spaceField='lga', timeField='saleTime',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  ## SLA1 
  
  if(verbose) cat('...Analyze at SLA1 Level\n')
  
  dmSla <- spaceTimeShard(stsData = matchData,
                          metric=c('dmYield'),
                          spaceField='sla1', timeField='saleTime',
                          defDim='time', stsLimit=apmOptions$geoTempLimit, 
                          calcs=list(median='median'))
  
  # By Use   
  dmSlaH <- spaceTimeShard(matchData[matchData$PropertyType == 'House',],
                           metric=c('dmYield'),
                           spaceField='sla1', timeField='saleTime',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  dmSlaU <- spaceTimeShard(matchData[matchData$PropertyType == 'Unit',],
                           metric=c('dmYield'),
                           spaceField='sla1', timeField='saleTime',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median')) 
  
  ## Suburb
  
  if(verbose) cat('...Analyze at Suburb Level\n')
  
  dmSuburb <- spaceTimeShard(stsData = matchData,
                             metric=c('dmYield'),
                             spaceField='suburb', timeField='saleTime',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  
  # By Use   
  dmSuburbH <- spaceTimeShard(matchData[matchData$PropertyType == 'House',],
                              metric=c('dmYield'),
                              spaceField='suburb', timeField='saleTime',
                              defDim='time', stsLimit=apmOptions$geoTempLimit, 
                              calcs=list(median='median'))
  
  dmSuburbU <- spaceTimeShard(matchData[matchData$PropertyType == 'Unit',],
                              metric=c('dmYield'),
                              spaceField='suburb', timeField='saleTime',
                              defDim='time', stsLimit=apmOptions$geoTempLimit, 
                              calcs=list(median='median')) 
  
  ## PostCode 
  
  if(verbose) cat('...Analyze at Postcode Level\n')
  
  dmPostcode <- spaceTimeShard(stsData = matchData,
                               metric=c('dmYield'),
                               spaceField='postCode', timeField='saleTime',
                               defDim='time', stsLimit=apmOptions$geoTempLimit, 
                               calcs=list(median='median'))
  
  # By Use   
  dmPostcodeH <- spaceTimeShard(matchData[matchData$PropertyType == 'House',],
                                metric=c('dmYield'),
                                spaceField='postCode', timeField='saleTime',
                                defDim='time', stsLimit=apmOptions$geoTempLimit, 
                                calcs=list(median='median'))
  
  dmPostcodeU <- spaceTimeShard(matchData[matchData$PropertyType == 'Unit',],
                                metric=c('dmYield'),
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


########## WORKING BELOW HERE ------------------------------------------------------------





apmTidyMatch <- function(match.results, method.type='median'){
  
  globH <- match.results$metro$house$stsDF
  globH$type <- 'house'
  globU <- match.results$metro$unit$stsDF
  globU$type <- 'unit'
  globH$geo.level <- globU$geo.level <- 'Global'
  globH$spaceName <- globU$spaceName <- 'Global'
  
  slaH <- match.results$sla$house$stsDF
  slaH$type <- 'house'
  slaU <- match.results$sla$unit$stsDF
  slaU$type <- 'unit'
  slaH$geo.level <- slaU$geo.level <- 'sla'
  
  lgaH <- match.results$lga$house$stsDF
  lgaH$type <- 'house'
  lgaU <- match.results$lga$unit$stsDF
  lgaU$type <- 'unit'
  lgaH$geo.level <- lgaU$geo.level <- 'lga'
  
  subH<- match.results$suburb$house$stsDF
  subH$type <- 'house'
  subU<- match.results$suburb$unit$stsDF
  subU$type <- 'unit'
  subH$geo.level <- subU$geo.level <- 'suburb'
  
  pcH<- match.results$postcode$house$stsDF
  pcH$type <- 'house'
  pcU<- match.results$postcode$unit$stsDF
  pcH$type <- 'unit'
  pcH$geo.level <- pcU$geo.level <- 'postCode'
  
  all.geo <- list(globH, globU, lgaH, lgaU, slaH, slaU,
                      subH, subU, pcH, pcU)
  
  all.match <- rbind.fill(all.geo)
  if(method.type == 'median'){
    all.match$method <- method.type
    names(all.match)[1:2] <- c('time', 'geo')
  } else {
    all.match$method <- method.type
    all.match$price <- 0
    all.match$rent <- 0
    names(all.match)[1:3] <- c('time', 'yield', 'geo')
  }
  
  return(all.match)  
}



# apmIndMethodWrap <- function(trans.data, geos=c('All', 'lga', 'sla1', 'suburb',
#                                                 'postcode'),
#                              verbose=FALSE){
#   
#   ## Prep data and objects
#   
#   aimw.list <- list()
#   idL <- 1
#   
#   ## All area analysis
#   
#   if('All' %in% geos){
#     if(verbose) cat('Estimating all area index method\n')
#     all.ind <- apmGeoIndex('all', 'all', trans.data)
#     aimw.list[[idL]] <- all.ind
#     idL <- idL + 1
#   }
#   
#   ## LGA area analysis
#   
#   if('lga' %in% geos){
#     if(verbose) cat('Estimating lga level index method\n')
#     lga.ind <- apmIndGeoWrap('lga', trans.data)
#     aimw.list[[idL]] <- lga.ind
#     idL <- idL + 1
#   }
#   
#   ## SLA1 area analysis
#   
#   if('sla1' %in% geos){
#     if(verbose) cat('Estimating sla1 level index method\n')
#     sla1.ind <- apmIndGeoWrap('sla1', trans.data)
#     aimw.list[[idL]] <- sla1.ind
#     idL <- idL + 1
#   }
#   
#   ## Suburb area analysis
#   
#   if('suburb' %in% geos){
#     if(verbose) cat('Estimating suburb level index method\n')
#     suburb.ind <- apmIndGeoWrap('suburb', trans.data)
#     aimw.list[[idL]] <- suburb.ind
#     idL <- idL + 1
#   }
#   
#   ## Post code area analysis  
#   
#   if('postcode' %in% geos){
#     if(verbose) cat('Estimating postcode level index method\n')
#     postcode.ind <- apmIndGeoWrap('postCode', trans.data)
#     aimw.list[[idL]] <- postcode.ind
#     idL <- idL + 1
#   }
#   
#   ## Fix names and return values  
#   names(aimw.list) <- geos
#   
#   return(aimw.list)
#   
# }