##########################################################################################
#                                                                                        #
#  Suite of functions for analyzing price to rent ratios with hedonic imputation method  #
#                                                                                        #
##########################################################################################

##########################################################################################
# Functions for creating price and rent imputed values at each location                        #
##########################################################################################

### Basic engine to run the impute regression on a set of sale and rental data -----------

hedimpEngine <- function(trans.data,             
                         reg.spec, 
                         geo.level='Global',
                         verbose = FALSE        
){
  
  # ARGUMENTS
  #
  # trans.data = transaction data with sales and rentals
  # reg.spec = regression specification to be used
  # verbose = show progress?
  
  ## Split data
  
  if(verbose) cat('...Separating sales and rentals\n')
  sale.data <- subset(trans.data, transType == 'sale')
  rent.data <- subset(trans.data, transType == 'rent')
  
  ## Estimate models and make new predictions
  
  # Fix equation
  
  reg.spec <- update(reg.spec, . ~ . -as.factor(postCode))
  
  # Esimate models
  if(verbose) cat('......Estimating sale and rent models\n')
  sale.model <- lm(reg.spec, data=sale.data)
  rent.model <- lm(reg.spec, data=rent.data)
  
  # Make predictions of imputed values
  if(verbose) cat('......Imputing values\n')
  hedimp.price <- exp(predict(sale.model, newdata=rent.data))
  hedimp.rent <- exp(predict(rent.model, newdata=sale.data))
  
  ## Building data compatability 
  
  if(verbose) cat('......Stacking observed and imputed values\n')
  
  sale.data$Price <- sale.data$transValue
  sale.data$hedimp.price <- round(exp(sale.model$fitted.values), 0)
  sale.data$Rent <- rep(0, nrow(sale.data))
  sale.data$hedimp.rent <- hedimp.rent
  
  rent.data$Price <- rep(0, nrow(rent.data))
  rent.data$hedimp.price <- hedimp.price
  rent.data$Rent <- rent.data$transValue
  rent.data$hedimp.rent <- round(exp(rent.model$fitted.values), 0)
  
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
  
  return(list(results = all.data[ ,c('UID', 'Price', 'hedimp.price',
                                     'Rent', 'hedimp.rent')],
              sale.model = sale.model.info,
              rent.model = rent.model.info))
  
}


hedimpGeoWrap <- function(geo.field,
                          x.data,
                          verbose=FALSE)
{
  
  # ARGUMENTS
  #
  # geo.field = geogrphic field for which to calculate all of the indexes
  # x.data = transaction data
  # time.field = field containing the time period analyzed
  
  ## Get the list of geographies to use
  
  if(verbose) cat('Getting list of geographic areas\n')
  
  if(geo.field != 'Global'){
    geo.list <- levels(as.factor(x.data[, geo.field]))
  } else {
    geo.list <- 'Global'
  }
  ## Apply geo index method across all
  
  if(verbose) cat('Calculating Indexes across all geographies\n')
  
  ind.list <- lapply(geo.list, FUN=hedimpModelWrap, x.data=x.data,
                     geo.field=geo.field, verbose=verbose)
  
  ## name list items
  
  names(ind.list) <- geo.list
  
  ## return values
  
  return(ind.list)
  
}    

hedimpModelWrap <- function(geo.value, geo.field, x.data, verbose){
  
  house.data <- x.data[x.data[ ,geo.field] == geo.value & 
                         x.data$PropertyType == 'House',]
  unit.data <- x.data[x.data[ ,geo.field] == geo.value & 
                        x.data$PropertyType == 'Unit',]
  
  h.r <- which(house.data$transType == 'rent')
  u.r <- which(unit.data$transType == 'rent')
  h.s <- which(house.data$transType == 'sale')
  u.s <- which(unit.data$transType == 'sale')
  
  if(length(table(house.data$transQtr[h.r])) == 20 &
     length(table(house.data$transQtr[h.s])) == 20){
    house.results <- hedimpEngine(house.data, apmOptions$houseEquation, 
                                  geo.level=geo.field,
                                  verbose = FALSE)
  } else {
    house.results <- NULL
  }
  
  if(length(table(unit.data$transQtr[u.r])) == 20 &
     length(table(unit.data$transQtr[u.s])) == 20){
    unit.results <- hedimpEngine(unit.data, apmOptions$unitEquation, 
                                 geo.level=geo.field,
                                 verbose = FALSE)
  } else{
    unit.results <- NULL
  }
  return(list(house=house.results,
              unit=unit.results))
  
}

### Assign the yields to the trans data --------------------------------------------------

hedimpAssignYields <- function(trans.data,            
                               hedimp.house,          
                               hedimp.unit            
)
{
  
  # ARGUMENTS
  #
  # trans.data = transaction data with sales and rentals
  # hedimp.house = imputed house regression results
  # hedimp.unit = imputed unit regression results
  
  # Extract values
  hedimpValues <- rbind(hedimp.house$results, hedimp.unit$results)
  
  # Calculate the ratio
  hedimpValues$imp.yield <- (hedimpValues$hedimp.rent * 52) / hedimpValues$hedimp.price
  
  # Calculate the mixed ratios
  idS <- which(hedimpValues$Rent == 0)
  idR <- which(hedimpValues$Price == 0)
  
  hedimpValues$imp.saleyield <- 0
  hedimpValues$imp.saleyield[idS] <- ((hedimpValues$hedimp.rent[idS] * 52) / 
                                       hedimpValues$Price[idS])
  hedimpValues$imp.rentyield <- 0
  hedimpValues$imp.rentyield[idR] <- ((hedimpValues$Rent[idR] * 52) / 
                                        hedimpValues$hedimp.price[idR])
  
  # Add Ratio to full dataset 
  trans.data$imp.yield <- hedimpValues$imp.yield[match(trans.data$UID, hedimpValues$UID)]
  trans.data$imp.saleyield <- hedimpValues$imp.saleyield[match(trans.data$UID,
                                                               hedimpValues$UID)]
  trans.data$imp.rentyield <- hedimpValues$imp.rentyield[match(trans.data$UID, 
                                                               hedimpValues$UID)]
  trans.data$imp.actyield <- ifelse(trans.data$imp.saleyield == 0, 
                                    trans.data$imp.rentyield, 
                                    trans.data$imp.saleyield)
  
  # Remove those with missing values
  xTrans <- subset(trans.data, !is.na(imp.yield)) 
  
  ## Return  transactions  
  
  return(xTrans)
}  

##########################################################################################
# Functions for creating yields, price and rent index over all geos                      #
##########################################################################################

hedimpYieldWrap <- function(cleanData, 
                            yield.field=imp.yield, # Clean trans data (apmDataObj)
                            verbose=FALSE
)
{
  
  ### Break down results by dimensions -------------------------------------------
  
  ## Metro
  
  if(verbose) cat('...Analyze at Global Level\n')
  
  # Metro 
  hedimpMetro <- spaceTimeShard(stsData = cleanData,
                            metric=c(yield.field),
                            spaceField='all', timeField='transQtr',
                            defDim='time', stsLimit=apmOptions$geoTempLimit, 
                            calcs=list(median='median'))
  
  # By Use
  hedimpMetroH <- spaceTimeShard(cleanData[cleanData$PropertyType == 'House', ],
                             metric=c(yield.field),
                             spaceField='all', timeField='transQtr',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  
  hedimpMetroU <- spaceTimeShard(cleanData[cleanData$PropertyType == 'Unit', ],
                             metric=c(yield.field),
                             spaceField='all', timeField='transQtr',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  ## LGA
  
  if(verbose) cat('...Analyze at LGA Level\n')
  
  hedimpLga <- spaceTimeShard(stsData = cleanData,
                          metric=c(yield.field),
                          spaceField='lga', timeField='transQtr',
                          defDim='time', stsLimit=apmOptions$geoTempLimit, 
                          calcs=list(median='median'))
  
  # By Use
  hedimpLgaH <- spaceTimeShard(cleanData[cleanData$PropertyType == 'House', ],
                           metric=c(yield.field),
                           spaceField='lga', timeField='transQtr',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  hedimpLgaU <- spaceTimeShard(cleanData[cleanData$PropertyType == 'Unit', ],
                           metric=c(yield.field),
                           spaceField='lga', timeField='transQtr',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median')) 
  
  ## SLA1
  
  if(verbose) cat('...Analyze at SLA Level\n')
  
  hedimpSla <- spaceTimeShard(stsData = cleanData,
                          metric=c(yield.field),
                          spaceField='sla1', timeField='transQtr',
                          defDim='time', stsLimit=apmOptions$geoTempLimit, 
                          calcs=list(median='median'))
  
  # By Use
  hedimpSlaH <- spaceTimeShard(cleanData[cleanData$PropertyType == 'House', ],
                           metric=c(yield.field),
                           spaceField='sla1', timeField='transQtr',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  hedimpSlaU <- spaceTimeShard(cleanData[cleanData$PropertyType == 'Unit', ],
                           metric=c(yield.field),
                           spaceField='sla1', timeField='transQtr',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  ## Suburb
  
  if(verbose) cat('...Analyze at Suburb Level\n')
  
  hedimpSuburb <- spaceTimeShard(stsData = cleanData,
                             metric=c(yield.field),
                             spaceField='suburb', timeField='transQtr',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  
  # By Use
  hedimpSuburbH <- spaceTimeShard(cleanData[cleanData$PropertyType == 'House', ],
                              metric=c(yield.field),
                              spaceField='suburb', timeField='transQtr',
                              defDim='time', stsLimit=apmOptions$geoTempLimit, 
                              calcs=list(median='median'))
  
  hedimpSuburbU <- spaceTimeShard(cleanData[cleanData$PropertyType == 'Unit', ],
                              metric=c(yield.field),
                              spaceField='suburb', timeField='transQtr',
                              defDim='time', stsLimit=apmOptions$geoTempLimit, 
                              calcs=list(median='median'))
  
  ## post code
  
  if(verbose) cat('...Analyze at Postcode Level\n')
  
  hedimpPostcode <- spaceTimeShard(stsData = cleanData,
                               metric=c(yield.field),
                               spaceField='postCode', timeField='transQtr',
                               defDim='time', stsLimit=apmOptions$geoTempLimit, 
                               calcs=list(median='median'))
  
  # By Use
  hedimpPostcodeH <- spaceTimeShard(cleanData[cleanData$PropertyType == 'House', ],
                                metric=c(yield.field),
                                spaceField='postCode', timeField='transQtr',
                                defDim='time', stsLimit=apmOptions$geoTempLimit, 
                                calcs=list(median='median'))
  
  hedimpPostcodeU <- spaceTimeShard(cleanData[cleanData$PropertyType == 'Unit', ],
                                metric=c(yield.field),
                                spaceField='postCode', timeField='transQtr',
                                defDim='time', stsLimit=apmOptions$geoTempLimit, 
                                calcs=list(median='median')) 
  
  ## Combine Results  
  
  hedimpResults <- list(Global=list(all=hedimpMetro, house=hedimpMetroH, unit=hedimpMetroU),
                    lga=list(all=hedimpLga, house=hedimpLgaH, unit=hedimpLgaU),
                    sla1=list(all=hedimpSla, house=hedimpSlaH, unit=hedimpSlaU),
                    suburb=list(all=hedimpSuburb, house=hedimpSuburbH, unit=hedimpSuburbU),
                    postCode=list(all=hedimpPostcode, house=hedimpPostcodeH, unit=hedimpPostcodeU))
  
  ## Return Results
  
  return(hedimpResults)  
  
}
