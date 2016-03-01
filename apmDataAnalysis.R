##########################################################################################
#                                                                                        #
#  Suite of functions for analyzing price to rent ratios with APM data                   #
#                                                                                        #
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
  return(list(results = allData[ ,c('UID', 'Price', 'impPrice', 
                                    'Rent', 'impRent')],
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
  
  irValues$saleYield <- 0
  irValues$saleYield[idS] <- (irValues$impRent[idS] * 52) / irValues$Price[idS]
  irValues$rentYield <- 0
  irValues$rentYield[idR] <- (irValues$Rent[idR] * 52) / irValues$impPrice[idR]
  
  # Add Ratio to full dataset 
  transData$impYield <- irValues$impYield[match(transData$UID, irValues$UID)]
  transData$saleYield <- irValues$saleYield[match(transData$UID, irValues$UID)]
  transData$rentYield <- irValues$rentYield[match(transData$UID, irValues$UID)]
  
  xTrans <- subset(transData, !is.na(impYield)) 
  
  return(xTrans)
}  

### Create a set of indexes based on impute regression results ---------------------------

apmCreateIndexes <- function(irHouse,           # House results from impute regression
                             irUnit             # Unit results from impute regression
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


### Function for extracting time index from impute results ---------------------

apmMakeIndex <- function(coefs,                    # Coefs from impute models
                         timeField='transQtr',     # Field to search for coefs in
                         verbose=FALSE)
{
  
  coefs.df <- as.data.frame(coefs)
  coefs.time <- c(0, coefs.df$Estimate[grep(timeField, rownames(coefs.df))])
  coefs.index <- exp(c(0, (coefs.time[-1] - coefs.time[-length(coefs.time)]))) - 1
  
  return(coefs.index)
}

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



