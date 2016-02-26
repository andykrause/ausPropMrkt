

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
                    lga=list(all=mmSla, house=mmSlaH, unit=mmSlaU),
                    lga=list(all=mmSuburb, house=mmSuburbH, unit=mmSuburbU),
                    lga=list(all=mmPostcode, house=mmPostcodeH, unit=mmPostcodeU))

 ## Return Results

  return(mmResults)
}

imputeMethodWrap <- function(cleanData,            # Clean trans data (apmDataObj)
                             verbose=FALSE
                             )
{

 ## Estimate models and make new predictions: Global by Use
  
  if(verbose) cat('Developing Price Imputation Models\n')
  
  # For houses
  if(verbose) cat('...Imputing House Prices and Rents\n')
  houseResults <- prrImputeReg(apmOptions$houseEquation, 
                               subset(cleanData, transType == 'sale' &
                                        PropertyType == 'House' & 
                                        QT_house_postCode == 1),
                               subset(cleanData, transType == 'rent' &
                                        PropertyType == 'House' & 
                                        QT_house_postCode == 1),
                               verbose=verbose)
  
  # For Units
  if(verbose) cat('...Imputing Unit Prices and Rents\n')
  unitResults <- prrImputeReg(apmOptions$unitEquation, 
                              subset(cleanData, transType == 'sale' &
                                       PropertyType == 'Unit' & 
                                       QT_unit_postCode == 1),
                              subset(cleanData, transType == 'rent' &
                                       PropertyType == 'Unit' & 
                                       QT_unit_postCode == 1),
                              verbose=verbose)
  
  assign('imputeRegResults', list(houseResults=houseResults,
                                  unitResults=unitResults),
         envir=.GlobalEnv)
  
  ## Calculate the ratio
  
  if(verbose) cat('...Calculating Yield Ratios\n')
  
  # Extract vales
  irValues <- rbind(houseResults$results, unitResults$results)
  
  # Calculate the ratio
  irValues$impYield <- (irValues$Rent * 52) / irValues$Price
  
  # Add Ratio to full dataset 
  cleanData$impYield <- irValues$impYield[match(cleanData$UID, irValues$UID)]
  xTrans <- subset(cleanData, !is.na(impYield)) 
  

  ### Break down results by dimensions -------------------------------------------
  
  ## Metro
  
  if(verbose) cat('...Analyze at Metro Level\n')
  
  # Metro 
  irMetro <- spaceTimeShard(stsData = xTrans,
                            metric=c('impYield'),
                            spaceField='all', timeField='transQtr',
                            defDim='time', stsLimit=apmOptions$geoTempLimit, 
                            calcs=list(median='median'))
  
  # By Use
  irMetroH <- spaceTimeShard(xTrans[xTrans$PropertyType == 'House', ],
                             metric=c('impYield'),
                             spaceField='all', timeField='transQtr',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  
  irMetroU <- spaceTimeShard(xTrans[xTrans$PropertyType == 'Unit', ],
                             metric=c('impYield'),
                             spaceField='all', timeField='transQtr',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  ## LGA
  
  if(verbose) cat('...Analyze at LGA Level\n')
  
  irLga <- spaceTimeShard(stsData = xTrans,
                          metric=c('impYield'),
                          spaceField='lga', timeField='transQtr',
                          defDim='time', stsLimit=apmOptions$geoTempLimit, 
                          calcs=list(median='median'))
  
  # By Use
  irLgaH <- spaceTimeShard(xTrans[xTrans$PropertyType == 'House', ],
                           metric=c('impYield'),
                           spaceField='lga', timeField='transQtr',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  irLgaU <- spaceTimeShard(xTrans[xTrans$PropertyType == 'Unit', ],
                           metric=c('impYield'),
                           spaceField='lga', timeField='transQtr',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median')) 
  
  ## SLA1
  
  if(verbose) cat('...Analyze at SLA Level\n')
  
  irSla <- spaceTimeShard(stsData = xTrans,
                          metric=c('impYield'),
                          spaceField='sla1', timeField='transQtr',
                          defDim='time', stsLimit=apmOptions$geoTempLimit, 
                          calcs=list(median='median'))
  
  # By Use
  irSlaH <- spaceTimeShard(xTrans[xTrans$PropertyType == 'House', ],
                           metric=c('impYield'),
                           spaceField='sla1', timeField='transQtr',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  irSlaU <- spaceTimeShard(xTrans[xTrans$PropertyType == 'Unit', ],
                           metric=c('impYield'),
                           spaceField='sla1', timeField='transQtr',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  ## Suburb
  
  if(verbose) cat('...Analyze at Suburb Level\n')
  
  irSuburb <- spaceTimeShard(stsData = xTrans,
                             metric=c('impYield'),
                             spaceField='suburb', timeField='transQtr',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  
  # By Use
  irSuburbH <- spaceTimeShard(xTrans[xTrans$PropertyType == 'House', ],
                              metric=c('impYield'),
                              spaceField='suburb', timeField='transQtr',
                              defDim='time', stsLimit=apmOptions$geoTempLimit, 
                              calcs=list(median='median'))
  
  irSuburbU <- spaceTimeShard(xTrans[xTrans$PropertyType == 'Unit', ],
                              metric=c('impYield'),
                              spaceField='suburb', timeField='transQtr',
                              defDim='time', stsLimit=apmOptions$geoTempLimit, 
                              calcs=list(median='median'))
  
  ## post code
  
  if(verbose) cat('...Analyze at Postcode Level\n')
  
  irPostcode <- spaceTimeShard(stsData = xTrans,
                               metric=c('impYield'),
                               spaceField='postCode', timeField='transQtr',
                               defDim='time', stsLimit=apmOptions$geoTempLimit, 
                               calcs=list(median='median'))
  
  # By Use
  irPostcodeH <- spaceTimeShard(xTrans[xTrans$PropertyType == 'House', ],
                                metric=c('impYield'),
                                spaceField='postCode', timeField='transQtr',
                                defDim='time', stsLimit=apmOptions$geoTempLimit, 
                                calcs=list(median='median'))
  
  irPostcodeU <- spaceTimeShard(xTrans[xTrans$PropertyType == 'Unit', ],
                                metric=c('impYield'),
                                spaceField='postCode', timeField='transQtr',
                                defDim='time', stsLimit=apmOptions$geoTempLimit, 
                                calcs=list(median='median')) 
  
  ## Combine Results  
  
  irResults <- list(metro=list(all=irMetro, house=irMetroH, unit=irMetroU),
                    lga=list(all=irLga, house=irLgaH, unit=irLgaU),
                    lga=list(all=irSla, house=irSlaH, unit=irSlaU),
                    lga=list(all=irSuburb, house=irSuburbH, unit=irSuburbU),
                    lga=list(all=irPostcode, house=irPostcodeH, unit=irPostcodeU))
  
  ## Return Results
  
  return(irResults)  
  
}
