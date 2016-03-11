##########################################################################################
#                                                                                        #
#  Suite of functions for analyzing price to rent ratios with sale rent method           #
#                                                                                        #
##########################################################################################

### Function to compare price and rent on only matched properties ------------------------

srmMatcher <- function(trans.data,           # Transaction data
                       indexObj,            # Index obj from apmAreaIndLevelWrap()
                       index.geo = 'Global',# Level at which to use adj Indexes
                       matchField = 'ID',   # Field containing matching ID
                       saleField = 'Price', # Field containing sale price
                       rentField = 'Rent',  # Field containing rent 
                       timeField = 'Year'   # Field containing time breakdown
){
  
  ## Deal with a global index.geo
  if(index.geo == 'Global') trans.data$Global <- 0
  
  ## Split into sales and rentals
  
  sales <- trans.data[trans.data$transType == 'sale', ]
  rentals <- trans.data[trans.data$transType == 'rent', ]
  
  ## Matching sales to rentals
  
  # Remove NAs in matchField
  xSales <- subset(sales, !is.na(sales[matchField]))
  xRentals <- subset(rentals, !is.na(rentals[matchField]))
  
  # Sort to order
  xSales <- xSales[order(xSales[, matchField]),]
  xRentals <- xRentals[order(xRentals[, matchField]),]
  
  # Extract matching field
  sMatch <- xSales[, matchField]
  rMatch <- xRentals[, matchField]
  
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
  
  mTrans <- srmTimeAdjGeoWrap(trans.data=mTrans,
                              indexObj=indexObj,
                              index.geo=index.geo)
  
  # Calc Yields
  mTrans$srm.saleyield <- (mTrans$adjRent * 52) / mTrans$saleValue
  mTrans$srm.rentyield <- (mTrans$rentValue * 52) / mTrans$adjSale
  mTrans$srm.yield <- (mTrans$srm.saleyield + mTrans$srm.rentyield) / 2
  
  # Add information on time between transactions
  mTrans$timeGap <- mTrans$rentTime - mTrans$saleTime
  
  ## Return Values    
  
  return(mTrans)  
} 

### Wrap function to apply time adjustment overall the entire choses geo -----------------

srmTimeAdjGeoWrap <- function(trans.data,
                              indexObj,
                              index.geo
){
  
  ## List of Geographies  
  
  if(index.geo != 'Global'){
    geo.list <- levels(as.factor(trans.data[, index.geo]))
  } else {
    geo.list <- 'Global'
  }
  
  ## Extract the relevant index objects
  
  idObj <- which(names(indexObj) == index.geo)
  index.obj <- indexObj[[idObj]]
  
  ## Make the matches
  
  match.list <- lapply(geo.list, FUN=srmTimeAdjuster, trans.data=trans.data,
                       indexObj=index.obj, index.geo=index.geo)
  
  
  ## Convert to data frame
  
  match.data <- rbind.fill(match.list)
  
  ## Return values  
  
  return(match.data)  
}

### Time adjusts the direct matches based on a given set of indexes ----------------------

srmTimeAdjuster <- function(trans.data,          # Transaction data
                            indexObj,           # Index obj from apmCreateIndexes()
                            index.geo,          # Geography to time adjust at
                            geo.value=NULL      # Specific Geo Value
)
{
  
  if(!is.null(geo.value)) trans.data <- trans.data[trans.data[ ,index.geo] == geo.value, ]
  
  idO <- which(names(indexObj) == geo.value)
  indexObj <- indexObj[[idO]]
  
  ## Split by use
  
  houseTrans <- trans.data[trans.data$PropertyType == "House", ]
  unitTrans <- trans.data[trans.data$PropertyType == "Unit", ]
  
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

### Function for applying match method to all properties ---------------------------------

srmYieldWrap <- function(matchData,             # Matched data object
                         verbose=FALSE)
{
  
  if(verbose) cat('Calculating yields with match method\n')
  
  ## All Metro  
  
  if(verbose) cat('...Analyze at Global Level\n')
  
  dmMetro <- spaceTimeShard(stsData = matchData,
                            metric=c('srm.yield'),
                            spaceField='all', timeField='saleTime',
                            defDim='time', stsLimit=apmOptions$geoTempLimit, 
                            calcs=list(median='median'))
  
  # By Use   
  dmMetroH <- spaceTimeShard(matchData[matchData$PropertyType == 'House',],
                             metric=c('srm.yield'),
                             spaceField='all', timeField='saleTime',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  
  dmMetroU <- spaceTimeShard(matchData[matchData$PropertyType == 'Unit',],
                             metric=c('srm.yield'),
                             spaceField='all', timeField='saleTime',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  
  ## LGA  
  
  if(verbose) cat('...Analyze at LGA Level\n')
  
  dmLga <- spaceTimeShard(stsData = matchData,
                          metric=c('srm.yield'),
                          spaceField='lga', timeField='saleTime',
                          defDim='time', stsLimit=apmOptions$geoTempLimit, 
                          calcs=list(median='median'))
  
  # By Use   
  dmLgaH <- spaceTimeShard(matchData[matchData$PropertyType == 'House',],
                           metric=c('srm.yield'),
                           spaceField='lga', timeField='saleTime',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  dmLgaU <- spaceTimeShard(matchData[matchData$PropertyType == 'Unit',],
                           metric=c('srm.yield'),
                           spaceField='lga', timeField='saleTime',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  ## SLA1 
  
  if(verbose) cat('...Analyze at SLA1 Level\n')
  
  dmSla <- spaceTimeShard(stsData = matchData,
                          metric=c('srm.yield'),
                          spaceField='sla1', timeField='saleTime',
                          defDim='time', stsLimit=apmOptions$geoTempLimit, 
                          calcs=list(median='median'))
  
  # By Use   
  dmSlaH <- spaceTimeShard(matchData[matchData$PropertyType == 'House',],
                           metric=c('srm.yield'),
                           spaceField='sla1', timeField='saleTime',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  dmSlaU <- spaceTimeShard(matchData[matchData$PropertyType == 'Unit',],
                           metric=c('srm.yield'),
                           spaceField='sla1', timeField='saleTime',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median')) 
  
  ## Suburb
  
  if(verbose) cat('...Analyze at Suburb Level\n')
  
  dmSuburb <- spaceTimeShard(stsData = matchData,
                             metric=c('srm.yield'),
                             spaceField='suburb', timeField='saleTime',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  
  # By Use   
  dmSuburbH <- spaceTimeShard(matchData[matchData$PropertyType == 'House',],
                              metric=c('srm.yield'),
                              spaceField='suburb', timeField='saleTime',
                              defDim='time', stsLimit=apmOptions$geoTempLimit, 
                              calcs=list(median='median'))
  
  dmSuburbU <- spaceTimeShard(matchData[matchData$PropertyType == 'Unit',],
                              metric=c('srm.yield'),
                              spaceField='suburb', timeField='saleTime',
                              defDim='time', stsLimit=apmOptions$geoTempLimit, 
                              calcs=list(median='median')) 
  
  ## PostCode 
  
  if(verbose) cat('...Analyze at Postcode Level\n')
  
  dmPostcode <- spaceTimeShard(stsData = matchData,
                               metric=c('srm.yield'),
                               spaceField='postCode', timeField='saleTime',
                               defDim='time', stsLimit=apmOptions$geoTempLimit, 
                               calcs=list(median='median'))
  
  # By Use   
  dmPostcodeH <- spaceTimeShard(matchData[matchData$PropertyType == 'House',],
                                metric=c('srm.yield'),
                                spaceField='postCode', timeField='saleTime',
                                defDim='time', stsLimit=apmOptions$geoTempLimit, 
                                calcs=list(median='median'))
  
  dmPostcodeU <- spaceTimeShard(matchData[matchData$PropertyType == 'Unit',],
                                metric=c('srm.yield'),
                                spaceField='postCode', timeField='saleTime',
                                defDim='time', stsLimit=apmOptions$geoTempLimit, 
                                calcs=list(median='median'))  
  
  ## Combine Results  
  
  dmResults <- list(Global=list(all=dmMetro, house=dmMetroH, unit=dmMetroU),
                    lga=list(all=dmLga, house=dmLgaH, unit=dmLgaU),
                    sla1=list(all=dmSla, house=dmSlaH, unit=dmSlaU),
                    suburb=list(all=dmSuburb, house=dmSuburbH, unit=dmSuburbU),
                    postCode=list(all=dmPostcode, house=dmPostcodeH, unit=dmPostcodeU))
  
  ## Return Results
  
  return(dmResults) 
}  
