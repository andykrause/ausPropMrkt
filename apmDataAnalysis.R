##########################################################################################
#                                                                                        #
#  Suite of functions for analyzing price to rent ratios with APM data                   #
#                                                                                        #
##########################################################################################

##########################################################################################
# General purpose analytical functions working on all method types                       #
##########################################################################################

apmGenTidyer <- function(yield.results,
                         geo.level){
  
 ## Extract results
  
  geo.results <- yield.results[[which(names(yield.results) == geo.level)]]
  
  # House
  house <- geo.results$house$stsDF
  house$type <- 'house'
  house$geo.level <- geo.level
  
  if(geo.level == 'Global') {
    house$spaceName <- 'Global'
  } 
  
  # Unit
  unit <- geo.results$unit$stsDF
  unit$type <- 'house'
  unit$geo.level <- geo.level
  
  if(geo.level == 'Global') {
    unit$spaceName <- 'Global'
  }
  
 ## Return Values
  
  return(rbind(house, unit))

}

###

apmGenTidyerGeoWrap <- function(yield.results,
                                geo.levels=apmOptions$geo.levels,
                                method.type='median'){
   
 ## Make geo list
  
   tidy.list <- lapply(as.list(geo.levels), FUN = apmGenTidyer,
                       yield.results=yield.results)
   tidy.df <- rbind.fill(tidy.list)
  
 ## Add method type
   
   tidy.df$method <- method.type
   
 ## Fix remaining columns 
   
   if(method.type == 'median'){
     names(tidy.df)[1:2] <- c('time', 'geo')
   } else {
     tidy.df$price <- 0
     tidy.df$rent <- 0
     names(tidy.df)[1:3] <- c('time', 'yield', 'geo')
   }
 
 ## Return Values   
     
  return(tidy.df)  
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



# 
# 
# apmTidyMatch <- function(match.results, method.type='median'){
#   
#   globH <- match.results$metro$house$stsDF
#   globH$type <- 'house'
#   globU <- match.results$metro$unit$stsDF
#   globU$type <- 'unit'
#   globH$geo.level <- globU$geo.level <- 'Global'
#   globH$spaceName <- globU$spaceName <- 'Global'
#   
#   slaH <- match.results$sla$house$stsDF
#   slaH$type <- 'house'
#   slaU <- match.results$sla$unit$stsDF
#   slaU$type <- 'unit'
#   slaH$geo.level <- slaU$geo.level <- 'sla'
#   
#   lgaH <- match.results$lga$house$stsDF
#   lgaH$type <- 'house'
#   lgaU <- match.results$lga$unit$stsDF
#   lgaU$type <- 'unit'
#   lgaH$geo.level <- lgaU$geo.level <- 'lga'
#   
#   subH<- match.results$suburb$house$stsDF
#   subH$type <- 'house'
#   subU<- match.results$suburb$unit$stsDF
#   subU$type <- 'unit'
#   subH$geo.level <- subU$geo.level <- 'suburb'
#   
#   pcH<- match.results$postcode$house$stsDF
#   pcH$type <- 'house'
#   pcU<- match.results$postcode$unit$stsDF
#   pcH$type <- 'unit'
#   pcH$geo.level <- pcU$geo.level <- 'postCode'
#   
#   all.geo <- list(globH, globU, lgaH, lgaU, slaH, slaU,
#                       subH, subU, pcH, pcU)
#   
#   all.match <- rbind.fill(all.geo)
#   if(method.type == 'median'){
#     all.match$method <- method.type
#     names(all.match)[1:2] <- c('time', 'geo')
#   } else {
#     all.match$method <- method.type
#     all.match$price <- 0
#     all.match$rent <- 0
#     names(all.match)[1:3] <- c('time', 'yield', 'geo')
#   }
#   
#   return(all.match)  
# }
# 


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