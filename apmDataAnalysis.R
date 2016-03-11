##########################################################################################
#                                                                                        #
#  Suite of functions for analyzing price to rent ratios with APM data                   #
#                                                                                        #
##########################################################################################

##########################################################################################
# General purpose analytical functions working on all method types                       #
##########################################################################################

### Generic data tidying engine ----------------------------------------------------------

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
  unit$type <- 'unit'
  unit$geo.level <- geo.level
  
  if(geo.level == 'Global') {
    unit$spaceName <- 'Global'
  }
  
 ## Return Values
  
  return(rbind(house, unit))

}

### Function to wrap the generic tidyer over one geo level -------------------------------

apmGenTidyerGeoWrap <- function(yield.results,
                                geo.levels=apmOptions$geo.levels,
                                method.type='spag'){
   
 ## Make geo list
  
   tidy.list <- lapply(as.list(geo.levels), FUN = apmGenTidyer,
                       yield.results=yield.results)
   tidy.df <- rbind.fill(tidy.list)
  
 ## Add method type
   
   tidy.df$method <- method.type
   
 ## Fix remaining columns 
   
   if(method.type == 'spag'){
     names(tidy.df)[1:2] <- c('time', 'geo')
   } else {
     tidy.df$price <- 0
     tidy.df$rent <- 0
     names(tidy.df)[1:3] <- c('time', 'yield', 'geo')
   }
 
 ## Return Values   
     
  return(tidy.df)  
}
 
 
### Swap yield information from imputations and matching methods -------------------------

apmYieldSwap <- function(trans.data,       # Transaction data
                         match.data        # Set of matched data from apmSaleRentMatch()
)
{
  
  ##  Split into sales and rent sets
  
  transSales <- trans.data[trans.data$transType == 'sale', ]
  transRents <- trans.data[trans.data$transType == 'rent', ]
  
  ## Apply the imputed yields to the match data  
  
  match.data$imp.saleyield <- transSales$imp.saleyield[match(match.data$saleID,
                                                               transSales$UID)]
  match.data$imp.rentyield <- transRents$imp.rentyield[match(match.data$rentID,
                                                                  transRents$UID)]
  match.data$imp.saleyieldX <- transSales$imp.yield[match(match.data$saleID,
                                                          transSales$UID)]
  match.data$imp.rentyieldX <- transRents$imp.yield[match(match.data$rentID,
                                                          transRents$UID)]
  
  ## Resolve situations where properties have more than one matched yield  
  
  matchSales <- tapply2DF(xData=match.data$srm.saleyield, byField=match.data$saleID, 
                          xFunc=median)
  matchRents <- tapply2DF(xData=match.data$srm.rentyield, byField=match.data$rentID, 
                          xFunc=median) 
  
  ## Add matched yields to the transaction data  
  
  transSales$srm.saleyield <- matchSales$Var[match(transSales$UID, matchSales$ID)]
  transSales$srm.rentyield <- NA
  transRents$srm.rentyield <- matchRents$Var[match(transRents$UID, matchRents$ID)]
  transRents$srm.saleyield <- NA
  
  ## Recombine trans.data
  
  return(list(trans.data=rbind(transSales, transRents),
              match.data=match.data))
  
}  

## Compare the differences between the matched observations and all observations ---------

apmCompareSamples <- function(trans.data)
{  
  
  ## Create a match trans showing only those observations used in the match data  
  
  match.trans <- trans.data[!is.na(trans.data$srm.saleyield) | 
                             !is.na(trans.data$srm.rentyield),]
  
  ## De trend sales prices and rents of all transactions
  
  # Create separate data sets
  c.unit.rent <- subset(trans.data, transType=='rent' & PropertyType == 'Unit')
  c.house.rent <- subset(trans.data, transType=='rent' & PropertyType == 'House')
  c.unit.sale <- subset(trans.data, transType=='sale' & PropertyType == 'Unit')
  c.house.sale <- subset(trans.data, transType=='sale' & PropertyType == 'House')
  
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
  m.unit.rent <- subset(match.trans, transType=='rent' & PropertyType == 'Unit')
  m.house.rent <- subset(match.trans, transType=='rent' & PropertyType == 'House')
  m.unit.sale <- subset(match.trans, transType=='sale' & PropertyType == 'Unit')
  m.house.sale <- subset(match.trans, transType=='sale' & PropertyType == 'House')
  
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
  return(list(match.trans = match.trans,
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