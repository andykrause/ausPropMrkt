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

##########################################################################################
#  Wrapper Function that runs all data analysis                                          #
##########################################################################################

apmFullDataAnalysis <- function(clean.trans,
                                data.path,
                                writeout=TRUE){
  
  if(verbose) cat('Create raw price and rent indexes at each level and geography\n')
  
  index.values <- indexLevelWrap(clean.trans, wrap.function='indexGeoWrap')
  
  ## Create the impute regression values
  
  if(verbose) cat('Creating Imputed Regression values\n')
  
  hedimp.data <- hedImpFullEstimation(clean.trans)
  
  ## Create a set of matched data (adjusted with global time indexes)
  if(verbose) cat('Building Matched Data\n')
  match.data <- srmMatcher(trans.data=clean.trans, index.obj=index.values,
                           index.geo='suburb',
                           match.field='AddressID', sale.field='transValue',
                           rent.field='transValue', time.field='transQtr')
  
  ## Add impute data to  and vice versa
  
  if(verbose) cat('Swapping Impute and Matched Yields\n')
  swappedData <- apmYieldSwap(trans.data=hedimp.data,
                              match.data=match.data)
  trans.data <- swappedData$trans.data
  match.data <- swappedData$match.data
  
### Create indices from each of the methods ----------------------------------------------

  ## Via the Index method  
  if(verbose) cat('Index analysis\n')
  index.results <- indexLevelWrap(index.values, wrap.function='indexTYGeoWrap')
  
  ## via the hedonic impute regression
  if(verbose) cat('Hedimp analysis\n')
  hedimp.results <- hedimpYieldWrap(hedimp.data, yield.field='imp.actyield', verbose)  
  
  ## Apply match method
  if(verbose) cat('Match analysis\n')
  match.results <- srmYieldWrap(match.data, verbose)
  
  ### Tidy up the data ---------------------------------------------------------------------
  
  ## Tidy each type  
  if(verbose) cat('Tidying Data\n')
  index.tidy <- indexLevelWrap(index.results, wrap.function='indexTidyerGeoWrap')
  srm.tidy <- apmGenTidyerGeoWrap(match.results, method.type='srm')
  hedimp.tidy <- apmGenTidyerGeoWrap(hedimp.results, method.type='hedimp')

  # Combine
  
  yield.results <- rbind(spag.tidy, index.tidy, hedimp.tidy, srm.tidy)  
  
  if(writeout){  
    if(verbose) cat('Writing Data\n')
    save(clean.trans, match.data, index.values, yield.results, 
         index.results, hedimp.results, match.results,
         file=paste0(data.path, 'yieldResults.RData'))  
    
    write.csv(clean.trans, paste0(data.path, 'imputedYields.csv'))
    write.csv(match.data, paste0(data.path, 'matchedYields.csv'))
  }
  
 ## Return data  
  
  return(list(tidy.data=yield.results,
              impute.data=clean.trans,
              match.data=match.data,
              index.values=index.values,
              results=list(index=index.results,
                           hedimp=hedimp.results,
                           match=match.results)))  
  
}

### Function to calculate bias from one method to other methods --------------------------


apmCalcBias <- function(geo.level,
                        yield.data,
                        comp.method='Match'){
  
  geo.data <- yield.data[yield.data$geo.level == geo.level, ]
  geo.data$uid <- paste0(geo.data$time, "..", geo.data$geo)
  
  geo.h <- geo.data[geo.data$type == 'Houses', ]
  geo.u <- geo.data[geo.data$type == 'Units', ]
  
  geo.hnm <- geo.h[geo.h$method != comp.method, ]
  geo.hm <- geo.h[geo.h$method == comp.method, ]
  names(geo.hm)[which(names(geo.hm) == 'yield')] <- 'comp.yield'
  
  geo.unm <- geo.u[geo.u$method != comp.method, ]
  geo.um <- geo.u[geo.u$method == comp.method, ]
  names(geo.um)[which(names(geo.um) == 'yield')] <- 'comp.yield'
  
  geo.hh <- merge(geo.hnm, geo.hm[,c('uid', 'comp.yield')])
  geo.hh$Bias <- geo.hh$yield - geo.hh$comp.yield
  
  geo.uu <- merge(geo.unm, geo.um[,c('uid', 'comp.yield')])
  geo.uu$Bias <- geo.uu$yield - geo.uu$comp.yield
  
  return(rbind(geo.hh, geo.uu))
  
} 
