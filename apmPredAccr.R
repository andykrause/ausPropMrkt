##########################################################################################
#                                                                                        #
#  Suite of functions for calculating the predictive error of prr results                #
#                                                                                        #
##########################################################################################

### Base error prediction engine   -------------------------------------------------------

apmErrorEngine <- function(srm.data,      
                           yield.data       
){
  
  # ARGUMENTS
  # 
  # srm.data = set of matched sale-rental data
  # yield.data = tidied yield data

 ## Subset into sales and rentals based on which observation is first
  
  s.data <- subset(srm.data, saleTime <= rentTime)
  r.data <- subset(srm.data, saleTime > rentTime)
  
 ## Set up objects for capture
  
  res.list <- list()
  rlX <- 1
  
 ## If sales first exist  
  
  if(nrow(s.data) != 0){
    s.data$tType <- 'sale'
    
    # Add the yield information at the time of the first transaction
    s.data$pYield <- yield.data$yield[match(s.data$rentTime, yield.data$time)]  
    
    # Predict rental value of sales and the error
    s.data$pValue <- ((s.data$adjSale * s.data$pYield) / 52) 
    s.data$error <- (s.data$rentValue - s.data$pValue) / s.data$rentValue
    
    res.list[[rlX]] <- s.data
    rlX <- 2
  }
  
  if(nrow(r.data) != 0){
    r.data$tType <- 'rent'
   
    # Add the yield information at the time of the first transaction
    r.data$pYield <- yield.data$yield[match(r.data$saleTime, yield.data$time)]  
  
    # Precict sale value of rentals and the error    
    r.data$pValue <- (r.data$adjRent * 52) / r.data$pYield
    r.data$error <- (r.data$saleValue - r.data$pValue) / r.data$saleValue
  
    res.list[[rlX]] <- r.data
  }
  
  # Merge data together  
  if(nrow(r.data) != 0 | nrow(s.data) != 0) {
    x.data <- rbind.fill(res.list)
  } else {
    x.data <- 'NA'
  }
  
  # Return values
  return(x.data)
}

### Wrapper to spread the error calcs over all three methods -----------------------------

apmErrorByMethod <- function(geo,
                             srm.data,        
                             yield.data,
                             geo.level    
){
  
  # ARGUMENTS
  # 
  # geo = specific geography to analyze
  # srm.data = set of matched sale-rental data
  # yield.data = tidied yield data
  # geo.level = geo.level at which to calculate the errors 
  
  if(geo!='Global') {
    srm.data <- srm.data[srm.data[,geo.level] == geo, ]
  }
  
  yield.data <- yield.data[yield.data$geo == geo & yield.data$geo.level==geo.level, ]
  
  if(nrow(srm.data) == 0 | nrow(yield.data) == 0) return('NA')
  
  # Calc error for median method
  spag.error <- apmErrorEngine(srm.data, yield.data[yield.data$method == 'spag',])
  spag.error$method<-'spag'
  
  # Calc error for imputation method
  index.error <- apmErrorEngine(srm.data, yield.data[yield.data$method == 'Index',])
  index.error$method<-'index'
  
  # Calc error for imputation method
  hedimp.error <- apmErrorEngine(srm.data, yield.data[yield.data$method == 'hedimp',])
  hedimp.error$method<-'hedimp'
  
  # calc error for matching method
  srm.error <- apmErrorEngine(srm.data, yield.data[yield.data$method == 'srm',])
  srm.error$method='srm'
  
  ##
  all.error <- rbind.fill(list(spag.error, index.error, hedimp.error, srm.error))
  
 ## Return values
  
  return(all.error)
  
}

apmPredGeoWrap <- function(srm.data,
                           yield.data,
                           geo.level='suburb'
                           )
{
  
  # ARGUMENTS
  # 
  # srm.data = set of matched sale-rental data
  # yield.data = tidied yield data
  # geo.level = geo.level at which to calculate the errors 
  
 ## Subset house and unit data
  
  h.data <- subset(srm.data, PropertyType == 'House')
  u.data <- subset(srm.data, PropertyType == 'Unit')
 
 ## Get the specific geo names
  
  if(geo.level == 'Global'){
    geo.list <- list('Global')
  } else {
    geo.list <- as.list(levels(as.factor(srm.data[,geo.level])))
  }
  
 ## Calculate the errors on the houses  
  
  # Run through all geos
  geo.house <- lapply(X=geo.list, FUN=apmErrorByMethod, srm.data = h.data, 
                      yield.data=yield.data[yield.data$type=='house',],
                      geo.level=geo.level)
  
  # Remove those with no results
  cut <- unlist(lapply(geo.house, class))
  cutX <- which(cut == 'character')
  if(length(cutX) > 0) geo.house <- geo.house[-cutX]
  
  # Turn to a data.frame and give label
  geo.house <- rbind.fill(geo.house)
  geo.house$type <- 'house'
  
 ## Calculate errors on units  
  
  # Run through all geos
  geo.unit <- lapply(X=geo.list, FUN=apmErrorByMethod, srm.data = u.data, 
                     yield.data=yield.data[yield.data$type=='unit',], 
                     geo.level=geo.level)
  
  # Remove those with no results
  cut <- unlist(lapply(geo.unit, class))
  cutX <- which(cut == 'character')
  if(length(cutX) > 0) geo.unit <- geo.unit[-cutX]
  
  # Turn to a data.frame and give label
  geo.unit <- rbind.fill(geo.unit)
  geo.unit$type <- 'unit'
  
 ## Bind both houses and units together  
  
  all.geo <- rbind.fill(list(geo.house, geo.unit))
  all.geo$geo.level <- geo.level
  
 ## Return values  
  
  return(all.geo)
  
}

### Level wrapper for error calculations -------------------------------------------------

apmPredLevelWrap <- function(srm.data,
                             yield.data,
                             toDF=FALSE){
  
 # ARGUMENTS
 # 
 # srm.data = set of matched sale-rental data
 # yield.data = tidied yield data
 # toDF = convert to a data.frame (or leave as a list?)
  
 ## Get list of geo levels  
  
  level.list <- as.list(apmOptions$geo.levels)
  
 ## Apply over all geo levels  
  
  level.res <- lapply(level.list, apmPredGeoWrap, dmData=srm.data, yieldData=yield.data)
  names(level.res) <- level.list
  
 ## If convert to DF
  
  if(toDF) level.res <- rbind.fill(level.res)
  
 ## Return Values
  
  return(level.res)
  
}
  
 