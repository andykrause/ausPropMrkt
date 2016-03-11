##########################################################################################
#                                                                                        #
#  Suite of functions for analyzing price to rent ratios with Index based method         #
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
# Functions for turning price indexes into yield indexes                                 #
##########################################################################################

apmIndMethodWrap <- function(index.values,
                             geos=c('Global', 'lga', 'sla1', 'suburb', 'postCode'),
                             verbose=FALSE){
  
  ## Prep data and objects
  
  aimw.list <- list()
  idL <- 1
  
  ## All area analysis
  
  if('Global' %in% geos){
    if(verbose) cat('Estimating global index method\n')
    glob.ind <- apmGeoIndex(index.values$Global)
    aimw.list[[idL]] <- glob.ind
    idL <- idL + 1
  }
  
  ## LGA area analysis
  
  if('lga' %in% geos){
    if(verbose) cat('Estimating lga level index method\n')
    lga.ind <- apmIndGeoWrap(index.values$lga)
    aimw.list[[idL]] <- lga.ind
    idL <- idL + 1
  }
  
  ## SLA1 area analysis
  
  if('sla1' %in% geos){
    if(verbose) cat('Estimating sla1 level index method\n')
    sla1.ind <- apmIndGeoWrap(index.values$sla1)
    aimw.list[[idL]] <- sla1.ind
    idL <- idL + 1
  }
  
  ## Suburb area analysis
  
  if('suburb' %in% geos){
    if(verbose) cat('Estimating suburb level index method\n')
    suburb.ind <- apmIndGeoWrap(index.values$suburb)
    aimw.list[[idL]] <- suburb.ind
    idL <- idL + 1
  }
  
  ## Post code area analysis  
  
  if('postCode' %in% geos){
    if(verbose) cat('Estimating postcode level index method\n')
    postcode.ind <- apmIndGeoWrap(index.values$postCode)
    aimw.list[[idL]] <- postcode.ind
    idL <- idL + 1
  }
  
  ## Fix names and return values  
  names(aimw.list) <- geos
  
  return(aimw.list)
  
}

apmIndGeoWrap <- function(series.obj){
  
  ## Get the list of geographies to use
  
  geo.list <- names(series.obj)
  
  ## Apply geo index method across all
  
  ind.list <- lapply(geo.list, FUN=apmGeoIndex, series.obj=series.obj)
  
  ## name list items
  
  names(ind.list) <- geo.list
  
  ## return values
  
  return(ind.list)
  
}

apmGeoIndex <- function(series.obj, series.name="Global"){
  
  ## Fix equations 
  
  if(series.name == 'Global'){ 
    raw.series <- series.obj$raw
  } else {
    raw.series <- series.obj[[which(names(series.obj) == series.name)]]$raw
  }
  
  
  if(raw.series$house.rent[1] != "NA" & raw.series$house.sale[1] != 'NA' &
     length(raw.series$house.rent) == length(raw.series$house.sale)){
    house.series <- (raw.series$house.rent * 52) / raw.series$house.sale
  } else {
    house.series <- "NA"
  }
  
  if(raw.series$unit.rent[1] != "NA" & raw.series$unit.sale[1] != 'NA' &
     length(raw.series$unit.rent) == length(raw.series$unit.sale)){
    unit.series <- (raw.series$unit.rent * 52) / raw.series$unit.sale
  } else {
    unit.series <- "NA"
  }
  
  ## Return values
  
  return(list(house.yields=house.series,
              unit.yields=unit.series,
              house.price=raw.series$house.sale,
              house.rent=raw.series$house.rent,
              unit.price=raw.series$unit.sale,
              unit.rent=raw.series$unit.rent))
}

##########################################################################################
# Functions for tidying up index method data                                             #
##########################################################################################




apmTidyIndexWrap <- function(index.results, 
                             geo.names=c('Global', 'lga', 'sla1', 'suburb', 'postCode')){
  
  
  atiw.list <- NULL
  gnIDX <- 1
  
  if('Global' %in% geo.names){
    temp.df <- apmTidyIndex(index.results, geo.name='Global')
    temp.df$geo.level <- 'Global'
    atiw.list[[gnIDX]] <- temp.df
    gnIDX <- gnIDX + 1
  }
  
  
  if('lga' %in% geo.names){
    temp.df <- apmTidyIndexGeo(index.results$lga, geo.level='lga')
    temp.df$geo.level <- 'lga'
    atiw.list[[gnIDX]] <- temp.df
    gnIDX <- gnIDX + 1
  }
  
  if('sla1' %in% geo.names){
    temp.df <- apmTidyIndexGeo(index.results$sla1, geo.level='sla1')
    temp.df$geo.level <- 'sla1'
    atiw.list[[gnIDX]] <- temp.df
    gnIDX <- gnIDX + 1
  }
  
  if('suburb' %in% geo.names){
    
    temp.df <- apmTidyIndexGeo(index.results$suburb, geo.level='suburb')
    temp.df$geo.level <- 'suburb'
    atiw.list[[gnIDX]] <- temp.df
    gnIDX <- gnIDX + 1
  }
  
  if('postCode' %in% geo.names){
    temp.df <- apmTidyIndexGeo(index.results$postCode, geo.level='postCode')
    temp.df$geo.level <- 'postCode'
    atiw.list[[gnIDX]] <- temp.df
    gnIDX <- gnIDX + 1
  }
  
  return(rbind.fill(atiw.list))
  
}



apmTidyIndexGeo <- function(index.results, geo.level){
  
  geo.list <- as.list(names(index.results))
  
  ind.list <- lapply(X=geo.list, FUN=apmTidyIndex, index.results=index.results)
  
  ind.class <- which(unlist(lapply(ind.list, class)) == 'data.frame')
  
  ind.df <- rbind.fill(ind.list[ind.class])  
  
  return(ind.df)
}


apmTidyIndex <- function(index.results, geo.name='Global'){
  
  index.results <- index.results[[which(names(index.results) == geo.name)]]
  
  hu <- NULL
  type <- NULL
  yield <- NULL
  price <- NULL
  rent <- NULL
  
  
  if(length(index.results$house.yields) == 20){
    hu <- c(hu, 'H') 
    type <- c(type, rep('house', 20))
    yield <- c(yield, index.results$house.yields)
    price <- c(price, index.results$house.price)
    rent <- c(rent, index.results$house.rent)
    
  }
  
  if(length(index.results$unit.yields) == 20){
    hu <- c(hu, 'U')  
    type <- c(type, rep('unit', 20))
    yield <- c(yield, index.results$unit.yields)
    price <- c(price, index.results$unit.price)
    rent <- c(rent, index.results$unit.rent)
  }
  
  if(length(hu) == 0) {
    return('NA')
  } else {
    
    
    
    tidy.index <- data.frame(method=rep('Index', 20 * length(hu)),
                             geo=rep(geo.name, 20 * length(hu)),
                             time=rep(1:20, length(hu)),
                             type=type,
                             yield=yield,
                             price=price,
                             rent=rent)
    return(tidy.index)
  }  
}
