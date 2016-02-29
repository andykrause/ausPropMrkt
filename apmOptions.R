
### Function to set the intial, default option levels --------------------------

apmSetOptions <- function(show=FALSE    # Print options to the screen
                          )
  {
  
 ## Set field from which to require obsevations
  
  reqFields <- c('transValue', 'AreaSize', 'Bedrooms', 'Baths', 'ssInteg', 'ssChoice')
  
 ## Set the initial limits
  
  areaLimits <- list(min=40, max=25000)
  bathLimits <- list(min=1, max=8)
  bedLimits <- list(unitMin=0, houseMin=1, unitMax=6, houseMax=8)  
  rentLimits <- list(min=125, max=2500)
  saleLimits <- list(min=150000, max=4000000)
  
 ## Set raw data column list
  
  rawColumnList <- c('UID', 'GeographicalID', 'EventID', 'AddressID', 
                     'FlatNumber', 'transDate', 'transValue', 'transType',
                     'PropertyType', 'Property_Latitude', 'Property_Longitude',
                     'AreaSize', 'Bedrooms', 'Baths', 'Parking','HasFireplace',
                     'HasPool', 'HasGarage', 'HasAirConditioning')
  
  naFields <- list('HasPool', 'HasGarage', 'HasAirConditioning', 'HasFireplace')
  ssFields <- c('L_choice_2500', 
                'T64_Integration_Segment_Length_Wgt_R25000_metric')
  unitTypes <- c('Unit', 'Studio')
  houseTypes <- c('House', 'Terrace', 'Townhouse', 'Villa', 'Duplex')
  
 ## Set time information
  
  startYear <- 2010
  startMonth <- 6
  geoTempLimit <- 3
  geoTempField <- "QT_house_postCode"
  
 ## Set equations for impute model  
  
  houseEquation <- log(transValue) ~ as.factor(postCode) + as.factor(transQtr) + 
    log(AreaSize) + Bedrooms + Baths + HasPool + HasGarage + Studio
  
  unitEquation <- log(transValue) ~ as.factor(postCode) + as.factor(transQtr) + 
    Bedrooms + Baths + HasPool + HasGarage + Terrace + Townhouse + Villa + Duplex
  
 ## Add to a temporary list of options  
  
  tempOptions <- list(areaLimits = areaLimits,
                      bathLimits = bathLimits,
                      bedLimits = bedLimits,
                      rentLimits = rentLimits,
                      saleLimits = saleLimits,
                      rawColumnList = rawColumnList,
                      reqFields = reqFields,
                      naFields = naFields,
                      ssFields = ssFields,
                      startYear = startYear,
                      startMonth = startMonth,
                      unitTypes = unitTypes,
                      houseTypes = houseTypes,
                      geoTempLimit = geoTempLimit,
                      geoTempField = geoTempField,
                      houseEquation = houseEquation,
                      unitEquation = unitEquation)
 
 ## Assign the options to the global environment  
   
  assign('apmOptions', tempOptions, envir=.GlobalEnv)
  
 ## If show, then print to screen  
  
  if(show) return(tempOptions)
}
  
### Function for changing individual options -----------------------------------

apmChangeOptions <- function(...,         # List of options to change
                             show=FALSE   # Print all options to screen?
                             )
  {
 
 ## Example function calls
  
  if(F){
    apmChangeOptions(list(bathLimits=list(min=1)))
    apmChangeOptions(list(bathLimits=list(min=1),
                          bathLimits=list(max=9),
                          rentLimits=list(max=2300)))
    }
  
 ## Extract options to change
  
  chgOpts <- list(...)[[1]]
  
 ## Make changes
  
  for(ij in 1:length(chgOpts)){
    
    # ID the name of opt to change
    optNames <- names(chgOpts)[ij]
    
    # ID the location of the option
    idOpts <- which(names(apmOptions) == optNames)
    
    # ID the location of the sub option (the particular value)
    idsubOpts <- which(names(apmOptions[[idOpts]]) == names(chgOpts[[ij]]))
    
    # Make the change to the global object
    apmOptions[[idOpts]][[idsubOpts]] <- chgOpts[[ij]][[1]]
    
    # Print the option change to the screen
    cat('Option Updated:\n', names(apmOptions)[[idOpts]], 
        names(apmOptions[[idOpts]])[[idsubOpts]], 'now set to', 
        apmOptions[[idOpts]][[idsubOpts]], '\n')
  }

 ## Assign new values to global environment  
    
 assign('apmOptions', apmOptions, envir=.GlobalEnv)
  
 ## If show, then print all to screen  
 if(show) return(apmOptions) 
}
  
  

