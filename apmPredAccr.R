if(F){

  pred <- apmPredModelWrap(dmData=results$match.data,
                           yieldData=all.tidy)
  
  
}

### calculate the predictive error -------------------------------------------------------

apmErrorEngine <- function(mData,     # Dataset of matched sales and rentals 
                           yData      # Timeseries of yield estimates
){
  
  # Subset into sales and rentals based on which observation is first
  sData <- subset(mData, saleTime <= rentTime)
  rData <- subset(mData, saleTime > rentTime)
  sData$tType <- 'sale'
  rData$tType <- 'rent'
  
  # Add the yield information at the time of the first transaction
  sData$pYield <- yData$yield[match(sData$rentTime, yData$time)]  
  rData$pYield <- yData$yield[match(rData$saleTime, yData$time)]  
  
  # Predict rental value of sales and the error
  sData$pValue <- ((sData$adjSale * sData$pYield) / 52) 
  sData$error <- (sData$rentValue - sData$pValue) / sData$rentValue
  
  # Precict sale value of rentals and the error    
  rData$pValue <- (rData$adjRent * 52) / rData$pYield
  rData$error <- (rData$saleValue - rData$pValue) / rData$saleValue
  
  # Merge data together  
  xData <- rbind(sData[,c('saleID', 'tType', 'error')],
                 rData[,c('saleID', 'tType', 'error')])
  
  # Return values
  return(xData)
}

### Wrapper to spread the error calcs over all three methods -----------------------------

apmErrorByMethod <- function(geo,
                             mData,       # Matched dataset
                             yieldData,
                             geo.level   # List of yield data from prrGetYields
){
  
  mData <- mData[mData[,geo.level] == geo, ]
  yieldData <- yieldData[yieldData$geo == geo & yieldData$geo.level==geo.level, ]
  
  # Calc error for median method
  spagError <- apmErrorEngine(mData, yieldData[yieldData$method == 'spag',])
  spagError$method<-'spag'
  
  # Calc error for imputation method
  indexError <- apmErrorEngine(mData, yieldData[yieldData$method == 'Index',])
  indexError$method<-'index'
  
  # Calc error for imputation method
  hedimpError <- apmErrorEngine(mData, yieldData[yieldData$method == 'hedimp',])
  hedimpError$method<-'hedimp'
  
  # calc error for matching method
  srmError <- apmErrorEngine(mData, yieldData[yieldData$method == 'srm',])
  srmError$method='srm'
  
  # Return Values
  return(list(spag=spagError,
              index=indexError,
              hedimp=hedimpError,
              srm=srmError))
  
}

apmPredGeoWrap <- function(dmData,
                           yieldData,
                           geo.level='suburb'){
  
  #   # Subset house and unit data
  hData <- subset(dmData, PropertyType == 'House')
  uData <- subset(dmData, PropertyType == 'Unit')
  
  geo.list <- list(levels(as.factor(dmData[,geo.level])))
  
  qq<- lapply(geo.list, apmErrorByMethod, dmData = hData, 
              yieldData=yieldData[yieldData$type=='house',], geo.level='suburb')
  
  aa <- apmErrorByMethod(geo='Carlton', mData=hData, yieldData=yieldData[yieldData$type=='house',],
                         geo.level='suburb')
  
  #     
  #   ## Calculate for Houses
  #       
  #   # Get geography names and extract necessary data
  #   geoListH <- levels(as.factor(yieldData$geo.level))
  #      
  #   
  #   
  cdata <- hData[hData$suburb == 'Carlton',]
  ycdata <- yieldData[yieldData$geo == 'Carlton' & yieldData$type=='house',]
  
  apmErrorByMethod(cdata, ycdata)
  
}

# 
# ### Wrapper to calculate the predictive accuracty of various yield trends ----------------
# 
# if(F){
# apmPredModelWrap <- function(dmData,              # matched dataset
#                              yieldData,           # yield data from prrGetYields()
#                              byUse=FALSE,         # Calculate by use?
#                              byGeog=FALSE,        # Calculate by Geography
#                              geoField=NULL        # Which field is the geog name in?
# ){
#   
#   # Small helper function to count number of observations
#   geoCount <- function(x){nrow(rbind.fill(x))}
# 

   
#   
#   prrCalcPredError(cdata, ycdata)
#   
# 
#   
#   
#    geoDataH <- lapply(geoListH, prrExtractGeoData, xData=hData, 
#                          geoField=geoField)
#       
#       # Identify non-empty dataset (by geog)
#       hCount <- unlist(lapply(geoDataH, nrow))
#       idH <- which(hCount > 0)
#       
#       # Extract necessary geographic yield information
#       geoYieldsH <- lapply(geoListH, prrExtractGeoYields, yieldData=yieldData$house)
#       
#       # Identify non-empty dataset (by geog)
#       idYH <- lapply(geoYieldsH, geoCount)
#       idYH <- which(idYH > 0)
#       
#       # Select those that meet both criteria
#       idH <- intersect(idH, idYH)
#       
#       # Trim data to those that are not empty
#       geoDataH <- geoDataH[idH]
#       geoYieldsH <- geoYieldsH[idH]
#       
#       # Estimate the prediction error  
#       geoH <- mapply(prrErrorByMethod, mData=geoDataH, yieldData=geoYieldsH)
#       
#       # Combine and rename results
#       hResults <- rbind.fill(geoH)
#       hResults$use <- 'house'
#       hResults$geog <- geoField
#       
#       ## Calculate for units  
#       
#       # Geo geography names and extract necessary data
#       geoListU <- levels(yieldData$unit$median$spaceName)
#       geoDataU <- lapply(geoListU, prrExtractGeoData, xData=uData, 
#                          geoField=geoField)
#       
#       # Identify non-empty datasets (by geog)
#       uCount <- unlist(lapply(geoDataU, nrow))
#       idU <- which(uCount > 0)
#       
#       # Extract necessary geographic yield information
#       geoYieldsU <- lapply(geoListU, prrExtractGeoYields, yieldData=yieldData$unit)
#       
#       # Identify non-empty dataset (by geog)
#       idYU <- lapply(geoYieldsU, geoCount)
#       idYU <- which(idYU > 0)
#       
#       # Select those that meet both criteria
#       idU <- intersect(idU, idYU)
#       
#       # Trim data to those that are not empty
#       geoDataU <- geoDataU[idU]
#       geoYieldsU <- geoYieldsU[idU]
#       
#       # Estimate the prediction error  
#       geoU <- mapply(prrErrorByMethod, mData=geoDataU, yieldData=geoYieldsU)
#       
#       # Combine and rename results
#       uResults <- rbind.fill(geoU)
#       uResults$use <- 'unit'
#       uResults$geog <- geoField
#       
#       ## Merge house and unit results
#       
#       xResults <- rbind(hResults, uResults)
#       
#     } else {
#       
#       ## if by Use by not geog  
#       
#       # Calculate errors for houses
#       hResults <- prrErrorByMethod(hData, yieldData$house)
#       hResults <- rbind.fill(hResults)
#       hResults$use <- 'house'
#       
#       # Calculate errors for units
#       uResults <- prrErrorByMethod(uData, yieldData$unit)
#       uResults <- rbind.fill(uResults)
#       uResults$use <- 'unit'
#       
#       # Combine results
#       xResults <- rbind(hResults, uResults)
#       xResults$geog <- 'all'
#     }
#   } else {
#     
#     ## if not by Use by by Geography
#     
#     if(byGeog){
#       
#       # Extract relevant geography names
#       geoList <- names(table(as.character(yieldData$mix$median$spaceName)))
#       
#       # Extract geographic base data
#       geoData <- lapply(geoList, prrExtractGeoData, xData=dmData, 
#                         geoField=geoField)
#       
#       # Extract yield trends
#       geoYields <- lapply(geoList, prrExtractGeoYields, yieldData=yieldData$mix)
#       
#       # Calculate all errors
#       geo <- mapply(prrErrorByMethod, mData=geoData, yieldData=geoYields)
#       xResults <- rbind.fill(geo)
#       xResults$geog <- geoField
#       
#     } else {
#       
#       ## If not by use and not by Geography
#       
#       # Calculate error results
#       xResults <- prrErrorByMethod(dmData, yieldData$mix)
#       xResults <- rbind.fill(xResults)
#       xResults$geog <- 'all'
#     }
#     xResults$use <- 'mix'
#   }
#   
#   
#   # Return Values  
#   return(list(median=subset(xResults, method=='median'),
#               impute=subset(xResults, method=='impute'),
#               match=subset(xResults, method=='match')))  
# }
# }

### Helper function to extract geographic from a given dataset ---------------------------

prrExtractGeoData <- function(geoName,      # Specific geographic name
                              xData,        # dataset 
                              geoField      # Field containing geographic names
){
  
  # ID and extract field
  gData <- xData[ ,geoField]
  
  # Label matching rows
  idx <- which(gData == geoName)
  
  # Return Values
  return(xData[idx, ])
  
}

### Helper function to extract the three yield types for a given geography ---------------

prrExtractGeoYields <- function(geoName,      # A specific geographic name 
                                yieldData     # yield data from prrGetYields()$mix
){
  
  # Extract yields
  gMedian <- subset(yieldData$median, spaceName==geoName)
  gImpute <- subset(yieldData$impute, spaceName==geoName)
  gMatch <- subset(yieldData$match, spaceName==geoName)
  
  # Return Values
  return(list(median=gMedian,
              impute=gImpute,
              match=gMatch))
  
}

   
   
   
   
   
   
   
   
   
   
   
   
   



