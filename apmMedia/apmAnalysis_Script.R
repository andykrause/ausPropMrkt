##########################################################################################
#                                                                                        #
#  Script for analyzing build PRR data for APM data                                      #
#                                                                                        #
##########################################################################################

### Preliminary commands -----------------------------------------------------------------

 ## Set parameters and paths

  # Parameters
  reBuildData <- FALSE
  reAnalyze <- FALSE
  offline <- TRUE
  verbose <- TRUE

  # Paths and file names
  dataPath <- "C:/data/research/priceRentMethComp/"
  exportPath <- dataPath
  salePath <- 'transData/newSales.csv'
  rentPath <- 'transData/newRentals.csv'
  geoPath=list(suburb='shapefiles/Vic_Suburbs.shp',
               lga='shapefiles/Vic_LGAs.shp',
               sla1='shapefiles/Vic_SLA1.shp',
               postcode='shapefiles/Vic_PostCodes.shp',
               ssFile='spatialData/allSS.csv')

 ## Source files

  # Source basic setup
  if(offline){
    source('c:/Code/research/ausPropMrkt/apmSetup.R')
  } else {
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/apmSetup.R'))
  } 

  # Source remaining functions
  sourceAPMFunctions(offline=offline, verbose=verbose)

  ## Set the global options

  apmSetOptions()

### Load Data ----------------------------------------------------------------------------  

 ## Re build data from scratch?

  if(reBuildData | !file.exists(paste0(dataPath, 'cleanTrans.RData'))){
  
    apmFullDataBuild(dataPath=dataPath, saleFile=salePath, rentFile=rentPath,
                     geoFiles=geoPath, offline=offline, verbose=verbose,
                     optionChanges=NULL)
  
  } 

### Analyze Raw Data ---------------------------------------------------------------------  

 ## Do data analysis  

  if(reAnalyze){
  
    # Load cleaned data 
    if(verbose) cat('Loading cleaned data\n')
    load(paste0(dataPath, 'cleanTrans.RData'))
  
    # Calculate full results
    apmFullDataAnalysis(trans.data=cleanTrans,
                        data.path=dataPath,
                        writeout=TRUE,
                        return.values=FALSE)
  
  } 

 ## Load analysis results      

  load(paste0(dataPath, 'yieldResults.RData'))
  load(paste0(dataPath, 'studyShps.RData'))

  ## Load SA1s file

  sa1s.shp <- readShapePoly('c:/data/aus/vic/geographic/vic_sa1s.shp',
                            proj4string=CRS("+init=epsg:4283"),
                            delete_null_obj=TRUE)

### Calculate yields at SA1 Level --------------------------------------------------------
 
 ## Extract 2015 Yields

  yields2015 <- trans.data[trans.data$transYear == 2015, ]

  # Divide into house and unit
  yields2015.house <- yields2015[yields2015$PropertyType == 'House', ]
  yields2015.unit <- yields2015[yields2015$PropertyType == 'Unit', ]

  
 ## Add SA1 designation  

  # Convert to Shapefile
  y.house <- SpatialPointsDataFrame(coords=cbind(yields2015.house$Property_Longitude,
                                                 yields2015.house$Property_Latitude),
                                    data=yields2015.house,
                                    proj4string=CRS("+init=epsg:4283"))

  y.unit <- SpatialPointsDataFrame(coords=cbind(yields2015.unit$Property_Longitude,
                                                yields2015.unit$Property_Latitude),
                                   data=yields2015.unit,
                                   proj4string=CRS("+init=epsg:4283"))

  # Add to houses
  spJoin <- over(y.house, sa1s.shp)
  yields2015.house$sa11 <- spJoin$SA1_MAIN11

  # Add to units
  spJoin <- over(y.unit, sa1s.shp)
  yields2015.unit$sa11 <- spJoin$SA1_MAIN11

 ## Calculate means and medians at sa1 level

  # Make Calculations
  h.mean <- tapply(yields2015.house$suburb.imp.actyield, yields2015.house$sa11, mean)
  u.mean <- tapply(yields2015.unit$suburb.imp.actyield, yields2015.unit$sa11, mean)
  h.median <- tapply(yields2015.house$suburb.imp.actyield, yields2015.house$sa11, median)
  u.median <- tapply(yields2015.unit$suburb.imp.actyield, yields2015.unit$sa11, median)

  # Convert into a data.frame
  house.sa1 <- data.frame(sa11=names(h.mean), mean=h.mean, median=h.median)
  unit.sa1 <- data.frame(sa11=names(u.mean), mean=u.mean, median=u.median)

 ## Add data to shapefile

  sa1s.shp@data$yield.hmed <- as.numeric(house.sa1$median[match(sa1s.shp@data$SA1_MAIN11,
                                                       house.sa1$sa11)])
  sa1s.shp@data$yield.hmean <- as.numeric(house.sa1$mean[match(sa1s.shp@data$SA1_MAIN11,
                                                       house.sa1$sa11)])
  sa1s.shp@data$yield.umed <- as.numeric(unit.sa1$median[match(sa1s.shp@data$SA1_MAIN11,
                                                      unit.sa1$sa11)])
  sa1s.shp@data$yield.umean <- as.numeric(unit.sa1$mean[match(sa1s.shp@data$SA1_MAIN11,
                                                       unit.sa1$sa11)])
 
 ## Trim Study SA1s to only those with house or units

  study.sa1s <- sa1s.shp[!is.na(sa1s.shp@data$yield.hmed) | 
                           !is.na(sa1s.shp@data$yield.umed), ]
  proj4string(study.sa1s) <- CRS("+init=epsg:4283")

 ## Write out shapefile

  writePolyShape(study.sa1s, fn='c:/temp/studysa1s.shp', factor2char = FALSE)


