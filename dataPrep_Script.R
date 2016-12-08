##########################################################################################
#                                                                                        #
#  Script for analyzing PRR from APM data                                                #
#                                                                                        #
##########################################################################################

### Preliminary commands -----------------------------------------------------------------

 ## Set parameters and paths

  # Parameters
  reBuildData <- FALSE
  reAnalyze <- FALSE
  verbose <- TRUE

  # Paths and file names
  dataPath <- "C:/data/research/priceRentMethComp/"
  codePath <- 'c:/code/research/ausPropMrkt/'
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

  source(paste0(codePath, 'apmSetup.R'))
  source(paste0(codePath, 'helperFunctions.R'))


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
  
  
  sa1s.shp <- readShapePoly('c:/dropbox/research/prRatio/analyses/apmAnalysis/sa1s.shp',
                            proj4string=CRS("+init=epsg:4283"),
                            delete_null_obj=TRUE)


 ## Calculate only with matched sample
  
  match.sample <- trans.data[trans.data$UID %in% match.data$saleID |
                               trans.data$UID %in% match.data$rentID, ]
  
  ms <- apmFullDataAnalysis(trans.data=match.sample,
                      data.path=dataPath,
                      writeout=FALSE,
                      return.values=TRUE)
  
  ms$match.data <- match.sample
  ms$results$match <- match.results
  
 ## Add full sample back into a list
  
  full <- list(yield.data=yield.data,
               impute.data=trans.data,
               match.data=match.data,
               index.data=index.data,
               results=list(index=index.results,
                            hedimp=hedimp.results,
                            match=match.results))
  
    
### Prep Data Visualization --------------------------------------------------------------  
  
 ## Set up graphics parameters 
  
  apmPlotOptions()
  
 ## Isolate and Rename the necessary data

  # Full sample
  full$yield.data$method <- as.character(full$yield.data$method)
  full$yield.data$method[full$yield.data$method=='hedimp'] <- 'Impute'
  full$yield.data$method[full$yield.data$method=='srm'] <- 'Match'
  full$yield.data$method <- factor(full$yield.data$method, 
                                   levels=c('Index', 'Impute', 'Match'))
  full.geo.data <- split(full$yield.data, full$yield.data$geo.level)
  
  # Matched sample
  ms$yield.data$method <- as.character(ms$yield.data$method)
  ms$yield.data$method[ms$yield.data$method=='hedimp'] <- 'Impute'
  ms$yield.data$method[ms$yield.data$method=='srm'] <- 'Match'
  ms$yield.data$method <- factor(ms$yield.data$method, 
                                   levels=c('Index', 'Impute', 'Match'))
  ms.geo.data <- split(ms$yield.data, ms$yield.data$geo.level)
  
### Create appreciation rates ------------------------------------------------------------ 
 
 ## Full Sample  
  
 ## Extract the index values
  
  full.glob.index <- full$index.data$Global$Global
  
 ## Build appr rate datas
  
  # House
  full.har <- c(0, (full.glob.index$house.sale[2:20] - 
                      full.glob.index$house.sale[1:19]))/100
  full.har <- data.frame(time=1:20, app.rate=full.har)
  
  # Unit
  full.uar <- c(0, (full.glob.index$unit.sale[2:20] - 
                      full.glob.index$unit.sale[1:19]))/100
  full.uar <- data.frame(time=1:20, app.rate=full.uar)
  
 ## Add appreciation rates to all geo.levels  
  
  full.dif.data <- calcDifGeoWrap(full.geo.data, full.har, full.uar, 
                                  apmOptions$geo.levels)

  
 ## Matched Sample  
  
  ## Extract the index values
  
  ms.glob.index <- ms$index.data$Global$Global
  
  ## Build appr rate datas
  
  # House
  ms.har <- c(0, (ms.glob.index$house.sale[2:20] - 
                      ms.glob.index$house.sale[1:19]))/100
  ms.har <- data.frame(time=1:20, app.rate=ms.har)
  
  # Unit
  ms.uar <- c(0, (ms.glob.index$unit.sale[2:20] - 
                      ms.glob.index$unit.sale[1:19]))/100
  ms.uar <- data.frame(time=1:20, app.rate=ms.uar)
  
  ## Add appreciation rates to all geo.levels  
  
  ms.dif.data <- calcDifGeoWrap(ms.geo.data, ms.har, ms.uar, 
                                  apmOptions$geo.levels)
  
### Save Workspace -----------------------------------------------------------------------
  
  save(full, full.dif.data, full.geo.data,
       ms, ms.dif.data, ms.geo.data, studyShapes,
       file=paste0(dataPath, 'rprWrkspce.RData'))
  
