##########################################################################################
#                                                                                        #
#  Script for analyzing PRR from APM data                                                #
#                                                                                        #
##########################################################################################

 ## Set parameters and paths

  reBuildData <- FALSE
  reAnalyze <- FALSE
  offline <- FALSE
  verbose <- TRUE
  
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

  if(offline){
    source('c:/Code/research/ausPropMrkt/apmSetup.R')
  } else {
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/apmSetup.R'))
  }

 ## Source remaining functions

  sourceAPMFunctions(offline=offline, verbose=verbose)

 ## Set options

  apmSetOptions()

### Load Data ----------------------------------------------------------------------------  

 ## Re build data from scratch?
  
  if(reBuildData | !file.exists(paste0(dataPath, 'cleanTrans.RData'))){

    apmFullDataBuild(dataPath=dataPath, saleFile=salePath, rentFile=rentPath,
                     geoFiles=geoPath, offline=offline, verbose=verbose,
                     optionChanges=NULL)
  
  } 
  
 ## Load prepared data
  
  if(verbose) cat('Loading cleaned data\n')
  load(paste0(dataPath, 'cleanTrans.RData'))
  load(paste0(dataPath, 'studyShps.RData'))
  
 ## Do data analysis  
  
  if(reAnalyze){
     results <- apmFullDataAnalysis(cleanTrans, dataPath)
  } else  {
    load(paste0(dataPath, 'yieldResults.RData'))
  }

### Create the necessary data objects for each method  -----------------------------------
 
 ## Compare matched data to all data
  
  if(verbose) cat('Swapping Impute and Matched Yields\n')
  comp.samp <- apmCompareSamples(trans.data)
  
  ## So there are differences.  How to adjust??  
  
### Simple Plots for Media output --------------------------------------------------------  

 ## Set up graphics parameters 
  
  # Set theme
  theme_prr <- theme_grey() +
    theme(text = element_text(size=11),
          panel.background = element_rect(colour='gray95', fill='gray95'),
          panel.grid.major=element_line(colour='white', size=.5),
          panel.grid.minor=element_line(colour='white', size=.1),
          plot.background=element_rect(fill='white'),
          axis.title.y=element_text(colour='black'),
          axis.text.y=element_text(hjust=1),
          legend.position='bottom',
          legend.background=element_rect(fill='white'),
          legend.key=element_rect(fill='white', color='white'),
          legend.text=element_text(color='black'),
          legend.title=element_blank(),
          legend.key.width=unit(2, "cm"),
          strip.background = element_rect(fill = "orange", 
                                          color = "orange", size = .1),
          strip.text.x = element_text(face = "bold"),
          strip.text.y = element_text(face = "bold"))
  
  # Set colors for plots
  methCols <- c('navy', 'royalblue2', 'skyblue')
  methSizes <- c(.5, 1.25, 2)
  methLines <- c(1, 1, 1)  
  
 ## Fix the data for better plotting  
  
  all.tidy <- results$tidy.data
  
  # Rename the methods  
  all.tidy$method[all.tidy$method=='spag'] <- 'Sp Aggr'
  all.tidy$method[all.tidy$method=='hedimp'] <- 'Impute'
  all.tidy$method[all.tidy$method=='srm'] <- 'Match'
  
  # Rename the property Types  
  all.tidy$type[all.tidy$type=='house'] <- 'Houses'
  all.tidy$type[all.tidy$type=='unit'] <- 'Units'
  
  # Remove the index method from plots
  plot.tidy <- all.tidy[all.tidy$method != 'Index', ]
  
  # Reset the factor levels
  plot.tidy$method <- factor(plot.tidy$method, levels = c('Sp Aggr', 'Impute', 'Match'))

 ## Make a global comparison of methods
  
  # Isolate the global data
    glob <- plot.tidy[plot.tidy$geo.level=='Global', ]
  
  # Global Plot  
  ggplot(glob, aes(x=time, y=yield, group=method))+
    facet_wrap(~type) +
    geom_line(aes(colour=method, size=method, linetype=method,
                  lineend='round', linejoin='round')) +
    scale_size_manual(values=methSizes) +
    scale_colour_manual(values=methCols) +
    scale_linetype_manual(values=methLines) + 
    xlab("") + ylab("Rental Yield\n") +
    scale_x_continuous(breaks=seq(0, 20, 4), labels=2011:2016) +
    scale_y_continuous(limits=c(.031, .049),
                       breaks=seq(.031, .049, .002), 
                       labels=paste0(format(100 * (seq(.031,
                                                       .049, .002)),
                                            nsmall=1), "%")) + 
    theme_prr
  
  
  ## Make comparison at suburb level
  
  sub.dataH <- plot.tidy[plot.tidy$geo.level=='suburb' & plot.tidy$type=='Houses'
                         & plot.tidy$method=='Impute',]
  
  carl.dataH <- sub.dataH[sub.dataH$geo=='Carlton',]
  balw.dataH <- sub.dataH[sub.dataH$geo=='Balwyn',]
  pakh.dataH <- sub.dataH[sub.dataH$geo=='Pakenham',]
  
  sub.dataU <- plot.tidy[plot.tidy$geo.level=='suburb' & plot.tidy$type=='Units'
                         & plot.tidy$method=='Impute',]
  
  carl.dataU <- sub.dataU[sub.dataU$geo=='Carlton',]
  balw.dataU <- sub.dataU[sub.dataU$geo=='Balwyn',]
  pakh.dataU <- sub.dataU[sub.dataH$geo=='Pakenham',]
  
  
  
  ggplot(sub.dataH, aes(x=time, y=yield, group=geo))+
    #facet_wrap(~type) +
    geom_line(color='gray40')+
    geom_line(data=carl.data, 
              aes(x=time, y=yield, group=geo),
              colour='blue') +
    geom_line(data=balw.data, 
              aes(x=time, y=yield, group=geo),
              colour='red')+
  geom_line(data=pakh.dataH, 
            aes(x=time, y=yield, group=geo),
            colour='green')+
    
    #geom_line(aes(colour=method, size=method, linetype=method,
    #              lineend='round', linejoin='round')) +
    scale_size_manual(values=methSizes) +
    scale_colour_manual(values=methCols) +
    scale_linetype_manual(values=methLines) + 
    xlab("") + ylab("Rental Yield\n") +
    scale_x_continuous(breaks=seq(0, 20, 4), labels=2011:2016) +
    scale_y_continuous(limits=c(.019, .060),
                       breaks=seq(.020, .060, .005), 
                       labels=paste0(format(100 * (seq(.020,
                                                       .060, .005)),
                                            nsmall=1), "%")) + 
    theme_prr
  
  ggplot(sub.dataU, aes(x=time, y=yield, group=geo))+
    #facet_wrap(~type) +
    geom_line(color='gray40')+
    geom_line(data=carl.dataU, 
              aes(x=time, y=yield, group=geo),
              colour='blue') +
    geom_line(data=balw.dataU, 
              aes(x=time, y=yield, group=geo),
              colour='red') +
    
    #geom_line(aes(colour=method, size=method, linetype=method,
    #              lineend='round', linejoin='round')) +
    scale_size_manual(values=methSizes) +
    scale_colour_manual(values=methCols) +
    scale_linetype_manual(values=methLines) + 
    xlab("") + ylab("Rental Yield\n") +
    scale_x_continuous(breaks=seq(0, 20, 4), labels=2011:2016) +
    scale_y_continuous(limits=c(.019, .060),
                       breaks=seq(.020, .060, .005), 
                       labels=paste0(format(100 * (seq(.020,
                                                       .060, .005)),
                                            nsmall=1), "%")) + 
    theme_prr
  
  
  
  
  
  
  
  
  
  
  glob.values <- index.values$Global$Global
  
  har <- c(0, ((glob.values$raw$house.sale[2:20] - glob.values$raw$house.sale[1:19])/
    glob.values$raw$house.sale[1:19]))
  uar <- c(0, ((glob.values$raw$unit.sale[2:20] - glob.values$raw$unit.sale[1:19])/
                 glob.values$raw$unit.sale[1:19]))
  
  globH <- glob[glob$type=='Houses',]
  globH$App.Rate <- rep(har, 3)
  globH$Bias <- globH$yield[41:60] - globH$yield  
  globHH <- globH[1:40,]
  globHH <- globHH[globHH$time != 1,]
  
  
  globU <- glob[glob$type=='Units',]
  globU$App.Rate <- rep(uar, 3)
  
  metroDiffPlot_A <- ggplot(globHH, aes(x=time, y=Bias)) + 
    geom_point(colour='black', size=2) + 
    geom_smooth(method=lm) +
    xlab("Home Price Movement in Qtr") +
    ylab("Difference in Rental Yield Estimate\n") +
#     scale_x_continuous(limits=c(-.03, .06),
#                        breaks=seq(-.02, .06, .02), 
#                        labels=paste0(format(100 *
#                                               (seq(-.02, .06, .02)),
#                                             nsmall=1), "%")) +
#     scale_y_continuous(limits=c(0, .011),
#                        breaks=seq(0, .01, .002), 
#                        labels=paste0(format(100 * 
#                                               (seq(0, .01, .002)),
#                                             nsmall=1), "%")) +
    facet_wrap(~method) +
    theme_prr
  metroDiffPlot_A
  
  
  
  
  
  
  
  ggplot(cleanTrans[cleanTrans$transQtr==20,], 
         aes(x=Property_Longitude, y=Property_Latitude, z=imp.yield)) +
    stat_contour(bins=3)
  
  
  ### Metro Level ----------------------------------------------------------------
  

  
  
#   
#   
# ### Build into 5 basic data object by geography --------------------------------
#   
#   data.by.geo <- apmConvertToGeo(medianResults, imputeResults, matchResults,
#                                  saleIndex)
# 
# ### Write out analytical results -----------------------------------------------  
#   
#   save(data.by.geo, matchData, medianResults, imputeResults, matchResults,
#        houseResults, unitResults, saleIndex, rentIndex,
#        file=paste0(exportPath, 'analysisResults.RData'))
#   
#   save(cleanTrans, file=paste0(dataPath, 'rawResults.RData'))  
# 
#   ## Clean up workspace
#   
#   rm(allTrans); rm(rentClean); rm(saleClean); gc()
#   
# ### Analyze Predictive Ability of Models ---------------------------------------
#   
#   ## Yield indexes  
#   
#   metroYields <- prrGetYields(data.by.geo$metroData)
#   lgaYields <- prrGetYields(data.by.geo$lgaData)
#   slaYields <- prrGetYields(data.by.geo$slaData)
#   postcodeYields <- prrGetYields(data.by.geo$postcodeData)
#   suburbYields <- prrGetYields(data.by.geo$suburbData)
#   
#   # Direct match data  
#   matchData$uID <- 1:nrow(matchData)
#   
#   ### Estimate prediction errors -------------------------------------------------  
#   
#   ## Estimate raw prediction errors
#   
#   # Metro Level
#   metroRes <- prrPredModelWrap(matchData, metroYields)
#   metroResU <- prrPredModelWrap(matchData, metroYields, byUse=TRUE)
#   
#   # LGA Level
#   lgaRes <- prrPredModelWrap(matchData, lgaYields, byGeog=TRUE, geoField='lga')
#   lgaResU <- prrPredModelWrap(matchData, lgaYields, byGeog=TRUE, 
#                               geoField='lga', byUse=TRUE)
#   
#   # Suburb Level
#   suburbRes <- prrPredModelWrap(matchData, suburbYields, byGeog=TRUE, 
#                                 geoField='suburb')
#   suburbResU <- prrPredModelWrap(matchData, suburbYields, byGeog=TRUE, 
#                                  geoField='suburb', byUse=TRUE)
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   ## Attached data to the matched dataset
#   
#   # Metro Level
#   matchData$mMed <- metroRes$median$error[match(matchData$uID, metroRes$median$uID)]
#   matchData$mMedU <- metroResU$median$error[match(matchData$uID, metroResU$median$uID)]
#   matchData$mImp <- metroRes$impute$error[match(matchData$uID, metroRes$impute$uID)]
#   matchData$mImpU <- metroResU$impute$error[match(matchData$uID, metroResU$impute$uID)]
#   matchData$mMat <- metroRes$match$error[match(matchData$uID, metroRes$match$uID)]
#   matchData$mMatU <- metroResU$match$error[match(matchData$uID, metroResU$match$uID)]
#   
#   # LGA Level
#   matchData$lMed <- lgaRes$median$error[match(matchData$uID, lgaRes$median$uID)]
#   matchData$lMedU <- lgaResU$median$error[match(matchData$uID, lgaResU$median$uID)]
#   matchData$lImp <- lgaRes$impute$error[match(matchData$uID, lgaRes$impute$uID)]
#   matchData$lImpU <- lgaResU$impute$error[match(matchData$uID, lgaResU$impute$uID)]
#   matchData$lMat <- lgaRes$match$error[match(matchData$uID, lgaRes$match$uID)]
#   matchData$lMatU <- lgaResU$match$error[match(matchData$uID, lgaResU$match$uID)]
#   
#   # Suburb Level
#   matchData$sMed <- suburbRes$median$error[match(matchData$uID, suburbRes$median$uID)]
#   matchData$sMedU <- suburbResU$median$error[match(matchData$uID, suburbResU$median$uID)]
#   matchData$sImp <- suburbRes$impute$error[match(matchData$uID, suburbRes$impute$uID)]
#   matchData$sImpU <- suburbResU$impute$error[match(matchData$uID, suburbResU$impute$uID)]
#   matchData$sMat <- suburbRes$match$error[match(matchData$uID, suburbRes$match$uID)]
#   matchData$sMatU <- suburbResU$match$error[match(matchData$uID, suburbResU$match$uID)]
#   
#   ## Convert to absolute values
#   
#   absPredResults <- lapply(matchData[ ,(which(colnames(matchData) == 'mMed'):
#                                        which(colnames(matchData) == 'sMatU'))], abs)
#   
#   ## Calculate the median absolute error  
#   
#   # Make calc
#   absPredMed <- lapply(absPredResults, median, na.rm=TRUE)
#   
#   # Convert into a table
#   absPredMed <- as.matrix(unlist(absPredMed))
#   predTable <- data.frame(metro=absPredMed[1:6],
#                           lga=absPredMed[7:12],
#                           suburb=absPredMed[13:18])
#   rownames(predTable) <- c('Median', 'Median by Use', 'Impute', 'Impute by Use',
#                            'Match', 'Match by Use')
#   
#   ## Calculate the hit rate
#   
#   # Build function to count 'non-hits'
#   countNA <- function(x){length(which(is.na(x)))/length(x)}
#   
#   # Make calc
#   hitRate <- lapply(absPredResults, countNA)
#   
#   # Convert into a table
#   hitRate <- 1 - as.matrix(unlist(hitRate))
#   hrTable <- data.frame(metro=hitRate[1:6],
#                         lga=hitRate[7:12],
#                         suburb=hitRate[13:18])
#   rownames(hrTable) <- c('Median', 'Median by Use', 'Impute', 'Impute by Use',
#                          'Match', 'Match by Use')
#   
#   ### Save workspace -------------------------------------------------------------
#   
#   save(matchData, predTable, hrTable, 
#        file=paste0(dataPath, 'predModelResults.RData'))
#   