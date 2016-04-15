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
  
### Prep Data Visualization --------------------------------------------------------------  
  
 ## Set up graphics parameters 
  
  apmPlotOptions()
  
 ## Isolate the necessary data

  yield.data$method <- as.character(yield.data$method)
  yield.data$method[yield.data$method=='hedimp'] <- 'Impute'
  yield.data$method[yield.data$method=='srm'] <- 'Match'
  yield.data$method <- factor(yield.data$method, levels=c('Index', 'Impute', 'Match'))
  geo.data <- split(yield.data, yield.data$geo.level)
  
### Plot yield trends at global level ----------------------------------------------------
  
 ## House yields
  
  house.glob <- ggplot(geo.data$Global[geo.data$Global$type == 'house',], 
                       aes(x=time, y=yield, group=method))+
    geom_line(aes(colour=method, size=method, linetype=method,
                  lineend='round', linejoin='round')) +
    facet_wrap(~type) +
    scale_size_manual(values=methSizes) +
    scale_colour_manual(values=houseCols) +
    scale_linetype_manual(values=methLines) + 
    xlab("") +
    ylab("Rent-Price Ratio\n") +
    scale_x_continuous(breaks=seq(0, 20, 4), 
                       labels=2011:2016) +
    scale_y_continuous(limits=c(.028, .049),
                       breaks=seq(.031, .049, .002), 
                       labels=seq(.031, .049, .002)) + 
    theme_prr + 
    theme(legend.key.width=unit(.55, "cm"))
  
 ## unit Yields  
  
  unit.glob <- ggplot(geo.data$Global[geo.data$Global$type == 'unit', ], 
                      aes(x=time, y=yield, group=method))+
    geom_line(aes(colour=method, size=method, linetype=method,
                  lineend='round', linejoin='round')) +
    scale_size_manual(values=methSizes) +
    scale_colour_manual(values=unitCols) +
    scale_linetype_manual(values=methLines) +
    facet_wrap(~type) +
    xlab("") +
    ylab("Rent-Price Ratio\n") +
    scale_x_continuous(breaks=seq(0, 20, 4), 
                       labels=2011:2016) +
    scale_y_continuous(limits=c(.028, .049),
                       breaks=seq(.031, .049, .002), 
                       labels=seq(.031, .049, .002)) + 
    theme_prr + 
    theme(legend.key.width=unit(.55, "cm"))
  
  ## Make Plot
  
  ggMultiPlots(house.glob, unit.glob, cols=2) 
  
### Show variatio of trends by suburb by method ------------------------------------------  
  
 ## Houses  
  
  sub.house <- ggplot(geo.data$suburb[geo.data$suburb$type=='house',], 
                       aes(x=time, y=yield, group=geo))+
    facet_wrap(~method, ncol=4) +
    geom_line(color='blue', alpha=.1) +
    geom_line(data=geo.data$Global[geo.data$Global$type=="house", ], 
              aes(x=time, y=yield, group=method),
              size=1.2, color='navy') +
    xlab("") +
    ylab("RPR by Suburb: Houses\n") +
    scale_x_continuous(breaks=seq(4, 20, 8), 
                       labels=c(2012, 2014, 2016)) +
    scale_y_continuous(limits=c(.015, .065),
                       breaks=seq(.02, .06, .01)) +
    theme_prr
  
 ## Units
  
  sub.unit <- ggplot(geo.data$suburb[geo.data$suburb$type=='unit',], 
                      aes(x=time, y=yield, group=geo))+
    facet_wrap(~method, ncol=4) +
    geom_line(color='forestgreen', alpha=.1) +
    geom_line(data=geo.data$Global[geo.data$Global$type=="unit", ], 
              aes(x=time, y=yield, group=method),
              size=1.2, color='forestgreen') +
    xlab("") +
    ylab("RPR by Suburb: Units\n") +
    scale_x_continuous(breaks=seq(4, 20, 8), 
                       labels=c(2012, 2014, 2016)) +
    scale_y_continuous(limits=c(.015, .065),
                       breaks=seq(.02, .06, .01)) +
    theme_prr

 ## Make Plot
    
  ggMultiPlots(sub.house, sub.unit) 
  
### Comparisons of Differences in RPRs methods by geo.level ------------------------------
  
 ## Prepare data  
  
  # Extract the index values
  glob.index <- index.data$Global$Global
  
  # Build house appr rate changes
  har <- c(0, ((glob.index$raw$house.sale[2:20] - glob.index$raw$house.sale[1:19])/
                 glob.index$raw$house.sale[1:19]))
  har <- lowess(har)$y
  har <- data.frame(time=1:20, app.rate=har)
  
  # Build unit appr rate changes
  uar <- c(0, ((glob.index$raw$unit.sale[2:20] - glob.index$raw$unit.sale[1:19])/
                 glob.index$raw$unit.sale[1:19]))
  uar <- lowess(uar)$y
  uar <- data.frame(time=1:20, app.rate=uar)
  
  # Make difference data
  dif.data <- calcDifGeoWrap(geo.data, har, uar, apmOptions$geo.levels)
  
 ## Make plots of differences between methods versus appreciation rate --------------------
  
 # Make house plot
   house.bias.plot.time <- ggplot(dif.data$houses, 
                                 aes(x=appr.rate, y=meth.dif, colour=geo.level)) + 
    geom_point(colour='black', size=.1, alpha=.1) + 
    geom_smooth(method=loess, size=2) +
    xlab("Time\n") +
    ylab("Rental Yield Bias from Matched Results\n") +
    scale_y_continuous(limits=c(-.008, 0.0005),
                       breaks=seq(-.01, 0, .0025)) +
    facet_wrap(~comp.method) +
    theme_prr
  house.bias.plot.time
  
 # Make unit plot
  unit.bias.plot.time <- ggplot(dif.data$units, 
                                 aes(x=appr.rate, y=meth.dif, colour=geo.level)) + 
    geom_point(colour='black', size=.1, alpha=.1) + 
    geom_smooth(method=loess, size=2) +
    xlab("Time\n") +
    ylab("Rental Yield Bias from Matched Results\n") +
    scale_y_continuous(limits=c(-.008, 0.0005),
                       breaks=seq(-.01, 0, .0025)) +
    facet_wrap(~comp.method) +
    theme_prr
  unit.bias.plot.time

### Density plot of differences in all vs matched ----------------------------------------
  
 ## LETS analyze this over time to show how it changes as the market heats up   
  
  abc <- function(trans.data, match.data){
  sh.data <- trans.data[trans.data$transType == 'sale' & 
                          trans.data$PropertyType == 'House', ]
  su.data <- trans.data[trans.data$transType == 'sale' & 
                          trans.data$PropertyType == 'Unit', ]
  rh.data <- trans.data[trans.data$transType == 'rent' & 
                          trans.data$PropertyType == 'House', ]
  ru.data <- trans.data[trans.data$transType == 'rent' & 
                          trans.data$PropertyType == 'Unit', ]
  
  mrh.data <- match.data[match.data$PropertyType == 'House',]
  mru.data <- match.data[match.data$PropertyType == 'Unit',]
  
  return(data.frame(sold=median(sh.data$transValue),
                    rent=median(mrh.data$adjSale)))
  }
  
  gg <- list()
  for(i in 1:20){
    gg[[i]] <- abc(trans.data[trans.data$transQtr==i,],
                   match.data[match.data$rentTime==i,])
  }
  aa <- rbind.fill(gg)
  plot(aa$sold, type='l', ylim=c(420000,650000))
  lines(aa$rent, col=2)
  
  h.index=index.data$Global$Global$house.sale
  u.index=index.data$Global$Global$unit.sale
  
  h.index <- data.frame(time=1:length(h.index),value=h.index)
  
  u.index <- data.frame(time=1:length(u.index), value=u.index)
  
  sh.data$adj <- (h.index$value[match(sh.data$transQtr, h.index$time)]/100)
  su.data$adj <- (u.index$value[match(su.data$transQtr, u.index$time)]/100)
  mrh.data$adj <- (h.index$value[match(mrh.data$saleTime, h.index$time)]/100)
  mru.data$adj <- (u.index$value[match(mru.data$saleTime, u.index$time)]/100)
  
  sh.data$a.value <- sh.data$transValue / sh.data$adj
  su.data$a.value <- su.data$transValue / su.data$adj
  mrh.data$a.value <- mrh.data$saleValue / mrh.data$adj
  mru.data$a.value <- mru.data$saleValue / mru.data$adj
  
  h.price <- try(t.test(sh.data$a.value, mrh.data$a.value), silent=TRUE)
  u.price <- try(t.test(su.data$a.value, mru.data$a.value), silent=TRUE)
  h.beds <- try(t.test(sh.data$Bedrooms, rh.data$Bedrooms), silent=TRUE)
  h.baths <- try(t.test(sh.data$Baths, rh.data$Baths), silent=TRUE)
  h.area <- try(t.test(sh.data$AreaSize, rh.data$AreaSize), silent=TRUE)
  
  u.beds <- try(t.test(su.data$Bedrooms, ru.data$Bedrooms), silent=TRUE)
  u.baths <- try(t.test(su.data$Baths, ru.data$Baths), silent=TRUE)
  
### Save full workspace  
 
  save.image(paste0(dataPath, 'rprWrkspce.RData'))
   
       
       