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


 
 ## Fix the factor levels in the errors  
   ## Load prepared data

  if(verbose) cat('Loading cleaned data\n')
  load(paste0(dataPath, 'cleanTrans.RData'))
  load(paste0(dataPath, 'studyShps.RData'))

### Analyze Raw Data ---------------------------------------------------------------------  
  
 ## Do data analysis  

  if(reAnalyze){
    
    # Calculate full results
    full.results <- apmFullDataAnalysis(clean.trans=cleanTrans,
                                        data.path=dataPath,
                                        writeout=TRUE)
    
    # Strip values out and clean up
    yield.data <- full.results$tidy.data
    hedimp.data <- full.results$impute.data
    match.data <- full.results$match.data
    index.data <- full.results$index.values
    results <- full.results$results
    rm(full.results); gc()
    
  } else  {
    
    load(paste0(dataPath, 'yieldResults.RData'))

  }

### Compare matched data to all data -----------------------------------------------------
  
  #if(verbose) cat('Swapping Impute and Matched Yields\n')
  comp.samp <- apmCompareSamples(clean.trans)
  
  ## TODO: So there are differences.  How to adjust??  

### Prep Data Visualization --------------------------------------------------------------  
  
 ## Set up graphics parameters 
  
  apmPlotOption()
  
 ## Isolate the necessary data
  
  yield.data <- yield.data[yield.data$method != 'spag', ]
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
  
  
  
  
  
  
  
  
  
  
  
### Comparisons of Differences in RPRs methods by unit by geo.level ----------------------
  
  ## Isolate the data  
  
  calcDifWrap <- function(x.data){
  h.data <- x.data[x.data$type == 'house', ]
  u.data <- x.data[x.data$type == 'unit', ]
  
  h.dif <- calcDif(h.data)
  u.dif <- calcDif(u.data)
  
  return(list(houses=h.dif,
              units=u.dif))
  
}  
    
  calcDif <- function(x.data){
  x.data$UID <- paste0(x.data$geo, "..", x.data$time)
  x.split <- split(x.data, as.factor(x.data$method))
  x.ind_mat <- merge(x.split$Index, x.split$Match[,c('UID', 'yield')], by='UID')
  x.ind_imp <- merge(x.split$Index, x.split$Impute[,c('UID', 'yield')], by='UID')
  x.imp_mat <- merge(x.split$Impute, x.split$Match[,c('UID', 'yield')], by='UID')
  
  x.ind_mat$meth.dif <- x.ind_mat$yield.x - x.ind_mat$yield.y
  x.ind_mat$comp.method <- 'Index - Match'
  
  x.ind_imp$meth.dif <- x.ind_imp$yield.x - x.ind_imp$yield.y
  x.ind_imp$comp.method <- 'Index - Impute'
  
  x.imp_mat$meth.dif <- x.imp_mat$yield.x - x.imp_mat$yield.y
  x.imp_mat$comp.method <- 'Impute - Match'
  
  return(rbind(x.ind_mat, x.ind_imp, x.imp_mat))
  
}  

  glob.dif <- calcDifWrap(geo.data$Global)
  lga.dif <- calcDifWrap(geo.data$lga)
  sub.dif <- calcDifWrap(geo.data$suburb)

  # Make house plot
  house.bias.plot.time <- ggplot(glob.dif$houses, 
                                 aes(x=time, y=meth.dif)) + 
    geom_point(colour='black', size=2) + 
    geom_smooth(method=loess, size=2) +
    geom_smooth(data=lga.dif$houses, aes(x=time, y=meth.dif), method=loess, size=2, color='red') +
    geom_smooth(data=sub.dif$houses, aes(x=time, y=meth.dif), method=loess, size=2, color='green') +
    xlab("Time\n") +
    ylab("Rental Yield Bias from Matched Results\n") +
    scale_x_continuous(limits=c(0, 20),
                       breaks=seq(0, 20, 4),
                       labels=2011:2016) +
    facet_wrap(~comp.method) +
    theme_prr
  house.bias.plot.time
  
  # Make house plot
  unit.bias.plot.time <- ggplot(glob.dif$units, 
                                 aes(x=time, y=meth.dif)) + 
    geom_point(colour='black', size=2) + 
    geom_smooth(method=loess, size=2) +
    geom_smooth(data=lga.dif$units, aes(x=time, y=meth.dif), method=loess, size=2, color='red') +
    geom_smooth(data=sub.dif$units, aes(x=time, y=meth.dif), method=loess, size=2, color='green') +
  
    xlab("Time\n") +
    ylab("Rental Yield Bias from Matched Results\n") +
    scale_x_continuous(limits=c(0, 20),
                       breaks=seq(0, 20, 4),
                       labels=2011:2016) +
    facet_wrap(~comp.method) +
    theme_prr
  unit.bias.plot.time

  
  
  
  
  
  
  
  
    
  

   
 ## Compare bias in method vs appreciation
   
   # Extract the index values
   glob.values <- index.values$Global$Global
   
   # Build a house and unit index
   har <- c(0, ((glob.values$raw$house.sale[2:20] - glob.values$raw$house.sale[1:19])/
                  glob.values$raw$house.sale[1:19]))
   har <- lowess(har)$y
   uar <- c(0, ((glob.values$raw$unit.sale[2:20] - glob.values$raw$unit.sale[1:19])/
                  glob.values$raw$unit.sale[1:19]))
   uar <- lowess(uar)$y
   
   
 
   # Make house plot
   
   house.bias.plot <- ggplot(globHH, 
                             aes(x=time, y=Bias)) + 
     geom_point(colour='blue', size=2) + 
     geom_smooth(method=loess, se=TRUE, size=2) +
     xlab("") +
     ylab("RPR Difference from Match\n") +
     scale_x_continuous(limits=c(1,20),
                        breaks=c(4,12,20), 
                        labels=c(2012, 2014, 2016)) +
     scale_y_continuous(limits=c(-.01, 0.002),
                        breaks=seq(-.01, 0, .002)) +
     facet_wrap(~method) +
     geom_hline(yintercept = 0) + 
     theme_prr
   
   house.bias.plot
   
     # Unit plot
   
   unit.bias.plot <- ggplot(globUU, 
                            aes(x=time, y=Bias)) + 
     geom_point(colour='forestgreen', size=2) + 
     geom_smooth(method=loess, se=TRUE, size=2, color='forestgreen') +
     xlab("") +
     ylab("RPR Difference from Match\n") +
     scale_x_continuous(limits=c(1,20),
                        breaks=c(4,12,20), 
                        labels=c(2012, 2014, 2016)) +
     scale_y_continuous(limits=c(-.01, 0.002),
                        breaks=seq(-.01, 0, .002)) +
     facet_wrap(~method) +
     geom_hline(yintercept = 0) + 
     theme_prr
   
   unit.bias.plot  

 ## Compare the biases over geo level
  
  # create data
  
  level.bias <- lapply(as.list(apmOptions$geo.level), 
                       FUN=apmCalcBias, yield.data=yield.tidy)       
  level.bias <- rbind.fill(level.bias)
  level.bias$geo.level <- factor(level.bias$geo.level, levels = c('Global', 'lga',
                                                            'sla1', 'postCode', 'suburb'))
  
  house.bias <- level.bias[level.bias$type == 'Houses',]
  unit.bias <- level.bias[level.bias$type == 'Units',]
  
  # PLot house results
  
  house.all.diff <- ggplot(house.bias, 
                           aes(x=time, y=Bias, group=geo.level, color=geo.level)) + 
    geom_point(colour='black', size=.3, alpha=.1) + 
    geom_smooth(method=loess, size=1, se=TRUE) +
    xlab("\nHouses\n") +
    ylab("Rent-Price Ratio Difference from Match\n") +
    scale_colour_manual(values=c('black', 'darkred', 'red', 'orange', 'yellow')) +
    
    scale_x_continuous(limits=c(0, 20),
                       breaks=seq(4, 20, 4),
                       labels=2012:2016) +
    scale_y_continuous(limits=c(-.01, 0.005),
                       breaks=seq(-.01, .005, .0025)) +
    
    facet_wrap(~method) +
    geom_hline(yintercept = 0) + 
    theme_prr
  house.all.diff
  
  
  unit.all.diff <- ggplot(unit.bias, 
                          aes(x=time, y=Bias, group=geo.level, color=geo.level)) + 
    geom_point(colour='black', size=.3, alpha=.1) + 
    geom_smooth(method=loess, size=1, se=TRUE) +
    xlab("\nUnits\n") +
    ylab("Rent-Price Ratio Difference from Match\n") +
    scale_colour_manual(values=c('black', 'darkred', 'red', 'orange', 'yellow')) +
    
    scale_x_continuous(limits=c(0, 20),
                       breaks=seq(4, 20, 4),
                       labels=2012:2016) +
    scale_y_continuous(limits=c(-.01, 0.005),
                       breaks=seq(-.01, .005, .0025)) +
    
    facet_wrap(~method) +
    geom_hline(yintercept = 0) + 
    theme_prr
  unit.all.diff
  
 ### Save plotting information            

   save.image(paste0(dataPath, 'aresWrkspce.RData'))
   
       
       