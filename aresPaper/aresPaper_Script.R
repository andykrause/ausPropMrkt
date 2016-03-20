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
  offline <- FALSE
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
    yield.results <- full.results$tidy.data
    clean.trans <- full.results$impute.data
    match.data <- full.results$match.data
    index.values <- full.results$index.values
    results <- full.results$results
    rm(full.results); gc()
    
  } else  {
    
    load(paste0(dataPath, 'yieldResults.RData'))

  }

### Compare matched data to all data -----------------------------------------------------
  
  #if(verbose) cat('Swapping Impute and Matched Yields\n')
  #comp.samp <- apmCompareSamples(trans.data)
  
  ## TODO: So there are differences.  How to adjust??  

### Calculate Predictive Errors ----------------------------------------------------------
  
 ## Calculate all errors
  
  all.errors <- apmPredLevelWrap(srm.data=match.data, yield.data=yield.results,
                                 verbose=TRUE)
  
  errors.tidy <- rbind.fill(lapply(X=as.list(apmOptions$geo.level), 
                                   FUN=apmSummErrors, 
                                   all.errors=all.errors))
  # Method
  errors.tidy$method <- as.character(errors.tidy$method)
  errors.tidy$method[errors.tidy$method == 'spag'] <- 'Sp Aggr'
  errors.tidy$method[errors.tidy$method == 'hedimp'] <- 'Impute'
  errors.tidy$method[errors.tidy$method == 'srm'] <- 'Match'
  errors.tidy$method[errors.tidy$method == 'index'] <- 'Index'
  errors.tidy$method <- factor(errors.tidy$method, levels = c('Sp Aggr', 'Index',
                                                              'Impute', 'Match'))
  
  # Geo.level
  errors.tidy$geo.level <- as.character(errors.tidy$geo.level)
  errors.tidy$geo.level[errors.tidy$geo.level == 'Global'] <- 'Metro'
  errors.tidy$geo.level[errors.tidy$geo.level == 'lga'] <- 'LGA'
  errors.tidy$geo.level[errors.tidy$geo.level == 'sla1'] <- 'SLA1'
  errors.tidy$geo.level[errors.tidy$geo.level == 'postCode'] <- 'Postcode'
  errors.tidy$geo.level[errors.tidy$geo.level == 'suburb'] <- 'Suburb'
  errors.tidy$geo.level <- factor(errors.tidy$geo.level, 
                                  levels = c('Metro', 'LGA', 'SLA1',
                                             'Postcode', 'Suburb'))

### Data Visualization -------------------------------------------------------------------  
  
 ## Set up graphics parameters 

  # Set colors for plots
  methCols <- c('navy', 'royalblue2', 'skyblue', 'gray50')
  methSizes <- c(.5, 1, 1.5, 2)
  methLines <- c(1, 1, 1, 1)  
  
  # Set graphical theme
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
  
 ## All types errors by method and geography  

  # Prep data
  all.e <- errors.tidy[errors.tidy$variable == 'all.abs', ]
  
 ## Plot for all types
  
  error.plot <- ggplot(all.e, 
                       aes(x=geo.level, y=value, group=method, shape=method,
                           color=method)) +
                       geom_point(size=6)+
                       scale_shape_manual(values = 15:18) +
                       scale_colour_manual(values = methCols) +
                       xlab("Geographic Level of Analysis\n") + 
                       ylab("Median Absolute Prediction Error\n") +
                       scale_y_continuous(limits=c(.08, .21),
                                          breaks=seq(.09, .21, .03), 
                                          labels=paste0(format(100 * (seq(.09, .21, .03)),
                                                        nsmall=1), "%")) + 
                       theme_prr +
                       theme(legend.position='right')
  
 ## Plot by use
  
  # Prep Data
  type.e <- errors.tidy[errors.tidy$variable == 'house.abs' | 
                          errors.tidy$variable == 'unit.abs', ]
  type.e$variable <- as.character(type.e$variable)
  type.e$variable[type.e$variable == 'house.abs'] <- "Houses"
  type.e$variable[type.e$variable == 'unit.abs'] <- "Unit"
  
  use.error.plot <- ggplot(type.e, 
                           aes(x=geo.level, y=value, group=method, shape=method,
                               colour=method)) +
                           facet_wrap(~variable) + 
                           geom_point(size=6)+
                           scale_shape_manual(values = 15:18) +
                           scale_colour_manual(values = methCols) +
                           xlab("Estimation Method") + 
                           ylab("Median Absolute Prediction Error\n") +
                           scale_y_continuous(limits=c(.08, .24),
                                              breaks=seq(.09, .24, .03), 
                                              labels=paste0(format(100 * (seq(.09,
                                                                         .24, .03)),
                                                            nsmall=1), "%")) + 
                           theme_prr +
                           theme(legend.position='right')
  
  ## Make plots by yield results
  
   yield.tidy <- yield.results

  # Rename the methods  
   yield.tidy$method <- as.character(yield.tidy$method)
   yield.tidy$method[yield.tidy$method == 'spag'] <- 'Sp Aggr'
   yield.tidy$method[yield.tidy$method == 'hedimp'] <- 'Impute'
   yield.tidy$method[yield.tidy$method == 'srm'] <- 'Match'
   yield.tidy$method[yield.tidy$method == 'index'] <- 'Index'
   yield.tidy$method <- factor(yield.tidy$method, levels = c('Sp Aggr', 'Index',
                                                               'Impute', 'Match'))
   # Rename the property Types  
   yield.tidy$type[yield.tidy$type == 'house'] <- 'Houses'
   yield.tidy$type[yield.tidy$type == 'unit'] <- 'Units'

  ## Make a global comparison of methods

   # Isolate the global data
   glob <- yield.tidy[yield.tidy$geo.level == 'Global', ]

   # Global Plot  
   glob.plot <- ggplot(glob, 
                       aes(x=time, y=yield, group=method))+
                       facet_wrap(~type) +
                       geom_line(aes(colour=method, size=method, linetype=method,
                                     lineend='round', linejoin='round')) +
                       scale_size_manual(values=methSizes) +
                       scale_colour_manual(values=methCols) +
                       scale_linetype_manual(values=methLines) + 
                       xlab("") +
                       ylab("Rent-Price Ratio\n") +
                       scale_x_continuous(breaks=seq(0, 20, 4), 
                                          labels=2011:2016) +
                       scale_y_continuous(limits=c(.030, .049),
                                          breaks=seq(.031, .049, .002), 
                                          labels=paste0(format(100 * (seq(.031,
                                                                          .049, .002)),
                                                        nsmall=1), "%")) + 
                        theme_prr

 ## 
   sub.data <- yield.tidy[yield.tidy$geo.level == 'suburb', ]
   
   # Global Plot  
   sub.plot <- ggplot(sub.data, 
                      aes(x=time, y=yield, group=geo))+
                      facet_wrap(type~method, ncol=4) +
                      geom_line(color='gray50', alpha=.3) +
                      geom_line(data=glob, 
                                aes(x=time, y=yield, group=method),
                                size=1.5) +
                      xlab("") +
                      ylab("Rental Yield by Suburb\n") +
                      scale_x_continuous(breaks=seq(0, 20, 4), 
                                         labels=2011:2016) +
                      scale_y_continuous(limits=c(.01, .1),
                                         breaks=seq(.02, .08, .02), 
                                         labels=paste0(format(100 * (seq(.02,
                                                                         .08, .02)),
                                                                        nsmall=1), "%")) +
                      theme_prr
   sub.plot
   
 ## Compare bias in method vs appreciation
   
   # Extract the index values
   glob.values <- index.values$Global$Global
   
   # Build a house and unit index
   har <- c(0, ((glob.values$raw$house.sale[2:20] - glob.values$raw$house.sale[1:19])/
                  glob.values$raw$house.sale[1:19]))
   uar <- c(0, ((glob.values$raw$unit.sale[2:20] - glob.values$raw$unit.sale[1:19])/
                  glob.values$raw$unit.sale[1:19]))
   
   # Build data for houses
   globH <- glob[glob$type == 'Houses',]
   globH$App.Rate <- rep(har, 4)
   globH$Bias <- globH$yield[61:80] - globH$yield  
   globHH <- globH[1:60,]
   globHH <- globHH[globHH$time != 1,]
   
   # Build data for units
   globU <- glob[glob$type == 'Units',]
   globU$App.Rate <- rep(uar, 4)
   globU$Bias <- globU$yield[61:80] - globU$yield  
   globUU <- globU[1:60,]
   globUU <- globUU[globUU$time != 1,]
   
   # Make house plot
   house.bias.plot <- ggplot(globHH, 
                             aes(x=App.Rate, y=Bias)) + 
                             geom_point(colour='black', size=2) + 
                             geom_smooth(method=loess, se=TRUE, size=2) +
                             xlab("Home Price Movement in Qtr\n") +
                             ylab("Rental Yield Bias from Matched Results\n") +
                             scale_x_continuous(limits=c(-.06, .085),
                                                breaks=seq(-.04, .08, .02), 
                                                labels=paste0(format(100 *
                                                              (seq(-.04, .08, .02)),
                                                               nsmall=1), "%")) +
                             scale_y_continuous(limits=c(0, .01),
                                                breaks=seq(0, .01, .002), 
                                                labels=paste0(format(100 * 
                                                              (seq(0, .01, .002)),
                                                                 nsmall=1), "%")) +
                             facet_wrap(~method) +
                             theme_prr
     house.bias.plot
   
     # Unit plot
     unit.bias.plot <- ggplot(globUU, 
                              aes(x=App.Rate, y=Bias)) + 
                              geom_point(colour='black', size=2) + 
                              geom_smooth(method=loess, se=TRUE, size=2) +
                              xlab("Home Price Movement in Qtr") +
                              ylab("Rental Yield Bias from Matched Results\n") +
                              scale_x_continuous(limits=c(-.035, .04),
                                                 breaks=seq(-.03, .04, .01), 
                                                 labels=paste0(format(100 *
                                                               (seq(-.03, .04, .01)),
                                                               nsmall=1), "%")) +
                              scale_y_continuous(limits=c(0, .011),
                                                 breaks=seq(0, .01, .002), 
                                                 labels=paste0(format(100 * 
                                                               (seq(0, .01, .002)),
                                                                    nsmall=1), "%")) +
                              facet_wrap(~method) +
                              theme_prr
      unit.bias.plot
     
  ## Look at it by time
      
      # Make house plot
      house.bias.plot.time <- ggplot(globHH, 
                                     aes(x=time, y=Bias)) + 
                                     geom_point(colour='black', size=2) + 
                                     geom_smooth(method=loess, size=2) +
                                     xlab("Time\n") +
                                     ylab("Rental Yield Bias from Matched Results\n") +
                                     scale_x_continuous(limits=c(0, 20),
                                                        breaks=seq(0, 20, 4),
                                                        labels=2011:2016) +
                                     facet_wrap(~method) +
                                     theme_prr
      house.bias.plot.time

      # Make house plot
       unit.bias.plot.time <- ggplot(globUU, 
                                     aes(x=time, y=Bias)) + 
                                     geom_point(colour='black', size=2) + 
                                     geom_smooth(method=loess, size=2) +
                                     xlab("Time\n") +
                                     ylab("Rental Yield Bias from Matched Results\n") +
                                     scale_x_continuous(limits=c(0, 20),
                                                        breaks=seq(0, 20, 4),
                                                        labels=2011:2016) +
                                     facet_wrap(~method) +
                                     theme_prr
       unit.bias.plot.time

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
                    xlab("\nTime\n") +
                    ylab("Rent-Price Ratio Difference from Match\n") +
                    scale_x_continuous(limits=c(0, 20),
                                       breaks=seq(0, 20, 4),
                                       labels=2011:2016) +
                    scale_y_continuous(limits=c(-.01, 0.005),
                                       breaks=seq(-.01, .005, .0025),
                                       labels=paste0(format(100 * 
                                                              seq(-.01, .005, .0025),
                                                            nsmall=1), "%")) +
    
                    facet_wrap(~method) +
                    geom_hline(yintercept = 0) + 
                    theme_prr
  house.all.diff
  
  unit.all.diff <- ggplot(unit.bias, 
                           aes(x=time, y=Bias, group=geo.level, color=geo.level)) + 
    geom_point(colour='black', size=.3, alpha=.1) + 
    geom_smooth(method=loess, size=1, se=TRUE) +
    xlab("\nTime\n") +
    ylab("Rent-Price Ratio Difference from Match\n") +
    scale_x_continuous(limits=c(0, 20),
                       breaks=seq(0, 20, 4),
                       labels=2011:2016) +
    scale_y_continuous(limits=c(-.01, 0.005),
                       breaks=seq(-.01, .005, .0025),
                       labels=paste0(format(100 * 
                                              seq(-.01, .005, .0025),
                                            nsmall=1), "%")) +
    
    facet_wrap(~method) +
    
    geom_hline(yintercept = 0) + 
    theme_prr
  unit.all.diff
  
 ### Save plotting information            

   rm(cleanTrans)
   save.image(paste0(dataPath, 'aresWrkspce.RData'))
   
       
       