################################################################################
#                                                                              #
#  Predictive Accuracy Analysis, PRR Model comparison Study                    #
#                                                                              #
################################################################################

### Preliminary Commands -------------------------------------------------------

 ## Load Libraries

  library(plyr)
  library(dplyr)
  library(ggplot2)
  library(reshape2)
  library(stringr)
  library(maptools)
  library(sp)
  library(rgeos)
  library(grid)

 ## Source Files

  # File containing function for working with prr and APM data
  source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
              'master/prrFunctions.R'))

 ## Set custom functions
  
  # Function for summarizing lm objects but ignoring fixed effects

  summFE <- function(lmObj, skipFields=NULL){
  
    coef <- summary(lmObj)[4]$coefficients
  
    if(!is.null(skipFields)){
      kill <- grep(skipFields, rownames(coef))
      coef <- coef[-kill, ]
    }
  
    return(list(coef=coef, 
                sigma=summary(lmObj)[6]$sigma, 
                df=summary(lmObj)[7]$df, 
                r2=summary(lmObj)[8]$r.squared,
                AIC=AIC(lmObj)))
  
  }

### Load Data ------------------------------------------------------------------  

 ## Set the path to the data

  data.path <- "C:/data/research/priceRentMethComp/"

 ## Load in saved workspace

  load(paste0(data.path, 'cleanData.RData'))
  
 ## Load the calculated yield data
  
  yield.data <- read.csv(paste0(data.path, 'rawresults.csv'))
  
 ## Load in the additional spatial variables
    
  spat.data <- read.csv(paste0(data.path, 'spatialData/prrSpatialVariables.csv'), header=T)

 ## Load in suburb designations
  
  subdes.data <- read.csv('c:/data/aus/vic/geographic/melbourne/suburbDesignations.csv',
                           header=T)
  
### Prepare data -------------------------------------------------------------------------
  
 ## Convert spatial distances to meters
  
  # Set conversion factor
  convFactor <- 92000
  
  # Make conversions
  for(cI in 2:ncol(spat.data)){
    spat.data[ ,cI] <- spat.data[ ,cI] * convFactor
  }

 ## Join spatial variables to yield.data
  
  yield.data <- merge(yield.data, spat.data, by='AddressID')
  
 ## Join suburb designation
  
  yield.data <- merge(yield.data, subdes.data, by='suburb')
  
 ## Create Distance Categories
  
  # Set breaks
  dist.breaks <- c(0, 400, 800, 1600, 3200, Inf)
  
  # Create new variables with the breaks
  yield.data$train <- as.numeric(cut(yield.data$trainDist, breaks=dist.breaks))
  yield.data$bus <- as.numeric(cut(yield.data$busDist, breaks=dist.breaks))
  yield.data$tram <- as.numeric(cut(yield.data$tramDist, breaks=dist.breaks))
  yield.data$seven <- as.numeric(cut(yield.data$seven11Dist, breaks=dist.breaks))
  
 ## Data cleaning
  
  # Remove very high/low yields
  yield.data <- subset(yield.data, yield >= .01 & yield <= .09)
  
  # Remove unnecessary fields
  kill <- grep('QT', names(yield.data))
  yield.data <- yield.data[ ,-kill]
  kill <- grep('YT', names(yield.data))
  yield.data <- yield.data[ ,-kill]
  yield.data$GeographicalID <- NULL
  yield.data$EventID <- NULL
  yield.data$FlatNumber <- NULL

 ## Limit data to most recent year
  
  y.data <- subset(yield.data, transQtr >= 17)
  
 ## Split yield data into units and houses
  
  uy.data <- subset(y.data, PropertyType == 'Unit')
  hy.data <- subset(y.data, PropertyType == 'House')
  
 ## Split dataset by suburbs
  
  uiy.data <- subset(uy.data, location == 'Inner')
  umy.data <- subset(uy.data, location == 'Middle')
  uoy.data <- subset(uy.data, location == 'Outer')
  hiy.data <- subset(hy.data, location == 'Inner')
  hmy.data <- subset(hy.data, location == 'Middle')
  hoy.data <- subset(hy.data, location == 'Outer')
  
### Build basic models looking into trains and trams -------------------------------------  
  
 ## Set the base specifications
  
  unit.spec <- as.formula(yield ~ as.factor(transQtr) + as.factor(suburb) + 
                           as.factor(Bedrooms) + HasGarage + HasPool + 
                            HasAirConditioning)
  
  house.spec <- as.formula(yield ~ as.factor(transQtr) + as.factor(suburb) + 
                            as.factor(Bedrooms) + HasGarage + HasPool + 
                             HasAirConditioning + AreaSize)
  
 ## Build basic model (naive to microspatial impacts)

  # Units
  unit.lm <- lm(unit.spec, data=uy.data)
  summFE(unit.lm, 'suburb')
  
  # Houses
  house.lm <- lm(house.spec, data=hy.data)
  summFE(house.lm, 'suburb')
  
 ## Add in spatial factors
  
  # Linear  
  unit.lm.lin <- lm(update(unit.spec, . ~ . + trainDist + tramDist),
                      data=uy.data)
  summFE(unit.lm.lin, 'suburb')
  
  house.lm.lin <- lm(update(house.spec, . ~ . + trainDist + tramDist), 
                       data=hy.data)
  summFE(house.lm.lin, 'suburb')
  
  # Log
  unit.lm.log <- lm(update(unit.spec, . ~ . + log(trainDist) + log(tramDist)),
                           data=uy.data)
  summFE(unit.lm.log, 'suburb')
  
  house.lm.log <- lm(update(house.spec, . ~ . + log(trainDist) + log(tramDist)), 
                            data=hy.data)
  summFE(house.lm.log, 'suburb')
  
  # Ring
  unit.lm.ring <- lm(update(unit.spec, . ~ . + as.factor(train) + as.factor(tram)),
                          data=uy.data)
  summFE(unit.lm.ring, 'suburb')
  
  house.lm.ring <- lm(update(house.spec, . ~ . + as.factor(train) + as.factor(tram)), 
                           data=hy.data)
  summFE(house.lm.ring, 'suburb')
  
  # Spline
  unit.lm.spl <- lm(update(unit.spec, . ~ . + 
                             trainDist +
                             I((trainDist >= dist.breaks[2]) * (trainDist - dist.breaks[2])) +  
                             I((trainDist >= dist.breaks[3]) * (trainDist - dist.breaks[3])) +
                             I((trainDist >= dist.breaks[4]) * (trainDist - dist.breaks[4])) +
                             I((trainDist >= dist.breaks[5]) * (trainDist - dist.breaks[5])) +
                             tramDist +
                             I((tramDist >= dist.breaks[2]) * (tramDist - dist.breaks[2])) +  
                             I((tramDist >= dist.breaks[3]) * (tramDist - dist.breaks[3])) +
                             I((tramDist >= dist.breaks[4]) * (tramDist - dist.breaks[4])) +
                             I((tramDist >= dist.breaks[5]) * (tramDist - dist.breaks[5]))
                           ),
                     data=uy.data)
  summFE(unit.lm.spl, 'suburb')

  house.lm.spl <- lm(update(house.spec, . ~ . + 
                             trainDist +
                             I((trainDist >= dist.breaks[2]) * (trainDist - dist.breaks[2])) +  
                             I((trainDist >= dist.breaks[3]) * (trainDist - dist.breaks[3])) +
                             I((trainDist >= dist.breaks[4]) * (trainDist - dist.breaks[4])) +
                             I((trainDist >= dist.breaks[5]) * (trainDist - dist.breaks[5])) +
                             tramDist +
                             I((tramDist >= dist.breaks[2]) * (tramDist - dist.breaks[2])) +  
                             I((tramDist >= dist.breaks[3]) * (tramDist - dist.breaks[3])) +
                             I((tramDist >= dist.breaks[4]) * (tramDist - dist.breaks[4])) +
                             I((tramDist >= dist.breaks[5]) * (tramDist - dist.breaks[5]))
                             ),
                     data=hy.data)
  summFE(house.lm.spl, 'suburb')
  
### Check for spatial heterogeneity by broad location (inner vs middle vs outer) --------------
  
  # Units
  unitI.lm.spl <- lm(as.formula(unit.lm.spl), data=uiy.data)
  summFE(unitI.lm.spl, 'suburb')
  
  unitM.lm.spl <- lm(as.formula(unit.lm.spl), data=umy.data)
  summFE(unitM.lm.spl, 'suburb')
  
  unitO.lm.spl <- lm(as.formula(unit.lm.spl), data=uoy.data)
  summFE(unitO.lm.spl, 'suburb')
  
  # Houses
  houseI.lm.spl <- lm(as.formula(house.lm.spl), data=hiy.data)
  summFE(houseI.lm.spl, 'suburb')
  
  houseM.lm.spl <- lm(as.formula(house.lm.spl), data=hmy.data)
  summFE(houseM.lm.spl, 'suburb')
  
  houseO.lm.spl <- lm(as.formula(house.lm.spl), data=hoy.data)
  summFE(houseO.lm.spl, 'suburb')

### Working Vizualization -----------------------------------------------------------------------  
  
  makeDistPreds <- function(modList,           ## List of models
                            modNames,          # Model names
                            dist.breaks,          # Ring Cuts
                            specName='base',   # Specification name
                            xData,             # Data
                            xNbr=1){           # Obs to simulate
    
    ## Make prediction dataset
    
    exHome <- list()
    for(i in 1:40){
      exHome[[i]] <- xData[xNbr, ]
    }
    exHome <-rbind.fill(exHome)
    exHome$trainDist <- seq(100, 4000, by=100)
    exHome$train <- 1
    exHome$train[exHome$trainDist > dist.breaks[1]] <- 2
    exHome$train[exHome$trainDist > dist.breaks[2]] <- 3
    exHome$train[exHome$trainDist > dist.breaks[3]] <- 4
    exHome$train[exHome$trainDist > dist.breaks[4]] <- 5
    
    ## Make Predictions
    preds <- list()
    for(i in 1:length(modList)){
      preds[[i]] <- predict(modList[[i]], exHome)
    }
    names(preds) <- modNames
    
    ## make Tidy dataset
    tidyData <- data.frame(modelType = rep(specName, 40*4),
                           distSpec = c(rep('Linear', 40), rep('Log', 40),
                                        rep('Ring', 40), rep('Spline', 40)),
                           distance = rep(exHome$trainDist, 4),
                           value = c(preds[[1]], preds[[2]],
                                     preds[[3]], preds[[4]])
    )
    
    return(list(preds=preds,
                tidyData=tidyData))
    
  }  
  
  basePreds <- makeDistPreds(list(unit.lm.lin, unit.lm.log, unit.lm.ring, unit.lm.spl),
                             modNames=c('Linear', 'Log', 'Ring', 'Spline'),
                             dist.breaks=dist.breaks,
                             specName='Base',
                             xData=uy.data,
                             xNbr=4)
  basePreds <- makeDistPreds(list(house.lm.lin, house.lm.log, house.lm.ring, house.lm.spl),
                             modNames=c('Linear', 'Log', 'Ring', 'Spline'),
                             dist.breaks=dist.breaks,
                             specName='Base',
                             xData=uy.data,
                             xNbr=4)
  
  
  ## Make Custom plot themes
  
  theme_tod <- theme_grey() +
    theme(strip.text.x = element_text(size = 10),
          legend.position='bottom',
          legend.title=element_blank(),
          legend.background=element_rect(fill='white'),
          legend.key=element_rect(fill='white', color='white'))
  
  
  basePlot <- ggplot(basePreds$tidyData, aes(x=as.numeric(distance), y=value, 
                                             group=distSpec,
                                             colour=distSpec)) + 
    geom_line(size=2) +
    scale_colour_manual(values=rep(1,4)) +
    xlab("\nMetres from Station") +  ylab("Example Home Value\n") +
#     scale_y_continuous(limits=c(600000, 920000),
#                        breaks=seq(600000, 900000, 100000), 
#                        labels=paste0("$", seq(600, 900, 100), 'k')) +
#     scale_x_continuous(breaks=seq(0, 2.000, .500), 
#                        labels=paste0(seq(0, 2000, 500),'m')) +
    facet_wrap(~distSpec, ncol=2) +
    theme_tod+ theme(legend.position="none")
  
  
  
  
  
  
  
   
