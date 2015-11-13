################################################################################
#                                                                              #
#  Data Visualization Code for the Price to Rent Ratio Model comparison Study  #
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

  # File containing function for analyzing data
  source(paste0('https://raw.githubusercontent.com/andykrause/',
                'dataAnalysisTools/master/stShardFunctions.R'))

 ## Set the path to the data

  dataPath <- "C:/Dropbox/Australia Data/ausPropData/melData/"
  subGeoFile <- 'Vic_Suburbs.shp'
  lgaGeoFile <- 'Vic_LGAs.shp'
  sla1GeoFile <- 'Vic_SLA1.shp'
  postGeoFile <- 'Vic_PostCodes.shp'

 ## Set viz themes
  
  theme_black <- theme_grey() +
    theme(text = element_text(size=9),
          panel.background = element_rect(colour='black', fill='black'),
          panel.grid.major=element_line(colour='gray20'),
          panel.grid.minor=element_line(colour='gray20'),
          plot.background=element_rect(fill='gray10'),
          axis.title.y=element_text(colour='white'),
          axis.text.y=element_text(hjust=1),
          legend.position='bottom',
          legend.background=element_rect(fill='gray10'),
          legend.key=element_rect(fill='gray10', color='gray10'),
          legend.text=element_text(color='white'),
          legend.title=element_blank())
  
  theme_prr <- theme_grey() +
    theme(text = element_text(size=10),
          plot.background=element_rect(fill='white'),
          axis.title.y=element_text(colour='black'),
          axis.text.y=element_text(hjust=1),
          legend.position='bottom',
          legend.background=element_rect(fill='white'),
          legend.key=element_rect(fill='white', color='white'),
          legend.text=element_text(color='black'),
          legend.title=element_blank(),
          legend.key.width=unit(2, "cm"))
  
 ## Set up custom functions
  
  createAggData <- function(mmObj, crObj, dmObj){  
    
    mmObj <- mmObj[,c('timeName', 'spaceName', 'yield')]
    crObj <- crObj$stsDF
    dmObj <- dmObj$stsDF
    names(crObj)[2] <- names(dmObj)[2] <- 'yield'
    
    mmGeo <- levels(mmObj$spaceName)
    crGeo <- levels(as.factor(crObj$spaceName))
    dmGeo <- levels(as.factor(dmObj$spaceName))
    allGeo <- intersect(intersect(mmGeo, crGeo),dmGeo)
    
    mmObj <- subset(mmObj, mmObj$spaceName %in% allGeo)
    crObj <- subset(crObj, crObj$spaceName %in% allGeo)
    dmObj <- subset(dmObj, dmObj$spaceName %in% allGeo)
    
    oLng <- nrow(mmObj)
    
    comData <- rbind(mmObj, crObj, dmObj)
    comData$method <- c(rep('Median', oLng), rep('Impute', oLng),
                        rep('Match', oLng))
    
    difData <- rbind(mmObj, crObj, dmObj)
    difData$yield <- NULL
    difData$method <- c(rep('1. Impute - Median', oLng),
                        rep('2. Match - Median', oLng),
                        rep('3. Match - Impute', oLng))
    difData$dif <- c(crObj$yield - mmObj$yield,
                     dmObj$yield - dmObj$yield,
                     dmObj$yield - crObj$yield)
    
    medX <- which(comData$comp$method == 'Median')
    impX <- which(comData$comp$method == 'Impute')
    matX <- which(comData$comp$method == 'Match')
    
    comMed <- data.frame(timeName = rep(1:oLng, 3),
                         method = c(rep('Median', oLng),
                                    rep('Impute', oLng),
                                    rep('Match', oLng)),
                         spaceName = rep('Median', oLng * 3),
                         yield = c(as.numeric(tapply(comData$comp$yield[medX], 
                                                     comData$comp$timeName[medX], 
                                                     median)),
                                   as.numeric(tapply(comData$comp$yield[impX], 
                                                     comData$comp$timeName[impX], 
                                                     median)),
                                   as.numeric(tapply(comData$comp$yield[matX], 
                                                     comData$comp$timeName[matX], 
                                                     median))))
    
    medXX <- which(comMed$method == 'Median')
    impXX <- which(comMed$method == 'Impute')
    matXX <- which(comMed$method == 'Match')
    
    difMed <- comMed
    difMed$yield <- NULL
    difMed$method <- c(rep('1. Impute - Median', oLng),
                        rep('2. Match - Median', oLng),
                        rep('3. Match - Impute', oLng))
    difMed$dif <- c(comMed$yield[impXX] - comMed$yield[medXX],
                    comMed$yield[matXX] - comMed$yield[medXX],
                    comMed$yield[matXX] - comMed$yield[impXX])
    
    
    ## Return
    return(list(comp = comData,
                diff = difData,
                compMed = comMed,
                diffMed = difMed))
  }
  
  
  
### Load Data ------------------------------------------------------------------  

  ## Load in saved workspace
  
  load(paste0(dataPath, 'analysisResults.RData'))

  ## Load in Geographical Data

  subShp <- readShapePoly(paste0(dataPath, subGeoFile))
  lgaShp <- readShapePoly(paste0(dataPath, lgaGeoFile))
  sla1Shp <- readShapePoly(paste0(dataPath, sla1GeoFile))
  postCodeShp <- readShapePoly(paste0(dataPath, postGeoFile))

################################################################################
### Data visualization ---------------------------------------------------------

### Metro Level, difference in the three ---------------------------------------

  ## Build data
  
  metroCompData <- data.frame(time=c(mmMetYields$timeName,
                                     crMetYields$stsDF$timeName,
                                     dmMetYields$stsDF$timeName),
                              method=c(rep('Median', 20),
                                       rep('Impute', 20),
                                       rep('Match', 20)),
                              yield=c(mmMetYields$yield,
                                      crMetYields$stsDF$median,
                                      dmMetYields$stsDF$median))
  metroCompData$method <- factor(metroCompData$method,
                                  levels=c('Median', 'Impute', 'Match'))
  
 ## Make plot

  metroPlot <- ggplot(metroCompData, aes(x=as.numeric(time), y=yield, 
                                        group=method)) + 
               geom_line(aes(colour=method, size=method, linetype=method)) +
               scale_size_manual(values=c(1.5, 1.5, 1.5)) +
               scale_colour_manual(values=c('gray10', 'blue', 'green')) +
               scale_linetype_manual(values=c(3, 2, 1)) + 
               xlab("") + ylab("Rental Yield\n") +
               scale_x_continuous(breaks=seq(2, 18, 4), labels=2011:2015) +
               scale_y_continuous(limits=c(.032, .048),
                                  breaks=seq(.032, .048, .002), 
                                  labels=paste0(format(100 * (seq(.032,
                                                                  .048, .002)),
                                          nsmall=1), "%")) +
               theme_prr

### Analyze difference between three based on home price trends ----------------  
  
 ## Build home price trend set
  
  # Raw home price trend 
  mmMetsP <- spaceTimeShard(stsData = xTrans[xTrans$transType=='sale',],
                            metric=c('transValue'),
                            spaceField='all', timeField='transQtr',
                            defDim='time', stsLimit=3, 
                            calcs=list(median='median'))

  # Turn into index
  pIndex <- c(0, (mmMetsP$stsDF$median[-1] / mmMetsP$stsDF$median[-20]) - 1)

 ## Build data set of differences in rental yield estimates  
  
  metroDiffData <- data.frame(priceIndex=rep(pIndex, 3),
                              method=c(rep('1. Impute - Median', 20),
                                       rep('2. Match - Median', 20),
                                       rep('3. Match - Impute', 20)),
                              yieldDif=c((crMetYields$stsDF$median-
                                            mmMetYields$yield),
                                         (dmMetYields$stsDF$median-
                                            mmMetYields$yield),
                                         (dmMetYields$stsDF$median-
                                            crMetYields$stsDF$median)))

 ## Make Plot
  
  metroDiffPlot <- ggplot(metroDiffData, aes(x=priceIndex, y=yieldDif)) + 
                   geom_point(colour='black', size=2) +  geom_smooth(method=lm) +
                   xlab("Home Price Movement in Qtr") +
                   ylab("Difference in Rental Yield Estimate\n") +
                   scale_x_continuous(limits=c(-.065, .12),
                       breaks=seq(-.04, .12, .04), 
                       labels=paste0(format(100 * (seq(-.04,
                                                       .12, .04)),
                                            nsmall=1), "%")) +
                   scale_y_continuous(limits=c(0, .011),
                       breaks=seq(0, .01, .0025), 
                       labels=paste0(format(100 * (seq(0, .01, .0025)),
                                            nsmall=1), "%")) +
                   facet_wrap(~method) +
                   theme_prr

### Break into house and unit --------------------------------------------------
  
 ## Build Data
  
  metroUseCompData <- data.frame(time=c(mmMetYields$timeName,
                                        crMetYields$stsDF$timeName,
                                        dmMetYields$stsDF$timeName,
                                        mmMetYields$timeName,
                                        crMetYields$stsDF$timeName,
                                        dmMetYields$stsDF$timeName),
                                 method=c(rep('Median', 20),
                                          rep('Impute', 20),
                                          rep('Match', 20),
                                          rep('Median', 20),
                                          rep('Impute', 20),
                                          rep('Match', 20)),
                                 use = c(rep('House', 60), rep('Unit', 60)),
                                 yield=c(mmMetYieldsH$yield,
                                         crMetYieldsH$stsDF$median,
                                         dmMetYieldsH$stsDF$median,
                                         mmMetYieldsU$yield,
                                         crMetYieldsU$stsDF$median,
                                         dmMetYieldsU$stsDF$median)) 
  metroUseCompData$method <- factor(metroUseCompData$method,
                                    levels=c('Median', 'Impute', 'Match'))

 ## Make Plot
  
  metroUsePlot <- ggplot(metroUseCompData, aes(x=as.numeric(time), y=yield, 
                                         group=method)) + 
    geom_line(aes(colour=method, size=method, linetype=method)) +
    scale_size_manual(values=c(1.5, 1.5, 1.5)) +
    scale_colour_manual(values=c('blue', 'green', 'gray10')) +
    scale_linetype_manual(values=c(1, 2, 3)) + 
    xlab("") + ylab("Rental Yield\n") +
    scale_x_continuous(breaks=seq(2, 18, 4), labels=2011:2015) +
    scale_y_continuous(limits=c(.03, .05),
                       breaks=seq(.03, .05, .002), 
                       labels=paste0(format(100 * (seq(.03,
                                                       .05, .002)),
                                            nsmall=1), "%")) +
    facet_wrap(~use) +
    theme_prr
  
  
### Metro difference by use ----------------------------------------------------  
  
  
  ## Build home price trend set
  
  # Raw home price trend 
  mmMetsPH <- spaceTimeShard(stsData = xTrans[xTrans$transType=='sale' &
                                              xTrans$PropertyType == 'House',],
                            metric=c('transValue'),
                            spaceField='all', timeField='transQtr',
                            defDim='time', stsLimit=3, 
                            calcs=list(median='median'))
  mmMetsPU <- spaceTimeShard(stsData = xTrans[xTrans$transType=='sale' &
                                              xTrans$PropertyType=='Unit',],
                            metric=c('transValue'),
                            spaceField='all', timeField='transQtr',
                            defDim='time', stsLimit=3, 
                            calcs=list(median='median'))
  
  # Turn into index
  pIndexH <- c(0, (mmMetsPH$stsDF$median[-1] / mmMetsPH$stsDF$median[-20]) - 1)
  pIndexU <- c(0, (mmMetsPU$stsDF$median[-1] / mmMetsPU$stsDF$median[-20]) - 1)
  
  ## Build data set of differences in rental yield estimates  
  
  metroUseDiffData <- data.frame(priceIndex=c(rep(pIndexH, 3),
                                           rep(pIndexU, 3)),
                              use=c(rep('House', 60), rep('Unit', 60)),             
                              method=c(rep('1. Impute - Median', 20),
                                       rep('2. Match - Median', 20),
                                       rep('3. Match - Impute', 20),
                                       rep('1. Impute - Median', 20),
                                       rep('2. Match - Median', 20),
                                       rep('3. Match - Impute', 20)),
                              yieldDif=c((crMetYieldsH$stsDF$median-
                                            mmMetYieldsH$yield),
                                         (dmMetYieldsH$stsDF$median-
                                            mmMetYieldsH$yield),
                                         (dmMetYieldsH$stsDF$median-
                                            crMetYieldsH$stsDF$median),
                                         (crMetYieldsU$stsDF$median-
                                            mmMetYieldsU$yield),
                                         (dmMetYieldsU$stsDF$median-
                                            mmMetYieldsU$yield),
                                         (dmMetYieldsU$stsDF$median-
                                            crMetYieldsU$stsDF$median)))
  
  ## Make Plot
  
  metroUseDiffPlot <- ggplot(metroUseDiffData, aes(x=priceIndex, y=yieldDif)) + 
    geom_point(colour='black', size=2) +  
    geom_smooth(method=lm, size=1.5, aes(colour=use)) +
    xlab("Home Price Movement in Qtr") +
    ylab("Difference in Rental Yield Estimate\n") +
    scale_x_continuous(limits=c(-.066, .143),
                       breaks=seq(-.04, .14, .04), 
                       labels=paste0(format(100 * (seq(-.04,
                                                       .14, .04)),
                                            nsmall=1), "%")) +
    scale_y_continuous(limits=c(-0.001, .011),
                       breaks=seq(0, .01, .0025), 
                       labels=paste0(format(100 * (seq(0, .01, .0025)),
                                            nsmall=1), "%")) +
    facet_wrap(~use+method, nrow=2) +
    theme_prr
  
  
###  LGA Analysis --------------------------------------------------------------

lgaData <- createAggData(mmLgaYields, crLgaYields, dmLgaYields, pIndex) 

lgaPlot <- ggplot(lgaData$comp, aes(x=timeName, y=yield, 
                                    group=spaceName)) + 
           geom_line(colour='gray50') +
           geom_line(data=lgaData$compMed, aes(x=timeName, y=yield), size=1.5) +
           #scale_size_manual(values=c(1.5, 1.5, 1.5)) +
           #scale_colour_manual(values=c('blue', 'green', 'gray10')) +
           #scale_linetype_manual(values=c(1, 2, 3)) + 
           xlab("") + ylab("Rental Yield\n") +
           #scale_x_continuous(breaks=seq(2, 18, 4), labels=2011:2015)   +
           #scale_y_continuous(limits=c(.03, .05),
           #                   breaks=seq(.03, .05, .002), 
           #                   labels=paste0(format(100 * (seq(.03,
           #                                                   .05, .002)),
           #                                        nsmall=1), "%")) +
           facet_wrap(~method) +
           theme_prr


lgaDifPlot <- ggplot(lgaData$diff, aes(x=pIndex, y=dif, 
                                          group=spaceName)) + 
              geom_point(size=0) +
              geom_smooth(method=lm, size=.2, se=FALSE, colour='gray60') +
              geom_smooth(data=lgaData$diffMed, method=lm, size=2,
                          colour='gray10', se=TRUE) +
  
  #scale_size_manual(values=c(1.5, 1.5, 1.5)) +
  #scale_colour_manual(values=c('blue', 'green', 'gray10')) +
  #scale_linetype_manual(values=c(1, 2, 3)) + 
  xlab("") + ylab("Rental Yield\n") +
  #scale_x_continuous(breaks=seq(2, 18, 4), labels=2011:2015) +
  #scale_y_continuous(limits=c(.03, .05),
  #                   breaks=seq(.03, .05, .002), 
  #                   labels=paste0(format(100 * (seq(.03,
  #                                                   .05, .002)),
  #                                        nsmall=1), "%")) +
  facet_wrap(~method) +
  theme_prr

  
### By Suburb ------------------------------------------------------------------


subData <- createAggData(mmSuburbYields, crSuburbYields, dmSubYields, pIndex) 

subPlot <- ggplot(subData$comp, aes(x=timeName, y=yield, 
                                    group=spaceName)) + 
  geom_line(colour='gray50') +
  geom_line(data=subData$compMed, aes(x=timeName, y=yield), size=1.5) +
  #scale_size_manual(values=c(1.5, 1.5, 1.5)) +
  #scale_colour_manual(values=c('blue', 'green', 'gray10')) +
  #scale_linetype_manual(values=c(1, 2, 3)) + 
  xlab("") + ylab("Rental Yield\n") +
  #scale_x_continuous(breaks=seq(2, 18, 4), labels=2011:2015)   +
  #scale_y_continuous(limits=c(.03, .05),
  #                   breaks=seq(.03, .05, .002), 
  #                   labels=paste0(format(100 * (seq(.03,
  #                                                   .05, .002)),
  #                                        nsmall=1), "%")) +
  facet_wrap(~method) +
  theme_prr


subDifPlot <- ggplot(subData$diff, aes(x=pIndex, y=dif, 
                                       group=spaceName)) + 
  geom_point(size=0) +
  geom_smooth(method=lm, size=.2, se=FALSE, colour='gray60') +
  geom_smooth(data=subData$diffMed, method=lm, size=2,
              colour='gray10', se=TRUE) +
  
  #scale_size_manual(values=c(1.5, 1.5, 1.5)) +
  #scale_colour_manual(values=c('blue', 'green', 'gray10')) +
  #scale_linetype_manual(values=c(1, 2, 3)) + 
  xlab("") + ylab("Rental Yield\n") +
  #scale_x_continuous(breaks=seq(2, 18, 4), labels=2011:2015) +
  scale_y_continuous(limits=c(-.01, .03),
                     breaks=seq(-.01, .03, .01), 
                     labels=paste0(format(100 * (seq(-.01, .03, .01)),
                                          nsmall=1), "%")) +
  facet_wrap(~method) +
  theme_prr

### Save Workspace -------------------------------------------------------------

  save.image(paste0(dataPath, 'vizResults.RData'))

  
  
  
  
  
  
  
  
