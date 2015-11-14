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

 ## Set colors
  
  methCols <- c('navy', 'royalblue2', 'skyblue')
  methSizes <- c(.5, 1.25, 2)
  methLines <- c(1, 1, 1)  
  
### Metro Level ----------------------------------------------------------------

 ## Metro All

  metroPlot <- ggplot(metroData$mix$comp,
                      aes(x=timeName, y=yield, group=method)) + 
               geom_line(aes(colour=method, size=method, linetype=method,
                             lineend='round', linejoin='round')) +
               scale_size_manual(values=methSizes) +
               scale_colour_manual(values=methCols) +
               scale_linetype_manual(values=methLines) + 
               xlab("") + ylab("Rental Yield\n") +
               scale_x_continuous(breaks=seq(2, 18, 4), labels=2011:2015) +
               scale_y_continuous(limits=c(.032, .048),
                                  breaks=seq(.032, .048, .002), 
                                  labels=paste0(format(100 * (seq(.032,
                                                                  .048, .002)),
                                          nsmall=1), "%")) +
               theme_prr

 ## Metro: Use Weighted
  
  metroPlotUW <- ggplot(metroData$useWgt$comp, 
                        aes(x=timeName, y=yield, group=method)) + 
                 geom_line(aes(colour=method, size=method, linetype=method,
                               lineend='round', linejoin='round')) +
                 scale_size_manual(values=methSizes) +
                 scale_colour_manual(values=methCols) +
                 scale_linetype_manual(values=methLines) + 
                 xlab("") + ylab("Rental Yield\n") +
                 scale_x_continuous(breaks=seq(2, 18, 4), labels=2011:2015) +
                 scale_y_continuous(limits=c(.032, .048),
                                    breaks=seq(.032, .048, .002), 
                                    labels=paste0(format(100 * 
                                                           (seq(.032, .048,
                                                                .002)),
                                            nsmall=1), "%")) +
                 theme_prr
  
 ## Metro: By Use  
  
  metroPlotUse <- ggplot(metroData$use$comp,
                         aes(x=timeName, y=yield, group=method)) + 
                  geom_line(aes(colour=method, size=method, linetype=method,
                                lineend='round', linejoin='round')) +
                  scale_size_manual(values=methSizes) +
                  scale_colour_manual(values=methCols) +
                  scale_linetype_manual(values=methLines) + 
                  xlab("") + ylab("Rental Yield\n") +
                  scale_x_continuous(breaks=seq(2, 18, 4), labels=2011:2015) +
                  scale_y_continuous(limits=c(.030, .050),
                                     breaks=seq(.030, .050, .002), 
                                     labels=paste0(format(100 * (
                                       seq(.030, .050, .002)),
                                            nsmall=1), "%")) +
                  facet_wrap(~use) +
                  theme_prr
  
 ## Metro Diff: All
  
  # By Appreciation
  metroDiffPlot_A <- ggplot(metroData$mix$diff, aes(x=pIndex, y=dif)) + 
                     geom_point(colour='black', size=2) + 
                     geom_smooth(method=lm) +
                     xlab("Home Price Movement in Qtr") +
                     ylab("Difference in Rental Yield Estimate\n") +
                     scale_x_continuous(limits=c(-.03, .06),
                                        breaks=seq(-.02, .06, .02), 
                                        labels=paste0(format(100 *
                                                      (seq(-.02, .06, .02)),
                                                       nsmall=1), "%")) +
                     scale_y_continuous(limits=c(0, .011),
                                        breaks=seq(0, .01, .002), 
                                        labels=paste0(format(100 * 
                                                      (seq(0, .01, .002)),
                                                       nsmall=1), "%")) +
                     facet_wrap(~method) +
                     theme_prr
  
  # By Time
  metroDiffPlot_T <- ggplot(metroData$mix$diff, aes(x=timeName, y=dif)) + 
                            geom_point(colour='black', size=2) + 
                     geom_smooth(method=lm) +
                     xlab("") +
                     ylab("Difference in Rental Yield Estimate\n") +
                     scale_x_continuous(breaks=seq(2, 18, 4), 
                                        labels=2011:2015) +
                     scale_y_continuous(limits=c(0, .011),
                                        breaks=seq(0, .01, .002), 
                                        labels=paste0(format(100 * 
                                                     (seq(0, .01, .002)),
                                                      nsmall=1), "%")) +
                     facet_wrap(~method) +
                     theme_prr
  
 ## Metro Diff: Use Weighted
  
  # By Appreciation
  metroDiffUWPlot_A <- ggplot(metroData$useWgt$diff, aes(x=pIndex, y=dif)) + 
                       geom_point(colour='black', size=2) + 
                       geom_smooth(method=lm) +
                       xlab("Home Price Movement in Qtr") +
                       ylab("Difference in Rental Yield Estimate\n") +
                       scale_x_continuous(limits=c(-.03, .06),
                                          breaks=seq(-.02, .06, .02), 
                                          labels=paste0(format(100 *
                                                       (seq(-.02, .06, .02)),
                                                        nsmall=1), "%")) +
                       scale_y_continuous(limits=c(0, .011),
                                          breaks=seq(0, .01, .002), 
                                          labels=paste0(format(100 * 
                                                       (seq(0, .01, .002)),
                                                        nsmall=1), "%")) +
                       facet_wrap(~method) +
                       theme_prr
  
  # By Time
  metroDiffUWPlot_T <- ggplot(metroData$useWgt$diff, aes(x=timeName, y=dif)) + 
                       geom_point(colour='black', size=2) + 
                       geom_smooth(method=lm) +
                       xlab("") +
                       ylab("Difference in Rental Yield Estimate\n") +
                       scale_x_continuous(breaks=seq(2, 18, 4), 
                                          labels=2011:2015) +
                       scale_y_continuous(limits=c(0, .011),
                                          breaks=seq(0, .01, .002), 
                                          labels=paste0(format(100 * 
                                                       (seq(0, .01, .002)),
                                                        nsmall=1), "%")) +
                       facet_wrap(~method) +
                       theme_prr
  
 ## Metro Diff: Use
  
  # By Appreciation
  metroDiffUPlot_A <- ggplot(metroData$use$diff, aes(x=pIndex, y=dif)) + 
    geom_point(colour='black', size=2) + 
    geom_smooth(method=lm) +
    xlab("Home Price Movement in Qtr") +
    ylab("Difference in Rental Yield Estimate\n") +
    scale_x_continuous(limits=c(-.03, .07),
                       breaks=seq(-.02, .06, .02), 
                       labels=paste0(format(100 *
                                              (seq(-.02, .06, .02)),
                                            nsmall=1), "%")) +
    scale_y_continuous(limits=c(0, .0092),
                       breaks=seq(0, .008, .002), 
                       labels=paste0(format(100 * 
                                              (seq(0, .008, .002)),
                                            nsmall=1), "%")) +
    facet_wrap(~use+method) +
    theme_prr
  
  # By Time
  metroDiffUPlot_T <- ggplot(metroData$use$diff, aes(x=timeName, y=dif)) + 
    geom_point(colour='black', size=2) + 
    geom_smooth(method=lm) +
    xlab("") +
    ylab("Difference in Rental Yield Estimate\n") +
    scale_x_continuous(breaks=seq(2, 18, 4), 
                       labels=2011:2015) +
    scale_y_continuous(limits=c(-.001, .011),
                       breaks=seq(0, .01, .002), 
                       labels=paste0(format(100 * 
                                              (seq(0, .01, .002)),
                                            nsmall=1), "%")) +
    facet_wrap(~use+method) +
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

  
  
  
  
  
  
  
  
