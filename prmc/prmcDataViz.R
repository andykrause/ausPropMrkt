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

 ## Set the path to the data and map files (FUTURE USE)

  #dataPath <- "C:/Dropbox/Australia Data/ausPropData/melData/"
  dataPath <- "D:/Data/Research/priceRentMethComp/"
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
                      scale_y_continuous(limits=c(0, .0098),
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
                      ylab("Difference in Rental Yield Estimate\n")  +
                      scale_x_continuous(breaks=seq(2, 18, 4), 
                                         labels=2011:2015) +
                      scale_y_continuous(limits=c(-.001, .011),
                                         breaks=seq(0, .01, .002), 
                                         labels=paste0(format(100 * 
                                                      (seq(0, .01, .002)),
                                                       nsmall=1), "%")) +
                      facet_wrap(~use+method) +
                      theme_prr

 ## Combine all Metro differences
  
  metroDiffComp <- ggplot(metroData$mix$diff, aes(x=timeName, y=dif)) + 
                      geom_point(colour='black', size=0) + 
                      geom_smooth(method=lm, se=FALSE, colour='black',
                                  size=1) +
                      geom_smooth(data=metroData$useWgt$diff,
                                  aes(x=timeName, y=dif),
                                  method=lm, se=FALSE,
                                  colour='red', size=1) +
                      geom_smooth(data=metroData$use$diff[1:60,],
                                  aes(x=timeName, y=dif),
                                  method=lm, se=FALSE,
                                  colour='blue', size=1) +
                      geom_smooth(data=metroData$use$diff[61:120,],
                                  aes(x=timeName, y=dif),
                                  method=lm, se=FALSE,
                                  colour='navy', size=1) +
                      xlab("") + 
                      ylab("Difference in Rental Yield Estimate\n")  +
                      scale_x_continuous(breaks=seq(2, 18, 4), 
                                         labels=2011:2015) +
                      scale_y_continuous(limits=c(-.001, .011),
                                         breaks=seq(0, .01, .002), 
                                         labels=paste0(format(100 * 
                                                      (seq(0, .01, .002)),
                                                       nsmall=1), "%")) +
                      facet_wrap(~method) +
                      theme_prr

### Lga Plots ------------------------------------------------------------------
 
  ## LGA All
  
  lgaPlot <- ggplot(lgaData$mix$comp, 
                    aes(x=timeName, y=yield, group=spaceName)) + 
    geom_line(colour='gray50', size= 0.1, lineend='round', linejoin='round') +
    geom_line(data=lgaData$mixWgt$comp, 
              aes(x=timeName, y=yield),
              colour='black', size=1.5, lineend='round', linejoin='round') +
    xlab("") + ylab("Rental Yield\n") +
    scale_x_continuous(breaks=seq(2, 18, 4), labels=2011:2015) +
    scale_y_continuous(limits=c(.015, .059),
                       breaks=seq(.015, .059, .004), 
                       labels=paste0(format(100 * (seq(.015, .059, .004)),
                                            nsmall=1), "%")) +
    facet_wrap(~method)+
    theme_prr
  
  ## LGA PLot Diffs all
  
  lgaDiffComp_T <- ggplot(lgaData$mix$diff, aes(x=timeName, y=dif, 
                                              group=spaceName)) + 
                 geom_point(colour='black', size=0) + 
                 stat_smooth(data=lgaData$mix$diff,
                             aes(x=timeName, y=dif, group=spaceName),
                             method=loess, se=FALSE, colour='gray50') +
    stat_smooth(data=lgaData$mixWgt$diff,
                aes(x=timeName, y=dif),
                method=loess, se=TRUE,
                colour='black', size=2) +
    stat_smooth(data=lgaData$useWgt$diff,
                aes(x=timeName, y=dif),
                method=loess, se=TRUE,
                colour='blue', size=2) +
    xlab("") + 
    ylab("Difference in Rental Yield Estimate\n")  +
    facet_wrap(~method) +
    theme_prr

  lgaDiffComp_A <- ggplot(lgaData$mix$diff, aes(x=pIndex, y=dif, 
                                                group=spaceName)) + 
    geom_point(colour='black', size=0) + 
    stat_smooth(data=lgaData$mix$diff,
                aes(x=pIndex, y=dif, group=spaceName),
                method=loess, se=FALSE, colour='gray50') +
    stat_smooth(data=lgaData$mixWgt$diff,
                aes(x=pIndex, y=dif),
                method=loess, se=TRUE,
                colour='black', size=2) +
    stat_smooth(data=lgaData$useWgt$diff,
                aes(x=pIndex, y=dif),
                method=loess, se=TRUE,
                colour='blue', size=2) +
    xlab("") + 
    ylab("Difference in Rental Yield Estimate\n")  +
    facet_wrap(~method) +
    theme_prr
  
  
    
### Suburb Plots ---------------------------------------------------------------
  
  ## Suburb All
  
  suburbPlot <- ggplot(suburbData$mix$comp, aes(x=timeName, y=yield, 
                                          group=spaceName)) + 
    geom_line(colour='gray50', size= 0.1, lineend='round', linejoin='round') +
    geom_line(data=suburbData$mixWgt$comp, 
              aes(x=timeName, y=yield),
              colour='black', size=1.5, lineend='round', linejoin='round') +
    xlab("") + ylab("Rental Yield\n") +
    scale_x_continuous(breaks=seq(2, 18, 4), labels=2011:2015) +
    scale_y_continuous(limits=c(.01, .079),
                       breaks=seq(.015, .059, .004), 
                       labels=paste0(format(100 * (seq(.015,
                                                       .059, .004)),
                                            nsmall=1), "%")) +
    facet_wrap(~method)+
    theme_prr
  
  ## LGA PLot Diffs all
  
  suburbDiffComp_T <- ggplot(suburbData$mix$diff, aes(x=timeName, y=dif, 
                                              group=spaceName)) + 
    geom_point(colour='black', size=0) + 
    stat_smooth(data=suburbData$mix$diff,
                aes(x=timeName, y=dif, group=spaceName),
                method=loess, se=FALSE, colour='gray50') +
    stat_smooth(data=suburbData$mixWgt$diff,
                aes(x=timeName, y=dif),
                method=loess, se=FALSE,
                colour='black', size=2) +
    stat_smooth(data=suburbData$useWgt$diff,
                aes(x=timeName, y=dif),
                method=loess, se=FALSE,
                colour='blue', size=2) +
    xlab("") + 
    ylab("Difference in Rental Yield Estimate\n")  +
    scale_y_continuous(limits=c(-.001, .011),
                       breaks=seq(0, .01, .002), 
                       labels=paste0(format(100 * (seq(0, .01, .002)),
                                                 nsmall=1), "%")) +
    facet_wrap(~method) +
    theme_prr
  
  suburbDiffComp_A <- ggplot(suburbData$mix$diff, 
                             aes(x=pIndex, y=dif, group=spaceName)) + 
    geom_point(colour='black', size=0) + 
    stat_smooth(data=suburbData$mix$diff,
                aes(x=pIndex, y=dif, group=spaceName),
                method=loess, se=FALSE, colour='gray50') +
    stat_smooth(data=suburbData$mixWgt$diff,
                aes(x=pIndex, y=dif),
                method=loess, se=FALSE,
                colour='black', size=2) +
    stat_smooth(data=suburbData$useWgt$diff,
                aes(x=pIndex, y=dif),
                method=loess, se=FALSE,
                colour='blue', size=2) +
    xlab("") + 
    ylab("Difference in Rental Yield Estimate\n")  +
    scale_y_continuous(limits=c(-.001, .011),
                       breaks=seq(0, .01, .002), 
                       labels=paste0(format(100 * (seq(0, .01, .002)),
                                            nsmall=1), "%")) +
    facet_wrap(~method) +
    theme_prr
  
  
### All types of differences
  
  allDiffComp_T <- ggplot(lgaData$mix$diff, aes(x=timeName, y=dif, 
                                                    group=spaceName)) + 
                 geom_point(colour='black', size=0) + 
    stat_smooth(data=suburbData$mixWgt$diff,
                aes(x=timeName, y=dif),
                method=loess, se=FALSE,
                colour='black', size=2, linetype=2) +
    stat_smooth(data=lgaData$mixWgt$diff,
                aes(x=timeName, y=dif),
                method=loess, se=FALSE,
                colour='black', size=2) +
    stat_smooth(data=lgaData$useWgt$diff,
                aes(x=timeName, y=dif),
                method=loess, se=FALSE,
                colour='blue', size=2) +
    stat_smooth(data=metroData$mix$diff,
                aes(x=timeName, y=dif),
                method=loess, se=FALSE,
                colour='red', size=2) +
    stat_smooth(data=metroData$useWgt$diff,
                aes(x=timeName, y=dif),
                method=loess, se=FALSE,
                colour='red', size=2, linetype=2) +
    stat_smooth(data=suburbData$useWgt$diff,
                aes(x=timeName, y=dif),
                method=loess, se=FALSE,
                colour='blue', size=2, linetype=2) +
    xlab("") + 
    ylab("Difference in Rental Yield Estimate\n")  +
    scale_y_continuous(limits=c(-.001, .011),
                       breaks=seq(0, .01, .002), 
                       labels=paste0(format(100 * 
                                              (seq(0, .01, .002)),
                                            nsmall=1), "%")) +
    facet_wrap(~method) +
    theme_prr
 
  
  
   
  allDiffComp_A <- ggplot(lgaData$mix$diff, aes(x=pIndex, y=dif, 
                                                group=spaceName)) + 
    geom_point(colour='black', size=0) + 
    stat_smooth(data=suburbData$mixWgt$diff,
                aes(x=pIndex, y=dif),
                method=loess, se=FALSE,
                colour='black', size=2) +
    stat_smooth(data=lgaData$mixWgt$diff,
                aes(x=pIndex, y=dif),
                method=loess, se=FALSE,
                colour='black', size=2, linetype=2) +
    stat_smooth(data=lgaData$useWgt$diff,
                aes(x=pIndex, y=dif),
                method=loess, se=FALSE,
                colour='blue', size=2) +
    stat_smooth(data=metroData$mix$diff,
                aes(x=pIndex, y=dif),
                method=loess, se=FALSE,
                colour='red', size=2) +
    stat_smooth(data=metroData$useWgt$diff,
                aes(x=pIndex, y=dif),
                method=loess, se=FALSE,
                colour='red', size=2, linetype=2) +
    stat_smooth(data=suburbData$useWgt$diff,
                aes(x=pIndex, y=dif),
                method=loess, se=FALSE,
                colour='blue', size=2, linetype=2) +
    xlab("") + 
    ylab("Difference in Rental Yield Estimate\n")  +
    scale_y_continuous(limits=c(0, .0105),
                       breaks=seq(0, .008, .002), 
                       labels=paste0(format(100 * (seq(0, .008, .002)),
                                                nsmall=1), "%")) +
    facet_wrap(~method) +
    theme_prr
  
### Save Workspace -------------------------------------------------------------

  save.image(paste0(dataPath, 'vizResults.RData'))

  
  
  
  
  
  
  
  
