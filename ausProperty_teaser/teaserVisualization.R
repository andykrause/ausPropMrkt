################################################################################
#                                                                              #
#  Data visualization code for the APM teaser analysis                         #
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

 ## Source Files

  # File containing function for working with prr and APM data
  source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
              'master/prrFunctions.R'))

  source(paste0('https://raw.githubusercontent.com/andykrause/',
                'dataAnalysisTools/master/stShardFunctions.R'))

 ## Set the path to the data

  dataPath <- "C:/Dropbox/Australia Data/ausPropData/melData/"

### Load the data --------------------------------------------------------------
  
  load(paste0(dataPath, 'cleanData.RData'))
  load(paste0(dataPath, 'analysisResults.rData'))

### Create a custom plotting theme ---------------------------------------------
  
  theme_mry <- theme_grey() +
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
  
################################################################################  
### Visualize Results ----------------------------------------------------------  

### Entire Metro Region --------------------------------------------------------
  
 ## Build data
  
  # Median Method
  mmMetro <- mmMetYields[ ,c('timeName', 'yield')]
  mmMetro$Method  <- 'Median Method'
  
  # Cross Regression Method
  crMetro <- crMetYields[[4]][ ,c('timeName', 'median')]
  crMetro$Method  <- 'Cross Regression'
  names(crMetro)[names(crMetro) == 'median'] <- 'yield'
  
  # Direct Match
  dmMetro <- dmMetYields[[4]][ ,c('timeName', 'median')]
  dmMetro$Method  <- 'Direct Match'
  names(dmMetro)[names(dmMetro) == 'median'] <- 'yield'
  
  # Combine
  metro <- rbind(mmMetro, crMetro, dmMetro)
  
 ## Make Plot with Direct match
  
  metroPlot_DM <- ggplot(metro, aes(x=as.numeric(timeName), y=yield, 
                                      group=Method)) + 
                  geom_line(aes(colour=Method, size=Method, linetype=Method)) +
                  scale_size_manual(values=c(1,1, 1)) +
                  scale_colour_manual(values=c('orange', 'green', 'gray60')) +
                  scale_linetype_manual(values=c(1,3, 3)) + 
                  xlab("") + ylab("Rental Yield\n") +
                  scale_x_continuous(breaks=seq(2, 18, 4), labels=2011:2015) +
                  scale_y_continuous(limits=c(.032, .048),
                         breaks=seq(.032, .048, .001), 
                         labels=paste0(format(100*(seq(.032, .048, .001)),
                                              nsmall=1), "%")) +
                  theme_mry
  

 ## Export Plot
 
  png(height=1200, width=2400, filename="c:/temp/metro_dm.png", type="cairo",
      res=300)
    print(metroPlot_DM)
  dev.off()
  
  ## Make Plot without Direct match
  
  metroPlot <- ggplot(metro[1:40,], aes(x=as.numeric(timeName), y=yield, 
                                    group=Method)) + 
               geom_line(aes(colour=Method, size=Method, linetype=Method)) +
               scale_size_manual(values=c(1,1)) +
               scale_colour_manual(values=c('orange', 'gray60')) +
               scale_linetype_manual(values=c(1,3)) + 
               xlab("") + ylab("Rental Yield\n") + 
               scale_x_continuous(breaks=seq(2, 18, 4), labels=2011:2015) +
               scale_y_continuous(limits=c(.032, .048),
                                  breaks=seq(.032, .048, .001), 
                                  labels=paste0(format(100*(seq(.032, .048, .001)),
                                                nsmall=1), "%")) +
               theme_mry
  
  
  ## Export Plot
  
  png(height=1200, width=2400, filename="c:/temp/metro.png", type="cairo",
      res=300)
  print(metroPlot)
  dev.off()

### Compare metro prices and rents ---------------------------------------------  
  
  ## Create cr simple price and rent values
  
  crMetroPrice <- spaceTimeShard(xTrans,
                                 metric='price',
                                 spaceField='all', 
                                 timeField = 'transQtr',
                                 defDim='time',
                                 stsLimit=1,
                                 calcs=list(median='median'))
  
  crMetroRent <- spaceTimeShard(xTrans,
                                metric='rent',
                                spaceField='all', 
                                timeField = 'transQtr',
                                defDim='time',
                                stsLimit=1,
                                calcs=list(median='median'))
  
 ## Combine into tidy data frame
  
  prComp <- data.frame(time = rep(1:20, 4),
                       method = c(rep('Cross Regression', 40),
                                  rep('Median Method', 40)),
                       type = c(rep('Prices', 20), rep('Rents', 20),
                                rep('Prices', 20), rep('Rents', 20)),
                       value = c(crMetroPrice[[2]]$median / 
                                   crMetroPrice[[2]]$median[1],
                                 crMetroRent[[2]]$median / 
                                   crMetroRent[[2]]$median[1],
                                 mmMetroSale[[2]]$median / 
                                   mmMetroSale[[2]]$median[1],
                                 mmMetroRent[[2]]$median / 
                                   mmMetroRent[[2]]$median[1]))
  prComp$value <- prComp$value * 100
  
  
 ## Make Plot
  
  prPlot <- ggplot(prComp, aes(x=as.numeric(time), y=value, 
                        group=method)) + 
    geom_line(aes(colour=method, size=method, linetype=method)) +
    scale_size_manual(values=c(1,1)) +
    scale_colour_manual(values=c('orange', 'gray60')) +
    scale_linetype_manual(values=c(1,3)) + 
    xlab("") + ylab("Price and Rent Index (June 2010 = 100)\n") +
    scale_x_continuous(breaks=seq(2, 18, 4), labels=2011:2015) +
    scale_y_continuous(limits=c(84, 124),
                       breaks=seq(84, 124, 4)) +
    theme_mry +
    facet_wrap(~type)
    
 ## Export Plot
  
  png(height=1200, width=2400, filename="c:/temp/price_v_rent.png", 
      type="cairo",
      res=300)
  print(prPlot)
  dev.off()

### Metro by Use ---------------------------------------------------------------  
  
 ## Extra cross reg values

  crMetroUse <- globBUQ$tidyPRR
  crMetroUse$value <- 1 / globBUQ$tidyPRR$value
  crMetroUse$method <- 'Cross Regression'
  crMetroUse$type <- c(rep("Houses", 20), rep('Units', 20))
  
  
 ## Creat the basic median value methods  
  
  mmMetroUHSale <- spaceTimeShard(allTrans[allTrans$transType == 'sale' &
                                           allTrans$PropertyType == 'House', ],
                                  metric='transValue',
                                  spaceField='all', 
                                  timeField = 'transQtr',
                                  defDim='time',
                                  stsLimit=1,
                                  calcs=list(median='median'))
  
  mmMetroUUSale <- spaceTimeShard(allTrans[allTrans$transType == 'sale' &
                                           allTrans$PropertyType == 'Unit', ],
                                  metric='transValue',
                                  spaceField='all', 
                                  timeField = 'transQtr',
                                  defDim='time',
                                  stsLimit=1,
                                  calcs=list(median='median'))
  
  mmMetroUHRent <- spaceTimeShard(allTrans[allTrans$transType == 'rent' &
                                           allTrans$PropertyType == 'House', ],
                                  metric='transValue',
                                  spaceField='all', 
                                  timeField = 'transQtr',
                                  defDim='time',
                                  stsLimit=1,
                                  calcs=list(median='median'))
  
  mmMetroUURent <- spaceTimeShard(allTrans[allTrans$transType == 'rent' &
                                           allTrans$PropertyType == 'Unit', ],
                                  metric='transValue',
                                  spaceField='all', 
                                  timeField = 'transQtr',
                                  defDim='time',
                                  stsLimit=1,
                                  calcs=list(median='median'))
  
  # Conver to tidy data frame
  mmMetroUH <- 1 / (mmMetroUHSale[[2]]$median /
                      (52 * mmMetroUHRent[[2]]$median))
  mmMetroUU <- 1 / (mmMetroUUSale[[2]]$median /
                      (52 * mmMetroUURent[[2]]$median))
  
  mmMetroUse <- data.frame(method = 'Median Method',
                           type = c(rep("Houses", 20), rep('Units', 20)),
                           time = rep(1:20, 2),
                           variable = 'prr',
                           value = as.numeric(c(mmMetroUH, mmMetroUU)))
                        
  # Combine
  metroUse <- rbind(crMetroUse[1:20,],
                    mmMetroUse[1:20,],
                    crMetroUse[21:40,],
                    mmMetroUse[21:40,])
  metroUse$colInd <- c(rep("Houses - Cross Regression",20),
                       rep("Houses - Median Method           ",20),
                       rep("Units - Cross Regression",20), 
                       rep("Units - Median Method",20))

 ## Combine datasets
  
  usePlot <- ggplot(metroUse, aes(x=as.numeric(time), y=value, 
                               group=method)) + 
    geom_line(aes(colour=colInd, size=colInd, linetype=colInd)) +
    scale_size_manual(values=c(1,1,1,1)) +
    scale_colour_manual(values=c('red', 'gray60', 'yellow', 'gray60')) +
    scale_linetype_manual(values=c(1,3,1,3)) + 
    xlab("") + ylab("Rental Yield\n") +
    scale_x_continuous(breaks=seq(2, 18, 4), labels=2011:2015) +
    scale_y_continuous(limits=c(.03, .048),
                       breaks=seq(.03, .048, .002), 
                       labels=paste0(format(100*(seq(.03, .048, .002)),
                                            nsmall=1), "%")) +
    theme_mry +
    facet_wrap(~type)
  usePlot
  
## Export Plot
  
  png(height=1200, width=2400, filename="c:/temp/byUse.png", 
      type="cairo",
      res=300)
  print(usePlot)
  dev.off()
 
### Rental Yield Variation 
  
  png(height=1200, width=2400, filename="c:/temp/roiVariation.png", 
      type="cairo",
      res=300)
  print(geoCompPlot(subQ, globQ, 'suburb', 'transQtr'))
  dev.off()
 
  prrMean <- function(x) mean(x$tidyPRR$value)
  subMean <- unlist(lapply(subQ, prrMean))
  bot3 <- order(subMean, decreasing=T)[3:1]
  top3 <- order(subMean)[1:3]
  all6 <- c(top3, bot3)
  sub6 <- subQ[all6]
  sub6DF <- rbind.fill(prrTidyToDF(sub6))
  sub6DF$type <- NULL
  sub6DF$variable <- NULL
  sub6DF$geoName <- factor(sub6DF$geoName, levels=names(subQ)[all6])
  sub6Tidy <- melt(sub6DF)
  sub6Tidy$value <- 1/sub6Tidy$value

  suburbPlot <- ggplot(sub6Tidy, aes(x=as.numeric(time), y=value, group=geoName,
                       colour=geoName)) + 
                       geom_line(size=1) +
                       scale_colour_manual(values=c('blue', 'royalblue' , 
                                                    'cadetblue', 'indianred1',
                                                    'red', 'darkred')) + 
                       xlab("") +  ylab("Rental Yield\n") +
                       scale_y_continuous(limits=c(.02, .07),
                                          breaks=seq(.02, .07, .01), 
                                          labels=paste0(format(100*(
                                                          seq(.02, .07, .01)),
                                                        nsmall=1), "%")) +
                       scale_x_continuous(breaks=seq(2,18,4), 
                                          labels=2011:2015) +
                       facet_wrap(~geoName) + 
                       theme_mry +
                           theme(strip.text.x = element_text(size = 10))
                                 
  png(height=1200, width=2400, filename="c:/temp/topSuburbs.png", 
      type="cairo",
      res=300)
  print(suburbPlot)
  dev.off()
  
  
### By Bedrooms ----------------------------------------------------------------
  
  # By Use
  crMetHB1 <- spaceTimeShard(xTrans[xTrans$PropertyType == 'House' &
                                         xTrans$Bedrooms == 1, ],
                                 metric=c('yield'),
                                 spaceField='all', timeField='transQtr',
                                 defDim='time', stsLimit=3, 
                                 calcs=list(median='median'))
  
  crMetHB2 <- spaceTimeShard(xTrans[xTrans$PropertyType == 'House' &
                                      xTrans$Bedrooms == 2, ],
                             metric=c('yield'),
                             spaceField='all', timeField='transQtr',
                             defDim='time', stsLimit=3, 
                             calcs=list(median='median'))
  
  crMetHB3 <- spaceTimeShard(xTrans[xTrans$PropertyType == 'House' &
                                      xTrans$Bedrooms == 3, ],
                             metric=c('yield'),
                             spaceField='all', timeField='transQtr',
                             defDim='time', stsLimit=3, 
                             calcs=list(median='median'))
  
  crMetHB4 <- spaceTimeShard(xTrans[xTrans$PropertyType == 'House' &
                                      xTrans$Bedrooms == 4, ],
                             metric=c('yield'),
                             spaceField='all', timeField='transQtr',
                             defDim='time', stsLimit=3, 
                             calcs=list(median='median'))
  
  crMetHB5 <- spaceTimeShard(xTrans[xTrans$PropertyType == 'House' &
                                      xTrans$Bedrooms >= 5, ],
                             metric=c('yield'),
                             spaceField='all', timeField='transQtr',
                             defDim='time', stsLimit=3, 
                             calcs=list(median='median'))
  
  crMetUB1 <- spaceTimeShard(xTrans[xTrans$PropertyType == 'Unit' &
                                      xTrans$Bedrooms == 1, ],
                             metric=c('yield'),
                             spaceField='all', timeField='transQtr',
                             defDim='time', stsLimit=3, 
                             calcs=list(median='median'))
  
  crMetUB2 <- spaceTimeShard(xTrans[xTrans$PropertyType == 'Unit' &
                                      xTrans$Bedrooms == 2, ],
                             metric=c('yield'),
                             spaceField='all', timeField='transQtr',
                             defDim='time', stsLimit=3, 
                             calcs=list(median='median'))
  
  crMetUB3 <- spaceTimeShard(xTrans[xTrans$PropertyType == 'Unit' &
                                      xTrans$Bedrooms >= 3, ],
                             metric=c('yield'),
                             spaceField='all', timeField='transQtr',
                             defDim='time', stsLimit=3, 
                             calcs=list(median='median'))
  
 ##
  crHB1 <- crMetHB1[[4]][,c('timeName', 'median')]
  names(crHB1)[2] <- 'yield'
  crHB1$Use <- 'House'
  crHB1$Beds <- '1 Bed  '
  
  crHB2 <- crMetHB2[[4]][,c('timeName', 'median')]
  names(crHB2)[2] <- 'yield'
  crHB2$Use <- 'House'
  crHB2$Beds <- '2 Beds  '
  
  crHB3 <- crMetHB3[[4]][,c('timeName', 'median')]
  names(crHB3)[2] <- 'yield'
  crHB3$Use <- 'House'
  crHB3$Beds <- '3 Beds  '
  
  crHB4 <- crMetHB4[[4]][,c('timeName', 'median')]
  names(crHB4)[2] <- 'yield'
  crHB4$Use <- 'House'
  crHB4$Beds <- '4 Beds  '
  
  crHB5 <- crMetHB5[[4]][,c('timeName', 'median')]
  names(crHB5)[2] <- 'yield'
  crHB5$Use <- 'House'
  crHB5$Beds <- '5+ Beds  '
  
  crUB1 <- crMetUB1[[4]][,c('timeName', 'median')]
  names(crUB1)[2] <- 'yield'
  crUB1$Use <- 'Unit'
  crUB1$Beds <- '1 Bed  '
  
  crUB2 <- crMetUB2[[4]][,c('timeName', 'median')]
  names(crUB2)[2] <- 'yield'
  crUB2$Use <- 'Unit'
  crUB2$Beds <- '2 Beds  '
  
  crUB3 <- crMetUB3[[4]][,c('timeName', 'median')]
  names(crUB3)[2] <- 'yield'
  crUB3$Use <- 'Unit'
  crUB3$Beds <- '3 Beds  '
  
  crBeds <- rbind(crHB1, crHB2, crHB3, crHB4, crHB5, crUB1, crUB2, crUB3)
  
  bedPlot <- ggplot(crBeds, aes(x=as.numeric(timeName), y=yield, 
                                  group=as.factor(Beds))) + 
    geom_line(aes(colour=as.factor(Beds))) +
    scale_colour_manual(values=c('red', 'orange', 'green', 'blue', 'purple')) +
    xlab("") + ylab("Rental Yield\n") +
    scale_x_continuous(breaks=seq(2, 18, 4), labels=2011:2015) +
    scale_y_continuous(limits=c(.03, .048),
                       breaks=seq(.03, .048, .002), 
                       labels=paste0(format(100*(seq(.03, .048, .002)),
                                            nsmall=1), "%")) +
    theme_mry +
    facet_wrap(~Use)
  bedPlot
  
  png(height=1200, width=2400, filename="c:/temp/byBedroom.png", 
      type="cairo",
      res=300)
  print(bedPlot)
  dev.off()
  