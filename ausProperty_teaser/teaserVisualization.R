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

  source('c:/dropbox/stsShardFunctions.R')

 ## Set the path to the data

  dataPath <- "C:/Dropbox/Australia Data/ausPropData/melData/"

### Load the data --------------------------------------------------------------
  
  load(paste0(dataPath, 'prrWrkspc.RData'))
  load(paste0(dataPath, 'plotObjs.rData'))

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

### Quarterly for entire metro region ------------------------------------------
  
 ## Extract crossReg method
 
  crMetro <- globQ$tidyPRR
  crMetro$value <- 1 / globQ$tidyPRR$value
  crMetro$type <- 'Cross Regression'

 ## Creat the basic median value methods  
  
  mmMetroSale <- spaceTimeShard(allTrans[allTrans$transType == 'sale', ],
                                 metric='transValue',
                                 spaceField='all', 
                                 timeField = 'transQtr',
                                 defDim='time',
                                 stsLimit=1,
                                 calcs=list(median='median'))
  
  mmMetroRent <- spaceTimeShard(allTrans[allTrans$transType == 'rent', ],
                                 metric='transValue',
                                 spaceField='all', 
                                 timeField = 'transQtr',
                                 defDim='time',
                                 stsLimit=1,
                                 calcs=list(median='median'))
  
  mmMetro <- 1 / (mmMetroSale[[2]]$median / (52 * mmMetroRent[[2]]$median))
  mmMetro <- data.frame(type = 'Median Method',
                        time = 1:20,
                        variable = 'prr',
                        value = as.numeric(mmMetro))
  
 ## Combine into a single object
  
  metroTidy <- rbind(crMetro, mmMetro)
  metroTidy$variable <- NULL
  
 ## Make Plot
  
  metroQPlot <- ggplot(metroTidy, aes(x=as.numeric(time), y=value, 
                                      group=type)) + 
                geom_line(aes(colour=type, size=type, linetype=type)) +
                scale_size_manual(values=c(1,1)) +
                scale_colour_manual(values=c('orange', 'gray60')) +
                scale_linetype_manual(values=c(1,3)) + 
                xlab("") + ylab("Rental Yield\n") +
                scale_x_continuous(breaks=seq(2, 18, 4), labels=2011:2015) +
                scale_y_continuous(limits=c(.032, .046),
                       breaks=seq(.032, .048, .001), 
                       labels=paste0(format(100*(seq(.032, .048, .001)),
                                            nsmall=1), "%")) +
                theme_mry
  

 ## Export Plot
 
  png(height=1200, width=2400, filename="c:/temp/metroQ.png", type="cairo",
      res=300)
    print(metroQPlot)
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
  
  
###################################
  
  
  metroPrice <- spaceTimeShard(xTrans, 
                               'price',
                               'all', 'transQtr', defDim='time',
                               stsLimit=1,
                               calcs=list(median='median',
                                          stdev='sd'))
  
  
  