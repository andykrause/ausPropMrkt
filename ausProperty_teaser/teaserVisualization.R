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

 ## Set the path to the data

  dataPath <- "C:/Dropbox/Australia Data/ausPropData/melData/"

### Load the data --------------------------------------------------------------
  
  load(paste0(dataPath, 'plotObjs.rData'))
  
### Visualize Results ----------------------------------------------------------  
  
 ## Quarterly for all areas
  
  # PRR
  ggplot(globQ$tidyPRR, aes(x=as.numeric(time), y=value)) + 
    geom_line(size=3, colour='orange') +
    theme(panel.background = element_rect(colour='black', fill='black'),
          panel.grid.major=element_line(colour='gray20'),
          panel.grid.minor=element_line(colour='gray20')) +
    xlab("") + ylab("Price to Rent Ratio") +
    scale_x_continuous(breaks=seq(2,18,4), labels=2011:2015) +
    scale_y_continuous(limits = c(21, 28)) +
    theme(plot.background=element_rect(fill='gray10'),
          axis.title.y=element_text(colour='white'))
  
  # Prices and Rents
  
  sX <- which(xTrans$transType == 'sale')
  rX <- which(xTrans$transType != 'sale')

  pMed <- as.data.frame(tapply(xTrans$transValue[sX],
                              xTrans$transQtr[sX], median))
  pMed$qtr <- rownames(pMed)
  names(pMed)[1] <- 'value'
  pMed$Transaction <- 'Prices'
  
  rMed <- as.data.frame(tapply(xTrans$transValue[rX], 
                                xTrans$transQtr[rX], median) * 52)
  rMed$qtr <- rownames(rMed)
  names(rMed)[1] <- 'value'
  rMed$Transaction <- 'Annual Rents'
  
  pMedR <- pMed
  pMedR$value <- (pMedR$value / pMed$value[1]) * 100
  rMedR <- rMed
  rMedR$value <- (rMedR$value / rMed$value[1]) * 100
  
  allMedR <- rbind(pMedR, rMedR)
  amTidy <- melt(allMedR, id=c('qtr', 'Transaction'))
  
  ggplot(amTidy, 
         aes(x=as.numeric(qtr), y=value, colour=Transaction)) + 
    geom_line(size=2) +
    scale_colour_manual(values=c('royalblue', 'green')) + 
    theme(panel.background = element_rect(colour='black', fill='black'),
          panel.grid.major=element_line(colour='gray20'),
          panel.grid.minor=element_line(colour='gray20')) +
    xlab("") + ylab("Index (2010 Q3 = 100)") +
  scale_x_continuous(breaks=seq(2,18,4), labels=2011:2015) +
    theme(legend.position='bottom', legend.title=element_blank()) + 
    theme(plot.background=element_rect(fill='gray10'),
          axis.title.y=element_text(colour='white'),
          legend.background=element_rect(fill='gray10'),
          legend.key=element_rect(fill='gray10', color='gray10'),
          legend.text=element_text(color='white'))

 ## Quarterly by use
  
  # PRR
  ggplot(globBUQ$tidyPRR, aes(x=as.numeric(time), y=value, 
                              group=type, colour=type)) + 
    geom_line(size=3) +
    scale_colour_manual(values=c('red', 'yellow')) + 
    theme(panel.background = element_rect(colour='black', fill='black'),
          panel.grid.major=element_line(colour='gray20'),
          panel.grid.minor=element_line(colour='gray20')) +
    xlab("") + ylab("Price to Rent Ratio") +
    scale_x_continuous(breaks=seq(2,18,4), labels=2011:2015) +
   theme(legend.position='bottom', legend.title=element_blank())+
  theme(plot.background=element_rect(fill='gray10'),
        axis.title.y=element_text(colour='white'),
        legend.background=element_rect(fill='gray10'),
        legend.key=element_rect(fill='gray10', color='gray10'),
        legend.text=element_text(color='white'))
  
 ## 
  
  geoCompPlot(subQ, globQ, 'suburb', 'transQtr')
 
  prrMin <- function(x) min(x$tidyPRR$value)
  prrMax <- function(x) max(x$tidyPRR$value)
  prrMean <- function(x) mean(x$tidyPRR$value)
  subMean <- unlist(lapply(subQ, prrMean))
  top3 <- order(subMean, decreasing=T)[1:3]
  bot3 <- order(subMean)[3:1]
  all6 <- c(top3, bot3)
  sub6 <- subQ[all6]
  sub6DF <- rbind.fill(prrTidyToDF(sub6))
  sub6DF$type <- NULL
  sub6DF$variable <- NULL
  sub6DF$geoName <- factor(sub6DF$geoName, levels=names(subQ)[all6])
  sub6Tidy <- melt(sub6DF)
  
  ggplot(sub6Tidy, aes(x=as.numeric(time), y=value, group=geoName,
                       colour=geoName)) + 
    geom_line(size=2) +
    scale_colour_manual(values=c('blue', 'royalblue' , 'cadetblue',
                                 'firebrick2', 'red', 'darkred')) + 
    theme(panel.background = element_rect(colour='black', fill='black'),
          panel.grid.major=element_line(colour='gray20'),
          panel.grid.minor=element_line(colour='gray20')) +
    xlab("") + ylab("Price to Rent Ratio") +
    scale_x_continuous(breaks=seq(2,18,4), labels=2011:2015) +
    facet_wrap(~geoName) + 
    theme(legend.position='bottom', legend.title=element_blank() )+
  theme(plot.background=element_rect(fill='gray10'),
        axis.title.y=element_text(colour='white'),
        legend.background=element_rect(fill='gray10'),
        legend.key=element_rect(fill='gray10', color='gray10'),
        legend.text=element_text(color='white'))
  
  
  
  
  
  
  
  