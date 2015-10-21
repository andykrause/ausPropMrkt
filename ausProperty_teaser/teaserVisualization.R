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
    scale_x_continuous(breaks=seq(2,18,4), labels=2011:2015)
  
  
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
    theme(legend.position='bottom', legend.title=element_blank())

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
   theme(legend.position='bottom', legend.title=element_blank())
  
 ## 