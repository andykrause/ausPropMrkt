##########################################################################################
#                                                                                        #
#   General Functions for dealing with APM Data                                          #  
#                                                                                        #
##########################################################################################

### Function to source all other necessary custom functions ------------------------------

sourceAPMFunctions <- function(offline=FALSE,              # Are you offline?
                               verbose=FALSE               # Show progress
)
{
  
  if(verbose) cat('Sourcing Custom APM Functions\n')
  
  if(offline){
    source('c:/Code/research/ausPropMrkt/prrFunctions.R')
    source('c:/Code/research/ausPropMrkt/apmDataOptions.R')
    source('c:/Code/research/ausPropMrkt/apmDataPrepFunctions.R')
    source('c:/Code/research/ausPropMrkt/apmAnalysisFunctions.R')
    
    source('c:/Code/research/dataAnalysisTools/stShardFunctions.R')
  } else {
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/prrFunctions.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/apmDataOptions.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/apmDataPrepFunctions.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/apmAnalysisFunctions.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/',
                  'dataAnalysisTools/master/stShardFunctions.R'))
  }
  
  if(verbose) cat('Loading Libraries\n')
  
  ## Load Libraries
  
  library(plyr)
  library(dplyr)
  library(reshape2)
  library(stringr)
  library(maptools)
  library(sp)
  library(rgeos)
  
  
}
