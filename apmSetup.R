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
    source('c:/Code/research/ausPropMrkt/apmOptions.R')
    source('c:/Code/research/ausPropMrkt/apmDataPrep.R')
    source('c:/Code/research/ausPropMrkt/apmDataAnalysis.R')
    source('c:/Code/dataMgmtTools/dataMungeTools.R')
    
    
    source('c:/Code/research/dataAnalysisTools/stShardFunctions.R')
  } else {
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/prrFunctions.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/apmOptions.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/apmDataPrep.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/apmDataAnalysis.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/',
                  'dataAnalysisTools/master/stShardFunctions.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/',
                  'dataMgmtTools/master/dataMungeTools.R'))
  }
  
 ## Load Libraries
  
  if(verbose) cat('Loading Libraries\n')
 
  require(plyr)
  require(dplyr)
  require(reshape2)
  require(stringr)
  require(maptools)
  require(sp)
  require(rgeos)
  require(spdep)
  
}
