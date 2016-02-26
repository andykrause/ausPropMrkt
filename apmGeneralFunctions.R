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
    source('c:/Code/research/ausPropMrkt/apmDatPrepFunctions.R')
    source('c:/Code/research/dataAnalysisTools/stShardFunctions.R')
  } else {
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/prrFunctions.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/apmDataOptions.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/apmDataPrepFunctions.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/',
                  'dataAnalysisTools/master/stShardFunctions.R'))
  }
}
