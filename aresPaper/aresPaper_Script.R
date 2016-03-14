##########################################################################################
#                                                                                        #
#  Script for analyzing PRR from APM data                                                #
#                                                                                        #
##########################################################################################

## Set parameters and paths

reBuildData <- FALSE
reAnalyze <- FALSE
offline <- FALSE
verbose <- TRUE

dataPath <- "C:/data/research/priceRentMethComp/"
exportPath <- dataPath

salePath <- 'transData/newSales.csv'
rentPath <- 'transData/newRentals.csv'

geoPath=list(suburb='shapefiles/Vic_Suburbs.shp',
             lga='shapefiles/Vic_LGAs.shp',
             sla1='shapefiles/Vic_SLA1.shp',
             postcode='shapefiles/Vic_PostCodes.shp',
             ssFile='spatialData/allSS.csv')

## Source files

if(offline){
  source('c:/Code/research/ausPropMrkt/apmSetup.R')
} else {
  source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                'master/apmSetup.R'))
}

## Source remaining functions

sourceAPMFunctions(offline=offline, verbose=verbose)

## Set options

apmSetOptions()

### Load Data ----------------------------------------------------------------------------  

## Re build data from scratch?

if(reBuildData | !file.exists(paste0(dataPath, 'cleanTrans.RData'))){
  
  apmFullDataBuild(dataPath=dataPath, saleFile=salePath, rentFile=rentPath,
                   geoFiles=geoPath, offline=offline, verbose=verbose,
                   optionChanges=NULL)
  
} 

## Load prepared data

if(verbose) cat('Loading cleaned data\n')
load(paste0(dataPath, 'cleanTrans.RData'))
load(paste0(dataPath, 'studyShps.RData'))

## Do data analysis  

if(reAnalyze){
  results <- apmFullDataAnalysis(cleanTrans, dataPath)
} else  {
  load(paste0(dataPath, 'yieldResults.RData'))
}

### Create the necessary data objects for each method  -----------------------------------

## Compare matched data to all data

if(verbose) cat('Swapping Impute and Matched Yields\n')
comp.samp <- apmCompareSamples(trans.data)

## So there are differences.  How to adjust??  









## Set up graphics parameters 

# Set theme
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

# Set colors for plots
methCols <- c('navy', 'royalblue2', 'skyblue', 'gray50')
methSizes <- c(.5, 1, 1.5, 2)
methLines <- c(1, 1, 1, 1)  

## Fix the data for better plotting  

all.tidy <- results$tidy.data

# Rename the methods  
all.tidy$method[all.tidy$method=='spag'] <- 'Sp Aggr'
all.tidy$method[all.tidy$method=='hedimp'] <- 'Impute'
all.tidy$method[all.tidy$method=='srm'] <- 'Match'

# Rename the property Types  
all.tidy$type[all.tidy$type=='house'] <- 'Houses'
all.tidy$type[all.tidy$type=='unit'] <- 'Units'

# Remove the index method from plots
#plot.tidy <- all.tidy[all.tidy$method != 'Index', ]
plot.tidy <- all.tidy

# Reset the factor levels
plot.tidy$method <- factor(plot.tidy$method, levels = c('Sp Aggr', 'Index', 'Impute', 'Match'))

## Make a global comparison of methods

# Isolate the global data
glob <- plot.tidy[plot.tidy$geo.level=='Global', ]

# Global Plot  
ggplot(glob, aes(x=time, y=yield, group=method))+
  facet_wrap(~type) +
  geom_line(aes(colour=method, size=method, linetype=method,
                lineend='round', linejoin='round')) +
  scale_size_manual(values=methSizes) +
  scale_colour_manual(values=methCols) +
  scale_linetype_manual(values=methLines) + 
  xlab("") + ylab("Rental Yield\n") +
  scale_x_continuous(breaks=seq(0, 20, 4), labels=2011:2016) +
  scale_y_continuous(limits=c(.030, .049),
                     breaks=seq(.031, .049, .002), 
                     labels=paste0(format(100 * (seq(.031,
                                                     .049, .002)),
                                          nsmall=1), "%")) + 
  theme_prr

##############################################




