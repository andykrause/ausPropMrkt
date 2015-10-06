################################################################################
#                                                                              #
#   Server for Price to Rent Ratio Analysis                                    #
#                                                                              #  
################################################################################

# load libraries

library(shiny)
library(xtable)

dataPath <- "C:/Dropbox/Australia Data/ausPropData/melData/"
load(paste0(dataPath, 'prr.RData'))
source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
              'master/prrFunctions.R'))


### Specify Function  (MOVE TO PRR FUNCTIONS LATER) ----------------------------

makeTimePlot <- function(ggObj,
                         timeField='Year',
                         group=NULL,
                         lineSize=2){
  
    ggObj$Time <- ggObj[,timeField]
    ggObj[,timeField] <- NULL
  
    if(is.null(group)){
     plotObj <- ggplot(ggObj, aes(x=Time, y=PRR)) + 
                geom_line(size=lineSize) +
                ggtitle('Price to Rent Ratios in Melbourne\n Cross Regression Method') +
                theme(legend.position='none')
    } else {
      plotObj <- ggplot(ggObj, aes(x=Time, y=PRR, color=Type)) + 
        geom_line(size=lineSize) +
        ggtitle('Price to Rent Ratios in Melbourne\n Cross Regression Method') +
        theme(legend.position='bottom')
      
    }
    return(plotObj)
} # Closes Functions 

###################################################################################
### Main Shiny Server -------------------------------------------------------------

shinyServer(function(input, output) {

###  Function that creates the plots
    
  output$timeTrends <- renderPlot({
      
    dataObj <- updateData()
  
      if(dataObj[1] == 'notReady'){
        par(mar=c(0, 0, 0, 0), xaxs='i', yaxs='i')
        plot(c(0,0,1,1), c(0,1,0,1), col=0, xaxt='n', yaxt='n')
        text(.5,.5, 'Data for this scenario is not ready', cex=4,
             color=2)
      } else{
        
        if(input$propType == 'hVu' | input$propType == 'allS'){
          makeTimePlot(dataObj, group='Type') 
        } 
        else 
        {
         makeTimePlot(dataObj)
        }
      }
      
    }) # Closes timeTrends Plot
  
### Reactive function to call UpdateData function ----------------------------------------  
 
   updateData <- reactive({
     
     # Set Default
     dataObj <- 'notReady'
     
     if(input$propType == 'allC'){
       if(input$timeType == 'timeAnnual'){
         if(input$geoType == 'All Metro'){
           dataObj <- globGG
         }
       }
     }
     
     if(input$propType == 'onlyHouse'){
       if(input$timeType == 'timeAnnual'){
         if(input$geoType == 'All Metro'){
           dataObj <- subset(typeGG, Type=='House')
         }
       }
     }
     
     if(input$propType == 'onlyUnit'){
       if(input$timeType == 'timeAnnual'){
         if(input$geoType == 'All Metro'){
           dataObj <- subset(typeGG, Type=='Unit')
         }
       }
     }
     
     if(input$propType == 'hVu'){
       if(input$timeType == 'timeAnnual'){
         if(input$geoType == 'All Metro'){
           dataObj <- typeGG
         }
       }
     }
     
     if(input$propType == 'allS'){
       if(input$timeType == 'timeAnnual'){
         if(input$geoType == 'All Metro'){
           xGlobGG <- globGG
           xGlobGG$Type <- 'Combined'
           dataObj <- rbind(typeGG, xGlobGG)
         }
       }
     }
     
     return(dataObj)
     
   }) # Closes updateData()

})  # Closes shinyServer