################################################################################
#                                                                              #
#   Server for Price to Rent Ratio Analysis                                    #
#                                                                              #  
################################################################################

# load libraries

<<<<<<< HEAD
  library(shiny)
  library(xtable)

# Set data path
  
  dataPath <- "C:/Dropbox/Australia Data/ausPropData/melData/"
  load(paste0(dataPath, 'prr.RData'))
=======
library(shiny)
library(xtable)
library(ggplot2)
library(maptools)
library(ggmap)

dataPath <- "C:/Dropbox/Australia Data/ausPropData/melData/"
load(paste0(dataPath, 'plotObjs.RData'))
#source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
#              'master/prrFunctions.R'))
source(paste0('d://code//r/research//auspropmrkt//prrFunctions.R'))
>>>>>>> master

# Source in necessary code files
  
  source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
              'master/prrFunctions.R'))

<<<<<<< HEAD
### Specify Function  (MOVE TO PRR FUNCTIONS LATER) ----------------------------

makeTimePlot <- function(ttObj,
                         timeField='Year',
                         group=NULL){
  
    ttObj$data$Time <- ttObj$data[,names(ttObj$data) == ttObj$timeScale]
    
    ggObj <- ttObj$data
    
    if(is.null(group)){
     plotObj <- ggplot(ggObj, aes(x=Time, y=PRR)) + 
                geom_line(size=ttObj$lineSize, colour=ttObj$plotColors) +
                ggtitle('Price to Rent Ratios in Melbourne\n Cross Regression Method') +
                theme(legend.position='none')
    } else {
      plotObj <- ggplot(ggObj, aes(x=Time, y=PRR, colour=Type)) + 
        geom_line(size=ttObj$lineSize) +
        ggtitle('Price to Rent Ratios in Melbourne\n Cross Regression Method') +
        theme(legend.position='bottom') +
        scale_colour_manual(values=ttObj$plotColors)
      
    }
    return(plotObj)
} # Closes Functions 

=======
>>>>>>> master
###################################################################################
### Main Shiny Server -------------------------------------------------------------

shinyServer(function(input, output) {

###  Function that creates the plots
#     
  output$timeTrends <- renderPlot({
      
<<<<<<< HEAD
    dataObj <- updateTTData()
    nbrType <- length(table(dataObj$data$Type))
    
      if(dataObj$status == 'notReady'){
        par(mar=c(0, 0, 0, 0), xaxs='i', yaxs='i')
        plot(c(0,0,1,1), c(0,1,0,1), col=0, xaxt='n', yaxt='n')
        text(.5,.5, 'Data for this scenario is not ready', cex=4,
             color=2)
      } else{
        
        if(nbrType > 1){
          makeTimePlot(dataObj, group='Type') 
        } 
        else 
        {
         makeTimePlot(dataObj)
        }
      }
      
    }) # Closes timeTrends Plot
  
### Reactive function to update time trend (tt) data ---------------------------  
  #eventReactive(input$rerun,
   updateTTData <- eventReactive(input$reRun,{
     
     # Set Default
     ttObj <- list(status = 'notReady',
                   timeScale = 'Year',
                   geo = 'All Metro',
                   data = NULL,
                   groupBy = NULL,
                   plotColors = 'black',
                   lineSize = 2,
                   smooth = FALSE)
     
     ## Set data type
     ttObj$data <- switch(input$propType,
                          'allC' = globGG,
                          'onlyHouse' = subset(typeGG, Type == 'House'),
                          'onlyUnit' = subset(typeGG, Type == 'Unit'),
                          'hVu' = typeGG,
                          NULL
                          )
     
     if(input$propType == 'allC') ttObj$data$Type == 'Combined'
     
     if(input$propType == 'allS'){
       xGlobGG <- globGG
       xGlobGG$Type <- 'Combined'
       ttObj$data <- rbind(typeGG, xGlobGG)
     }
     
     if(!is.null(ttObj$data)) ttObj$status <- "Ready"
     
     if(input$propType == 'allS' | input$propType == 'hvu') ttObj$groupBy == 'Type'
     
     # Set plot colors
     ttObj$plotColors <- switch(input$propType,
                          'allC' = 'blue',
                          'onlyHouse' = 'orange',
                          'onlyUnit' = 'purple',
                          'hVu' = c('orange', 'purple'),
                          'allS' = c('blue', 'orange', 'purple'),
                          NULL
     )
     
     ### If by suburb
     if(input$geoType != 'All Metro'){
      geoIDs <- unlist(strsplit(input$geoType, ',')) 
      geoData <- subGG[subGG$Suburb %in% geoIDs,]
      ttObj$data <- geoData
      ttObj$data$Type <- geoData$Suburb  # Remove this later
     }
     
    # TO DO: Make an option to compare H V U within suburbs or not    
       
     return(ttObj)
=======
    dataObj <- updateData()
    if(dataObj[[1]] == 'notFound'){
      plot(c(0,0,1,1), c(0,1,1,0), col=0)
      text(x=.25, y=.25, 'No analysis for this combination')
    } else {
      prrTimePlot(dataObj)
    }  
      
   }) # Closes timeTrends Plot

  output$locMap <- renderPlot({
    
    if(input$geogType == 'lga'){
      xLga <- lgaShp[lgaShp@data$LGA_NAME11 %in% names(lgaQ),]
      xxLga <- fortify(xLga)
      xxLga$xCol='skyblue'
      xxLga$xCol[xxLga$group == 12.1] <- 2
      
      ggplot(xxLga, aes(long,lat, group=group, colour=xCol)) + 
        geom_polygon(fill=xxLga$xCol) +
        geom_path(color='white') + 
        theme(panel.background=element_rect(fill='black'), 
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              legend.position='none') 

    } 
   
    if(input$geogType == 'postCode'){
      xPC <- postCodeShp[postCodeShp@data$POA_2006 %in% names(pcQ),]
      xxPC <- fortify(xPC)
      xxPC$xCol='skyblue'
      xxPC$xCol[xxPC$group == 0.1] <- 2
      
      ggplot(xxPC, aes(long,lat, group=group, colour=xCol)) + 
        geom_polygon(fill=xxPC$xCol) +
        geom_path(color='white') + 
        theme(panel.background=element_rect(fill='black'), 
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              legend.position='none') 
      
    }   
  })
    
### Reactive function to call UpdateData function ----------------------------------------  
 
   updateData <- reactive({
      
     dataObj <- prrFindObj(geoType=input$geogType, 
                           timeType=input$timeType, 
                           useType=input$useType,
                           wgtType=input$wgtType, 
                           valType='prr',
                           geoName=input$geoName)
      
    return(dataObj)
>>>>>>> master
     
   }) # Closes updateData()

})  # Closes shinyServer