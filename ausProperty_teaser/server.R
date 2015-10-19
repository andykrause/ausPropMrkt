################################################################################
#                                                                              #
#   Server for Price to Rent Ratio Analysis                                    #
#                                                                              #  
################################################################################

# load libraries

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


###################################################################################
### Main Shiny Server -------------------------------------------------------------

shinyServer(function(input, output) {

###  Function that creates the plots
#     
  output$timeTrends <- renderPlot({
      
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
     
   }) # Closes updateData()

})  # Closes shinyServer