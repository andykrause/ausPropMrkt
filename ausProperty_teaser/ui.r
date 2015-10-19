################################################################################
#                                                                              #
#   User Interface for Price to Rent Ratio Shiny Page                          #                                               #
#                                                                              #
################################################################################

# Set library(s)

  library(shiny)

# Define UI for miles per gallon application

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Price to Rent Ratios in Melbourne: 2010 to 2015"),
  
  # Sidebar with controls to select the model and the variable to display
  
   sidebarLayout(
     sidebarPanel(
       
       h4("Geographic Breakdown"),
       selectInput("geogType", "Select Geography Type",
                   c('Entire Metro Region' = 'metro',
                     'By Local Govt Area' = 'lga',
                     'By Stat. Local Area (1)' = 'sla1',
                     'By Post Code' = 'postCode',
                     'By Suburb' = 'suburb')),
       
       h4("Time Breakdown"),
       selectInput("timeType", "Select Time Scale",
                   c('By Year' = 'year',
                     'By Quarter' = 'qtr')),
       
       h4("Property Use"),
       selectInput("useType", "Select Use Breakdown",
                   c('House & Unit combined' = 'comb',
                      'Uses separate' = 'use')),
       
        h4("Weight"),
        checkboxInput("wgtType", "Weighted (combine uses)", value = FALSE),
      
        h4("Geographic Area"),
        textInput("geoName", label='Geography Name', value = NULL)
      ),

   # Main plotting panel   
     
   mainPanel( 
    tabsetPanel(
     tabPanel("Time Trends", plotOutput("timeTrends")),
     tabPanel("Location Map", plotOutput('locMap')))
   )
  )
))

