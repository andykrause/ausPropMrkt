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
       
<<<<<<< HEAD
       # Select Input Type
       h4("Property Use Types"),
       selectInput("propType", "Select Property Type",
                   c('All Combined' = 'allC',
                     'Houses Only' = 'onlyHouse',
                     'Units Only' = 'onlyUnit',
                     'House vs Unit' = 'hVu',
                     'All Separate' = 'allS')),
       h4("Time Scale"),
       selectInput("timeType", "Select Temporal Scale",
                   c('Annual' = 'timeAnnual',
                     'Quarterly' = 'timeQuarterly',
                     '10 Day Increments' = 'time10')),
       h4("Geographic Scale Scale"),
       textInput("geoType", "Enter Suburb Names Separated by Commas.  
                 Enter 'All Metro' for entire region", "All Metro"),

       actionButton('reRun', "Replot results")
       
     ),
=======
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
>>>>>>> master

   # Main plotting panel   
     
   mainPanel( 
    tabsetPanel(
     tabPanel("Time Trends", plotOutput("timeTrends")),
     tabPanel("Location Map", plotOutput('locMap')))
   )
  )
))


# TODOs

# Add conditional page to allow user to change visual parameters, colors, lines, etc
# Add in quarterly analysis
# Add in 10 day analysis
# Add in breakdown by suburbs (make a show suburb table conditional pane)


