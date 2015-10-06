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
                 Enter 'All Metro' for entire region", "All Metro")

     ),

   # Main plotting panel   
     
   mainPanel(
     plotOutput('timeTrends')
   )
  )
))

