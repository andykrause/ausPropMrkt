# Australian Residential Property Market Analyses

This repository contains code dealing with the analysis of Australian residential property markets.  A number of data sources, file type and research questions are addressed in this project.  Each different section of the project is termed a 'component' and it treated separately below.

### List of components (working)

1. **DataMgmt:** Building and managing a database of Australian Residential Property Markets
2. **Price-to-Rent:** Analysis of price-to-rent ratios in Australia's capital cities

\  
&nbsp;

\  
&nbsp;

## Documentation

### 1. DataMgmt Component
\  
&nbsp;

#### Extracting Australian Property Monitor (APM) data

The Australian Property Monitor (APM), through the Australia Urban Research Infrastructure Network (AURIN) provided a key data source for this project.  APM data is downloaded (via AURIN) in small chunks (by House, Unit or Land) from various areas around the country.  The downloaded data is a .zip file containing a .csv of raw data as well as and some supporting metadata.  

We have created a set of R functions to unzip, rename and combine all like transaction types (sale, auction or rental) in to a single database file for a given geographica area (in this case, capital city region).  This process also creates individual .csv files for each transaction type.  The process below describes how to do this.

#### Setting up your directories

This process requires a specific directory structure.  

[ds1]: https://github.com/andykrause/ausPropMrkt/blob/master/figures/dirStrct1.PNG?raw=true
[ds2]: https://github.com/andykrause/ausPropMrkt/blob/master/figures/dirStrct2.PNG?raw=true


1. Begin by create a separate folder for each geographic area for which you wish to create separate file.  These can be at any level and any name you prefer.

 ![Directory1][ds1]

2. Place all corresponding .zip files into their respective folders (sold, rent, auct, etc.)

 ![Directory2][ds2]
 
 \  
&nbsp;

#### Converting .zip files

Begin by downloading the **apmDataPrep.R** file from this repository at: [https://github.com/andykrause/ausPropMrkt](https://github.com/andykrause/ausPropMrkt "Git")

The following are required R libraries: `RCurl, RODBC, RSQLite, plyr`

Next, set your base directory (**basePath**) to the individual geographic level you are working on, for example ``c:/temp/Adelaide``.  NOTE:  This process needs to be accomplished one geography at a time (for now).  

     basePath <- 'c:/temp/Adelaide'
 
Then, call the **buildAPMData()** function, where `basePath` is the basePath specified above, `newFileName` is the name of the output file (.db format), `transList` is the list of transaction type folder that you have created and `verbose` determines whether or not progress updates will be displayed in the R console. For `transList`, the name of each list item will be the name of the combined .csv file that is generated, the object in each list item (`=c('sold', 'auct')`) are the various .zip file types that will combined together. 

     buildAPMData(basePath,
                  newFileName = 'Adelaide.db',
                  transList = list('rentals'='rent','sales'=c('sold', 'auct')),
                  verbose = TRUE)   

This function will create a SQLite database called *Adelaide.db* containing a table of rentals data and a table of sales data (both 'sold' and 'auctions'). Again, individual .csv files for each transaction type will be outputted into the basePath directory as well. 

\  
&nbsp;

\  
&nbsp;

### 2. Price-to-Rent Component

MORE TO COME

This is my first line.