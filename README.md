# Australian Property Market Analyses

This repository contains code dealing with the analysis of Australian residential property markets.  A number of data sources, file type and research questions are addressed in this project.  

## Documentation

### Extracting Australian Property Monitor (APM) data

The Australian Property Monitor (APM), through the Australia Urban Research Infrastructure Network (AURIN) provide a key data source for this project.  APM data is downloaded (via AURIN) in small chunks (by House, Unit or Land) from various areas around the country.  The downloaded data is a .ZIP file of a .csv and some supporting metadata.  

We have created a set of R function to unzip, rename and combine all like transaction types (sale, auction or rental) in to a single database file for a given region.  This process also creates individual .csv files for each transaction type.  The process below describes how to do this.

#### Setting up your directories

This process requires a specific directory structure.  

[ds1]: https://github.com/andykrause/ausPropMrkt/blob/master/figures/dirStrct1.PNG?raw=true
[ds2]: https://github.com/andykrause/ausPropMrkt/blob/master/figures/dirStrct2.PNG?raw=true
[ds3]: https://github.com/andykrause/ausPropMrkt/blob/master/figures/dirStrct3.PNG?raw=true


1. Begin by create a separate folder for each geographic area for which you wish to create separate file.  These can be at any level and any name you prefer.

 ![Directory1][ds1]

2. Within each geographic area folder, create separate folders for the various transaction types that you wish to analyse. These can also be of any name, but the names will need to be given as an argument in the functions described below. Note that you can combine raw files of any types together that you wish (such as Auction and Sold).

 ![Directory2][ds2]

3. Place all corresponding .zip files into their respective folders (sold, rent, auct, etc.)

 ![Directory3][ds3]

#### Converting .zip files

Begin by downloading all code files from this repository at: [https://github.com/andykrause/ausPropMrkt](https://github.com/andykrause/ausPropMrkt "Git")

The following are required R libraries: `RCurl, RODBC, RSQLite, plyr`

Next, set your base directory (**basePath**) to the individual geographic level you are working on, for example ``c:/temp/Adelaide``.  NOTE:  This process needs to be accomplished one geography at a time (for now).  

     basePath <- 'c:/temp/Adelaide'
 
Then, call the **buildAPMData()** function, where `basePath` is the basePath specified above, `newFileName` is the name of the output file (.db format), `transList` is the list of transaction type folder that you have created and `verbose` determines whether or not progress updates will be displayed in the R console.  

     buildAPMData(basePath,
                  newFileName = 'Adelaide.db',
                  transList = c('sold','auct','rent'),
                  verbose = TRUE)   

Again, individual .csv files for each transaction type will be outputted into the basePath directory as well. 

MORE TO COME
