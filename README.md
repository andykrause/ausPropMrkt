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

2. Create separate folders for the various transaction types that you wish to analyse. These can also be of any name, but the names will need to be given as an argument in the functions described below. Note that you can combine raw files of any types together that you wish (such as Auction and Sold).

 ![Directory2][ds2]

3. Place all corresponding .zip files into their respective folders (sold, rent, auct, etc.)

 ![Directory3][ds3]





