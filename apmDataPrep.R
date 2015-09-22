
### Example of how to use the code

if(F){
  
  # Set base path
  basePath <- 'c:/temp/apm'
  
  # Run full conversion
  buildAPMData(basePath,
               newFileName = 'DarwinS.db',
               transList = list('rentals'='rent','sales'=c('sold', 'auct')),
               verbose = TRUE)
  
  # Instruction for simply converting .zips to .csvs
  transType <- 'sold'
  convertAPMData(basePath, 'sold', TRUE)
  convertAPMData(basePath, 'rent', TRUE)
  convertAPMData(basePath, 'auct', TRUE)
  
}

### Master function that converts all zips into a single .db -------------------------

buildAPMData <- function(basePath,                                    # Path to data
                         newFileName = 'test.db',                     # Name of export file
                         transList = list('rentals'='rent',
                                          'sales'=c('sold', 'auct')), # List of file types
                         verbose=TRUE                                 # Show progress?
                         ){
  
  # Set up libraries
  require(RSQLite)
  require(plyr)
  require(RODBC)
  require(RCurl)
  
  # Source necessary files
  sourceHttps(paste0("https://raw.githubusercontent.com/andykrause/dataMgmtTools/",
                      "master/basicConversionTools.R"))
  
  # Convert ZIPS to .csvs
  if(verbose) cat('Converting ZIPS to .csvs', '\n')
  for(tL in 1:length(transList)){
    convertAPMData(basePath, transType=transList[[tL]], 
                   folderName=names(transList[tL]), verbose=verbose)
  }
  
  # Combine all .csvs into a SQLite database
  if(verbose) cat('Converting ZIPS to .csvs', '\n')
  convertCSVtoSQLite(dataPathCurrent=basePath,
                     newFileName=newFileName,
                     verbose=verbose,
                     overWrite=TRUE,
                     tableNames=c('Rentals','Sales'))
  
  # Success output
  if(verbose) cat('\n***CONVERSION SUCCESSFUL.  Data saved in:', 
                  paste0(basePath, '/', newFileName), '***\n')
  
}

### Helper function that unzips, combines and turns .zips in to a .csv -----------------------------

convertAPMData <- function(basePath,                # The main directory where the datalives
                           transType = 'sold',      # Type of data (subdirectory) to work on
                           folderName = NULL,
                           verbose=TRUE             # Do you want to see the progress?
                           ){

  # Require libraries
  require(plyr)
  
  # Fix to lower
  transType <- tolower(transType)

  # Get a list of zip files to extract - remove non zip
  if(verbose) cat("Reading in .ZIP files to be extracted\n")
  zipFiles <- tolower(list.files(basePath))
  zipFiles <- as.list(zipFiles[grep('.zip', zipFiles)])
  
  ### TODO:  Functionalize this!
  if(length(transType) == 1){
    zipFiles <- as.list(zipFiles[grep(transType, zipFiles)])
  } else {
    zFiles <- list(0)
    for(zF in 1:length(transType)){
      zFiles[[zF]] <- as.list(zipFiles[grep(transType[zF], zipFiles)])
    }
    zipFiles <- as.list(unlist(zFiles))
  }
  
  # Convert to .csv
  if(verbose) cat("Converting to .csv\n")
  lapply(zipFiles, extractAPMData, dataPath=basePath, verbose=TRUE)
 
  # Conversion Message
  if(verbose) cat(length(zipFiles), 'Files extracted\n')
  
  # Read in all data
  if(verbose) cat('Reading in all .csv files \n')
  csvFiles <- list.files(basePath)
  csvFiles <- as.list(paste0(basePath, '/', csvFiles[grep('.csv', csvFiles)]))
  
  # Select only .csv with correct transaction type
  if(length(transType) == 1){
    csvFiles <- as.list(csvFiles[grep(transType, csvFiles)])
  } else {
    zFiles <- list(0)
    for(zF in 1:length(transType)){
      zFiles[[zF]] <- as.list(csvFiles[grep(transType[zF], csvFiles)])
    }
    csvFiles <- as.list(unlist(zFiles))
  }
  
  # Read .csv into memory
  csvData <- lapply(csvFiles, read.csv, stringsAsFactors=FALSE)
  
  # Remove temp files
  lapply(csvFiles, file.remove)
  
  # Merge into a single file
  if(verbose) cat('Merging to a single file \n')
  csvData <- rbind.fill(csvData)
  
  # Write out the file
  if(is.null(folderName)) folderName <- transType[1]
  if(verbose) cat('Writing out merged ', folderName, 'file to ',
                  paste0(basePath, '/', folderName, '.csv'),'\n')
  write.csv(csvData, paste0(basePath, '/', folderName, '.csv'))
}

### Helper functions that handles the unzipping and renaming process -------------------------------

extractAPMData <- function(fileName,             # File name to be extracted
                           dataPath,             # Path to the data (.zip file)
                           verbose=TRUE          # Do you want to see the progress?
                           ){
  
  # Create temporary directory  
  tempPath = gsub('.zip', '', paste0(dataPath, '/', fileName))
  dir.create(tempPath, showWarnings = FALSE)
  
  # Extract data
  if(verbose) cat('Extracting, renaming and moving', gsub('.zip', '.csv', fileName), '\n')
  unzip(paste0(dataPath, '/', fileName), exdir=tempPath)
  
  # Remove all non .csv files
  fNames <- list.files(tempPath)
  xCut <- c(grep('.csv', fNames))
  if(length(xCut) != 0) xNames <- as.list(paste0(tempPath, '/', fNames[-xCut]))
  lapply(xNames, file.remove)
  
  # Rename File
  file.rename(from=paste0(tempPath, '/', list.files(tempPath)),
              to=paste0(tempPath, '/', gsub('.zip', '', fileName), '.csv'))
  
  # Move File
  file.copy(from=paste0(tempPath, '/', list.files(tempPath)),
            to=paste0(dataPath, '/', list.files(tempPath)),
            overwrite=TRUE)
  
  # Delete old file and directory
  file.remove(paste0(tempPath, '/', list.files(tempPath)))
  unlink(tempPath, recursive = TRUE) 
           
}

### Helper function that allow for sources files directly from github ---------------------

sourceHttps <- function(u,                       # URL of file
                        unlink.tmp.certs = FALSE # Security cert handling
                        ) {
  # load package
  require(RCurl)
  
  # read script lines from website using a security certificate
  if(!file.exists("cacert.pem")){
    download.file(url="http://curl.haxx.se/ca/cacert.pem",
                                               destfile = "cacert.pem")
  }
  script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
  if(unlink.tmp.certs) unlink("cacert.pem")
  
  # parase lines and evealuate in the global environement
  eval(parse(text = script), envir= .GlobalEnv)
}




