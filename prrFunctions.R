####################################################################################################
### Below here is where I'm working on building a master function that will analyse the data
### with all methods possible and across all geographic levels desired
### WORK IN PROGRESS 
####################################################################################################

### Function to compare price and rent on only matched properties ----------------------------------

prrDirectCompare <- function(sales,               # Data.frame of sales
                             rentals,             # Data.frame of rentals
                             matchField = 'ID',   # Field containing matching ID
                             saleField = 'Price', # Field containing sale price
                             rentField = 'Rent',  # Field containing rent 
                             timeField = 'Year'   # Field containing time breakdown
                             ){
  
 ## Example of how to use function
  
  if(F){
    prr <- prrDirectCompare(sales=Sales, rentals=Rentals,
                            matchField='AddressID', saleField='Price',
                            rentField='Rent', timeField='Year')  
  }
  
 ## Matching sales to rentals
  
  # Remove NAs in matchField
  xSales <- subset(sales, !is.na(sales[matchField]))
  xRentals <- subset(rentals, !is.na(rentals[matchField]))
  
  # Sort to order
  xSales <- xSales[order(xSales[,matchField]),]
  xRentals <- xRentals[order(xRentals[,matchField]),]
  
  # Extract matching field
  sMatch <- xSales[ ,matchField]
  rMatch <- xRentals[ ,matchField]
  
  # Perform cross match identification
  mSales <- xSales[!is.na(match(sMatch, rMatch)), ]
  mRentals <- xRentals[!is.na(match(rMatch, sMatch)), ]
  
  # Make the match
  mTrans <- merge(mSales[, c(matchField, saleField, timeField)],
                  mRentals[, c(matchField, rentField, timeField)],
                  by=matchField)
  
  # Rename Match Fields
  names(mTrans) <- c(matchField, saleField, 'saleTime', rentField, 'rentTime')
  
 ## Make time adjustments to matched transactions
  
  # Create the rent index
  rentTrend <- as.numeric(tapply(mTrans[,rentField], mTrans$rentTime, median))
  rentIndex <- rentTrend / rentTrend[1]
  
  # Create the sale index
  saleTrend <- as.numeric(tapply(mTrans[,saleField], mTrans$saleTime, median))
  saleIndex <- saleTrend / saleTrend[1]
  
  # Make the adjustments to the rentals
  rentAdj <- (rentIndex[as.numeric(as.factor(mTrans$saleTime))] /
                rentIndex[as.numeric(as.factor(mTrans$rentTime))])
  mTrans$adjRent <- mTrans[,rentField] * rentAdj
  
  # Make the adjustments to the sales
  saleAdj <- (saleIndex[as.numeric(as.factor(mTrans$rentTime))] /
                saleIndex[as.numeric(as.factor(mTrans$saleTime))])
  mTrans$adjSale <- mTrans[,saleField] * saleAdj
  
 ## Compute Ratios
  
  # Compute observation level ratios
  sAnchRatio <- mTrans[,saleField] / (mTrans$adjRent * 52 / 12)
  rAnchRatio <- mTrans$adjSale / (mTrans[,rentField] * 52 / 12)
  mAnchRatio <- (sAnchRatio + rAnchRatio) / 2
  
  # Calculate the median per time period
  sAnchIndex <- tapply(sAnchRatio, mTrans$saleTime, median)
  rAnchIndex <- tapply(rAnchRatio, mTrans$rentTime, median)
  mAnchIndex <- (sAnchIndex + rAnchIndex) / 2
  
 ## Return Values    
  return(list(matchTrans = mTrans,
              rawRatios = list(saleBased = sAnchRatio,
                               rentBased = rAnchRatio,
                               mixBased = mAnchRatio),
              prrIndex = list(saleBased = sAnchIndex,
                              rentBased = rAnchIndex,
                              mixBased = mAnchIndex),
              timeIndex = list(saleIndex = saleIndex,
                               rentIndex = rentIndex)))  
}  

### Function to convert various APM date structures into R date structure --------------------------

fixAPMDates <- function(xDates      # Vector of dates to be fixed
                        )
  {
  
 ## Set required libraries
  
  require(stringr)

 ## Break down dates
  
  # Remove Time
  xDates <- gsub(" 0:00", "", xDates)
  
  # Find location of slashes
  sLoc <- matrix(unlist(str_locate_all(xDates, '/')), ncol=4, byrow=TRUE)[,1:2]
  
  # Correct Days
  days <- as.numeric(substr(xDates, 1, sLoc[ ,1] - 1))
  days <- ifelse(days < 10, paste0('0', days), as.character(days))
    
  # Correct Months
  months <- as.numeric(substr(xDates, sLoc[ ,1] + 1, sLoc[ ,2] - 1))
  months <- ifelse(months < 10, paste0('0', months), as.character(months))
  
  # Correct years
  years <- as.numeric(substr(xDates, sLoc[ ,2] + 1, 50))
  years <- ifelse(years < 2000, paste0('20', years), as.character(years))

 ## Recombine into R date format  
    
  newDates <- as.Date(paste0(days, '/' , months, '/', years), "%d/%m/%Y")

 ## Return Values  
  
  return(newDates)
}

### Regression function that creates imputed rent and sales values -------------------------------

prrCrossReg <- function(formula,               # LM regression formula
                        saleData,              # Data containing sales
                        rentData,              # Data containing rentals
                        verbose = FALSE        # Show progress?
                        ){
  
 ## Estimate models and make new predictions
  
  # Esimate models
  if(verbose) cat('Estimating sale and rent models\n')
  saleModel <- lm(formula, data=saleData)
  rentModel <- lm(formula, data=rentData)
  
  # Make predictions of imputed values
  if(verbose) cat('Imputing values\n')
  impPrice <- exp(predict(saleModel, newdata=xRentals))
  impRent <- exp(predict(rentModel, newdata=xSales))
  
  # Apply cross values
  if(verbose) cat('Stacking observed and imputed values\n')
  saleData$Price <- xSales$transValue
  rentData$Price <- impPrice
  saleData$Rent <- impRent
  rentData$Rent <- xRentals$transValue
  
  # Combine data back together
  if(verbose) cat('Merging data\n')
  allData <- rbind(saleData, rentData)
  
 ## Return values
  return(list(allData = allData,
              saleModel = saleModel,
              rentModel = rentModel))
  
}