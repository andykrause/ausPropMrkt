####################################################################################################
##  
##  Melbourne Teaser Model
##
####################################################################################################

### Workflow ---------------------------------->

## 1. Load data
## 2. Clean Data
## 3. Create global model for Rental and Sold Market
## 4. Aplly global model to other market
## 6. Create Price / Rent ratio

## 1. Load data

Rent_All = 
Sold_All = SoldP



## 2. Clean Data

cleanData = function(x,npar=TRUE,print=TRUE){
  x = subset(x, x$Property_Latitude < 0)
  x = subset(x, x$AreaSize < 2500)
  x = subset(x, x$AreaSize > 50)
  x = subset(x, x$Parking < 6)
  x = subset(x, x$EventPrice < 5000000)
  x = subset(x, x$EventPrice > 0)
  x = subset(x, x$Bedrooms < 8)
  
  names(x)[64] = "PropertyRentRatio"
  print("PropertyRentRatio")
  names(x)[65] = "Rent"
  print("Rent")
  names(x)[66] = "Price"
  print("Price")
  return(x)
  }
  
Rent_All = cleanData(Rent_All)
Sold_All = cleanData(Sold_All)
  
Rent_All$Rent = Rent_All$EventPrice
Rent_All$Price = Rent_All$EventPrice
Rent_All$PropertyRentRatio = Rent_All$EventPrice
Sold_All$Price = Sold_All$EventPrice
Sold_All$Rent = Sold_All$EventPrice
Sold_All$PropertyRentRatio = Sold_All$EventPrice

### 2.1 Function to convert various APM date structures into R date structure --------------------------


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


Rent_All$DateNew = fixAPMDates(as.vector(Rent_All$EventDate))
Sold_All$DateNew = fixAPMDates(as.vector(Sold_All$EventDate))


# create dataFiel YearNew

fixAPMyears <- function(xDates      # Vector of dates to be fixed
)
{
  
  ## Set required libraries
  
  require(stringr)
  
  ## Break down dates
  
  # Remove Time
  xDates <- gsub(" 0:00", "", xDates)
  
  # Find location of slashes
  sLoc <- matrix(unlist(str_locate_all(xDates, '/')), ncol=4, byrow=TRUE)[,1:2]
  
  # Correct years
  years <- as.numeric(substr(xDates, sLoc[ ,2] + 1, 50))
  years <- ifelse(years < 2000, paste0('20', years), as.character(years))
  
  ## Return Values  
  
  return(years)
}

Sold_All$YearNew = fixAPMyears(Sold_All$EventDate)
Rent_All$YearNew = fixAPMyears(Rent_All$EventDate)

### Finding locations where a sensible model can be built
## 3.1. find postcodes with rental / sold / units / houses

summary(shKeep)

houseSales = subset(Rent_All, PropertyType == 'House')
saleHTable = table(houseSales[,'PostcodeID'], houseSales[,'YearNew'])
shKeep = which(apply(saleHTable, 1, min) >= 3)


Sold_All$AddressID

findPostCodes = function(rentals, sales, identifyer = 'AddressID', timefield = 'YearNew', locField = 'PostcodeID', geoTempLimit = 3)
{
  # Remove NAs in matchField
  xSales = subset(sales, !is.na(sales[identifyer]))
  xRentals = subset(rentals, !is.na(rentals[identifyer]))
  
  # Sort to order
  xSales = xSales[order(xSales[,identifyer]),]
  xRentals = xRentals[order(xRentals[,identifyer]),]
  
  # Split transactions by use
  houseSales = subset(xSales, PropertyType == 'House')
  unitSales = subset(xSales, PropertyType == 'Unit')
  houseRentals = subset(xRentals, PropertyType == 'House')
  unitRentals = subset(xRentals, PropertyType == 'Unit')
  
  # Find suburbs that meet criteria 
  saleHTable = table(houseSales[,locField], houseSales[,timeField])
  shKeep = which(apply(saleHTable, 1, min) >= geoTempLimit)
  shGeo = rownames(saleHTable[shKeep, ])
  saleUTable = table(unitSales[,locField], unitSales[,timeField])
  suKeep = which(apply(saleUTable, 1, min) >= geoTempLimit)
  suGeo = rownames(saleUTable[suKeep, ])
  rentHTable = table(houseRentals[,locField], houseRentals[,timeField])
  rhKeep = which(apply(rentHTable, 1, min) >= geoTempLimit)
  rhGeo = rownames(rentHTable[rhKeep, ])
  rentUTable = table(unitRentals[,locField], unitRentals[,timeField])
  ruKeep = which(apply(rentUTable, 1, min) >= geoTempLimit)
  ruGeo = rownames(rentUTable[ruKeep, ])
  bothGeo = intersect(intersect(intersect(shGeo, suGeo), rhGeo), ruGeo)
  houseGeo = intersect(shGeo,rhGeo)
  unitGeo = intersect(suGeo, ruGeo)
  eitherGeo = union(houseGeo, unitGeo)
  
  # Create tables
  return(list(bothGeo = bothGeo,
              houseGeo = houseGeo,
              unitGeo = unitGeo,
              eitherGeo = eitherGeo))  
  
}

thresData = findPostCodes(Rent_All, Sold_All)




## 3.2 reduce data to subburbs / zipcode with minimal numbers

applyThres <- function(thresData,       # Threshold data object from prrGeoLimit
                       transData,       # Set of transaction data
                       timePrefix='YT', # Which time was used YT or QT
                       geo="postCode"   # Which geo to use (one at a time)
){
  
  # Pull out single designations
  both <- ifelse(transData[,geo] %in% thresData[[1]],1,0)
  house <- ifelse(transData[,geo] %in% thresData[[2]],1,0)
  unit <- ifelse(transData[,geo] %in% thresData[[3]],1,0)
  either <- ifelse(transData[,geo] %in% thresData[[4]],1,0)
  
  # Combine them
  all <- as.data.frame(cbind(both, house, unit, either))
  
  # Rename
  names(all) <- paste0(timePrefix, "_", names(all), "_",geo)
  
  # Add to existing transactions
  return(cbind(transData, all))
}



## Check
summary(Rent_All)
summary(Sold_All)

## 4. Subdivide Data

Rent_Units = subset(Rent_All, Rent_All$PropertyType == 'Unit')
Rent_House = subset(Rent_All, Rent_All$PropertyType == 'House')
Sold_Units = subset(Sold_All, Sold_All$PropertyType == 'Unit')
Sold_House = subset(Sold_All, Sold_All$PropertyType == 'House')

summary(Rent_Units)
summary(Sold_House)
summary(Rent_House)
summary(Sold_Units)
  
## 4. Create global model for Rental and Sold Market
findModel = function(x,npar=TRUE,print=TRUE){
  x = lm( log(EventPrice) ~ EventYear+AreaSize+Bedrooms+Parking+factor(Postcode),x)
  summary(x)
  return(x)
}
Rent_Unit_Model = findModel(Rent_Units)
Sold_Unit_Model = findModel(Sold_Units)
Rent_House_Model = findModel(Rent_House)
Sold_House_Model = findModel(Sold_House)

summary(Sold_House_Model)


## 5. Aplly global model to other market

# create new variable for created price

predictApply = function(datasetToApply,predictionmodel,rentYes,npar=TRUE,print=TRUE){
  if(rentYes){
    datasetToApply$Rent = exp(predict (predictionmodel,newdata=datasetToApply))
    print("applying rental model")
  }else{
    datasetToApply$Price = exp(predict (predictionmodel,newdata=datasetToApply))
    print("applying price model")
    
  }
  
}


Sold_House$Rent = exp(predict(Rent_House_Model,newdata = Sold_House))
Sold_Units$Rent = exp(predict(Rent_Unit_Model, newdata = Sold_Units))
Rent_House$Price = exp(predict(Sold_House_Model,newdata = Rent_House))
Rent_Units$Price = exp(predict(Sold_Unit_Model, newdata = Rent_Units))


#predictApply(Sold_House,Rent_House_Model,rentYes = TRUE)
#predictApply(Sold_Units,Rent_Unit_Model,rentYes = TRUE)
#predictApply(Rent_House,Sold_House_Model,rentYes = FALSE)
#predictApply(Rent_Units,Sold_Unit_Model,rentYes = FALSE)



## 6. Create Price / Rent ratio
applyRatio = function(datasetToApply,npar=TRUE,print=TRUE){
  datasetToApply$PropertyRentRatio = datasetToApply$Price/datasetToApply$Rent
}

Sold_House$PropertyRentRatio = Sold_House$Price/Sold_House$Rent
Sold_Units$PropertyRentRatio = Sold_Units$Price/Sold_Units$Rent
Rent_House$PropertyRentRatio = Rent_House$Price/Rent_House$Rent
Rent_Units$PropertyRentRatio = Rent_Units$Price/Rent_Units$Rent

summary(Sold_House)
summary(Sold_Units)
summary(Rent_House)
summary(Rent_Units)

## Combine Data

allSoldData = rbind(Sold_House,Sold_Units)
allRentData = rbind(Rent_Units, Rent_House)

## 7. Show Development over time



## Print Plots
plot(Sold_Units$EventMonth,Sold_Units$PropertyRentRatio, xlim = c(1,12) , ylim = c(0,10000) )

allRentData = subset(allRentData,allRentData$PropertyRentRatio<5000)
allRentData = subset(allRentData,allRentData$PropertyRentRatio>50)

bins = seq(min(allRentData$PropertyRentRatio, na.rm = T),max(allRentData$PropertyRentRatio, na.rm = T)+100,25)
hist(allRentData$PropertyRentRatio,breaks = bins)

allSoldData = subset(allSoldData,allSoldData$PropertyRentRatio<5000)
allSoldData = subset(allSoldData,allSoldData$PropertyRentRatio>50)

bins = seq(min(allSoldData$PropertyRentRatio, na.rm = T),max(allSoldData$PropertyRentRatio, na.rm = T)+100,25)
hist(allSoldData$PropertyRentRatio,breaks = bins)

## Export Values

write.csv(allSoldData,"/Users/gaschwanden/Desktop/allSoldData.csv", na="" )
write.csv(allRentData,"/Users/gaschwanden/Desktop/allRentData.csv", na="" )
