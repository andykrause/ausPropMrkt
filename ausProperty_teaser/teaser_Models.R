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
  
  names(x)[64] <- "PropertyRentRatio"
  print("PropertyRentRatio")
  names(x)[65] <- "Rent"
  print("Rent")
  names(x)[66] <- "Price"
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

summary(Rent_All)
summary(Sold_All)

## 3. Subdivide Data

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
  x = lm( log(EventPrice) ~ EventYear+AreaSize+Bedrooms+Parking,x)
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
