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
  x = subset(x, x$EventPrice < 5000)
  x = subset(x, x$EventPrice > 0)
  x = subset(x, x$Bedrooms < 8)
  return(x)
  }
  
Rent_All = cleanData(Rent_All)
Sold_All = cleanData(Sold_All)
  
  

## 3. Subdivide Data

Rent_Units = subset(Rent_All, Rent_All$PropertyType == 'Unit')
Rent_House = subset(Rent_All, Rent_All$PropertyType == 'House')
Sold_Units = subset(Sold_All, Sold_All$PropertyType == 'Unit')
Sold_House = subset(Sold_All, Sold_All$PropertyType == 'House')

  
## 4. Create global model for Rental and Sold Market

findModel = function(x,npar=TRUE,print=TRUE){
  x = lm( log(EventPrice) ~ EventYear+AreaSize+Bedrooms+Parking,x)
  summary(x)
  return(x)
}

Rent_Unit_Model = findModel(Rent_Units)
summary(Rent_Unit_Model)


## 5. Aplly global model to other market

# create new variable for created price

testData = predict.lm(Rent_Unit_Model,Sold_Units, se.fit = TRUE)

price.range = c(min(Rent_Units$EventPrice),max(Rent_Units$EventPrice))
size.range = c(min(Rent_Units$AreaSize),max(Rent_Units$AreaSize))

plot(testData$fit,testData$se.fit )
  
## 6. Create Price / Rent ratio