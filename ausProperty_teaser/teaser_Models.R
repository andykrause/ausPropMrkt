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
  
  names(x)[64] <- "PropertyRentRatio"
  names(x)[65] <- "Rent"
  names(x)[66] <- "Price"
  
  return(x)
  }
  
Rent_All = cleanData(Rent_All)
Sold_All = cleanData(Sold_All)
  
Rent_All$Rent = Rent_All$EventPrice
Sold_All$Price = Sold_All$EventPrice

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



testData = predict (Rent_Unit_Model,newdata=Sold_Units)
Sold_Units$Rent = testData

Sold_Units$PropertyRentRatio = Sold_Units$Price/Sold_Units$Rent

plot(Sold_Units$PropertyRentRatio,Sold_Units$EventMonth, xlim = c(0,10000) , ylim = c(0,12) )

summary(Sold_Units)

## 6. Create Price / Rent ratio