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

  
  

## 3. Combine Data


  
## 4. Create global model for Rental and Sold Market

Global_Model_Rent = lm(log(EventPrice)~ +as.factor(EventYear)+AreaSize+Bedrooms+PropertyType+
                         Parking*HasGarage+HasStudy+HasWalkInWardrobe+HasHeating+HasAirConditioning+HasEnsuite+
                         HasBalcony,Rent_All)

Global_Model_Sold = lm(log(EventPrice)~ +as.factor(EventYear)+AreaSize+Bedrooms+PropertyType+
                       Parking*HasGarage+HasStudy+HasWalkInWardrobe+HasHeating+HasAirConditioning+HasEnsuite+
                       HasBalcony,Rent_All)

## 5. Aplly global model to other market

# create new variable for created price

applyModel = function(pricingModel,dataFrame,npar=TRUE,print=TRUE){
  
  
  
}
  
## 6. Create Price / Rent ratio