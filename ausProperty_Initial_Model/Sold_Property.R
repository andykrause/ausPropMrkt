#rm(Property_Melbourne_Sold_5_y_May_2015_20150728)

SoldP = Property_Melbourne_Sold_5_y_May_2015_20150728

summary(SoldP)

###################################################
##
## Nomralizing against individual Characters 
##
#####################################################

SoldP = Property_Melbourne_Sold_5_y_May_2015_20150728
SoldP = subset(SoldP, SoldP$Postcode == 3051)



############
## Remove 0, NAN, outliers
######

SoldP = subset(SoldP, SoldP$Property_Latitude < 0)
SoldP = subset(SoldP, SoldP$AreaSize < 2500)
SoldP = subset(SoldP, SoldP$AreaSize > 50)
SoldP = subset(SoldP, SoldP$Parking < 6)
SoldP = subset(SoldP, SoldP$EventPrice < 5000000)
SoldP = subset(SoldP, SoldP$EventPrice > 0)
SoldP = subset(SoldP, SoldP$Bedrooms < 8)

summary(SoldP)

#bins = seq(0,2500,5)
#hist(SoldP$AreaSize, breaks = bins)


#bins = seq(0,8,1)
#hist(SoldP$Bedrooms, breaks = bins)

SoldHouses = subset(SoldP, SoldP$PropertyCategorisation == "House")
SoldUnits = subset(SoldP, SoldP$PropertyCategorisation == "Unit")

#bins = seq(0,2500,25)
#bins = seq(0,8,1)
#par(mfrow=c(2,1)) 
#hist(SoldHouses$Bedrooms, breaks = bins)
#hist(SoldUnits$Bedrooms, breaks = bins)

#Which dataset you want to normalize

SoldP = SoldHouses

names(SoldP)[59] <- "EventPriceNew"
names(SoldP)[60] <- "EventPriceSQM"

SoldP$EventPriceSQM = SoldP$EventPrice/SoldP$AreaSize


Rooms_all_HasGarage = subset(SoldP,SoldP$HasGarage == 1)
Rooms_all_HasGarage_n = subset(SoldP,SoldP$HasGarage == 0)
AreaSizeMean =  mean(SoldP$AreaSize,na.rm=T)
HasGarage_extra_units= (mean(Rooms_all_HasGarage$EventPriceSQM,na.rm = T) - mean(Rooms_all_HasGarage_n$EventPriceSQM,na.rm = T))/AreaSizeMean

print("HasGarage")
print(HasGarage_extra_units)

for (i in 1:nrow(SoldP)){

  if (SoldP$HasGarage[i] == 0){
  
    SoldP$EventPriceNew[i] = SoldP$EventPriceSQM[i]
  }else{

  SoldP$EventPriceNew[i] = SoldP$EventPriceSQM[i] - HasGarage_extra_units

  }
}

Rooms_all_HasLockUpGarage = subset(SoldP,SoldP$HasLockUpGarage == 1)
Rooms_all_HasLockUpGarage_n = subset(SoldP,SoldP$HasLockUpGarage == 0)
HasLockUpGarage_extra_units= (mean(Rooms_all_HasLockUpGarage$EventPriceNew,na.rm = T) - mean(Rooms_all_HasLockUpGarage_n$EventPriceNew,na.rm = T))/AreaSizeMean
print("HasLockUpGarage")
print(HasLockUpGarage_extra_units)

for (i in 1:nrow(SoldP)){
  if (SoldP$HasLockUpGarage[i] == 0){
    
    SoldP$EventPriceNew[i] = SoldP$EventPriceNew[i]
  }else{
    
    SoldP$EventPriceNew[i] = SoldP$EventPriceNew[i] - HasLockUpGarage_extra_units
    
  }
}

Rooms_all_HasSpa = subset(SoldP,SoldP$HasSpa == 1)
Rooms_all_HasSpa_n = subset(SoldP,SoldP$HasSpa == 0)
HasSpa_extra_units= (mean(Rooms_all_HasSpa$EventPriceNew,na.rm = T) - mean(Rooms_all_HasSpa_n$EventPriceNew,na.rm = T))/AreaSizeMean
print("HasSpa")
print(HasSpa_extra_units)

for (i in 1:nrow(SoldP)){
  if (SoldP$HasSpa[i] == 0){
    
    SoldP$EventPriceNew[i] = SoldP$EventPriceNew[i]
  }else{
    
    SoldP$EventPriceNew[i] = SoldP$EventPriceNew[i] - HasSpa_extra_units
    
  }
}

Rooms_all_HasStudy = subset(SoldP,SoldP$HasStudy == 1)
Rooms_all_HasStudy_n = subset(SoldP,SoldP$HasStudy == 0)
HasStudy_extra_units= (mean(Rooms_all_HasStudy$EventPriceNew,na.rm = T) - mean(Rooms_all_HasStudy_n$EventPriceNew,na.rm = T))/AreaSizeMean
print("HasStudy")
print(HasStudy_extra_units)

for (i in 1:nrow(SoldP)){
  if (SoldP$HasStudy[i] == 0){
    
    SoldP$EventPriceNew[i] = SoldP$EventPriceNew[i]
  }else{
    
    SoldP$EventPriceNew[i] = SoldP$EventPriceNew[i] - HasStudy_extra_units
    
  }
}

Rooms_all_HasPool = subset(SoldP,SoldP$HasPool == 1)
Rooms_all_HasPool_n = subset(SoldP,SoldP$HasPool == 0)
HasPool_extra_units= (mean(Rooms_all_HasPool$EventPriceNew,na.rm = T) - mean(Rooms_all_HasPool_n$EventPriceNew,na.rm = T))/AreaSizeMean
print("HasPool")
print(HasPool_extra_units)

for (i in 1:nrow(SoldP)){
  if (SoldP$HasPool[i] == 0){
    
    SoldP$EventPriceNew[i] = SoldP$EventPriceNew[i]
  }else{
    
    SoldP$EventPriceNew[i] = SoldP$EventPriceNew[i] - HasPool_extra_units
    
  }
}


Rooms_all_HasSunroom = subset(SoldP,SoldP$HasSunroom == 1)
Rooms_all_HasSunroom_n = subset(SoldP,SoldP$HasSunroom == 0)
HasSunroom_extra_units= (mean(Rooms_all_HasSunroom$EventPriceNew,na.rm = T) - mean(Rooms_all_HasSunroom_n$EventPriceNew,na.rm = T))/AreaSizeMean
print("HasSunroom")
print(HasSunroom_extra_units)

for (i in 1:nrow(SoldP)){
  if (SoldP$HasSunroom[i] == 0){
    
    SoldP$EventPriceNew[i] = SoldP$EventPriceNew[i]
  }else{
    
    SoldP$EventPriceNew[i] = SoldP$EventPriceNew[i] - HasSunroom_extra_units
    
  }
}


Rooms_all_HasCourtyard = subset(SoldP,SoldP$HasCourtyard == 1)
Rooms_all_HasCourtyard_n = subset(SoldP,SoldP$HasCourtyard == 0)
HasCourtyard_extra_units= (mean(Rooms_all_HasCourtyard$EventPriceNew,na.rm = T) - mean(Rooms_all_HasCourtyard_n$EventPriceNew,na.rm = T))/AreaSizeMean

print("HasCourtyard")
print(HasCourtyard_extra_units)

for (i in 1:nrow(SoldP)){
  if (SoldP$HasCourtyard[i] == 0){
    
    SoldP$EventPriceNew[i] = SoldP$EventPriceNew[i]
  }else{
    
    SoldP$EventPriceNew[i] = SoldP$EventPriceNew[i] - HasCourtyard_extra_units
    
  }
}

Rooms_all_HasBalcony = subset(SoldP,SoldP$HasBalcony == 1)
Rooms_all_HasBalcony_n = subset(SoldP,SoldP$HasBalcony == 0)
HasBalcony_extra_units= (mean(Rooms_all_HasBalcony$EventPriceNew,na.rm = T) - mean(Rooms_all_HasBalcony_n$EventPriceNew,na.rm = T))/AreaSizeMean
print("HasBalcony")
print(HasBalcony_extra_units)

for (i in 1:nrow(SoldP)){
  if (SoldP$HasBalcony[i] == 0){
    
    SoldP$EventPriceNew[i] = SoldP$EventPriceNew[i]
  }else{
    
    SoldP$EventPriceNew[i] = SoldP$EventPriceNew[i] - HasBalcony_extra_units
    
  }
}

for (i in 1:nrow(SoldP)){
  SoldP$EventPriceSQM[i] = SoldP$EventPriceNew[i] / SoldP$AreaSize[i]
}



summary(SoldP)

#text working
write.table(SoldP, "/Users/gaschwanden/Desktop/Property_houses_Prices_Normalized.txt", sep=",", na="")

############
## drawing a histogram
######

#Compare Price before and after normalizing
SoldPro = subset(SoldP,SoldP$EventPrice < 1000000)
par(mfrow=c(2,1)) 
bins = seq(0,1000000,25000)
hist(SoldPro$EventPrice, breaks = bins)
hist(SoldPro$FinalResultEventPrice, breaks = bins)

mean(SoldPro$EventPrice,rm.na = T)
mean(SoldPro$FinalResultEventPrice,rm.na = T)

hist(SoldPro$EventPriceNew, breaks = bins)

#Price per SQM
SoldPro = subset(SoldP,SoldP$EventPriceSQM < 5000)
par(mfrow=c(1,1)) 
bins = seq(0,5000,25)
hist(SoldPro$EventPriceSQM, breaks = bins)
density = density(SoldPro$EventPriceSQM, na.rm = T)
plot(density)

###########################################################
## Year on year price development
###########################################################

## make vector for absolute prices and relative prices
absoluteSoldPrices = seq(0,60,1)
relativeSoldPrice = seq(0,60,1)

## find mean price for each month

##  store mean price in vector

for (j in 2010:2015){
  currentYear = subset(SoldP,SoldP$EventYear==j)
  #summary(currentYear)
  print(j)
  for (i in 1:12){
    currentMonth = subset(currentYear,currentYear$EventMonth==i)
    #summary(currentMonth)
    currentMonthPrice = mean(currentMonth$EventPrice, na.rm = T)/mean(currentMonth$AreaSize,na.rm=T)
    #print(currentMonthPrice)
    
    slot = (j-2010)*12+i
    #print("Slot")
    #print(slot)
    absoluteSoldPrices[slot]=currentMonthPrice
    #print(i)
    #print(currentMonthPrice)
  }
}
absoluteSoldPrices

for (i in 0:60){
  relativeSoldPrice[i]=absoluteSoldPrices[i+12]/absoluteSoldPrices[i]
}

relativeSoldPrice

########################################
#compare the relative prices

relativePricesCompared = seq(0,60,1)

for(i in 0:60){
  relativePricesCompared[i] = absoluteSoldPrices[i] / (52*absolutePrices[i])
  print(relativePricesCompared[i])
}
# one of the sold prices is 2.49
# check if direct division is better

relativePricesCompared
absoluteValue = 0
for(i in 6:52){
 absoluteValue = absoluteValue+relativePricesCompared[i]
 print(relativePricesCompared[i])
}
absoluteValue
###########################################################
## Export

#Excel not working
library(xlsx)
write.xlsx(Normalized_Property_Prices, "/Users/gaschwanden/Desktop/mydata.xlsx")

#text working
write.table(Normalized_Property_Prices, "/Users/gaschwanden/Desktop/Property_Prices_Normalized.txt", sep=",", na="")


#CSV
write.csv(Normalized_Property_Prices,"/Users/gaschwanden/Desktop/Property_Units_Prices_Normalized.csv", na="" )
###########################################################


## 3D plot
NoNAN = subset(SoldP,  SoldP$AreaSize < 10000)
NoNAN = subset(NoNAN, NoNAN$EventPrice < 5000000)
NoNAN = subset(NoNAN, NoNAN$Bedrooms < 6)

attach(NoNAN)
library(scatterplot3d)
scatterplot3d(Bedrooms, EventPrice, AreaSize, type = "p", highlight.3d = TRUE)

###########################################################
install.packages("aod")
install.packages("ggplot2")

sapply(SoldP, sd)
SoldP$EventPrice

object_LM = lm(log(EventPrice)~ as.factor(Postcode)+as.factor(EventYear)+AreaSize+Bedrooms+PropertyType+
                 Parking*HasGarage+HasStudy+HasWalkInWardrobe+HasHeating+HasAirConditioning+HasEnsuite+
                 HasBalcony,SoldP)

summary(SoldP)
summary(object_LM)
head(SoldP)
