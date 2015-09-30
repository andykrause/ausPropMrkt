Rent2015 = Melbourne.Rental.Data.12.months.to.May.2015...20150729
Rent2014 = Melbourne.Rental.Data.12.months.to.May.2014...20150729
Rent2013 = Melbourne.Rental.Data.12.months.to.May.2013...20150729
Rent2012 = Melbourne.Rental.Data.12.months.to.May.2012...20150729
Rent2011 = Melbourne.Rental.Data.12.months.to.May.2011...20150729


#####################################################################################
#
# Merge
#
#####################################################################################

Rent_All = rbind(Rent2011,Rent2012,Rent2013,Rent2014,Rent2015)

summary(Rent_All)
write.table(Rent_All, "/Users/gaschwanden/Desktop/Rent_All.txt", sep=",", na="")

Rent_All = Rent_working_on


#####################################################################################
# 
#  Normalize   
#              
##############################################################################

############
## Remove 0, NAN, outliers
######



Rent_All = subset(Rent_All, Rent_All$Property_Latitude < 0)
Rent_All = subset(Rent_All, Rent_All$AreaSize < 2500)
Rent_All = subset(Rent_All, Rent_All$AreaSize > 50)
Rent_All = subset(Rent_All, Rent_All$Parking < 6)
Rent_All = subset(Rent_All, Rent_All$EventPrice < 5000000)
Rent_All = subset(Rent_All, Rent_All$EventPrice > 0)
Rent_All = subset(Rent_All, Rent_All$Bedrooms < 8)

summary(Rent_All)

#bins = seq(0,2500,5)
#hist(Rent_All$AreaSize, breaks = bins)


#bins = seq(0,8,1)
#hist(Rent_All$Bedrooms, breaks = bins)

RentHouses = subset(Rent_All, Rent_All$PropertyCategorisation == "House")
RentUnits = subset(Rent_All, Rent_All$PropertyCategorisation == "Unit")

#bins = seq(0,2500,25)
#bins = seq(0,8,1)
#par(mfrow=c(2,1)) 
#hist(RentHouses$Bedrooms, breaks = bins)
#hist(RentUnits$Bedrooms, breaks = bins)

##############################################################################
#Which dataset you want to normalize
##############################################################################


Rent_All = RentHouses
#Rent_All = RentUnits


##############################################################################
#add variables
##############################################################################

names(Rent_All)[59] <- "EventPriceNew"
names(Rent_All)[60] <- "EventPriceSQM"

SoldP$EventPriceSQM = SoldP$EventPrice/SoldP$AreaSize
AreaSizeMean =  mean(SoldP$AreaSize,na.rm=T)

##############################################################################
#
# using the difference in mean price values to adjust the price
#
##############################################################################


Rooms_all_HasGarage = subset(Rent_All,Rent_All$HasGarage == 1)
Rooms_all_HasGarage_n = subset(Rent_All,Rent_All$HasGarage == 0)
HasGarage_extra_units= (mean(Rooms_all_HasGarage$EventPrice,na.rm = T) - mean(Rooms_all_HasGarage_n$EventPrice,na.rm = T))/AreaSizeMean
print("HasGarage")
print(HasGarage_extra_units)

for (i in 1:nrow(Rent_All)){
  
  if (Rent_All$HasGarage[i] == 0){
    
    Rent_All$EventPriceNew[i] = Rent_All$EventPrice[i]
  }else{
    
    Rent_All$EventPriceNew[i] = Rent_All$EventPrice[i] - HasGarage_extra_units
    
  }
}

Rooms_all_HasLockUpGarage = subset(Rent_All,Rent_All$HasLockUpGarage == 1)
Rooms_all_HasLockUpGarage_n = subset(Rent_All,Rent_All$HasLockUpGarage == 0)
HasLockUpGarage_extra_units= (mean(Rooms_all_HasLockUpGarage$EventPriceNew,na.rm = T) - mean(Rooms_all_HasLockUpGarage_n$EventPriceNew,na.rm = T))/AreaSizeMean
print("HasLockUpGarage")
print(HasLockUpGarage_extra_units)

for (i in 1:nrow(Rent_All)){
  if (Rent_All$HasLockUpGarage[i] == 0){
    
    Rent_All$EventPriceNew[i] = Rent_All$EventPriceNew[i]
  }else{
    
    Rent_All$EventPriceNew[i] = Rent_All$EventPriceNew[i] - HasLockUpGarage_extra_units
    
  }
}

Rooms_all_HasSpa = subset(Rent_All,Rent_All$HasSpa == 1)
Rooms_all_HasSpa_n = subset(Rent_All,Rent_All$HasSpa == 0)
HasSpa_extra_units= mean(Rooms_all_HasSpa$EventPriceNew,na.rm = T) - mean(Rooms_all_HasSpa_n$EventPriceNew,na.rm = T)/AreaSizeMean
print("HasSpa")
print(HasSpa_extra_units)

for (i in 1:nrow(Rent_All)){
  if (Rent_All$HasSpa[i] == 0){
    
    Rent_All$EventPriceNew[i] = Rent_All$EventPriceNew[i]
  }else{
    
    Rent_All$EventPriceNew[i] = Rent_All$EventPriceNew[i] - HasSpa_extra_units
    
  }
}

Rooms_all_HasStudy = subset(Rent_All,Rent_All$HasStudy == 1)
Rooms_all_HasStudy_n = subset(Rent_All,Rent_All$HasStudy == 0)
HasStudy_extra_units= (mean(Rooms_all_HasStudy$EventPriceNew,na.rm = T) - mean(Rooms_all_HasStudy_n$EventPriceNew,na.rm = T))/AreaSizeMean
print("HasStudy")
print(HasStudy_extra_units)

for (i in 1:nrow(Rent_All)){
  if (Rent_All$HasStudy[i] == 0){
    
    Rent_All$EventPriceNew[i] = Rent_All$EventPriceNew[i]
  }else{
    
    Rent_All$EventPriceNew[i] = Rent_All$EventPriceNew[i] - HasStudy_extra_units
    
  }
}

Rooms_all_HasPool = subset(Rent_All,Rent_All$HasPool == 1)
Rooms_all_HasPool_n = subset(Rent_All,Rent_All$HasPool == 0)
HasPool_extra_units= (mean(Rooms_all_HasPool$EventPriceNew,na.rm = T) - mean(Rooms_all_HasPool_n$EventPriceNew,na.rm = T))/AreaSizeMean
print("HasPool")
print(HasPool_extra_units)

for (i in 1:nrow(Rent_All)){
  if (Rent_All$HasPool[i] == 0){
    
    Rent_All$EventPriceNew[i] = Rent_All$EventPriceNew[i]
  }else{
    
    Rent_All$EventPriceNew[i] = Rent_All$EventPriceNew[i] - HasPool_extra_units
    
  }
}


Rooms_all_HasSunroom = subset(Rent_All,Rent_All$HasSunroom == 1)
Rooms_all_HasSunroom_n = subset(Rent_All,Rent_All$HasSunroom == 0)
HasSunroom_extra_units= (mean(Rooms_all_HasSunroom$EventPriceNew,na.rm = T) - mean(Rooms_all_HasSunroom_n$EventPriceNew,na.rm = T))/AreaSizeMean
print("HasSunroom")
print(HasSunroom_extra_units)

for (i in 1:nrow(Rent_All)){
  if (Rent_All$HasSunroom[i] == 0){
    
    Rent_All$EventPriceNew[i] = Rent_All$EventPriceNew[i]
  }else{
    
    Rent_All$EventPriceNew[i] = Rent_All$EventPriceNew[i] - HasSunroom_extra_units
    
  }
}


Rooms_all_HasCourtyard = subset(Rent_All,Rent_All$HasCourtyard == 1)
Rooms_all_HasCourtyard_n = subset(Rent_All,Rent_All$HasCourtyard == 0)
HasCourtyard_extra_units= (mean(Rooms_all_HasCourtyard$EventPriceNew,na.rm = T) - mean(Rooms_all_HasCourtyard_n$EventPriceNew,na.rm = T))/AreaSizeMean

print("HasCourtyard")
print(HasCourtyard_extra_units)

for (i in 1:nrow(Rent_All)){
  if (Rent_All$HasCourtyard[i] == 0){
    
    Rent_All$EventPriceNew[i] = Rent_All$EventPriceNew[i]
  }else{
    
    Rent_All$EventPriceNew[i] = Rent_All$EventPriceNew[i] - HasCourtyard_extra_units
    
  }
}

Rooms_all_HasBalcony = subset(Rent_All,Rent_All$HasBalcony == 1)
Rooms_all_HasBalcony_n = subset(Rent_All,Rent_All$HasBalcony == 0)
HasBalcony_extra_units= (mean(Rooms_all_HasBalcony$EventPriceNew,na.rm = T) - mean(Rooms_all_HasBalcony_n$EventPriceNew,na.rm = T))/AreaSizeMean
print("HasBalcony")
print(HasBalcony_extra_units)

for (i in 1:nrow(Rent_All)){
  if (Rent_All$HasBalcony[i] == 0){
    
    Rent_All$EventPriceNew[i] = Rent_All$EventPriceNew[i]
  }else{
    
    Rent_All$EventPriceNew[i] = Rent_All$EventPriceNew[i] - HasBalcony_extra_units
    
  }
}

for (i in 1:nrow(Rent_All)){
  Rent_All$EventPriceSQM[i] = Rent_All$EventPriceNew[i] / Rent_All$AreaSize[i]
}


###########################################################
##
## Year on year price development
##
###########################################################

## make vector for absolute prices and relative prices
absolutePrices = seq(0,60,1)
relativeRentalPrice = seq(0,60,1)

## find mean price for each month
print(SoldP$EventDate)
typeof(SoldP$EventDate)
currentMonthlyPrice = subset(SoldP,SoldP$EventDate>15)



##  store mean price in vector

for (j in 2010:2015){
  currentYear = subset(Rent_All,Rent_All$EventYear==j)
  print(j)
    for (i in 1:12){
      currentMonth = subset(currentYear,currentYear$EventMonth==i)
      summary(currentMonth)
      currentMonthPrice = mean(currentMonth$EventPrice, na.rm = T)/mean(currentMonth$AreaSize,na.rm=T)
      slot = (j-2010)*12+i
      print("Slot")
      print(slot)
      absolutePrices[slot]=currentMonthPrice
      print(i)
      print(currentMonthPrice)
  }
}
absolutePrices


for (i in 0:60){
  relativeRentalPrice[i]=absolutePrices[i+12]/absolutePrices[i]
}

relativeRentalPrice



###########################################################
##
## Export Values
##
###########################################################

#text working
write.table(Normalized_Property_Prices, "/path/Property_Houses_Rent_Normalized.txt", sep=",", na="")
