

Fema_rawdata <-read.csv("openfema_claims20190331.csv")
View(Fema_rawdata)
summary(Fema_rawdata)
str(Fema_rawdata)



library(tidyverse)
library(tidyr)
library(dplyr)
library(gdata) 
library(lubridate)
library(ggplot2)
library(Hmisc)
library(car)
library(caret)
library(Metrics)
library(MASS)

save.image(file='Model_PCA_060520.RData')


#Removing duplicate records
Fema_RD_xDuplicate <- Fema_rawdata[!duplicated(Fema_rawdata),]

#Add ID column
Fema_RD_xDuplicate <- mutate(Fema_RD_xDuplicate, id = rownames(Fema_RD_xDuplicate))

######################## Data cleaning###############################

#Treating Outliers

bp_lfe <- boxplot(Fema_RD_xDuplicate$lowestfloorelevation, outcol = "red")
View(bp_lfe) 
bp_outlier_lfe <- boxplot.stats(Fema_RD_xDuplicate$lowestfloorelevation)
bp_Buidingvalue <- boxplot(Fema_RD_xDuplicate$amountpaidonbuildingclaim, outcol = "red")
View(bp_Buidingvalue) 
sc_buildingvalue <- scatter.smooth(Fema_RD_xDuplicate$amountpaidonbuildingclaim)
View(sc_buildingvalue)
bp_contentvalue <- boxplot(Fema_RD_xDuplicate$amountpaidoncontentsclaim, outcol = "red")
View(bp_contentvalue) 


# removing records with Negative claim amount
Fema_RD_xDuplicate[ , c(28:30,35:36)][is.na(Fema_RD_xDuplicate[ ,c(28:30,35:36)])] <- 0

#Total Loss and TIV
Fema_RD_xDuplicate$TotalClaim <- Fema_RD_xDuplicate$amountpaidonbuildingclaim + Fema_RD_xDuplicate$amountpaidoncontentsclaim
+ Fema_RD_xDuplicate$amountpaidonincreasedcostofcomplianceclaim

Fema_RD_xDuplicate$TotalInsuredValue <- Fema_RD_xDuplicate$totalbuildinginsurancecoverage + 
  Fema_RD_xDuplicate$totalcontentsinsurancecoverage


#replacing Na with 0 in numeric and interger fields
Fema_RD_xDuplicate[ , c(28:30,35:36,41:42)][is.na(Fema_RD_xDuplicate[ ,c(28:30,35:36,41:42)])] <- 0



#Check records with Total loss = 0
length(which(Fema_RD_xDuplicate$TotalClaim == 0))
#534157


#the lat/longs are truncated . so updating them using Zipcode package

data(zipcode)

zipcode$city <- toupper(zipcode$city)

Fema_RD_xDuplicate$zip <- as.character(Fema_RD_xDuplicate$reportedzipcode)
Fema_RD_xDuplicate$city <- as.character(Fema_RD_xDuplicate$reportedcity)

# removing records with negative claim value value
Fema_RD_xDups_xNeg1 <- filter(Fema_RD_xDuplicate, Fema_RD_xDuplicate$TotalClaim <=0 )


Fema_RD_xDups_xNeg$city <- gsub("BC","BEACH",Fema_RD_xDups_xNeg$city)
Fema_RD_xDups_xNeg$city <- gsub("BC","BCH","BEACH",Fema_RD_xDups_xNeg$city)
Fema_RD_xDups_xNeg$city.x <- gsub("BCH","BEACH",Fema_RD_xDups_xNeg$city.x)


#merge dataframe and add selective column to the existing dataframes
Fema_RD_xDups_xNeg <- merge(x = Fema_RD_xDups_xNeg, y = zipcode[, c(1,4:5)], by.x=c("reportedzipcode"), by.y=c("zip"), all.x = TRUE)


#converted city in zipcode to Upper and then By - city worked
Fema_RD_xDups_xNeg <- merge(x = Fema_RD_xDups_xNeg, y = zipcode[, c(1:2,4:5)], by = "zip", all.x = TRUE)
test<- merge(x = test, y = zipcode[, c(1:2,4:5)], by = "zip", all.x = TRUE)


test<- merge(x = test, y = zipcode[, c(1:2,4:5)], by = "city", all.x = TRUE)

Fema_RD_xDups_xNeg$e_latitude <- ifelse(is.na(Fema_RD_xDups_xNeg$latitude.y), Fema_RD_xDups_xNeg$latitude.x, Fema_RD_xDups_xNeg$latitude.y)
Fema_RD_xDups_xNeg$e_longitude <- ifelse(is.na(Fema_RD_xDups_xNeg$longitude.y), Fema_RD_xDups_xNeg$longitude.x, Fema_RD_xDups_xNeg$longitude.y)


Fema_RD_xDups_xNeg$e_floodzone <- ifelse(Fema_RD_xDups_xNeg$floodzone %in% c("", " ", "NA"),0,Fema_RD_xDups_xNeg$floodzone)

#Grouping Flood Zones

Fema_RD_xDups_xNeg <- Fema_RD_xDups_xNeg %>% mutate(Category_FloodZone = case_when(is.na(floodzone) ~ "Undertermined",
                                                                                   
                                                                                   floodzone %in% c("B","X","C","C00","C31") ~ "B, X and C",
                                                                                   
                                                                                   floodzone %in% c("A","A--","A00","AA") ~ "A",
                                                                                   
                                                                                   floodzone %in% c("A01","A02", "A03", "A04", "A4","A05", "A06","A6", "A07","A7", "A08", "A09","A9",
                                                                                                    
                                                                                                    "A10", "A11", "A12", "A13", "A14", "A15", "A16", "A17", "A18", 
                                                                                                    "A19", "A20","A21", "A22", "A23", "A24", "A25", "A26", "A27",
                                                                                                    "A28", "A29", "A30","A32","A36", "A50","A60") ~ "A1 - A30",
                                                                                   
                                                                                   floodzone == "A99" ~ "A99",floodzone == "AE" ~ "AE",
                                                                                   floodzone %in% c("AH","AHB") ~ "AH", floodzone %in% c("AO","A0B","AO5") ~ "AO",
                                                                                   floodzone == "AR" ~ "AR",floodzone %in% c("AH","AHB") ~ "AH",
                                                                                   floodzone == "D" ~ "D",
                                                                                   floodzone %in% c(0,000,"NA","") ~ "Undertermined",
                                                                                   floodzone == "V" ~ "V",floodzone %in% c("VE","V01", "V02",   "V03",   "V04",   "V05", "V06", "V07","01","05","07",  
                                                                                                                           "V08",  "V09", "V10", "V11","V12",   "V13",   "V14",   "V15",  
                                                                                                                           "V16", "V17",   "V18",   "V19",   "V20","V21",  "V22",   "V23",
                                                                                                                           "V24",  "V25",   "V26",   "V27",   "V28",   "V29",   "V30") ~ "VE,V01 - V30"))


Category_FL <- table(Fema_RD_xDups_xNeg$Category_FloodZone)
View(Category_FL)

# BFE Median value by differnt group zones
Median_BFE <- Fema_RD_xDups_xNeg %>% group_by(Category_FloodZone) %>% summarise(Med_BFE = median(basefloodelevation,na.rm =TRUE),
                                                                                Med_LAG = median(lowestadjacentgrade,na.rm =TRUE),
                                                                                Med_LFE = median(lowestfloorelevation ,na.rm =TRUE))
View(Median_BFE)
###################################################
IQR(Fema_RD_xDups_xNeg$basefloodelevation,na.rm =TRUE)--8.2
IQR(Fema_RD_xDups_xNeg$lowestfloorelevation,na.rm =TRUE)--11.8
IQR(Fema_RD_xDups_xNeg$lowestadjacentgrade,na.rm =TRUE)--9.9
############Updating BFE,LFE and LAG########################
Fema_RD_xDups_xNeg$e_BFE <-  ifelse(is.na(Fema_RD_xDups_xNeg$basefloodelevation) & Fema_RD_xDups_xNeg$Category_FloodZone %in% c("AH","AE"),9,
                                    ifelse (is.na(Fema_RD_xDups_xNeg$basefloodelevation)  & Fema_RD_xDups_xNeg$Category_FloodZone == "VE,V01-V30",12,
                                            ifelse(is.na(Fema_RD_xDups_xNeg$basefloodelevation) &  Fema_RD_xDups_xNeg$Category_FloodZone %in% c("AR", "A1 - A30"), 10,Fema_RD_xDups_xNeg$basefloodelevation)))


Fema_RD_xDups_xNeg$e_LFE <-  ifelse(is.na(Fema_RD_xDups_xNeg$lowestfloorelevation) & Fema_RD_xDups_xNeg$Category_FloodZone %in% c("A1 - A30","AE"),10,
                                    ifelse (is.na(Fema_RD_xDups_xNeg$lowestfloorelevation)  & Fema_RD_xDups_xNeg$Category_FloodZone == "AH",9.6,
                                            ifelse(is.na(Fema_RD_xDups_xNeg$lowestfloorelevation) &  Fema_RD_xDups_xNeg$Category_FloodZone == "VE,V01-V30", 15.1,
                                                   ifelse(is.na(Fema_RD_xDups_xNeg$lowestfloorelevation) &  Fema_RD_xDups_xNeg$Category_FloodZone == "AR",14.65,Fema_RD_xDups_xNeg$lowestfloorelevation))))


Fema_RD_xDups_xNeg$e_LAG <-  ifelse(is.na(Fema_RD_xDups_xNeg$lowestadjacentgrade) & Fema_RD_xDups_xNeg$Category_FloodZone == "A1 - A30",5.2,
                                    ifelse (is.na(Fema_RD_xDups_xNeg$lowestadjacentgrade)  & Fema_RD_xDups_xNeg$Category_FloodZone == "AH",7,
                                            ifelse(is.na(Fema_RD_xDups_xNeg$lowestadjacentgrade) &  Fema_RD_xDups_xNeg$Category_FloodZone == "VE,V01-V30", 6.2,
                                                   ifelse(is.na(Fema_RD_xDups_xNeg$lowestadjacentgrade) &  Fema_RD_xDups_xNeg$Category_FloodZone == "AR",8.35,
                                                          ifelse(is.na(Fema_RD_xDups_xNeg$lowestadjacentgrade) & Fema_RD_xDups_xNeg$Category_FloodZone == "AH", 8.3, Fema_RD_xDups_xNeg$lowestfloorelevation)))))



Fema_RD_xDups_xNeg$LevelOfRisk <- ifelse(startsWith(Fema_RD_xDups_xNeg$Category_FloodZone,"A",trim =TRUE),"High Risk",
                                         ifelse(Fema_RD_xDups_xNeg$floodzone == "B" & Fema_RD_xDups_xNeg$Category_FloodZone == "B, X and C","Moderate Risk",
                                                ifelse(Fema_RD_xDups_xNeg$floodzone %in% c("C","X","C00") &  Fema_RD_xDups_xNeg$Category_FloodZone == "B, X and C","Low Risk",
                                                       ifelse(startsWith(Fema_RD_xDups_xNeg$Category_FloodZone,"V",trim =TRUE),"High Risk Coastal",
                                                              ifelse(Fema_RD_xDups_xNeg$floodzone %in% c(NA,""),"Undetermined","check")))))

quantile(Fema_RD_xDups_xNeg$e_BFE,na.rm =TRUE)
test$e_BFE <-  ifelse(!is.na(test$basefloodelevation) & test$Category_FloodZone %in% c("AH","AE"),9,
                      ifelse (!is.na(test$basefloodelevation)  & test$Category_FloodZone == "VE,V01-V30",12,
                              ifelse(!is.na(test$basefloodelevation) &  test$Category_FloodZone %in% c("AR", "A1 - A30"), 10,test$basefloodelevation)))

test$e_BFE <-  ifelse(!is.na(test$e_BFE) & test$floodzone %in% c("AH","AE"),9,
                      +                                      ifelse (!is.na(test$e_BFE)  & test$floodzone == "VE,V01-V30",12,
                                                                     +                                              ifelse(!is.na(test$basefloodelevation) &  test$floodzone %in% c("AR", "A1 - A30"), 10,test$basefloodelevation)))
quantile(Fema_RD_xDups_xNeg$e_BFE,na.rm =TRUE)
#0%   25%   50%   75%  100% 
#-6002     9    10    10  9998 

boxplot(Fema_RD_xDups_xNeg$e_BFE)
Fema_RD_xDups_xNeg$e_BFE <- ifelse(Fema_RD_xDups_xNeg$e_BFE > 15, 8.2,Fema_RD_xDups_xNeg$e_BFE)
Fema_RD_xDups_xNeg$e_BFE <- ifelse(Fema_RD_xDups_xNeg$e_BFE < 0, 8.2,Fema_RD_xDups_xNeg$e_BFE)
boxplot(Fema_RD_xDups_xNeg$e_BFE)

IQR(Fema_RD_xDups_xNeg$e_BFE,na.rm =TRUE)--1
IQR(Fema_RD_xDups_xNeg$lowestfloorelevation,na.rm =TRUE)--11.8
IQR(Fema_RD_xDups_xNeg$lowestadjacentgrade,na.rm =TRUE)--9.9


quantile(Fema_RD_xDups_xNeg$e_LAG,na.rm =TRUE)
#     0%     25%     50%     75%    100% 
# -1851.5     5.2     5.2     8.7 99999.9 

Fema_RD_xDups_xNeg$e_LAG <- ifelse(Fema_RD_xDups_xNeg$e_LAG > 11, 9.9,Fema_RD_xDups_xNeg$e_LAG)
Fema_RD_xDups_xNeg$e_LAG <- ifelse(Fema_RD_xDups_xNeg$e_LAG < -0, 9.9,Fema_RD_xDups_xNeg$e_LAG)


quantile(Fema_RD_xDups_xNeg$e_LFE,na.rm =TRUE)
# 0%     25%     50%     75%    100% 
# -6002.0    10.0    10.0    10.0 99999.9 

median(Fema_RD_xDups_xNeg$e_LFE)

Fema_RD_xDups_xNeg$e_LFE <- ifelse(Fema_RD_xDups_xNeg$e_LFE > 15.2, 11.8,Fema_RD_xDups_xNeg$e_LFE)
Fema_RD_xDups_xNeg$e_LFE <- ifelse(Fema_RD_xDups_xNeg$e_LFE < 0, 11.8,Fema_RD_xDups_xNeg$e_LFE)

boxplot(Fema_RD_xDups_xNeg$e_LFE, horizontal = TRUE)

Fema_RD_xDups_xNeg$e_ElevationDifference <-  Fema_RD_xDups_xNeg$e_LFE - Fema_RD_xDups_xNeg$e_BFE

Fema_RD_xDups_xNeg$e_BFE <-  ifelse(is.na(Fema_RD_xDups_xNeg$e_BFE),8.2,Fema_RD_xDups_xNeg$e_BFE)
Fema_RD_xDups_xNeg$e_LAG <-  ifelse(is.na(Fema_RD_xDups_xNeg$e_LAG),11.8,Fema_RD_xDups_xNeg$e_LAG)
Fema_RD_xDups_xNeg$e_LFE <-  ifelse(is.na(Fema_RD_xDups_xNeg$e_LFE),9.9,Fema_RD_xDups_xNeg$e_LFE)


summary(Fema_RD_xDups_xNeg$e_LAG)
summary(Fema_RD_xDups_xNeg$e_ElevationDifference)
####################aggregating data==Count and sum####################

ByStateZoneLeveofrisk <-  Fema_RD_xDups_xNeg %>% group_by(state,Category_FloodZone,levelOfRisk) %>% summarise(RecordCount = n(),
                                                                                         TotalInsuredValue = sum(TotalInsuredValue,na.rm = TRUE),
                                                                                         BuildingValue = sum(totalbuildinginsurancecoverage,na.rm = TRUE),
                                                                                         ContetnValue = sum(totalcontentsinsurancecoverage, na.rm = TRUE),
                                                                                         TotalLoss = sum(TotalClaim,na.rm = TRUE),
                                                                                         BuildingLoss = sum(amountpaidonbuildingclaim, na.rm = TRUE),
                                                                                         ContentLoss = sum(amountpaidoncontentsclaim,na.rm = TRUE),
                                                                                         CostofComplianceLoss = sum(amountpaidonincreasedcostofcomplianceclaim, na.rm = TRUE))

state <- Fema_RD_xDups_xNeg %>% group_by(state) %>% summarise(TotalClaim = sum(TotalClaim), TIV = sum(TotalInsuredValue))

###############Assumptions and Missing values to Unknown########################################
#Obstruction Type
Obstruction <- Fema_RD_xDups_xNeg %>% group_by(e_Obstruction) %>% summarise(RecordCount = n(),TotalClaim = sum(TotalClaim), TIV = sum(TotalInsuredValue))
View(Obstruction)


#Condominium
Condo <- Fema_RD_xDups_xNeg %>% group_by(condominiumindicator) %>% summarise(RecordCount = n(),TotalClaim = sum(TotalClaim), TIV = sum(TotalInsuredValue))
View(Condo)
Fema_RD_xDups_xNeg$condominiumindicator <- as.character(Fema_RD_xDups_xNeg$condominiumindicator)
Fema_RD_xDups_xNeg$e_condo <- ifelse(Fema_RD_xDups_xNeg$condominiumindicator == "","Unknown",Fema_RD_xDups_xNeg$condominiumindicator)

table(Fema_RD_xDups_xNeg$e_condo)
write.csv(Condo,file = "LossTIv By Condo.csv")


#Occupancy
Fema_RD_xDups_xNeg$e_occupancy <- ifelse(is.na(Fema_RD_xDups_xNeg$occupancytype),0,Fema_RD_xDups_xNeg$occupancytype)
table(Fema_RD_xDups_xNeg$e_occupancy)
# Occupancy NA to 0

#agriculturestructureindicator
table(Fema_RD_xDups_xNeg$agriculturestructureindicator)

class(Fema_RD_xDups_xNeg$agriculturestructureindicator)
Fema_RD_xDups_xNeg$e_Agri_Indi <- as.character(Fema_RD_xDups_xNeg$agriculturestructureindicator)

Fema_RD_xDups_xNeg$e_Agri_Indi <- ifelse(Fema_RD_xDups_xNeg$e_Agri_Indi == "","Unknown",Fema_RD_xDups_xNeg$e_Agri_Indi)
table(Fema_RD_xDups_xNeg$e_Agri_Indi)

Fema_RD_xDups_xNeg$e_Agri_Indi <-  as.factor(Fema_RD_xDups_xNeg$e_Agri_Indi)

# N Unknown       Y 
# 115537 1745146      49 

#elevatedbuildingindicator
table(Fema_RD_xDups_xNeg$elevatedbuildingindicator)

class(Fema_RD_xDups_xNeg$elevatedbuildingindicator)

Fema_RD_xDups_xNeg$e_elev_indi <- as.character(Fema_RD_xDups_xNeg$elevatedbuildingindicator)

Fema_RD_xDups_xNeg$e_elev_indi <- ifelse(Fema_RD_xDups_xNeg$e_elev_indi == "","Unknown",Fema_RD_xDups_xNeg$e_elev_indi)
table(Fema_RD_xDups_xNeg$e_elev_indi)

# N     Unknown       Y 
# 1536092   41036  283604 

Fema_RD_xDups_xNeg$e_elev_indi <-  as.factor(Fema_RD_xDups_xNeg$e_elev_indi)

#elevationcertificateindicator
table(Fema_RD_xDups_xNeg$elevationcertificateindicator)

class(Fema_RD_xDups_xNeg$elevationcertificateindicator)

Fema_RD_xDups_xNeg$e_elevcerti_indi <- as.character(Fema_RD_xDups_xNeg$elevationcertificateindicator)

Fema_RD_xDups_xNeg$e_elevcerti_indi <- ifelse(Fema_RD_xDups_xNeg$e_elevcerti_indi == "","Unknown",Fema_RD_xDups_xNeg$e_elevcerti_indi)
table(Fema_RD_xDups_xNeg$e_elevcerti_indi)

# 1       2       3       4       A       B       C       D       E Unknown 
# 230402   85170  154062    9313      99      80      18      33     179 1381376 


Fema_RD_xDups_xNeg$e_elevcerti_indi <-  as.factor(Fema_RD_xDups_xNeg$e_elevcerti_indi)

#houseworship
table(Fema_RD_xDups_xNeg$houseworship)

class(Fema_RD_xDups_xNeg$houseworship)

Fema_RD_xDups_xNeg$e_worship <- as.character(Fema_RD_xDups_xNeg$houseworship)

Fema_RD_xDups_xNeg$e_worship <- ifelse(Fema_RD_xDups_xNeg$e_worship == "","Unknown",Fema_RD_xDups_xNeg$e_worship)
# table(Fema_RD_xDups_xNeg$e_worship)

# N Unknown       Y 
# 173390 1683542    3800 


Fema_RD_xDups_xNeg$e_worship <-  as.factor(Fema_RD_xDups_xNeg$e_worship)


#locationofcontents
table(Fema_RD_xDups_xNeg$locationofcontents)

class(Fema_RD_xDups_xNeg$locationofcontents)

Fema_RD_xDups_xNeg$e_contloc <- as.character(Fema_RD_xDups_xNeg$locationofcontents)

Fema_RD_xDups_xNeg$e_contloc <- ifelse(Fema_RD_xDups_xNeg$e_contloc == "","Unknown",Fema_RD_xDups_xNeg$e_contloc)
table(Fema_RD_xDups_xNeg$e_contloc)

# N Unknown       Y 
# 173390 1683542    3800 


Fema_RD_xDups_xNeg$e_contloc <-  as.factor(Fema_RD_xDups_xNeg$e_contloc)


#nonprofitindicator
table(Fema_RD_xDups_xNeg$nonprofitindicator)

class(Fema_RD_xDups_xNeg$locationofcontents)

Fema_RD_xDups_xNeg$e_npnprofitindicator <- as.character(Fema_RD_xDups_xNeg$nonprofitindicator)

Fema_RD_xDups_xNeg$e_npnprofitindicator <- ifelse(Fema_RD_xDups_xNeg$e_npnprofitindicator == "","Unknown",Fema_RD_xDups_xNeg$e_contloc)
table(Fema_RD_xDups_xNeg$e_npnprofitindicator)

# N Unknown       Y 
# 173390 1683542    3800 


Fema_RD_xDups_xNeg$e_contloc <-  as.factor(Fema_RD_xDups_xNeg$e_contloc)


#e_Obstruction
table(Fema_RD_xDups_xNeg$e_Obstruction)

class(Fema_RD_xDups_xNeg$locationofcontents)

Fema_RD_xDups_xNeg$e_Obstruction <- as.character(Fema_RD_xDups_xNeg$obstructiontype)

Fema_RD_xDups_xNeg$e_Obstruction <- ifelse(Fema_RD_xDups_xNeg$obstructiontype %in% c("","*","00","0","1"),"Unknown",
                                       Fema_RD_xDups_xNeg$obstructiontype)
 table(Fema_RD_xDups_xNeg$e_Obstruction)

 # 10      11      12      13      14      15      16      17      18      19      20      21      22 
 # 2964     686     470   54823   11412   32116     341    6760     133     435      11      25      10 
 # 23      24       6       7       8       9 Unknown 
 # 1       5 1020934   26421    4745     611  697829 

Fema_RD_xDups_xNeg$e_Obstruction <-  as.factor(Fema_RD_xDups_xNeg$e_Obstruction)

#postfirmconstructionindicator
table(Fema_RD_xDups_xNeg$postfirmconstructionindicator)
#          N       Y 
# 52265 1382870  425597 

class(Fema_RD_xDups_xNeg$postfirmconstructionindicator)

Fema_RD_xDups_xNeg$e_FIRMConst_Indi<- as.character(Fema_RD_xDups_xNeg$postfirmconstructionindicator)

Fema_RD_xDups_xNeg$e_FIRMConst_Indi <- ifelse(Fema_RD_xDups_xNeg$e_FIRMConst_Indi == "","Unknown",Fema_RD_xDups_xNeg$e_FIRMConst_Indi)
table(Fema_RD_xDups_xNeg$e_FIRMConst_Indi)

#   N      Unknown    Y 
# 1382870   52265  425597 

Fema_RD_xDups_xNeg$e_FIRMConst_Indi <-  as.factor(Fema_RD_xDups_xNeg$e_FIRMConst_Indi)

#ratemethod
table(Fema_RD_xDups_xNeg$ratemethod)
#           1       2       3       4       5       6       7       8       9       A       B       E 
# 48635 1468359   40020   26099    1913     137      21  229223     274     715    1176   22510      22 
# F       G       P       Q       S       T       W 
# 6   10510    2712    6801     290       2    1307 

class(Fema_RD_xDups_xNeg$ratemethod)

Fema_RD_xDups_xNeg$e_RateMethod<- as.character(Fema_RD_xDups_xNeg$ratemethod)

Fema_RD_xDups_xNeg$e_RateMethod <- ifelse(Fema_RD_xDups_xNeg$e_RateMethod == "","Unknown",Fema_RD_xDups_xNeg$e_RateMethod)
table(Fema_RD_xDups_xNeg$e_RateMethod)

#   N      Unknown    Y 
# 1382870   52265  425597 

Fema_RD_xDups_xNeg$e_RateMethod <-  as.factor(Fema_RD_xDups_xNeg$e_RateMethod)


#smallbusinessindicatorbuilding
table(Fema_RD_xDups_xNeg$smallbusinessindicatorbuilding)
#             N       Y 
# 1667201  190419    3112 

class(Fema_RD_xDups_xNeg$smallbusinessindicatorbuilding)

Fema_RD_xDups_xNeg$e_smallbusinessIndi<- as.character(Fema_RD_xDups_xNeg$smallbusinessindicatorbuilding)

Fema_RD_xDups_xNeg$e_smallbusinessIndi <- ifelse(Fema_RD_xDups_xNeg$e_smallbusinessIndi == "","Unknown",Fema_RD_xDups_xNeg$e_smallbusinessIndi)
table(Fema_RD_xDups_xNeg$e_smallbusinessIndi)

#   N      Unknown    Y 
# 1382870   52265  425597 

Fema_RD_xDups_xNeg$e_smallbusinessIndi <-  as.factor(Fema_RD_xDups_xNeg$e_smallbusinessIndi)

#primaryresidence
table(Fema_RD_xDups_xNeg$primaryresidence)
#             N       Y 
# 757687 300913 802132 

class(Fema_RD_xDups_xNeg$primaryresidence)

Fema_RD_xDups_xNeg$e_PResidence<- as.character(Fema_RD_xDups_xNeg$primaryresidence)

Fema_RD_xDups_xNeg$e_PResidence <- ifelse(Fema_RD_xDups_xNeg$e_PResidence == "","Unknown",Fema_RD_xDups_xNeg$e_PResidence)
table(Fema_RD_xDups_xNeg$e_PResidence)

#    N Unknown       Y 
#300913  757687  802132 

Fema_RD_xDups_xNeg$e_PResidence <-  as.factor(Fema_RD_xDups_xNeg$e_PResidence)



View(Fema_RD_xDups_xNeg)

#update region
Fema_RD_xDups_xNeg$e_state <- Fema_RD_xDups_xNeg$state

Fema_RD_xDups_xNeg$e_HuRegion <-  ifelse(Fema_RD_xDups_xNeg$state == HU_Regions$STATECODE,HU_Regions$Region_HU,"Unassigned")

  Fema_RD_xDups_xNeg <- merge(x = Fema_RD_xDups_xNeg, y = HU_Regions, by.x = c("e_state"), by.y = c("STATECODE"), all.x = TRUE)

  
  Fema_RD_xDups_xNeg<- Fema_RD_xDups_xNeg[,-c(59,61)]
  
  rm(Fema_RD_xDups_xNeg_HuRegion)
  sum(is.na(Fema_RD_xDups_xNeg$Region_HU))
  #304511
  Fema_RD_xDups_xNeg$Region_HU <- ifelse(is.na(Fema_RD_xDups_xNeg$Region_HU),"Unassigned",Fema_RD_xDups_xNeg$Region_HU)
  
str(HU_Regions)
#only 11NA

class(Fema_RD_xDups_xNeg$obstructiontype)

Fema_RD_xDups_xNeg$e_Obstruction <- as.character(Fema_RD_xDups_xNeg$obstructiontype)

Fema_RD_xDups_xNeg$e_Obstruction <- ifelse(Fema_RD_xDups_xNeg$e_Obstruction %in% c("0","00","1") , 0 ,
                                          ifelse(Fema_RD_xDups_xNeg$e_Obstruction %in% c("","*"),"Unknown",Fema_RD_xDups_xNeg$e_Obstruction))

Fema_RD_xDups_xNeg$e_Obstruction

Locationofcontent <- Fema_RD_xDups_xNeg %>% group_by(e_Obstruction) %>% summarise(RecordCount = n(),TotalClaim = sum(TotalClaim), TIV = sum(TotalInsuredValue))
View(Obstruction)

#Filtering and select few columns

metadata %>%
  
  filter(cit == "plus") %>%
  
  select(sample, generation, clade)

#Updating lat long
rm(x)
x <- Fema_RD_xDups_xNeg %>%filter(is.na(Fema_RD_xDups_xNeg$city.y)) %>% select(id,state,reportedcity, city.x)
View(x)
y <- merge(x,zipcode[,c(2:3, 4:5)], by.x = c("city.x","state"),by.y = c("city","state"), all.x = TRUE )
z <- y%>% group_by(id,state,city.x) %>% summarize(latitude = max(latitude),longitude=max(longitude))
##44336




#Updating the lat long

sum(is.na(Fema_RD_xDups_xNeg$e_latitude))
sum(is.na(Fema_RD_xDups_xNeg$e_longitude))
--33564
Fema_RD_xDups_xNeg$e_latitude <- ifelse(is.na(Fema_RD_xDups_xNeg$e_latitude),z$latitude[match(Fema_RD_xDups_xNeg$id, z$id)],Fema_RD_xDups_xNeg$e_latitude)
Fema_RD_xDups_xNeg$e_longitude <- ifelse(is.na(Fema_RD_xDups_xNeg$e_longitude),z$longitude[match(Fema_RD_xDups_xNeg$id, z$id)],Fema_RD_xDups_xNeg$e_longitude)
#38779-25808 updated

rm(Fema_RD_xDups_xNeg$latitude.z)
Fema_RD_xDups_xNeg


#check the number of NA
sum(is.na(Fema_RD_xDups_xNeg$latitude.y))--38779
sum(is.na(Fema_RD_xDups_xNeg$e_latitude))
--33564
##After 
sum(is.na(Fema_RD_xDups_xNeg$city.y))

sum(is.na(z$latitude))


x = transform(x, city.x = colsplit(city.x, " ", names = c('a', 'xZip')))
xa$city <-xa$city.x

xa <- Fema_RD_xDups_xNeg %>%filter(is.na(Fema_RD_xDups_xNeg$e_latitude)) %>% select(id,state,reportedcity, city.x)
za <- stringdist_inner_join(xa,zipcode, by = "city", distance_col = NULL)


#####Merging on approximate match.Some values in the city columns do not match exactly. therefor eneed to do approx match
stringdist_inner_join(Dataset1, Data2,
                      by ="Name", distance_col = NULL)



z<- x %>% 
  left_join(df2,  by = "addr") %>% 
  mutate(num = ifelse(.$num.y %in% df2$num, .$num.y, df$num)) %>% 
  select(addr, num)



# BFE Median value by differnt group zones

rm(Median_BFE)
test <- head(Fema_RD_xDups_xNeg,100)
Median_BFE <- Fema_RD_xDups_xNeg %>% group_by(Category_FloodZone) %>% summarise(Med_BFE = median(basefloodelevation,na.rm =TRUE),
                                                                                Med_LAG = median(lowestadjacentgrade,na.rm =TRUE),
                                                                                Med_LFE = median(lowestfloorelevation ,na.rm =TRUE))
View(Median_BFE)

############Replacing Basement crawlspace with na to 0#################
Fema_RD_xDups_xNeg$basementenclosurecrawlspacetype[is.na(Fema_RD_xDups_xNeg$basementenclosurecrawlspacetype)] <- 0

#####Yearofloss
min(as.Date(Fema_RD_xDups_xNeg$dateofloss))
"1973-04-04" --,  DATEDIFF(day, '1970-01-01', dateofloss) AS DateDiff
max(as.Date(Fema_RD_xDups_xNeg$dateofloss))
"2019-03-21"
###No of Days

test$NoofDays <- floor(test$NoofDays)


Fema_RD_xDups_xNeg$NoofDays <- difftime(Fema_RD_xDups_xNeg$Eventdate,min(as.Date(Fema_RD_xDups_xNeg$Eventdate)),
                                        units = "days") 
Fema_RD_xDups_xNeg$NoofDays <- floor(Fema_RD_xDups_xNeg$NoofDays)


##########################Graphs#############################################
str(ByStateZoneLeveofrisk)

ByStateZoneLeveofrisk$state <- factor(ByStateZoneLeveofrisk$state,
                                      levels = ByStateZoneLeveofrisk[order(ByStateZoneLeveofrisk$state), "State"]) )


# Loss By state and Level of Risk
ggplot(ByStateZoneLeveofrisk, aes( x = reorder(state, (TotalLoss/1000000)) , y = TotalLoss/1000000, fill = levelOfRisk))+ geom_bar(stat = "identity")+
  labs(title = "State and Level of Risk", x="State", y= "TotalLoss(Million)") +coord_flip()


ggplot(test1, aes(x = TotalInsuredValue, y = TotalClaim, size = TotalClaim))+
  geom_point(aes(color = state),alpha = 0.5) + scale_x_log10()+
  labs(title = "By State TIV and Loss", x = "Total Insured Value", y = "Total Loss",
       size = "Loss", color = "State")

ggplot(ByStateZoneLeveofrisk, aes(x = TotalInsuredValue, y = TotalLoss, size = TotalLoss))+
  geom_point(aes(color = state),alpha = 0.5) + scale_x_log10()+
  labs(title = "By State TIV and Loss", x = "Total Insured Value", y = "Total Loss",
       size = "Loss", color = "State")

#Loss By Basement and Level of Risk
Fema_RD_xDups_xNeg$basementenclosurecrawlspacetype <- as.factor(Fema_RD_xDups_xNeg$basementenclosurecrawlspacetype)
ggplot(Fema_RD_xDups_xNeg, aes(x = reorder(basementenclosurecrawlspacetype, -(TotalClaim/1000000)), y = TotalClaim/1000000, fill = levelOfRisk))+
  geom_bar(stat = "identity") +
  labs(title = "Loss By Basement Type", x = "Basement Type", y = "Total Loss(Millions)", fill = "Level of Risk")


#Loss By Condomium and Level of Risk
ggplot(Fema_RD_xDups_xNeg, aes(x = fct_infreq(condominiumindicator,ordered = TRUE), y = TotalClaim/1000000, fill = levelOfRisk))+
  geom_bar(stat = "identity")+
  labs(title = "Loss By Condominium Type", x = "Condominium Type", y = "Total Loss(Millions)", fill = "Level of Risk") 

  #Loss By Location Content and Level of Risk
Fema_RD_xDups_xNeg$locationofcontents <- as.factor(Fema_RD_xDups_xNeg$locationofcontents )

  ggplot(Fema_RD_xDups_xNeg, aes(x = fct_infreq(locationofcontents,ordered = TRUE) , y = TotalClaim/1000000, fill = levelOfRisk))+
  geom_bar(stat = "identity")+
  labs(title = "Loss By Location Content", x = "Location Content", y = "Total Loss(Millions)", fill = "Level of Risk") +
theme(axis.text.x = element_text(angle= 20, vjust = 1.0,hjust = 1.0)) 

    
    #Loss By num stories and Level of Risk
  Fema_RD_xDups_xNeg$numberoffloorsintheinsuredbuilding <- as.factor(Fema_RD_xDups_xNeg$numberoffloorsintheinsuredbuilding )
  
    ggplot(Fema_RD_xDups_xNeg, aes(x = fct_infreq(numberoffloorsintheinsuredbuilding,ordered = TRUE) , y = TotalClaim/1000000, fill = levelOfRisk))+
    geom_bar(stat = "identity")+
    labs(title = "Loss By Number of Stories", x = "Number of Stories", y = "Total Loss(Millions)", fill = "Level of Risk") 
    
    
    #Loss By obstruction Type and Level of Risk
    Fema_RD_xDups_xNeg$obstructiontype <- as.factor(Fema_RD_xDups_xNeg$obstructiontype)
    
    ggplot(Fema_RD_xDups_xNeg, aes(x = fct_infreq(obstructiontype, ordered = TRUE), y = TotalClaim/1000000, fill = levelOfRisk))+
      geom_bar(stat = "identity")+
      labs(title = "Loss By Obstruction Type", x = "Obstruction Type", y = "Total Loss(Millions)", fill = "Level of Risk") 
    
    
    #Loss By obstruction Type and Level of Risk
    Fema_RD_xDups_xNeg$occupancytype <- as.factor(Fema_RD_xDups_xNeg$occupancytype)
    
    ggplot(Fema_RD_xDups_xNeg, aes(x = fct_infreq(occupancytype, ordered = TRUE) , y = TotalClaim/1000000, fill = levelOfRisk))+
      geom_bar(stat = "identity")+
      labs(title = "Loss By Occupancy Type", x = "Occupancy Type", y = "Total Loss(Millions)", fill = "Level of Risk") 
    

 
    #Loss By Basenment Structure and Level of Risk already a factor
    #Fema_RD_xDups_xNeg$basementEnclosureCrawlspaceType <- as.factor(Fema_RD_xDups_xNeg$basementEnclosureCrawlspaceType)
    class(Fema_RD_xDups_xNeg$basementenclosurecrawlspacetype)
    ggplot(Fema_RD_xDups_xNeg, aes(x = fct_infreq(basementenclosurecrawlspacetype, ordered = TRUE) , y = TotalClaim/1000000, fill = levelOfRisk))+
      geom_bar(stat = "identity")+
      labs(title = "Loss By Basement Structure", x = "Basement Structure", y = "Total Loss(Millions)", fill = "Level of Risk") 
    
        
    
    # CRS Discount
    #scatter
    ggplot(Fema_RD_xDups_xNeg, aes(x = crsdiscount , y = TotalClaim/1000000)+ geom_point()
         + labs(title = "Loss By Basement Structure", x = "Basement Structure", y = "Total Loss(Millions)", fill = "Level of Risk") 
        
         class(Fema_RD_xDups_xNeg$crsdiscount)
        # numeric
        
        Fema_RD_xDups_xNeg$crsdiscount <-  as.factor(Fema_RD_xDups_xNeg$crsdiscount)
      #bar  
         ggplot(Fema_RD_xDups_xNeg, aes(x = fct_infreq(crsdiscount, ordered = TRUE) , y = TotalClaim/1000000, fill = levelOfRisk))+
           geom_bar(stat = "identity")+
           labs(title = "Loss By CRS Discount", x = "CRS Discount", y = "Total Loss(Millions)", fill = "Level of Risk") 
      #Primary residnce   
         ggplot(Fema_RD_xDups_xNeg, aes(x = fct_infreq(primaryresidence, ordered = TRUE) , y = TotalClaim/1000000, fill = levelOfRisk))+
           geom_bar(stat = "identity")+
           labs(title = "Loss By Primary Residence", x = "Primary Residence", y = "Total Loss(Millions)", fill = "Level of Risk") 
     #Flood Zone  
       ggplot(Fema_RD_xDups_xNeg, aes(x = fct_infreq(Category_FloodZone, ordered = TRUE) , y = TotalClaim/1000000, fill = levelOfRisk))+
       geom_bar(stat = "identity")+
       labs(title = "Loss By Flood Zone", x = "Flood Zone", y = "Total Loss(Millions)", fill = "Level of Risk") 
    #Elevation Certificate Indicator     
       ggplot(Fema_RD_xDups_xNeg, aes(x = fct_infreq(elevationcertificateindicator, ordered = TRUE) , y = TotalClaim/1000000, fill = levelOfRisk))+
         geom_bar(stat = "identity")+
         labs(title = "Loss By Elevation Certificate", x = "Elevation Certificate", y = "Total Loss(Millions)", fill = "Level of Risk") 
       
       
       # Histogram
       ggplot(Fema_RD_xDups_xNeg, aes(x = (TotalClaim/1000000), fill = levelOfRisk))+geom_histogram() +
         scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x))) +
         labs(title = "Claims by level of Risk", x = "Claim Amount", y = "Record Count", fill = "Level of Risk") 
       
       
       class(Fema_RD_xDups_xNeg$Region_HU)
       # Character to factor
       Fema_RD_xDups_xNeg$Region_HU <-  as.character(Fema_RD_xDups_xNeg$Region_HU)
       
       #HU Region     
       ggplot(Fema_RD_xDups_xNeg, aes(x = fct_infreq(Region_HU, ordered = FALSE), y = TotalClaim/1000000, fill = levelOfRisk))+
         geom_bar(stat = "identity")+
         labs(title = "Loss By Hurricane Regions", x = "Hurricane Regions", y = "Total Loss(Millions)", fill = "Level of Risk") 
      
        #HU Region     
       ggplot(Fema_RD_xDups_xNeg, aes(x = fct_infreq(levelOfRisk, ordered = FALSE), y = TotalClaim/1000000))+
         geom_bar(stat = "identity")+
         labs(title = "Loss By Level Of Risk", x = "Level Of Risk", y = "Total Loss(Millions)")
      
        ggplot(Fema_RD_xDups_xNeg, aes(x = fct_infreq(agriculturestructureindicator, ordered = FALSE), y = TotalClaim/1000000))+
         geom_bar(stat = "identity")+
         labs(title = "Loss By Level Of Risk", x = "Level Of Risk", y = "Total Loss(Millions)")
       
       
       
plot_usmap(regions = "states")

#Loss Map
plot_usmap(data = ByStateZoneLeveofrisk,values = "TotalLoss")
#TIV Map
plot_usmap(data = ByStateZoneLeveofrisk,values = "TotalInsuredValue")


###############################Final Hurricane Claim records################################################
HisHurEvent <-read_csv("AllHurricaneEvents.csv")

View(HisHurEvent)
rm(HurEvent1)


HurEvent1 <- HisHurEvent
str(HisHurEvent)

# State and Storm. are factors convert to character
HisHurEvent$StatesAffected <- as.character(HisHurEvent$StatesAffected)
HisHurEvent$Storm. <- as.character(HisHurEvent$Storm.)

HisHurEvent$EventDate <- as.POSIXct(HisHurEvent$EventDate, format ='%m/%d/%Y')
HisHurEvent$StartDate <- as.POSIXct(HisHurEvent$StartDate, format ='%m/%d/%Y')
HisHurEvent$EndDate <- as.POSIXct(HisHurEvent$EndDate, format ='%m/%d/%Y')

str(HisHurEvent)

# No of Days
min(as.Date(Fema_RD_xDups_xNeg$dateofloss))
#"1973-04-04"

HisHurEvent$NoofDays <- difftime(HisHurEvent$EventDate,min(as.Date(Fema_RD_xDups_xNeg$dateofloss)),units = "days") 

HisHurEvent$NoofDays <- floor(HisHurEvent$NoofDays)

HisHurEvent$NoofDays <- as.numeric(HisHurEvent$NoofDays)

###adding Repetitive columns and generating the duration of events
HisHurEvent$EventDuration <-  (HisHurEvent$EndDate - HisHurEvent$StartDate) + 2

HisHurEvent <- HisHurEvent[rep(rownames(HisHurEvent), HisHurEvent$EventDuration),]

HisHurEvent$DaysAffected_5  <-  HisHurEvent$NoofDays + (sequence(5)-2)
HisHurEvent$DaysAffected_7  <- HisHurEvent$NoofDays + (sequence(7)-2)

HisHurEvent$DaysAffected_5_7 <-  ifelse(HisHurEvent$EventDuration == 9,HisHurEvent$DaysAffected_7,HisHurEvent$DaysAffected_5)

#write.csv(HisHurEvent, file = "HisHurEvent")

HistHurEventDays <- HisHurEvent %>% select(Storm., Event.Date, States.Affected, NoofDays, DaysAffected_5_7),

HistHurEventDays <- HisHurEvent %>% dplyr::select(Storm., Event.Date, States.Affected, NoofDays, DaysAffected_5_7)

View(HistHurEventDays)

colnames(HistHurEventDays)[colnames(HistHurEventDays) == "NoofDays"] <- "HU_NoofDays"
 rm(HurEventDays6)
Fema_Claims_onlyHistHuEvent<- merge(Fema_RD_xDups_xNeg,HistHurEventDays, by.x = c("state","NoofDays"),by.y = c("States.Affected","DaysAffected_5_7"), all.y = TRUE)
#844824


Fema_Claims_onlyHistHuEvent1 <- read.csv("Fema_Claims_onlyHistHuEvent1.csv")
summary(Fema_Claims_onlyHistHuEvent$id)
summary(Fema_RD_xDups_xNeg$id)

summary(FEMA_Claims_nonHU$id)
Fema_Claims_onlyHistHuEventxNA <- subset(Fema_Claims_onlyHistHuEvent,!is.na(Fema_Claims_onlyHistHuEvent[,69]))
#844644
count(unique(Fema_Claims_onlyHistHuEvent$id))
View(Fema_Claims_onlyHistHuEvent)
write.csv(Fema_Claims_onlyHistHuEvent, file = "Fema_Claims_onlyHistHuEvent.csv")

HUid <- Fema_Claims_onlyHistHuEvent$id
FEMA_Claims_nonHU <- subset(Fema_RD_xDups_xNeg, !(id %in% Fema_Claims_onlyHistHuEvent$id))

FEMA_Claims_nonHU <- subset(Fema_RD_xDups_xNeg, !(id %in% HUid))
write.csv(FEMA_Claims_nonHU, file = "FEMA_Claims_nonHU.csv")

library(lubridate)

summary(Fema_Claims_onlyHistHuEvent)

####Maintain only required columns

Fema_RD_xDuplicate[ , c(28:30,35:36)][is.na(Fema_RD_xDuplicate[ ,c(28:30,35:36)])] <- 0

ModelFemaData <- Fema_Claims_onlyHistHuEvent[c(11,43,45:49,58:70,73)]
ModelFemaData <- ModelFemaData[c(-2,-(8:14))]
ModelFemaData <- ModelFemaData[c(-5,-24, -22)]
ModelFemaDatxNA <- na.omit(ModelFemaData) 
summary(ModelFemaDatxNA)
                        
str(ModelFemaDatxNA)


table(ModelFemaDatxNA$condominiumindicator)
ModelFemaDatxNA$condominiumindicator <- ifelse(ModelFemaDatxNA$condominiumindicator == "","Unknown",ModelFemaDatxNA$condominiumindicator)

ModelFemaDatxNA$condominiumindicator < - 
#Convert Factor columns to numberic
ModelFemaDatxNA$e_condo <- as.numeric(ModelFemaDatxNA$e_condo)
ModelFemaDatxNA$e_elevcerti_indi <- as.numeric(ModelFemaDatxNA$e_elevcerti_indi)
ModelFemaDatxNA$e_Obstruction <- as.numeric(ModelFemaDatxNA$e_Obstruction)
ModelFemaDatxNA$e_RateMethod <- as.numeric(ModelFemaDatxNA$e_RateMethod)
ModelFemaDatxNA$HU_NoofDays    <- as.numeric(ModelFemaDatxNA$HU_NoofDays)

str(ModelFemaDatxNA)

ModelFemaDatxNA$econdo <- as.numeric(ModelFemaDatxNA$econdo)

table(ModelFemaDatxNA$e_contloc)




###################################Create Dummy Variables/One Hot Encoding###################################################

##all columns at once
install.packages("dummies")
library(dummies)

#Find TIV 0 values and remove the rows
length(which(ModelFemaDatxNA$TotalInsuredValue == 0))

ModelFemaDatxNA$TotalInsuredValue == 0

####After runnning PCA, unknown came to be PC, therefore remove it

s(ModelFemaDatxNA1[ModelFemaDatxNA1$condominiumindicator == "Unknown"]))
    summary(ModelFemaDatxNA1)
    
    table(ModelFemaDatxNA$condominiumindicator)
    table(ModelFemaDatxNA$e_smallbusinessInd)
    
    ModelFemaDatxNA$condominiumindicator <- as.factor(ModelFemaDatxNA$condominiumindicator)
#model 1 - remove    
    ModelFemaDatxUnk  <- ModelFemaDatxNA[ModelFemaDatxNA$e_smallbusinessIndi !="Unknown",]
       ModelFemaDatxUnk  <- ModelFemaDatxUnk[ModelFemaDatxUnk$ e_worship !="Unknown",]
    summary(ModelFemaDatxUnk)
#model 2 - remove    

    ModelFemaDatxUnk2  <- ModelFemaDatxUnk[ModelFemaDatxUnk$e_Obstruction !="Unknown",]
    str(ModelFemaDatxUnk2)

#model 3 - remove    
    
    ModelFemaDatxUnk3  <- ModelFemaDatxUnk2[ModelFemaDatxUnk2$e_FIRMConst_Indi !="Unknown",]  
ModelFemaDatxNA1 <- ModelFemaDatxNA[ModelFemaDatxNA$TotalInsuredValue != 0,]
summary(ModelFemaDatxNA1)



ModelFemaDatxUnk$LossRatio <- (ModelFemaDatxUnk$TotalClaim/ModelFemaDatxUnk$TotalInsuredValue)*100
ModelFemaDatxUnk <- subset(ModelFemaDatxUnk, select = -c(TotalClaim, TotalInsuredValue))

ModelFemaDatxUnk3 <- subset(ModelFemaDatxUnk3, select = -c(LossRatio,TotalClaim, TotalInsuredValue))

View(ModelFemaDatxUnk)



str(ModelFemaDatxUnk3)
summary(ModelFemaDatxUnk2)

ModelFemaDatxUnk2 <- sapply(ModelFemaDatxUnk2, as.numeric)
 rm(ModelwDummyxUnk3)
 
 summary(ModelFemaDatxUnk3$e_FIRMConst_Indi)

ModelwDummyxUnk3 <- dummy.data.frame(ModelFemaDatxUnk3, names = c("condominiumindicator","levelOfRisk","e_Agri_Indi","e_elev_indi",
                                                          "e_elevcerti_indi","e_worship","e_contloc","e_npnprofitindicator",
                                                          "e_Obstruction","e_FIRMConst_Indi","e_RateMethod","e_smallbusinessIndi", "e_PResidence","e_state",
                                                          "Storm."))
ModelwDummyxUnk3$LossRatio <- ModelFemaDatxUnk$LossRatio

library(corrgram)

cor <- cor(ModelwDummyxUnk3, ModelwDummyxUnk3, method = "pearson")
#cor_F <- cor(ModelwDummyxUnk3, ModelwDummyxUnk3, method = "pearson")
cor_df<- cor_df%>%mutate(cor_abs = abs(cor)) %>% arrange(desc(cor_abs))
#cor_df_F<- cor_df%>%mutate(cor_abs = abs(cor_F)) %>% arrange(desc(cor_abs))

corxUnk5 <- cor(ModelwDummyxUnk5, ModelwDummyxUnk5, method = "pearson")
corxUnk5_df<- data.frame(cor=corxUnk5[1:81,82], varn = names(corxUnk5[1:81,82]))
str(corxUnk5_df)
corxUnk5_df<- corxUnk5_df%>%mutate(cor_abs = abs(cor)) %>% arrange(desc(cor_abs))

plot(corxUnk5_df$cor_abs, type="l")


list_varn_5 <- corxUnk5_df %>% filter(cor_abs>0.03)
str(list_varn_5)

list_varn_5$varn <- as.character(list_varn_5$varn)

filter_df_5 <- ModelwDummyxUnk5 %>% dplyr::select(LossRatio,one_of(list_varn_5$varn))
rm(filter_df_5)

corrgram(filter_df_xLR_1,lower.panel=panel.cor,upper.panel=panel.pie, cor.method = "pearson")
# 


cor_df<- data.frame(cor=cor[1:94,95], varn = names(cor[1:94,95])) 
install.packages("dplyr")
library(dplyr)

#cor_df<- cor_df%>%mutate(cor_abs = abs(cor)) %>% arrange(desc(cor_abs))

library(ggplot2)
plot(cor_df$cor_abs, type="l")

hist(ModelFemaDatxUnk$TotalClaim)


list_varn <- cor_df %>% filter(cor_abs>0.07)
str(list_varn)

list_varn$varn <- as.character(list_varn$varn)

filter_df <- ModelwDummyxUnk3 %>% dplyr::select(LossRatio,one_of(list_varn$varn))
rm(filter_df)
head(filter_df)

corrgram(filter_df,lower.panel=panel.cor,upper.panel=panel.pie, cor.method = "pearson")

#highly correlated  features include e_FRIMConst_indiN, e_elev_indiY, e_worshipN. e_stateTX  and few more 
#highly correlated and it would be good if we could eliminate them, without losing their prediction power. That is exactly our use case for dimensionality reduction with PCA. 
#Keep this matrix in mind, later on we will compare it with the principle components.

#PCA - is  an unsupervised learning technique, hence response variable must be removed.
#The base R function prcomp() is used to perform PCA. By default, it centers the variable to have mean equals to zero.
#With parameter scale. = T, we normalize the variables to have standard deviation equals to 1.
rm(prin_compUnk2)


#Modelprep2 <- sapply( ModelwDummyxUnk2, as.numeric)
str(ModelwDummyxUnk2)

#Method 1
prin_compUnk3 <- prcomp(filter_df, scale. = T, center = T)
summary(prin_compUnk3)
# Importance of components:
#                          PC1     PC2    PC3     PC4     PC5     PC6     PC7     PC8     PC9
# Standard deviation     2.0312 1.65747 1.5572 1.44560 1.39271 1.28303 1.22443 1.18849 1.10736
# Proportion of Variance 0.1474 0.09811 0.0866 0.07463 0.06927 0.05879 0.05354 0.05045 0.04379
# Cumulative Proportion  0.1474 0.24546 0.3321 0.40670 0.47597 0.53476 0.58831 0.63875 0.68255
#                         PC10    PC11    PC12    PC13    PC14    PC15    PC16    PC17    PC18
# Standard deviation     1.09098 1.02590 0.99854 0.95475 0.90149 0.88996 0.81951 0.76928 0.64301
# Proportion of Variance 0.04251 0.03759 0.03561 0.03256 0.02902 0.02829 0.02399 0.02114 0.01477
# Cumulative Proportion  0.72506 0.76265 0.79826 0.83081 0.85983 0.88812 0.91211 0.93324 0.94801
#                          PC19    PC20    PC21    PC22    PC23    PC24    PC25      PC26
# Standard deviation     0.62497 0.57059 0.55014 0.52519 0.40097 0.01432 0.01098 1.331e-14
# Proportion of Variance 0.01395 0.01163 0.01081 0.00985 0.00574 0.00001 0.00000 0.000e+00
# Cumulative Proportion  0.96196 0.97359 0.98440 0.99425 0.99999 1.00000 1.00000 1.000e+00
#                         PC27      PC28
# Standard deviation     5.496e-15 8.821e-16
# Proportion of Variance 0.000e+00 0.000e+00
# Cumulative Proportion  1.000e+00 1.000e+00

# first 10 components explain 72% of variance and 15 components explain 88% of varaince

names(prin_comp)
#"sdev"     "rotation" "center"   "scale"    "x"
#center and scale =  mean and standard deviation respectively,  normalization prior to implementing PCA
#Rotation = most imp factor
#x = matrix x has the principal component score vect
prin_compUnk3$center

prin_comp$rotation[1:5,1:4]
#PC! = e_FIRMConst_IndiN *0.145 + e_FIRMConst_IndiY*0.0019

#                       PC1        PC2         PC3          PC4
# LossRatio          0.1451178 -0.1737339  0.12984973 -0.093234290
# e_FIRMConst_IndiN  0.1009112 -0.4586242 -0.07659007 -0.001402254
# e_FIRMConst_IndiY -0.1009112  0.4586242  0.07659007  0.001402254
# e_stateTX          0.2348062  0.0965766  0.29999361 -0.337126180
# Storm.Harvey       0.2348062  0.0965766  0.29999361 -0.337126180

a <- prin_compUnk3$x[,1:15]
#first 15 varaibles

round(colMeans(prin_compUnk3$x),2) 
# PC1  PC2  PC3  PC4  PC5  PC6  PC7  PC8  PC9 PC10 PC11 PC12 PC13 PC14 PC15 PC16 PC17 PC18 PC19 
# 0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0 
# PC20 PC21 PC22 PC23 PC24 PC25 PC26 PC27 PC28 
# 0    0    0    0    0    0    0    0    0 

round(colMeans(a,2) )
# PC1  PC2  PC3  PC4  PC5  PC6  PC7  PC8  PC9 PC10 PC11 PC12 PC13 PC14 PC15 
# 0    0    0    0    0    0    0    0    0    0    0    0    0    0    0 

# 
# install.packages("devtools")
# 
# dat_pca <- as.data.frame(prin_compUnk3$scores)
# dat_pca$y <- y
# 
# plot(y ~ Comp.3, data = dat_pca)



# library(devtools)
# install_github("vqv/ggbiplot")
# 
# library(ggbiplot)
# 
# ggbiplot(prin_compUnk3, labels=rownames())

dim(prin_compUnk3$x)
# 824892    108
# 36048    28

plot(prin_compUnk3, main = "Variance explained by Principle Components",
     xlab = "Principle Component", ylab = "Proportion of variance explained", type="l")

spca <- summary(prin_compUnk3)
plot(spca$importance[3,], type="l")

??plot

plot(cor_df$cor_abs, xlab = "Principle Component", ylab = "Proportion of variance explained", type = "b")
biplot(prin_compUnk3, scale = 0)
??pca

#check
var <- prin_compUnk3$sdev
apply(prin_compUnk3$x,2,var) also called eigenvaleus
# PC1          PC2          PC3          PC4          PC5          PC6          PC7 
# 4.125712e+00 2.747205e+00 2.424888e+00 2.089766e+00 1.939649e+00 1.646176e+00 1.499219e+00 
# PC8          PC9         PC10         PC11         PC12         PC13         PC14 
# 1.412506e+00 1.226244e+00 1.190236e+00 1.052471e+00 9.970772e-01 9.115431e-01 8.126766e-01 
# PC15         PC16         PC17         PC18         PC19         PC20         PC21 
# 7.920336e-01 6.715907e-01 5.917930e-01 4.134611e-01 3.905919e-01 3.255751e-01 3.026590e-01 
# PC22         PC23         PC24         PC25         PC26         PC27         PC28 
# 2.758225e-01 1.607784e-01 2.051957e-04 1.204713e-04 1.810973e-28 3.385139e-29 5.273898e-31 
prin_compUnk3$sdev

var12 <- prin_compUnk3$sdev[ ,1:12]

View(var)

a <- prin_compUnk3$x[ ,1:15]
apply(a,2,var) 

rm(var)
pca_var <- var^2
pca_var[1:12]
#8.417743 4.292017 3.547960 2.968441 2.623893 2.466260 2.254125 2.034061 1.942962 1.610481
#4.125712 2.747205 2.424888 2.089766 1.939649 1.646176 1.499219 1.412506 1.226244 1.190236 with 28 features

round(cor(a),2) 
# PC1   PC2   PC3   PC4  PC5   PC6   PC7   PC8  PC9  PC10  PC11 PC12  
# PC1   1.00  0.00  0.00  0.00 0.00  0.00  0.00  0.00 0.00  0.00  0.00 
# PC2   0.00  1.00  0.00  0.00 0.00  0.00  0.00  0.00 0.00  0.00  0.00 
# PC3   0.00  0.00  1.00  0.00 0.00  0.00  0.00  0.00 0.00  0.00  0.00 
# PC4   0.00  0.00  0.00  1.00 0.00  0.00  0.00  0.00 0.00  0.00  0.00 
# PC5   0.00  0.00  0.00  0.00 1.00  0.00  0.00  0.00 0.00  0.00  0.00 
# PC6   0.00  0.00  0.00  0.00 0.00  1.00  0.00  0.00 0.00  0.00  0.00 
# PC7   0.00  0.00  0.00  0.00 0.00  0.00  1.00  0.00 0.00  0.00  0.00 
# PC8   0.00  0.00  0.00  0.00 0.00  0.00  0.00  1.00 0.00  0.00  0.00 
# PC9   0.00  0.00  0.00  0.00 0.00  0.00  0.00  0.00 1.00  0.00  0.00 
# PC10  0.00  0.00  0.00  0.00 0.00  0.00  0.00  0.00 0.00  1.00  0.00 
# PC11  0.00  0.00  0.00  0.00 0.00  0.00  0.00  0.00 0.00  0.00  1.00

write.csv(filter_df,file = "filter_df.csv")


filter_df_xLR <- filter_df[,-1]
cor(filter_df_xLR[,1:15],prin_compUnk3$x[,1:4])
#                                                                        PC1         PC2         PC3          PC4
# e_FIRMConst_IndiN                                                    0.20496936 -0.76015567 -0.11926645 -0.002027101
# e_FIRMConst_IndiY                                                   -0.20496936  0.76015567  0.11926645  0.002027101
# e_stateTX                                                            0.47693485  0.16007278  0.46715160 -0.487350325
# Storm.Harvey                                                         0.47693485  0.16007278  0.46715160 -0.487350325
# e_LAG                                                                0.28301677 -0.51245525 -0.04605130 -0.093062865
# e_stateSC                                                           -0.12393546 -0.12710899 -0.57896944  0.215066390
# e_RateMethod2                                                       -0.22817402  0.63289755  0.02664509  0.089468344
# e_elev_indiN                                                         0.78508560  0.12371204 -0.27348103  0.116415661
# e_elev_indiY                                                        -0.78507679 -0.12365696  0.27333971 -0.116392489
# Storm.Matthew                                                       -0.12888856 -0.18099748 -0.59693697  0.285383468
# e_elevcerti_indi1                                                    0.51468783 -0.11008221 -0.10204424 -0.001480512
# e_contlocManufactured (mobile) home or travel trailer on foundation -0.02790714 -0.07486201  0.13271907 -0.056103144
# e_Obstruction17                                                      0.77587354  0.22690947 -0.24398197  0.129744943
# e_BFE                                                               -0.12983358  0.01766915 -0.37609102  0.039601967
# e_Obstruction7                                                      -0.13423156 -0.03900074  0.08715199  0.172320340


cor(dat[,1:6],pca.scores[,1])

# find max variance to retain as much information as possible using these components.
prop_pca_var <- pca_var/sum(pca_var)
prop_pca_var[1:12] 
#[1] 0.07867050 0.04011230 0.03315850 0.02774244 0.02452237 0.02304916 0.02106659 0.01900992 0.01815852 0.01505122

prop_pca_var_12 <- pca_var[1:12]/sum(pca_var[1:12])
#scree plot
plot(prop_pca_var, xlab = "Principle Component", ylab = "Proportion of variance explained", type = "b")
# 20 feature the varibility

plot(prop_pca_var_12, xlab = "Principle Component", ylab = "Proportion of variance explained", type = "b")
# 20 feature the varibility

biplot(prin_compUnk3, scale = 0)

#Second Method
plot(prin_compUnk3, type="l")

# get names of the PC

colnames(prin_compUnk3$x)


prin_compUnk3$x[,1]

plot(cumsum(prop_pca_var), xlab = "Principle Component", ylab = "Proportion of variance explained", type = "b")
#15 Varaibles explains 99% of the varaiblility
screeplot(prin_comp)

#Second Method
plot(spca$importance[3,], type="l")


pca_df <- data.frame(prin_compUnk3$x)
#pca_df_2 <- pca_df %>% dplyr::select(-13) # 
pca_df_2 <- pca_df[ , -seq(ncol(pca_df)-16, ncol(pca_df))]
pca_df_1$LossRatio <- filter_df$LossRatio
pca_df_2$LossRatio <- filter_df$LossRatio

rm(pca_df_2)

View(prin_compUnk3$x)

corrgram(pca_df_2,lower.panel=panel.cor,upper.panel=panel.pie)


#########################################################################################################
#Method 3
install.packages("psych")
library(psych)

pairs.panels(traindata[,1:16],gap = 0, bg = c("red","yellow","blue")[traindata$LossRatio], pch = 21)

filter_df_xLR_1<- filter_df_xLR[, -16 : -27]
rm(filter_df_xLR_1)
p <- ncol(filter_df_xLR_1) # no of variables
R <- cor(filter_df_xLR_1)

cortest.bartlett(R,n=nrow(filter_df_xLR_1)) 
# $chisq
# [1] Inf
# 
# $p.value
# [1] 0
# 
# $df
# [1] 351,
#  105


pca_2<-principal(filter_df_xLR_1,nfactor=p,rotate="none")
pca_2
#Principal Components Analysis
# Call: principal(r = filter_df_xLR_1, nfactors = p, rotate = "none")
# Standardized loadings (pattern matrix) based upon correlation matrix
#                                                                       PC1   PC2   PC3   PC4   PC5   PC6   PC7   PC8   PC9  PC10  PC11  PC12 PC13 PC14 PC15 h2       u2 com
# e_FIRMConst_IndiN                                                    0.28  0.74 -0.43 -0.12 -0.35 -0.05  0.14  0.13  0.08  0.02  0.00  0.00 0.00    0    0  1  0.0e+00 2.8
# e_FIRMConst_IndiY                                                   -0.28 -0.74  0.43  0.12  0.35  0.05 -0.14 -0.13 -0.08 -0.02  0.00  0.00 0.00    0    0  1 -4.4e-16 2.8
# e_stateTX                                                            0.49 -0.51 -0.43  0.51 -0.10 -0.03  0.19 -0.02  0.01  0.00 -0.03  0.01 0.00    0    0  1 -2.2e-16 4.3
# Storm.Harvey                                                         0.49 -0.51 -0.43  0.51 -0.10 -0.03  0.19 -0.02  0.01  0.00 -0.03  0.01 0.00    0    0  1 -6.7e-16 4.3
# e_LAG                                                                0.30  0.41 -0.32  0.29  0.43  0.07 -0.36 -0.01 -0.23  0.40  0.05 -0.09 0.00    0    0  1 -2.2e-16 7.3
# e_stateSC                                                           -0.14  0.43  0.48  0.56 -0.03  0.05  0.23 -0.12  0.07 -0.08  0.36 -0.18 0.00    0    0  1  1.3e-15 4.8
# e_RateMethod2                                                       -0.23 -0.47  0.35 -0.06 -0.56  0.01 -0.03  0.15  0.26  0.42  0.02 -0.09 0.00    0    0  1  0.0e+00 4.8
# e_elev_indiN                                                         0.84 -0.01  0.43 -0.17  0.06 -0.03  0.09  0.09 -0.08 -0.02 -0.10 -0.18 0.01    0    0  1 -6.7e-16 1.8
# e_elev_indiY                                                        -0.84  0.01 -0.43  0.17 -0.06  0.03 -0.09 -0.09  0.08  0.02  0.10  0.18 0.01    0    0  1 -8.9e-16 1.8
# Storm.Matthew                                                       -0.14  0.50  0.50  0.36 -0.02  0.06  0.29 -0.31 -0.01  0.16 -0.32  0.18 0.00    0    0  1 -2.2e-16 5.8
# e_elevcerti_indi1                                                    0.55  0.12  0.00  0.08  0.00  0.12 -0.47 -0.34  0.57 -0.10 -0.04 -0.01 0.00    0    0  1  6.7e-16 3.9
# e_contlocManufactured (mobile) home or travel trailer on foundation -0.04 -0.03 -0.11 -0.07  0.23  0.89  0.22  0.23  0.15  0.01 -0.02  0.00 0.00    0    0  1  2.2e-16 1.6
# e_Obstruction17                                                      0.78 -0.06  0.35 -0.13  0.08 -0.02  0.03  0.11  0.01  0.09  0.26  0.40 0.00    0    0  1 -1.1e-15 2.4
# e_BFE                                                               -0.14  0.19  0.31  0.56 -0.07 -0.05 -0.34  0.61  0.02 -0.14 -0.12  0.07 0.00    0    0  1  1.0e-15 3.8
# e_Obstruction7                                                      -0.20  0.03 -0.10 -0.04  0.65 -0.42  0.30  0.22  0.45  0.09 -0.03 -0.02 0.00    0    0  1  1.1e-16 3.9
# 
#                       PC1  PC2  PC3  PC4  PC5  PC6  PC7  PC8  PC9 PC10 PC11 PC12 PC13 PC14 PC15
# SS loadings           3.22 2.50 2.08 1.47 1.27 1.01 0.87 0.79 0.70 0.42 0.35 0.31    0    0    0
# Proportion Var        0.21 0.17 0.14 0.10 0.08 0.07 0.06 0.05 0.05 0.03 0.02 0.02    0    0    0
# Cumulative Var        0.21 0.38 0.52 0.62 0.70 0.77 0.83 0.88 0.93 0.96 0.98 1.00    1    1    1
# Proportion Explained  0.21 0.17 0.14 0.10 0.08 0.07 0.06 0.05 0.05 0.03 0.02 0.02    0    0    0
# Cumulative Proportion 0.21 0.38 0.52 0.62 0.70 0.77 0.83 0.88 0.93 0.96 0.98 1.00    1    1    1
# 
# Mean item complexity =  3.8
# Test of the hypothesis that 15 components are sufficient.
# 
# The root mean square of the residuals (RMSR) is  0 
# with the empirical chi square  0  with prob <  NA 
# 
# Fit based upon off diagonal values = 1


attributes(pca_2)
[1] "values"       "rotation"     "n.obs"        "communality"  "loadings"     "fit"         
[7] "fit.off"      "fn"           "Call"         "uniquenesses" "complexity"   "chi"         
[13] "EPVAL"        "R2"           "objective"    "residual"     "rms"          "factors"     
[19] "dof"          "null.dof"     "null.model"   "criteria"     "STATISTIC"    "PVAL"        
[25] "weights"      "r.scores"     "Vaccounted"   "Structure"    "scores"      

pca_2$factors

install.packages("hornpa")
library(hornpa)
hornpa(k=15, size = 36048,reps = 500, seed = 1234)

# Component  Mean  0.95
# 1 1.034 1.042
# 2 1.027 1.033
# 3 1.022 1.026
# 4 1.017 1.021
# 5 1.013 1.016
# 6 1.008 1.011
# 7 1.004 1.006
# 8 0.999 1.003
# 9 0.995 0.998
# 10 0.992 0.994
# 11 0.988 0.991
# 12 0.983 0.987
# 13 0.978 0.982
# 14 0.973 0.977
# 15 0.966 0.971


#eigenvalues
# [1] 4.1257123 2.7472054 2.4248879 2.0897655 1.9396490 1.6461761 1.4992187 1.4125063 1.2262439 1.1902356 1.0524715 0.9970772
# [13] 0.9115431 0.8126766 0.7920336


#Only keep Principle component where eigenvalue is > than values in 95%.PC 13 to 15 have
#eigenvalues <values in 95% So keep top 12 features

pca_2_12 <- pca_2[, -13 : -15]

#check variablility for 12 variables
var12 <- prin_compUnk3$sdev[,1:12]

pca_var <- var^2
###########################################################################################################
#method 3
install.packages("factoMineR")
library(FactoMineR)

res_PCA1 <- PCA(filter_df_xLR_1, graph = FALSE)

print(res_PCA)

install.packages("factoextra")
library(factoextra)
eig.val <- get_eigenvalue(res_PCA1)
#the eigenvalues measure the amount of variation retained by each principal component. 
print(eig.val)
#       eigenvalue     variance.percent     cumulative.variance.percent
# Dim.1  3.224614e+00     2.149743e+01                    21.49743
# Dim.2  2.498347e+00     1.665564e+01                    38.15307
# Dim.3  2.084295e+00     1.389530e+01                    52.04837
# Dim.4  1.469666e+00     9.797772e+00                    61.84614
# Dim.5  1.268523e+00     8.456819e+00                    70.30296
# Dim.6  1.014801e+00     6.765339e+00                    77.06830
# Dim.7  8.719038e-01     5.812692e+00                    82.88099
# Dim.8  7.895348e-01     5.263565e+00                    88.14456
# Dim.9  6.999188e-01     4.666125e+00                    92.81068
# Dim.10 4.209867e-01     2.806578e+00                    95.61726
# Dim.11 3.473777e-01     2.315851e+00                    97.93311
# Dim.12 3.098282e-01     2.065521e+00                    99.99863
# Dim.13 2.052392e-04     1.368261e-03                   100.00000
# Dim.14 1.836415e-25     1.224277e-24                   100.00000
# Dim.15 4.004567e-26     2.669711e-25                   100.00000



fviz_eig(res_PCA, addlabels = TRUE, ylim = c(0, 50))

var_p1 <- get_pca_var(res_PCA1)
head(var_p1$cos2, 10)
#                     Dim.1        Dim.2     Dim.3       Dim.4        Dim.5
# e_FIRMConst_IndiN 0.08115654 5.438820e-01 0.1882462 0.014522200 0.1244990675
# e_FIRMConst_IndiY 0.08115654 5.438820e-01 0.1882462 0.014522200 0.1244990675
# e_stateTX         0.24421441 2.614883e-01 0.1890559 0.256017183 0.0101504717
# Storm.Harvey      0.24421441 2.614883e-01 0.1890559 0.256017183 0.0101504717
# e_LAG             0.09202103 1.700750e-01 0.1045467 0.084396839 0.1885626721
# e_stateSC         0.01909835 1.880858e-01 0.2310290 0.313661621 0.0011811710
# e_RateMethod2     0.05267522 2.250683e-01 0.1198064 0.003337959 0.3185190916
# e_elev_indiN      0.71291892 4.386218e-05 0.1868841 0.027833273 0.0039894115
# e_elev_indiY      0.71298981 4.394182e-05 0.1867150 0.027801191 0.0039801614
# Storm.Matthew     0.01902632 2.496940e-01 0.2546350 0.132744299 0.0002479061


fviz_pca_var(res_PCA1, col.var_p1 = "black")
# Bifurcates the corelations of variables into positiv and negative
#The plot above is also known as variable correlation plots. It shows the relationships between all variables. It can be interpreted as follow:

#Positively correlated variables are grouped together.
#Negatively correlated variables are positioned on opposite sides of the plot origin (opposed quadrants).
#The distance between variables and the origin measures the quality of the variables on the factor map. Variables that are away from the origin are well represented on the factor map.

install.packages("corrplot")
library(corrplot)
corrplot(var_p1$cos2,is.corr = FALSE)

fviz_cos2(res_PCA1, choice = "var", axes = 1:2)

# Color by cos2 values: quality on the factor map
png(file = "Imp of var with corr model4.png")

# 
fviz_pca_var(res_PCA1, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE )

#The larger the value of the contribution, the more the variable contributes to the component.

corrplot(var_p$contrib, is.corr=FALSE) 


png(file = "contri to dim1 dim2 Unk4.png")
fviz_contrib(res_PCA1, choice = "var", axes = 1:2, top = 10)

fviz_contrib(res_PCA1, choice = "var", axes = 1, top = 10)

fviz_contrib(res_PCA1, choice = "var", axes = 2, top = 10)


dev.off()

##remove e_Obstruction = unknown--GO TO DUMMY
##remove e_FIRMConst_Indi = unknown--GO TO DUMMY

View(ModelDatawPCA)

# 80 features  explains 98%  variability
ModelFemaDatxFinal80 <- ModelFemaDatxUnk[ModelFemaDatxUnk$e_Obstruction !="Unknown",]
ModelFemaDatxFinal80 <- ModelFemaDatxFinal80[ModelFemaDatxFinal80$e_FIRMConst_Indi !="Unknown",]
ModelDatawPCA <- data.frame(LossRatio = ModelFemaDatxFinal80$LossRatio, prin_compUnk3$x)

###############################################################

# #model 4 - remove    
# #Frim indicator - whether the construction or improvement was made after December 31, 1974, 
# ModelFemaDatxUnk4  <- ModelFemaDatxUnk[ModelFemaDatxUnk$e_FIRMConst_Indi == "Y",]  
# # elevated buidling is  a building with no basement
# ModelFemaDatxUnk5  <- ModelFemaDatxUnk4[ModelFemaDatxUnk4$e_elev_indi == "Y",]
# 
# 
# summary(ModelFemaDatxUnk5$levelOfRisk)
# rm(ModelFemaDatxUnk5)
# 
# summary(ModelFemaDatxUnk5$e_FIRMConst_Indi)
# 
# ModelDataxUnk5_1 <- subset(ModelFemaDatxUnk5, select = -c(LossRatio,TotalClaim, TotalInsuredValue))
# ModelwDummyxUnk5 <- dummy.data.frame(ModelDataxUnk5_1, names = c("condominiumindicator","levelOfRisk","e_Agri_Indi","e_elev_indi",
#                                                                   "e_elevcerti_indi","e_worship","e_contloc","e_npnprofitindicator",
#                                                                   "e_Obstruction","e_FIRMConst_Indi","e_RateMethod","e_smallbusinessIndi", "e_PResidence","e_state",
#                                                                   "Storm."))
# ModelwDummyxUnk5$LossRatio <- ModelFemaDatxUnk5$LossRatio
# 
# corxUnk5 <- cor(ModelwDummyxUnk5, ModelwDummyxUnk5, method = "pearson")
# corxUnk5_df<- data.frame(cor=corxUnk5[1:81,82], varn = names(corxUnk5[1:81,82])) 
# str(corxUnk5_df)
# corxUnk5_df<- corxUnk5_df%>%mutate(cor_abs = abs(corxUnk5_df)) %>% arrange(desc(cor_abs))
# 
# rm(corxUnk5_df)
# 
# rm(ModelwDummyxUnk5)
# 
# library(corrgram)
# 
# rm(corxUnk5)
# 
# corxUnk5 <- cor(ModelwDummyxUnk5, ModelwDummyxUnk5, method = "pearson")
# str(cor)
# 
# length(abs(cor))
# cor_df1<- cor_df%>%mutate(cor_abs = abs(cor)) %>% arrange(desc(cor_abs))
# 
# corxUnk5_df<- data.frame(cor=corxUnk5[1:81,82], varn = names(corxUnk5[1:81,82])) 
# 
# install.packages("dplyr")
# library(dplyr)
# 
# str(corxUnk5_df)
# length(corxUnk5_df)
# 
# library(dplyr)
# 
# str(cor_df)
# rm(cor_unk5_df)
# cor_unk5_df<- corxUnk5_df %>% mutate(cor_abs1 = abs(corxUnk5)) %>% arrange(desc(cor_abs1))
# # 
# # 'data.frame':	81 obs. of  2 variables:
# #   $ cor : num  -0.06493 -0.05109 0.07505 0.00172 -0.1298 ...
# # $ varn: Factor w/ 81 levels "condominiumindicator3",..: 1 2 3 4 8 25 26 71 72 73 ...
# 
# library(ggplot2)
# plot(cor_unk5_df$cor_abs, type="l")
# 
# hist(ModelFemaDatxUnk$TotalClaim)
# 
# rm(list_varn_5)
# 
# # list_varn_5 <- cor_unk5_df %>% filter(cor_abs>0.7)
# # str(cor_unk5_df)
# 
# # 'data.frame':	1 obs. of  3 variables:
# #   $ cor    : num 0.987
# # $ varn   : Factor w/ 80 levels "condominiumindicator3",..: 65
# # $ cor_abs: num 0.987
# 
# cor_unk5_df$varn <- as.character(cor_unk5_df$varn)
# 
# max(cor_unk5_df$cor_abs)
# #0.16
# 
# View(ModelwDummyxUnk5)
# 
# filter_df_5 <- ModelwDummyxUnk5 %>% dplyr::select(LossRatio,one_of(cor_unk5_df$varn))
# rm(filter_df_5)
# head(filter_df)
# 
# corrgram(filter_df_5,lower.panel=panel.cor,upper.panel=panel.pie, cor.method = "pearson")
# 
# list_varn <- cor_df %>% filter(cor_abs>0.07)
# str(list_varn)
# 
# list_varn$varn <- as.character(list_varn$varn)
# 
# filter_df <- ModelwDummyxUnk3 %>% dplyr::select(LossRatio,one_of(list_varn$varn))
# rm(filter_df)
# head(filter_df)


