#BACKWARD SELECTION
View(data)
data$newcount<-round((data$Tot_Fatal_15/data$Pop_2015)*100000)
data$VehicleMilesTrav1000<-data$VehicleMilesTrav/1000
data$IncomePerCapita1000<-data$IncomePerCapita/1000
data$Cell_AllBan <- ifelse(data$Cell_HHBFactor==1 & data$Cell_TBFactor==1, 1, 0)
data$Cell_HHBFactor <- ifelse(data$Cell_HandheldBan==1,1,0)
data$Cell_TBFactor <- ifelse(data$Cell_TextingBan==1,1,0)
data$Cell_EnforceAllUseAllDrivers<-ifelse(data$Cell_EnforcePrimary==3, 1, 0)


#Controling Variables
model <- glm(newcount ~ PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + AvgTemp + Snow_LowHighInd, family=quasipoisson(), data=data)
summary(model)

model <- glm(newcount ~ PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd, family=quasipoisson(), data=data)
summary(model)

#Remove AvgTemp
model.A <- glm(newcount ~ PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + Snow_LowHighInd, family=quasipoisson(), data=data)
summary(model.A)

#Remove AvgTemp & VMT1000 
model.B <- glm(newcount ~ PopGrowth + Median_UR_2015 + IncomePerCapita1000 + Snow_LowHighInd, family=quasipoisson(), data=data)
summary(model.B)

#Remove AvgTemp & VMT1000 & PopGrowth
model.C <- glm(newcount ~  Median_UR_2015 + IncomePerCapita1000 + Snow_LowHighInd, family=quasipoisson(), data=data)
summary(model.C)


#*******************************************************************
#*******************************************************************
#***************
#Cell Phones
#***************
data$Cell_HHBFactor <- ifelse(data$Cell_HandheldBan==1,1,0)
data$Cell_TBFactor <- ifelse(data$Cell_TextingBan==1,1,0)
data$Cell_EnforceAllUseAllDrivers<-ifelse(data$Cell_EnforcePrimary==3, 1, 0)

mod_Cella <- glm(newcount ~  Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + Snow_LowHighInd + Cell_HandheldBan + Cell_YoungAllBan + Cell_YoungAllBanAge + Cell_TextingBan + Cell_EnforcePrimary, family=quasipoisson(), data=data)
summary(mod_Cella)

mod_Cellb <- glm(newcount ~  Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + Snow_LowHighInd + Cell_HHBFactor + Cell_YoungAllBan + Cell_YoungAllBanAge + Cell_TBFactor + Cell_EnforceAllUseAllDrivers, family=quasipoisson(), data=data)
summary(mod_Cellb)

(mod_Cella$deviance - mod_Cellb$deviance)
mod_Cella$df 
mod_Cellb$df
pchisq(1.74,0,lower.tail=FALSE)

#Remove Young Age Ban
mod_Cell <- glm(newcount ~  Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + Snow_LowHighInd + Cell_HHBFactor + Cell_YoungAllBan + Cell_TBFactor + Cell_EnforceAllUseAllDrivers, family=quasipoisson(), data=data)
summary(mod_Cell)

#Remove Enforce All Use All Drivers
mod_Cell1 <- glm(newcount ~ YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + Snow_LowHighInd + Cell_HHBFactor + Cell_YoungAllBan + Cell_TBFactor, family=quasipoisson(), data=data)
summary(mod_Cell1)

#Remove Enforce TB Factor
mod_Cell2 <- glm(newcount ~ YoungPopPercent +  Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + Snow_LowHighInd + Cell_HHBFactor + Cell_YoungAllBan, family=quasipoisson(), data=data)
summary(mod_Cell2)
#chisq.test(mod_Cell1$deviance- mod_Cell2$deviance, mod_Cell1$df-mod_Cell2$df)
?logLik
logLik(mod_Cell1$deviance - mod_Cell2$deviance)
pchisq(-.139,1,lower.tail = FALSE) #1
#We see that there is not a significant difference between having TB_Factor in our model vs not.
#Therefore we remove it.

#Recall that Cell_AllBan & Cell_YoungAllBan are dicotomous
#We see that by controlling for states with similar Young Population, Median UR, VMT, Income Per Capita, & Amounts of Snow there is a significant decrease in the number of fatal accidents that
#occur in states with all bans within the young population (under 18) (both texting and cell phone) compared to states that have neither or both)
#Although not significant we see that there is also a nearly significant decrease in the amount of accidents within states that ban all cellphone use for the 
#adult and young population.  The reason that this is not significant is because the young population is included in this. We can see that if we removed young from 
#the model that the Cell_AllBan would be significant

#Code Both Bans 
data$Cell_AllBan <- ifelse(data$Cell_HHBFactor==1 & data$Cell_TBFactor==1, 1, 0)

#Evaluate Cell_AllBan added to the model
mod_Cell1 <- glm(newcount ~ YoungPopPercent +  Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + Snow_LowHighInd + Cell_AllBan + Cell_YoungAllBan, family=quasipoisson(), data=data)
summary(mod_Cell1)

mod_Cell2 <- glm(newcount ~ YoungPopPercent +  Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + Snow_LowHighInd + Cell_AllBan, family=quasipoisson(), data=data)
summary(mod_Cell2)

(41.988 - 45.404)
pchisq(3.416,1,lower.tail = FALSE)
#Shows theat there is a significant difference between the two models and threfore the variable Cell_YoungAllBan should not be removed

#EXAMPLE
#pchisq(mod$deviance, df=mod$df.residual, lower.tail=FALSE)
#[1] 0.00733294
#*******************************************************************
#*******************************************************************
#***************
#Seatbelts
#***************
#SB_Primary
#Factor Primary SB which applies to all people and not children only
data$SB_Primary_Factor<-ifelse(data$SB_Primary==1, 1, 0)
#SB_MaxFineFirstOff
#All Fines that are over $50
data$SB_MaxFineOver50 <- ifelse(data$SB_MaxFineFirstOff>=50,1,0)
#SB_SeatCoveredLaw
#Factor for all seats not just front seats
data$SB_SeatLawFactor<-ifelse(data$SB_SeatCoveredLaw==1,1,0)


mod_SB <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + AvgTemp + Snow_LowHighInd +
               SB_Primary + SB_AgeCoveredLaw + SB_SeatCoveredLaw + SB_MaxFineFirstOff, family=quasipoisson(), data=data)
summary(mod_SB)
#Notice here we are controlling for all of the variables.
#We see that the primary enforcement law is significant
#Less fatalities for states that have a primary law compared to states that do not-- when all other variables are controlled for

#Remove SB_MaxFineFirstOff
library(splines)
mod_SB1 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
               SB_Primary + SB_AgeCoveredLaw + SB_SeatCoveredLaw, family=quasipoisson(), data=data)
summary(mod_SB1)
(39.071-38.935)
pchisq(0.136,1,lower.tail = FALSE) #0.7122904
#No difference it can be removed

library(splines)
mod_SB1 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
                 SB_Primary + SB_AgeCoveredLaw + SB_SeatCoveredLaw, family=quasipoisson(), data=data)
summary(mod_SB1)

#Remove Age
mod_SB2 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + AvgTemp + Snow_LowHighInd +
                 SB_Primary + SB_SeatCoveredLaw, family=quasipoisson(), data=data)
summary(mod_SB2)

#Remove AvgTemp
mod_SB3 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + Snow_LowHighInd +
                 factor(SB_Primary) + factor(SB_SeatCoveredLaw), family=quasipoisson(), data=data)
summary(mod_SB3)
#Notice that the primary seatbelt level 2 is contained within 1

#Here we see that when using the original seatbelt laws that the primary enfore law is significant and 
#contributes to a lower amount of fatalities in states that have this law


mod_SB4 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + Snow_LowHighInd +
                 factor(SB_Primary) , family=quasipoisson(), data=data)
summary(mod_SB4)
#********

#TRY CODED VARIABLES
mod_SBa <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + AvgTemp + Snow_LowHighInd +
                 SB_Primary_Factor + SB_AgeCoveredLaw + SB_SeatLawFactor + SB_MaxFineOver50, family=quasipoisson(), data=data)
summary(mod_SBa)

#Look at a natural spline with Temp
mod_SBb <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                SB_Primary_Factor + SB_AgeCoveredLaw + SB_SeatLawFactor + SB_MaxFineOver50, family=quasipoisson(), data=data)
summary(mod_SBb)

#Look at SB_MaxFineOver50
mod_SBc <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 SB_Primary_Factor + SB_AgeCoveredLaw + SB_SeatLawFactor, family=quasipoisson(), data=data)
summary(mod_SBc)

#Remove Age Law is Covered
mod_SBc <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 SB_Primary_Factor + SB_AgeCoveredLaw + SB_SeatLawFactor, family=quasipoisson(), data=data)
summary(mod_SBc)

#Remove SB_AgeCoveredLaw
mod_SBd <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 SB_Primary_Factor + SB_SeatLawFactor, family=quasipoisson(), data=data)
summary(mod_SBd)

data$SB_Primary_Factor<-ifelse(data$SB_Primary==1, 1, 0)
#SB_MaxFineFirstOff
#All Fines that are over $50
data$SB_MaxFineOver50 <- ifelse(data$SB_MaxFineFirstOff>=50,1,0)
#SB_SeatCoveredLaw
#Factor for all seats not just front seats
data$SB_SeatLawFactor<-ifelse(data$SB_SeatCoveredLaw==1,1,0)


#*******************************************************************
#*******************************************************************
#***************
#ChildRestraints
#***************
#ChildRestraint_MaxFine
#Factor all fines greater than $70
data$CR_MaxFineFactor<-ifelse(data$ChildRestraint_Max>70,1,0)

mod_CR1 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 CR_MaxFineFactor + factor(ChildGenRestraint) + ChildRestraint_RF, family=quasipoisson, data=data)
summary(mod_CR1)
#Note: 5 is the youngest--- and 8 is all children under 8

mod_CR2 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 CR_MaxFineFactor, family=quasipoisson, data=data)
summary(mod_CR2)
#No significant variables here
#Lets hope this is bceause all parents properly protect their children

mod_CR3 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                  ChildGenRestraint + ChildRestraint_RF, family=quasipoisson, data=data)
summary(mod_CR3)

#*******************************************************************
#***************
#Young Driver---- LP
#***************
mod_LP1 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                LP_Age + LP_Length + LP_SupDriveTime + LP_ReqNightDrive , family=quasipoisson, data=data)
summary(mod_LP1)

#Look at Ave Temp NS
mod_LP2 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
                 LP_Age + LP_Length + LP_SupDriveTime + LP_ReqNightDrive , family=quasipoisson, data=data)
summary(mod_LP2)


mod_LP3 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 LP_Age + LP_Length + LP_SupDriveTime , family=quasipoisson, data=data)
summary(mod_LP3)

#Remove Length
mod_LP4 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 LP_Age + LP_SupDriveTime , family=quasipoisson, data=data)
summary(mod_LP4)


#Remove AvgTemp
mod_LP4 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + Snow_LowHighInd +
                 LP_Age + LP_SupDriveTime , family=quasipoisson, data=data)
summary(mod_LP4)

#LP Age is significant-----


#*******************************************************************
#***************
#Young Driver ------RL
#***************

mod_RL1 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
                 RL_MinAge + RL_PassengerRestrict + RL_NightRestrict + RL_PassRestLength + RL_NightRestLength, family=quasipoisson, data=data)
summary(mod_RL1)

#Remove Temp
mod_RL2 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + Snow_LowHighInd +
                 RL_MinAge + RL_PassengerRestrict + RL_NightRestrict + RL_PassRestLength + RL_NightRestLength, family=quasipoisson, data=data)
summary(mod_RL2)

#Remove Passenger Rest Length 
mod_RL3 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + Snow_LowHighInd +
                 RL_MinAge + RL_PassengerRestrict + RL_NightRestrict + RL_NightRestLength, family=quasipoisson, data=data)
summary(mod_RL3)

#Remove Night Rest Length 
mod_RL3 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + Snow_LowHighInd +
                 RL_MinAge + RL_PassengerRestrict + RL_NightRestrict, family=quasipoisson, data=data)
summary(mod_RL3)

#None of these results are significant.
#All VAlue except Night Restrict are negative which is what we would expect. 
#*******************************************************************
#*******************************************************************
#***************
#Older Driver
#***************
#Older
#Factor those with renewal every 4 years or less and proof of vision each time
data$OlderRegRenewPfVision<-ifelse(data$LicenseRenewCycleOlder<=4 & data$PfVisionRenewOlder==1,1,0)

mod_Old1 <- glm(newcount ~ YoungPopPercent + OldPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
                 + Definition_Older + OlderRegRenewPfVision, family=quasipoisson, data=data)
summary(mod_Old1)

#Remove the Younng
mod_Old2 <- glm(newcount ~OldPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
+ Definition_Older + OlderRegRenewPfVision, family=quasipoisson, data=data)
summary(mod_Old2)

mod_Old3 <- glm(newcount ~OldPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
                  + Definition_Older , family=quasipoisson, data=data)
summary(mod_Old3)

#Nothing is significant here

#*******************************************************************
#*******************************************************************
#***************
#Speed Limits & Agressive Driving
#***************

mod_Speed1 <- glm(newcount ~ YoungPopPercent + OldPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
                  + MaxSL_Rural + MaxSL_Urban + MaxSL_AccessRd + MaxSL_OtherRd + Speed_MaxFine,
                  Speed_JailAlternative, family=quasipoisson, data=data)
summary(mod_Speed1)
#For every increase in speed on an urban road there is an increase in accidents----
#Congestion?
#Access Rd for every mph decrease in speed limit there is a decrease in fatalties.
#Also for every $1 increase of a max fine there is a decrease in fatalties.

mod_Speed2 <- glm(newcount ~ YoungPopPercent + OldPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
                     MaxSL_Urban + MaxSL_AccessRd + MaxSL_OtherRd + Speed_MaxFine,
                  Speed_JailAlternative, family=quasipoisson, data=data)
summary(mod_Speed2)

#*******************************************************************
#*******************************************************************
#***************
#Helmet Use
#***************

#Helmet Law
#Factor to look at motorcycle law & for all drivers
data$MotorCycAllDrivers<-ifelse(data$MtrcycleHelmetAge==99 &data$MtrcycleHelmetLaw==1,1,0)

#Overal Strict Helmet Law
#Factor to see if state has bike helmet law and motorcycle helmet law (MC for all riders)
data$StrictHelmetLaws<-ifelse(data$MtrcycleHelmetAge==99 & data$BikeHelmetLaw==1,1,0)

#ALL
mod_Helmet1 <- glm(newcount ~ YoungPopPercent + OldPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
                      BikeHelmetLaw + BikeHelmetAge + MtrcycleHelmetLaw + MtrcycleHelmetAge, family=quasipoisson, data=data)
summary(mod_Helmet1)

#Take out Mtrcycle Age
mod_Helmet2 <- glm(newcount ~ YoungPopPercent + OldPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
                     BikeHelmetLaw + BikeHelmetAge + MtrcycleHelmetLaw , family=quasipoisson, data=data)
summary(mod_Helmet2)

#Take out Bike Age
mod_Helmet3 <- glm(newcount ~ YoungPopPercent + OldPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
                     BikeHelmetLaw  + MtrcycleHelmetLaw , family=quasipoisson, data=data)
summary(mod_Helmet3)

#Take out Old Pop
mod_Helmet4 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                     BikeHelmetLaw  + MtrcycleHelmetLaw , family=quasipoisson, data=data)
summary(mod_Helmet4)

#Look at coded variables
mod_Helmet5 <- glm(newcount ~ YoungPopPercent + OldPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
                     StrictHelmetLaws + BikeHelmetAge +  MotorCycAllDrivers + MtrcycleHelmetAge, family=quasipoisson, data=data)
summary(mod_Helmet5)

#Take out Pop Growth
mod_Helmet6 <- glm(newcount ~ YoungPopPercent + OldPopPercent  + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
                     StrictHelmetLaws + BikeHelmetAge +  MotorCycAllDrivers + MtrcycleHelmetAge, family=quasipoisson, data=data)
summary(mod_Helmet6)

#Removing Motorcycle Age
mod_Helmet7 <- glm(newcount ~ YoungPopPercent + OldPopPercent  + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
                     StrictHelmetLaws + BikeHelmetAge +  MotorCycAllDrivers, family=quasipoisson, data=data)
summary(mod_Helmet7)
#We see that Bike Age looks significant!
#*******************************************************************
#*******************************************************************
#***************
#DUI
#***************

mod_DUI1<- glm(newcount ~ YoungPopPercent + OldPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
                     DUI_BACMaxUnder21 + DUI_LicenseSuspFO + DUI_RestorePrivDSusp +
                     DUI_IgnitionIL_FO + DUI_Checkpoints + MarijuanaLegal, family=quasipoisson, data=data)
summary(mod_DUI1)


#Old & PopGrowth Remove
mod_DUI2<- glm(newcount ~ YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 DUI_BACMaxUnder21 + DUI_LicenseSuspFO + DUI_RestorePrivDSusp +
                 DUI_IgnitionIL_FO + DUI_Checkpoints + MarijuanaLegal, family=quasipoisson, data=data)
summary(mod_DUI2)

#Checkpoints Removed
mod_DUI2<- glm(newcount ~ YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 DUI_BACMaxUnder21 + DUI_LicenseSuspFO + DUI_RestorePrivDSusp +
                 DUI_IgnitionIL_FO + MarijuanaLegal, family=quasipoisson, data=data)
summary(mod_DUI2)

#Code
data$NoTolerance <- ifelse(data$DUI_BACMaxUnder21==0,1,0 )


#Recode
mod_DUI3<- glm(newcount ~ YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 NoTolerance + DUI_LicenseSuspFO + DUI_RestorePrivDSusp +
                 DUI_IgnitionIL_FO + MarijuanaLegal, family=quasipoisson, data=data)
summary(mod_DUI3)

#Remove DUI_IgnitionIL_FO
mod_DUI4<- glm(newcount ~ YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 NoTolerance + DUI_LicenseSuspFO + DUI_RestorePrivDSusp +
                 MarijuanaLegal, family=quasipoisson, data=data)
summary(mod_DUI4)


data$NoRest <- ifelse(data$DUI_RestorePrivDSusp==0,1,0)

#Model
mod_DUI5<- glm(newcount ~ YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 NoTolerance + DUI_LicenseSuspFO + NoRest +
                 MarijuanaLegal, family=quasipoisson, data=data)
summary(mod_DUI5)

mod_DUI5<- glm(newcount ~ YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 NoTolerance + NoRest +
                 MarijuanaLegal, family=quasipoisson, data=data)
summary(mod_DUI5)


mod_DUI6<- glm(newcount ~ YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 NoTolerance + factor(MarijuanaLegal), family=quasipoisson, data=data)
summary(mod_DUI6)

#Only Marijuana Is Illegal Here 
data$MarjMedicalUse<- ifelse(data$MarijuanaLegal==1,1,0)


mod_DUI7<- glm(newcount ~ YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 NoTolerance + MarjMedicalUse, family=quasipoisson, data=data)
summary(mod_DUI7)

#*******************************************************************
#*******************************************************************
#***************
#Cameras
#***************
mod_Camera1<- glm(newcount ~ YoungPopPercent + OldPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 NoCameras + RL_Speed_Cameras + Speed_MaxFine + Aggressive, family=quasipoisson, data=data)
summary(mod_Camera1)

#Remove No Cameras 
mod_Camera2<- glm(newcount ~ YoungPopPercent + OldPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                     RL_Speed_Cameras + Speed_MaxFine + Aggressive, family=quasipoisson, data=data)
summary(mod_Camera2)

#Remove Max Fine
mod_Camera2<- glm(newcount ~ YoungPopPercent + OldPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                    RL_Speed_Cameras + Aggressive, family=quasipoisson, data=data)
summary(mod_Camera2)

#Nothing is Significant!!!! 


#*******************************************************************************************************
#*******************************************************************************************************
#*******************************************************************************************************
#Now lets consier models that look at all of the types of Laws Together


#First Lets Look At All of the Relevant Ones 

#2 significant
mod_ALL <- glm(newcount ~ YoungPopPercent + OldPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                              MarjMedicalUse + BikeHelmetAge + Speed_MaxFine + MaxSL_AccessRd + MaxSL_Urban + LP_Age +
                                SB_Primary_Factor + Cell_YoungAllBan, family=quasipoisson, data=data)
summary(mod_ALL)

#Reduce DF and remove OLD
mod_ALL <- glm(newcount ~ YoungPopPercent  + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
                 MarjMedicalUse + BikeHelmetAge + Speed_MaxFine + MaxSL_AccessRd + MaxSL_Urban + LP_Age +
                 SB_Primary_Factor + Cell_YoungAllBan, family=quasipoisson, data=data)
summary(mod_ALL)

#Remove Speed_MaxFine
mod_ALL <- glm(newcount ~ YoungPopPercent  + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
                 MarjMedicalUse + BikeHelmetAge  + MaxSL_AccessRd + MaxSL_Urban + LP_Age +
                 SB_Primary_Factor + Cell_YoungAllBan, family=quasipoisson, data=data)
summary(mod_ALL)

#Access Road 
mod_ALL <- glm(newcount ~ YoungPopPercent  + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
                 MarjMedicalUse + BikeHelmetAge  + MaxSL_Urban + LP_Age +
                 SB_Primary_Factor + Cell_YoungAllBan, family=quasipoisson, data=data)
summary(mod_ALL)


#Primary & Median
mod_ALL <- glm(newcount ~ YoungPopPercent  + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
                 MarjMedicalUse + BikeHelmetAge  + MaxSL_Urban + LP_Age +
                 Cell_YoungAllBan, family=quasipoisson, data=data)
summary(mod_ALL)


#Remove Avf
mod_ALL <- glm(newcount ~ YoungPopPercent  + VehicleMilesTrav1000 + IncomePerCapita1000 + Snow_LowHighInd +
                 MarjMedicalUse + BikeHelmetAge  + MaxSL_Urban + LP_Age +
                 Cell_YoungAllBan, family=quasipoisson, data=data)
summary(mod_ALL)


#*******************************************************************************************************
#*******************************************************************************************************
#*******************************************************************************************************

mod_NEW <- glm(newcount ~ YoungPopPercent + OldPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + 
                 ns(AvgTemp,3) + Snow_LowHighInd + MarjMedicalUse + NoTolerance + Cell_AllBan + StrictHelmetLaws + MotorCycAllDrivers + 
                 SB_Primary_Factor + BikeHelmetAge + MaxSL_Urban + MaxSL_AccessRd + LP_Age, family=quasipoisson, data=data)
summary(mod_NEW)

mod_NEW2 <- glm(newcount ~ YoungPopPercent + OldPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + 
                 ns(AvgTemp,3) + Snow_LowHighInd + MarjMedicalUse + NoTolerance + Cell_AllBan + StrictHelmetLaws + MotorCycAllDrivers + 
                 SB_Primary_Factor + BikeHelmetAge + MaxSL_Urban + MaxSL_AccessRd, family=quasipoisson, data=data)
summary(mod_NEW2)

mod_NEW3 <- glm(newcount ~ YoungPopPercent + OldPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + 
                  ns(AvgTemp,3) + Snow_LowHighInd + MarjMedicalUse + NoTolerance  + StrictHelmetLaws + MotorCycAllDrivers + 
                  SB_Primary_Factor + BikeHelmetAge + MaxSL_Urban + MaxSL_AccessRd, family=quasipoisson, data=data)
summary(mod_NEW3)


mod_NEW4 <- glm(newcount ~ YoungPopPercent + OldPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + 
                  ns(AvgTemp,3) + Snow_LowHighInd + MarjMedicalUse + StrictHelmetLaws + MotorCycAllDrivers + 
                  SB_Primary_Factor + BikeHelmetAge + MaxSL_Urban + MaxSL_AccessRd, family=quasipoisson, data=data)
summary(mod_NEW4)


mod_NEW5 <- glm(newcount ~ YoungPopPercent + OldPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + 
                  ns(AvgTemp,3) + Snow_LowHighInd + MarjMedicalUse + StrictHelmetLaws + MotorCycAllDrivers + 
                  SB_Primary_Factor + BikeHelmetAge + MaxSL_Urban , family=quasipoisson, data=data)
summary(mod_NEW5)


mod_NEW6 <- glm(newcount ~ YoungPopPercent + OldPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + 
                  ns(AvgTemp,3) + Snow_LowHighInd + MarjMedicalUse + StrictHelmetLaws + MotorCycAllDrivers + 
                  SB_Primary_Factor + BikeHelmetAge + MaxSL_Urban , family=quasipoisson, data=data)
summary(mod_NEW6)


mod_NEW7 <- glm(newcount ~ YoungPopPercent + OldPopPercent + VehicleMilesTrav1000 + IncomePerCapita1000 + 
                  ns(AvgTemp,3) + Snow_LowHighInd + MarjMedicalUse + StrictHelmetLaws + MotorCycAllDrivers + 
                  SB_Primary_Factor + BikeHelmetAge + MaxSL_Urban , family=quasipoisson, data=data)
summary(mod_NEW7)


mod_NEW8 <- glm(newcount ~ YoungPopPercent + OldPopPercent + VehicleMilesTrav1000 + IncomePerCapita1000 + 
                  ns(AvgTemp,3) + Snow_LowHighInd + MarjMedicalUse + StrictHelmetLaws  + 
                  SB_Primary_Factor + BikeHelmetAge + MaxSL_Urban , family=quasipoisson, data=data)
summary(mod_NEW8)


mod_NEW9 <- glm(newcount ~ YoungPopPercent + OldPopPercent + VehicleMilesTrav1000 + IncomePerCapita1000 + 
                  ns(AvgTemp,3) + Snow_LowHighInd + MarjMedicalUse + StrictHelmetLaws  + 
                  SB_Primary_Factor + BikeHelmetAge + MaxSL_Urban , family=quasipoisson, data=data)
summary(mod_NEW9)




