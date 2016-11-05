#MODEL SELECTION
#This document evaluates classes of laws separately.

## @knitr ModelSelectionPartOne
library(splines)

#BACKWARD SELECTION
data<-read.csv("~/Downloads/FARS_Data.csv", header=TRUE, sep=",")
data$newcount<-round((data$Tot_Fatal_15/data$Pop_2015)*100000)
data$VehicleMilesTrav1000<-data$VehicleMilesTrav/1000
data$IncomePerCapita1000<-data$IncomePerCapita/1000
data$Cell_AllBan <- ifelse(data$Cell_HHBFactor==1 & data$Cell_TBFactor==1, 1, 0)
data$Cell_HHBFactor <- ifelse(data$Cell_HandheldBan==1,1,0)
data$Cell_TBFactor <- ifelse(data$Cell_TextingBan==1,1,0)
data$Cell_EnforceAllUseAllDrivers<-ifelse(data$Cell_EnforcePrimary==3, 1, 0)
data$Cell_YoungAllBanAge_Factor <- ifelse(data$Cell_YoungAllBanAge==17 | data$Cell_YoungAllBanAge==18 | data$Cell_YoungAllBanAge==21,1,0)
data$CR_MaxFineFactor<-ifelse(data$ChildRestraint_Max>70,1,0)
data$ChildRestraint_RF_Factor<-ifelse(data$ChildRestraint_RF==1 | data$ChildRestraint_RF==2,1,0)
data$ChildGenRestraint_Factor <- ifelse(data$ChildGenRestraint==1 | data$ChildGenRestraint==2 | data$ChildGenRestraint==3 | data$ChildGenRestraint==4 | data$ChildGenRestraint==5,1,0)
data$MarjMedicalUse<- ifelse(data$MarijuanaLegal==1,1,0)
data$NoTolerance <- ifelse(data$DUI_BACMaxUnder21==0,1,0 )
data$NoRest <- ifelse(data$DUI_RestorePrivDSusp==0,1,0)
data$DUI_IgnitionFO_Factor <- ifelse(data$DUI_IgnitionIL_FO==1,1,0)
data$OlderRegRenewPfVision<-ifelse(data$LicenseRenewCycleOlder<=4 & data$PfVisionRenewOlder==1,1,0)
data$MotorCycAllDrivers<-ifelse(data$MtrcycleHelmetAge==99 &data$MtrcycleHelmetLaw==1,1,0)
data$StrictHelmetLaws<-ifelse(data$MtrcycleHelmetAge==99 & data$BikeHelmetLaw==1,1,0)

#Controling Variables
model <- glm(newcount ~ PopGrowth + YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd, family=quasipoisson(), data=data)
summary(model)

#*******************************************************************
#*******************************************************************
#***************
#Cell Phones
#***************
data$Cell_EnforceAllUseAllDrivers<-ifelse(data$Cell_EnforcePrimary==3, 1, 0)
data$Cell_AllBan <- ifelse(data$Cell_HHBFactor==1 & data$Cell_TBFactor==1, 1, 0)
data$Cell_HHBFactor <- ifelse(data$Cell_HandheldBan==1,1,0)
data$Cell_TBFactor <- ifelse(data$Cell_TextingBan==1,1,0)
data$Cell_YoungAllBanAge_Factor <- ifelse(data$Cell_YoungAllBanAge==17 | data$Cell_YoungAllBanAge==18 | data$Cell_YoungAllBanAge==21,1,0)
data$SB_Primary_Factor<-ifelse(data$SB_Primary==1, 1, 0)
data$SB_MaxFineOver50 <- ifelse(data$SB_MaxFineFirstOff>=50,1,0)
data$SB_SeatLawFactor<-ifelse(data$SB_SeatCoveredLaw==1,1,0)


mod_Cella <- glm(newcount ~  PopGrowth + YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd + Cell_HHBFactor + Cell_YoungAllBan + Cell_YoungAllBanAge_Factor + Cell_TBFactor + Cell_EnforceAllUseAllDrivers, family=quasipoisson(), data=data)
summary(mod_Cella)

#Remove Cell_EnforceAllUseAllDrivers
mod_Cellb <- glm(newcount ~  PopGrowth + YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd + Cell_HHBFactor + Cell_YoungAllBan + Cell_YoungAllBanAge_Factor + Cell_TBFactor, family=quasipoisson(), data=data)
summary(mod_Cellb)
#Perform Chi-Sq Tests to determine whether the covariate can be removed from the model
pchisq((mod_Cellb$deviance - mod_Cella$deviance), (mod_Cellb$df.residual - mod_Cella$df.residual), lower.tail=FALSE) #0.9151138
#Chi Sq Test suggests that there is no significant difference between the covariate in the model verses without. Thus we permanently drop it from the model.

#Remove Cell_TBFactor
mod_Cellc <- glm(newcount ~  PopGrowth + YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd + Cell_HHBFactor + Cell_YoungAllBan + Cell_YoungAllBanAge_Factor, family=quasipoisson(), data=data)
summary(mod_Cellc)
pchisq((mod_Cellc$deviance - mod_Cellb$deviance), (mod_Cellc$df.residual - mod_Cellb$df.residual), lower.tail=FALSE) #0.9254659
#Chi Sq Test suggests that there is no significant difference between the covariate in the model verses without. Thus we permanently drop it from the model.

#We see that by controlling for states with similar PopGrowth, Young Population, Median UR, VMT, Income Per Capita, Average Temp, & Amounts of Snow there is a significant 
#decrease in the number of fatal accidents that occur in states with a cell phone ban in the 'young' population, as well as a hand held ban in general.
#Although YoungAllBanAge is not significant I believe this is influenced by YoungAllBan factor. This factor also uniquely provides a positive result.

#All bans within the young population (under 18) (both texting and cell phone) compared to states that have neither or both)
#Although not significant we see that there is also a nearly significant decrease in the amount of accidents within states that ban all cellphone use for the 
#adult and young population. 

#*******
#Code Both Bans ----- USE THIS!!!! Best Signifance & Dispersion Better
data$Cell_AllBan <- ifelse(data$Cell_HHBFactor==1 & data$Cell_TBFactor==1, 1, 0)
#Evaluate Cell_AllBan added to the model
mod_Celld <- glm(newcount ~  PopGrowth + YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd + Cell_AllBan + Cell_YoungAllBan + Cell_YoungAllBanAge_Factor + Cell_EnforceAllUseAllDrivers, family=quasipoisson(), data=data)
summary(mod_Celld)
#*******

#Remove Cell_EnforceAllUseAllDrivers ****DONT USE-- dispersion parameter is too small***** 
mod_Celle <-  glm(newcount ~ PopGrowth + YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd + Cell_AllBan + Cell_YoungAllBan + Cell_YoungAllBanAge_Factor , family=quasipoisson(), data=data)
summary(mod_Celle)
pchisq((mod_Celld$deviance - mod_Celle$deviance), (mod_Celld$df.residual - mod_Celle$df.residual), lower.tail=FALSE)


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

#Evaluate model with coded variables
mod_SB <- glm(newcount ~ PopGrowth + YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                SB_Primary_Factor + SB_AgeCoveredLaw + SB_SeatLawFactor + SB_MaxFineOver50, family=quasipoisson(), data=data)
summary(mod_SB)
#Notice here we are controlling for all of the variables.
#We see that the primary enforcement law is significant
#Less fatalities for states that have a primary law compared to states that do not-- when all other variables are controlled for

#Remove SB_MaxFineOver50
mod_SB1 <- glm(newcount ~ PopGrowth + YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 SB_Primary_Factor + SB_AgeCoveredLaw + SB_SeatLawFactor, family=quasipoisson(), data=data)
summary(mod_SB1)
pchisq((mod_SB$deviance - mod_SB1$deviance), (mod_SB$df.residual - mod_SB1$df.residual), lower.tail=FALSE)

#Remove Age
mod_SB2 <- glm(newcount ~ PopGrowth + YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 SB_Primary_Factor + SB_SeatLawFactor, family=quasipoisson(), data=data)
summary(mod_SB2)
pchisq((mod_SB2$deviance - mod_SB1$deviance), (mod_SB2$df.residual - mod_SB1$df.residual), lower.tail=FALSE) #0.5495452

#********* Use this
#Remove SB_SeatLawFactor
mod_SB3 <- glm(newcount ~ PopGrowth + YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 SB_Primary_Factor, family=quasipoisson(), data=data)
summary(mod_SB3)
pchisq((mod_SB3$deviance - mod_SB2$deviance), (mod_SB3$df.residual - mod_SB2$df.residual), lower.tail=FALSE) #0.3831891
#*********
#Here we see that when using the original seatbelt laws that the primary enfore law is significant and 
#contributes to a lower amount of fatalities in states that have this law

#*******************************************************************
#*******************************************************************
#***************
#ChildRestraints
#***************
#ChildRestraint_MaxFine
#Factor all fines greater than $70
data$CR_MaxFineFactor<-ifelse(data$ChildRestraint_Max>70,1,0)
data$ChildRestraint_RF_Factor<-ifelse(data$ChildRestraint_RF==1 | data$ChildRestraint_RF==2,1,0)
data$ChildGenRestraint_Factor <- ifelse(data$ChildGenRestraint==1 | data$ChildGenRestraint==2 | data$ChildGenRestraint==3 | data$ChildGenRestraint==4 | data$ChildGenRestraint==5,1,0)


mod_CR1 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 CR_MaxFineFactor + factor(ChildGenRestraint) + ChildRestraint_RF_Factor, family=quasipoisson, data=data)
summary(mod_CR1)

#Refactor ChildGenRestraint_Factor (3-7 all restraints)
data$ChildGenRestraint_Factor <- ifelse(data$ChildGenRestraint==1 | data$ChildGenRestraint==2 | data$ChildGenRestraint==3 | data$ChildGenRestraint==4 | data$ChildGenRestraint==5,1,0)
mod_CR2 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 CR_MaxFineFactor + ChildGenRestraint_Factor + ChildRestraint_RF_Factor, family=quasipoisson, data=data)
summary(mod_CR2)
pchisq((mod_SB3$deviance - mod_SB2$deviance), (mod_SB3$df.residual - mod_SB2$df.residual), lower.tail=FALSE) #0.3831891


#********* USE This
#Remove Max Fine Factor
mod_CR3 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 ChildGenRestraint_Factor + ChildRestraint_RF_Factor, family=quasipoisson, data=data)
summary(mod_CR3)
#********* 
#Nothing Significant

#*******************************************************************
#***************
#Young Driver---- LP
#***************
mod_LP1 <- glm(newcount ~ PopGrowth + YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 LP_Age + LP_Length + LP_SupDriveTime + LP_ReqNightDrive , family=quasipoisson, data=data)
summary(mod_LP1)

#Remove Required Night Driving
mod_LP2 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 LP_Age + LP_Length + LP_SupDriveTime , family=quasipoisson, data=data)
summary(mod_LP2)

#Remove Length
mod_LP4 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 LP_Age + LP_SupDriveTime , family=quasipoisson, data=data)
summary(mod_LP4)
#LP Age is significant-----
#Dispersion is also good


#*******************************************************************
#***************
#Young Driver ------RL
#***************

mod_RL1 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 RL_MinAge + RL_PassengerRestrict + RL_NightRestrict + RL_PassRestLength + RL_NightRestLength, family=quasipoisson, data=data)
summary(mod_RL1)

#Remove Passenger Rest Length 
mod_RL2 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 RL_MinAge + RL_PassengerRestrict + RL_NightRestrict + RL_NightRestLength, family=quasipoisson, data=data)
summary(mod_RL2)

#Remove Night Rest Length 
mod_RL3 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 RL_MinAge + RL_PassengerRestrict + RL_NightRestrict, family=quasipoisson, data=data)
summary(mod_RL3)

#Remove Night Rest Length 
mod_RL4 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 RL_MinAge + RL_PassengerRestrict, family=quasipoisson, data=data)
summary(mod_RL4)
#None of these results are significant.
#However, all values are negative

#*******************************************************************
#*******************************************************************
#***************
#Older Driver
#***************
#Older
#Factor those with renewal every 4 years or less and proof of vision each time
data$OlderRegRenewPfVision<-ifelse(data$LicenseRenewCycleOlder<=4 & data$PfVisionRenewOlder==1,1,0)

mod_Old1 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                  + Definition_Older + OlderRegRenewPfVision, family=quasipoisson, data=data)
summary(mod_Old1)

#Remove DefinitionOlder
mod_Old2 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                  OlderRegRenewPfVision , family=quasipoisson, data=data)
summary(mod_Old2)
#Nothing is significant here

#*******************************************************************
#*******************************************************************
#***************
#Speed Limits & Agressive Driving
#***************

mod_Speed1 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                    MaxSL_Rural + MaxSL_Urban + MaxSL_AccessRd + MaxSL_OtherRd + Speed_MaxFine + Speed_JailAlternative + Aggressive, family=quasipoisson, data=data)
summary(mod_Speed1) 

#Remove Aggressive
mod_Speed1a <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                     MaxSL_Rural + MaxSL_Urban + MaxSL_AccessRd + MaxSL_OtherRd + Speed_MaxFine + Speed_JailAlternative, family=quasipoisson, data=data)
summary(mod_Speed1a) 

#Remove Jail Alternative 
mod_Speed2 <- glm(newcount ~ YoungPopPercent + OldPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                    MaxSL_Rural + MaxSL_Urban + MaxSL_AccessRd + MaxSL_OtherRd + Speed_MaxFine, family=quasipoisson, data=data)
summary(mod_Speed2)

#Remove Speed_MaxFine
mod_Speed3 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                    MaxSL_Rural + MaxSL_Urban + MaxSL_AccessRd + MaxSL_OtherRd, family=quasipoisson, data=data)
summary(mod_Speed3)

#Remove Access Road
mod_Speed4 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                    MaxSL_Rural + MaxSL_Urban + MaxSL_OtherRd, family=quasipoisson, data=data)
summary(mod_Speed4)

#Remove Rural Road
mod_Speed4 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                    MaxSL_Urban + MaxSL_OtherRd, family=quasipoisson, data=data)
summary(mod_Speed4)
#Other Roads & Urban Road are Significant
#For every increase in speed on an urban road there is an increase in accidents----
#Congestion?

#mod_Speed2 <- glm(newcount ~ YoungPopPercent + OldPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,2) + Snow_LowHighInd +
#                    MaxSL_Urban + MaxSL_AccessRd + MaxSL_OtherRd + Speed_MaxFine,
#                  Speed_JailAlternative, family=quasipoisson, data=data)
#summary(mod_Speed2)
#Access Rd for every mph decrease in speed limit there is a decrease in fatalties.
#Also for every $1 increase of a max fine there is a decrease in fatalties.


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
mod_Helmet1 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                     BikeHelmetLaw + BikeHelmetAge + MtrcycleHelmetLaw + MtrcycleHelmetAge, family=quasipoisson, data=data)
summary(mod_Helmet1)

#Take out Mtrcycle Age
mod_Helmet2 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                     BikeHelmetLaw + BikeHelmetAge + MtrcycleHelmetLaw , family=quasipoisson, data=data)
summary(mod_Helmet2)

#Take out Bike Age
mod_Helmet3 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                     BikeHelmetLaw  + MtrcycleHelmetLaw , family=quasipoisson, data=data)
summary(mod_Helmet3)

#Look at coded variables
mod_Helmet4 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                     StrictHelmetLaws + BikeHelmetAge +  MotorCycAllDrivers + MtrcycleHelmetAge, family=quasipoisson, data=data)
summary(mod_Helmet4)

#Removing Motorcycle Age
mod_Helmet7 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                     StrictHelmetLaws + BikeHelmetAge +  MotorCycAllDrivers, family=quasipoisson, data=data)
summary(mod_Helmet7)
#We see that Bike Age looks significant!

#Remove MotorCycAllDrivers
mod_Helmet8 <- glm(newcount ~ YoungPopPercent + PopGrowth + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                     StrictHelmetLaws + BikeHelmetAge, family=quasipoisson, data=data)
summary(mod_Helmet8)

#*******************************************************************
#*******************************************************************
#***************
#DUI
#***************
data$MarjMedicalUse<- ifelse(data$MarijuanaLegal==1,1,0)
data$NoTolerance <- ifelse(data$DUI_BACMaxUnder21==0,1,0 )
data$NoRest <- ifelse(data$DUI_RestorePrivDSusp==0,1,0)
data$DUI_IgnitionFO_Factor <- ifelse(data$DUI_IgnitionIL_FO==1,1,0)
data$OlderRegRenewPfVision<-ifelse(data$LicenseRenewCycleOlder<=4 & data$PfVisionRenewOlder==1,1,0)

mod_DUI1<- glm(newcount ~  PopGrowth + YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd + 
                 NoTolerance + DUI_LicenseSuspFO + NoRest + DUI_IgnitionFO_Factor + DUI_Checkpoints + MarjMedicalUse, family=quasipoisson, data=data)
summary(mod_DUI1)

#Checkpoints Removed
mod_DUI2<- glm(newcount ~  PopGrowth + YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd + 
                 NoTolerance + DUI_LicenseSuspFO + NoRest + DUI_IgnitionFO_Factor + MarjMedicalUse, family=quasipoisson, data=data)
summary(mod_DUI2)

#Remove NoRest
mod_DUI3<- glm(newcount ~  PopGrowth + YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd + 
                 NoTolerance + DUI_LicenseSuspFO + DUI_IgnitionFO_Factor + MarjMedicalUse, family=quasipoisson, data=data)
summary(mod_DUI3)

#****Best Dispersion
#Remove DUI_IgnitionFO_Factor
mod_DUI4<- glm(newcount ~  PopGrowth + YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd + 
                 NoTolerance + DUI_LicenseSuspFO + DUI_IgnitionFO_Factor + MarjMedicalUse, family=quasipoisson, data=data)
summary(mod_DUI4)
#******

#Remove DUI_LicenseSuspFO
mod_DUI5<- glm(newcount ~ PopGrowth + YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                 NoTolerance + DUI_IgnitionFO_Factor + MarjMedicalUse, family=quasipoisson, data=data)
summary(mod_DUI5)


#*******************************************************************
#*******************************************************************
#***************
#Cameras
#***************
mod_Camera1<- glm(newcount ~ PopGrowth + YoungPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                    NoCameras + RL_Speed_Cameras + Speed_MaxFine + Aggressive, family=quasipoisson, data=data)
summary(mod_Camera1)

#Remove No Cameras 
mod_Camera2<- glm(newcount ~ YoungPopPercent + OldPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                    RL_Speed_Cameras + Speed_MaxFine + Aggressive, family=quasipoisson, data=data)
summary(mod_Camera2)

#Remove Max Fine
mod_Camera3<- glm(newcount ~ YoungPopPercent + OldPopPercent + Median_UR_2015 + VehicleMilesTrav1000 + IncomePerCapita1000 + ns(AvgTemp,3) + Snow_LowHighInd +
                    RL_Speed_Cameras + Aggressive, family=quasipoisson, data=data)
summary(mod_Camera3)

#Nothing is Significant!!!! 
#However we can see that Red Light and Speed Cameras & Agressive Driving Laws decrease the amount of accidents
