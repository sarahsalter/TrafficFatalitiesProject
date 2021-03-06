
## @knitr RowBindCode

#*****************************************************
#1. Read in Accident Level Data Sets from 2010-2015
#2. Compare Colnames Across All Datasets to Determine which Variables are Consistent Acorss All 5 Years.
#3. Subset Datasets from Each Year to Include Only the Consistent Variables Evaluated at Every year Between 2010-2015.
#4. Rowbind Datasets On Like Variables (Arranged in the Same Order) to Create One Dataeset from Years 2010-2015.
#5. Plot Exploratory Plot to See the Patterns in Accident Level Data across Time.
#****************************************************

#Load necessary packages
library(knitr)
library(foreign)
library(haven)

#Unzip & Download Data File 
#Read Accident Dataset from Years 2010-2015 
accident_data2015 = read_sas("~/Downloads/FARS_Accident2015.sas7bdat")
accident_data2014 = read_sas("~/Downloads/FARS_Accident2014.sas7bdat")
accident_data2013 = read_sas("~/Downloads/FARS_Accident2013.sas7bdat")
accident_data2012 = read_sas("~/Downloads/FARS_Accident2012.sas7bdat")
accident_data2011 = read_sas("~/Downloads/FARS_Accident2011.sas7bdat")
accident_data2010 = read_sas("~/Downloads/FARS_Accident2010.sas7bdat")

#*****************************************************
#Compare all column names 
#*****************************************************

accident_colnames_15 <- colnames(accident_data2015)
#length(accident_colnames_15) #52

accident_colnames_14 <- colnames(accident_data2014)
#length(accident_colnames_14) #50

accident_colnames_13 <- colnames(accident_data2013)
#length(accident_colnames_13) #50

accident_colnames_12 <- colnames(accident_data2012)
#length(accident_colnames_12) #50

accident_colnames_11 <- colnames(accident_data2011)
#length(accident_colnames_11) #50

accident_colnames_10 <- colnames(accident_data2010)
#length(accident_colnames_10) #47

#**************************
#Compare the least amount to the second least amount (2010 compared to 2011,2012,2013,2014)
#**************************
same_names <- rep(0,100)
col_names1 <- colnames(accident_data2011)
col_names2 <- colnames(accident_data2010)
for(i in 1:length(col_names1)){
  for(j in 1:length(col_names2)){
    if(col_names1[i]==col_names2[j]){
      same_names[i]=col_names2[j]
    }
  }
}
#print(subset(same_names, same_names!="0"))
same_2011_2010 <-subset(same_names, same_names!="0")
#length(same_2011_2010) #47

#**************************
#Compare the least amount to the second least amount (2010 compared to 2015)
#**************************
same_names <- rep(0,100)
col_names1 <- colnames(accident_data2015)
col_names2 <- colnames(accident_data2010)
for(i in 1:length(col_names1)){
  for(j in 1:length(col_names2)){
    if(col_names1[i]==col_names2[j]){
      same_names[i]=col_names2[j]
    }
  }
}
#print(subset(same_names, same_names!="0"))
same_2015_2010 <-subset(same_names, same_names!="0")
#length(same_2015_2010) #46

#**************************
#Compare the least amount to the second least amount (2011,2012,2013,2014 compared to 2015)
#**************************
same_names <- rep(0,100)
col_names1 <- colnames(accident_data2015)
col_names2 <- colnames(accident_data2011)
for(i in 1:length(col_names1)){
  for(j in 1:length(col_names2)){
    if(col_names1[i]==col_names2[j]){
      same_names[i]=col_names2[j]
    }
  }
}
#print(subset(same_names, same_names!="0"))
same_2015_2011 <-subset(same_names, same_names!="0")
#length(same_2015_2011) #49


#**************************
#(1): Compare the least amount to the second least amount (2010 compared to 2011,2012,2013,2014)
#same_2011_2010
#length(same_2011_2010) #47
#(2): Compare the least amount to the second least amount (2010 compared to 2015)
#same_2015_2010
#length(same_2015_2010) #46
#(3): Compare the least amount to the second least amount (2011,2012,2013,2014 compared to 2015)
#same_2015_2011
#length(same_2015_2011) #49
#**************************

#Compare (1) to (2)
same_names <- rep(0,100)
col_names1 <- same_2011_2010
col_names2 <- same_2015_2010
for(i in 1:length(col_names1)){
  for(j in 1:length(col_names2)){
    if(col_names1[i]==col_names2[j]){
      same_names[i]=col_names2[j]
    }
  }
}
#print(subset(same_names, same_names!="0"))
same_1_2 <-subset(same_names, same_names!="0")
#length(same_1_2) #46


#Compare (2) to (3)
same_names <- rep(0,100)
col_names1 <- same_2011_2010
col_names2 <- same_2015_2011
for(i in 1:length(col_names1)){
  for(j in 1:length(col_names2)){
    if(col_names1[i]==col_names2[j]){
      same_names[i]=col_names2[j]
    }
  }
}
#print(subset(same_names, same_names!="0"))
same_2_3 <-subset(same_names, same_names!="0")
#length(same_2_3) #46


#Compare the comparisons
same_names <- rep(0,100)
col_names1 <- same_1_2
col_names2 <- same_2_3
for(i in 1:length(col_names1)){
  for(j in 1:length(col_names2)){
    if(col_names1[i]==col_names2[j]){
      same_names[i]=col_names2[j]
    }
  }
}
#print(subset(same_names, same_names!="0"))


#*******************************************************************************
#1st: Compare the ones that are not the SAME Variables that are in each dataset
#*******************************************************************************
#setdiff(colnames(accident_data2015), colnames(accident_data2011))
#[1] "RUR_URB"  "FUNC_SYS" "RD_OWNER"
#setdiff(colnames(accident_data2011), colnames(accident_data2015))
#[1] "ROAD_FNC"

#setdiff(colnames(accident_data2015), colnames(accident_data2010))
#[1] "PVH_INVL"   "PERNOTMVIT" "PERMVIT"    "RUR_URB"    "FUNC_SYS"   "RD_OWNER" 
#setdiff(colnames(accident_data2010), colnames(accident_data2015))
#[1] "ROAD_FNC"

#setdiff(colnames(accident_data2010), colnames(accident_data2011))
#0
#setdiff(colnames(accident_data2011), colnames(accident_data2010))
#[1] "PVH_INVL"   "PERNOTMVIT" "PERMVIT" 

#*******************************************************************************
#RANDOM SUMMARIES
#sort(unique(accident_data2012$PVH_INVL)) # 0 1 2 3 4 5 6 9
#sort(unique(accident_data2013$PVH_INVL)) # 0 1 2 3 4 5 6 7 8 11
#sort(unique(accident_data2014$PVH_INVL)) # 0 1 2 3 4 5 6 7 10 11
#*******************************************************************************


#**********************************************************************************
#Decision: Need to remove all colnames from:
# "PVH_INVL", "PERNOTMVIT", "PERMVIT", "RUR_URB", "FUNC_SYS", "RD_OWNER", "ROAD_FNC"
#This will produce datatsets with consistent variables in them.
#**********************************************************************************

# ***** 2015 *****
vect_position <- rep(0,length(accident_data2015))
for(i in 1:length(accident_data2015)){
  if(colnames(accident_data2015)[i] == "PVH_INVL"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2015)[i] == "PERNOTMVIT"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2015)[i] == "PERMVIT"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2015)[i] == "RUR_URB"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2015)[i] == "FUNC_SYS"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2015)[i] == "RD_OWNER"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2015)[i] == "ROAD_FNC"){
    vect_position[i] <- i
  }
}
#vect_position
vect_position <- subset(vect_position, vect_position!=0)
#vect_position
subset_accident_data2015 <- accident_data2015[,-c(vect_position)]
#dim(subset_accident_data2015) #32166  46
#View(subset_accident_data2015)


# ***** 2014 *****
vect_position <- rep(0,length(accident_data2014))
for(i in 1:length(accident_data2014)){
  if(colnames(accident_data2014)[i] == "PVH_INVL"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2014)[i] == "PERNOTMVIT"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2014)[i] == "PERMVIT"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2014)[i] == "RUR_URB"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2014)[i] == "FUNC_SYS"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2014)[i] == "RD_OWNER"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2014)[i] == "ROAD_FNC"){
    vect_position[i] <- i
  }
}
#vect_position
vect_position <- subset(vect_position, vect_position!=0)
#vect_position
subset_accident_data2014 <- accident_data2014[,-c(vect_position)]
#dim(subset_accident_data2014) #30056    46
#View(subset_accident_data2014)


# ***** 2013 *****
vect_position <- rep(0,length(accident_data2013))
for(i in 1:length(accident_data2013)){
  if(colnames(accident_data2013)[i] == "PVH_INVL"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2013)[i] == "PERNOTMVIT"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2013)[i] == "PERMVIT"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2013)[i] == "RUR_URB"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2013)[i] == "FUNC_SYS"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2013)[i] == "RD_OWNER"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2013)[i] == "ROAD_FNC"){
    vect_position[i] <- i
  }
}
#vect_position
vect_position <- subset(vect_position, vect_position!=0)
#vect_position
subset_accident_data2013 <- accident_data2013[,-c(vect_position)]
#dim(subset_accident_data2013)  #30202    46
#View(subset_accident_data2013)


# ***** 2012 *****
vect_position <- rep(0,length(accident_data2012))
for(i in 1:length(accident_data2012)){
  if(colnames(accident_data2012)[i] == "PVH_INVL"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2012)[i] == "PERNOTMVIT"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2012)[i] == "PERMVIT"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2012)[i] == "RUR_URB"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2012)[i] == "FUNC_SYS"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2012)[i] == "RD_OWNER"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2012)[i] == "ROAD_FNC"){
    vect_position[i] <- i
  }
}
#vect_position
vect_position <- subset(vect_position, vect_position!=0)
#vect_position
subset_accident_data2012 <- accident_data2012[,-c(vect_position)]
#dim(subset_accident_data2012)  #31006    46
#View(subset_accident_data2012)


# ***** 2011 *****
vect_position <- rep(0,length(accident_data2011))
for(i in 1:length(accident_data2011)){
  if(colnames(accident_data2011)[i] == "PVH_INVL"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2011)[i] == "PERNOTMVIT"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2011)[i] == "PERMVIT"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2011)[i] == "RUR_URB"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2011)[i] == "FUNC_SYS"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2011)[i] == "RD_OWNER"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2011)[i] == "ROAD_FNC"){
    vect_position[i] <- i
  }
}
#vect_position
vect_position <- subset(vect_position, vect_position!=0)
#vect_position
subset_accident_data2011 <- accident_data2011[,-c(vect_position)]
#dim(subset_accident_data2011) #29867    46
#View(subset_accident_data2011)


# ***** 2010 *****
vect_position <- rep(0,length(accident_data2010))
for(i in 1:length(accident_data2010)){
  if(colnames(accident_data2010)[i] == "PVH_INVL"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2010)[i] == "PERNOTMVIT"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2010)[i] == "PERMVIT"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2010)[i] == "RUR_URB"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2010)[i] == "FUNC_SYS"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2010)[i] == "RD_OWNER"){
    vect_position[i] <- i
  }
  if(colnames(accident_data2010)[i] == "ROAD_FNC"){
    vect_position[i] <- i
  }
}
#vect_position
vect_position <- subset(vect_position, vect_position!=0)
#vect_position
subset_accident_data2010 <- accident_data2010[,-c(vect_position)]
dim(subset_accident_data2010) #30296    46
#View(subset_accident_data2011)


#if(sum(vect_position)==0) {subset_accident = accident_data2010}
#if(sum(vect_position)!=0) {subset_accident_data2010 = subset(vect_position, vect_position!=0)}
#vect_position <- subset(vect_position, vect_position!=0)
#if(sum(vect_position)!=0) {subset_accident_data2010 <- accident_data2010[,-c(vect_position)]}

#***************************************************************
# ROWBIND all datasets with like columns
#***************************************************************

all_merged_accidents <- rbind(subset_accident_data2015, subset_accident_data2014, subset_accident_data2013, subset_accident_data2012, subset_accident_data2011, subset_accident_data2010)
#View(all_merged_accidents)



## @knitr CodeExploratoryPlot
#*****************************************************************************************************************************
#Look at code to plot the exploratory analysis plot observing Accidents, Fatalities, & DUIs across 2015.
#*****************************************************************************************************************************
all_merged_accidents <- rbind(subset_accident_data2015, subset_accident_data2014, subset_accident_data2013, subset_accident_data2012, subset_accident_data2011, subset_accident_data2010)

#Create the merged data ste
#Create a unique ID number for each case; This is important because each year repeats the case numbers used in each state
uniqueID <- seq(1:183593)
all_merged_accidents$uniqueID <- uniqueID
#View(all_merged_accidents)

#************************************************************************
tot_days_endofeachmonth <- c(31,59,90,120,151,181,212,243,273,304,334,365)
max<-c(0,31,28,31,30,31,30,31,31,30,31,30) 
#length(max) #12

#RECODE day so that days go from 1-365 starting from January 1st 2015 to December 31st 2015
day<-rep(0,nrow(accident_data2015))
for(i in 1:nrow(accident_data2015)){
  if(accident_data2015$YEAR[i]==2015){
    if(accident_data2015$MONTH[i]==1){
      day[i]<-max[1] + accident_data2015$DAY[i]
    }
    if(accident_data2015$MONTH[i]==2){
      day[i]<-max[1] + max[2] + accident_data2015$DAY[i]
    }
    if(accident_data2015$MONTH[i]==3){
      day[i]<-max[1] + max[2] + max[3] + accident_data2015$DAY[i]
    }
    if(accident_data2015$MONTH[i]==4){
      day[i]<-max[1] + max[2] + max[3] + max[4] + accident_data2015$DAY[i]
    }
    if(accident_data2015$MONTH[i]==5){
      day[i]<-max[1] + max[2] + max[3] + max[4] + max[5] + accident_data2015$DAY[i]
    }
    if(accident_data2015$MONTH[i]==6){
      day[i]<-max[1] + max[2] + max[3] + max[4] + max[5] + max[6] + accident_data2015$DAY[i]
    }
    if(accident_data2015$MONTH[i]==7){
      day[i]<-max[1] + max[2] + max[3] + max[4] + max[5] + max[6] + max[7] + accident_data2015$DAY[i]
    }
    if(accident_data2015$MONTH[i]==8){
      day[i]<-max[1] + max[2] + max[3] + max[4] + max[5] + max[6] + max[7] + max[8] + accident_data2015$DAY[i]
    }
    if(accident_data2015$MONTH[i]==9){
      day[i]<-max[1] + max[2] + max[3] + max[4] + max[5] + max[6] + max[7] + max[8] + max[9] + accident_data2015$DAY[i]
    }
    if(accident_data2015$MONTH[i]==10){
      day[i]<-max[1] + max[2] + max[3] + max[4] + max[5] + max[6] + max[7] + max[8] + max[9] + max[10] + accident_data2015$DAY[i]
    }
    if(accident_data2015$MONTH[i]==11){
      day[i]<-max[1] + max[2] + max[3] + max[4] + max[5] + max[6] + max[7] + max[8] + max[9] + max[10] + max[11] + accident_data2015$DAY[i]
    }
    if(accident_data2015$MONTH[i]==12){
      day[i]<-max[1] + max[2] + max[3] + max[4] + max[5] + max[6] + max[7] + max[8] + max[9] + max[10] + max[11] + max[12] + accident_data2015$DAY[i]
    }
  }
}  
#day
#sort(unique(day))


#**************
#Calculate the number of fatalities on each day in each state in 2015
#**************
fatal<-rep(0,56)
for(i in 1:nrow(accident_data2015)){
  for(j in 1:56){
    if(accident_data2015$STATE[i]==j)
      fatal[j] <- fatal[j] + accident_data2015$FATALS[i]
    
  }
}  
#fatal

state<-1:56
df1<-data.frame(fatal,state)
df1[-c(3,7,14,43,52),]
#sum(accident_data2015$FATALS) #Check to make sure the sum is accurate

#**************
#Calculate the number of Accidents on each day in each state in 2015
#**************
total<-rep(0,56)
for(i in 1:nrow(accident_data2015)){
  for(j in 1:56){
    if(accident_data2015$STATE[i]==j){
      total[j]<-total[j]+1
    }
  }
}
Total<-total 

state<-1:56
df5<-data.frame(total,state)
df5[-c(3,7,14,43,52),]

#**************
#Calculate the number of DUIs on each day for each state in 2015
#**************
accident_data2015$DRUNK_DR<-ifelse(accident_data2015$DRUNK_DR>=1,1,0)
dui<-rep(0,56)
for(i in 1:nrow(accident_data2015)){
  for(j in 1:56){
    if(accident_data2015$STATE[i]==j)
      dui[j] <- dui[j] + accident_data2015$DRUNK_DR[i]
    
  }
}
#dui
#sum(dui) #8862
#length(which(accident_data2015$DRUNK_DR==1)) #8862

state<-1:56
df2<-data.frame(dui,state)
df2[-c(3,7,14,43,52),]
df2[-c(3,7,14,43,52),]


#*****************************************************************************************************************************
#Look at code to plot the exploratory analysis plot observing Accidents, Fatalities, & DUIs across 2010-2015.
#**********************************************************************************************************************************
#Evaluate the number of accidents and the number of fatalities that have occured between 2010-2015
max<-c(0,31,28,31,30,31,30,31,31,30,31,30) 
max_LP<-c(0,31,29,31,30,31,30,31,31,30,31,30) 
max_year<-c(0,365,730,1096,1461,1826)
#max_year
#This tries to look at a for loop and account for the leap year afterwards
#all_merged_accidents
day<-rep(0,nrow(all_merged_accidents))
for(i in 1:nrow(all_merged_accidents)){
  for(j in 1:6){
    if(all_merged_accidents$YEAR[i]==j+2009){
      max <-c(0,31,28,31,30,31,30,31,31,30,31,30) 
      if(all_merged_accidents$YEAR[i]==2012){
        max=max_LP
      }
      if(all_merged_accidents$MONTH[i]==1){
        day[i]<-max_year[j] + max[1] + all_merged_accidents$DAY[i]
      }
      if(all_merged_accidents$MONTH[i]==2){
        day[i]<-max_year[j] + max[1] + max[2] + all_merged_accidents$DAY[i]
      }
      if(all_merged_accidents$MONTH[i]==3){
        day[i]<-max_year[j] + max[1] + max[2] + max[3] + all_merged_accidents$DAY[i]
      }
      if(all_merged_accidents$MONTH[i]==4){
        day[i]<-max_year[j] + max[1] + max[2] + max[3] + max[4] + all_merged_accidents$DAY[i]
      }
      if(all_merged_accidents$MONTH[i]==5){
        day[i]<-max_year[j] + max[1] + max[2] + max[3] + max[4] + max[5] + all_merged_accidents$DAY[i]
      }
      if(all_merged_accidents$MONTH[i]==6){
        day[i]<-max_year[j] + max[1] + max[2] + max[3] + max[4] + max[5] + max[6] + all_merged_accidents$DAY[i]
      }
      if(all_merged_accidents$MONTH[i]==7){
        day[i]<-max_year[j] + max[1] + max[2] + max[3] + max[4] + max[5] + max[6] + max[7] + all_merged_accidents$DAY[i]
      }
      if(all_merged_accidents$MONTH[i]==8){
        day[i]<-max_year[j] + max[1] + max[2] + max[3] + max[4] + max[5] + max[6] + max[7] + max[8] + all_merged_accidents$DAY[i]
      }
      if(all_merged_accidents$MONTH[i]==9){
        day[i]<-max_year[j] + max[1] + max[2] + max[3] + max[4] + max[5] + max[6] + max[7] + max[8] + max[9] + all_merged_accidents$DAY[i]
      }
      if(all_merged_accidents$MONTH[i]==10){
        day[i]<-max_year[j] + max[1] + max[2] + max[3] + max[4] + max[5] + max[6] + max[7] + max[8] + max[9] + max[10] + all_merged_accidents$DAY[i]
      }
      if(all_merged_accidents$MONTH[i]==11){
        day[i]<-max_year[j] + max[1] + max[2] + max[3] + max[4] + max[5] + max[6] + max[7] + max[8] + max[9] + max[10] + max[11] + all_merged_accidents$DAY[i]
      }
      if(all_merged_accidents$MONTH[i]==12){
        day[i]<-max_year[j] + max[1] + max[2] + max[3] + max[4] + max[5] + max[6] + max[7] + max[8] + max[9] + max[10] + max[11] + max[12] + all_merged_accidents$DAY[i]
      }
    }
  }
}  
#sort(unique(day))
all_merged_accidents$day <- day

#**************
#Calculate the number of accidents occuring on the same day through 5 years being observed
#**************
#Find out how many accidents are occuring on the same day
#length(unique(day)) #2191
total<-rep(0,2191)
for(j in 1:2191){
  total[j]<-length(which(all_merged_accidents$day==j))
}
#total
#sum(total) #Check to make sure this is correct
#nrow(all_merged_accidents) #Verify that the number of accident files matches 
day_number<-1:2191
df_numb_yearly_accidents <- data.frame(day_number, total) 
#View(df_numb_yearly_accidents)

#Check
#Check Day 366-- represents the 1st day of year 2011
#length(which(all_merged_accidents$YEAR==2011 & all_merged_accidents$MONTH==1 & all_merged_accidents$DAY==1)) #117
#Check Day 1461-- represents the last day of the year 2013
#length(which(all_merged_accidents$YEAR==2013 & all_merged_accidents$MONTH==12 & all_merged_accidents$DAY==31)) #77

#Plot Total vs Day
#length(day)
#plot(day_number, total,
#     xlab="Day Number (01/01/10-12/31/15)",
#     ylab="Total Number of Accidents",
#     main="Accident Fatality Seasonality",
#     col="blue")
#abline(v=166,col="red")
#abline(v=258,col="red")
#abline(v=151,col="red")
#abline(v=273,col="red")
#abline(v=151+365,col="red")
#abline(v=273+365,col="red")
#abline(v=151+365+366,col="red")
#abline(v=273+365+366,col="red")
#abline(v=151+365+366+365,col="red")
#bline(v=273+365+366+365,col="red")
#abline(v=151+365+366+365+365,col="red")
#abline(v=273+365+366+365+365,col="red")
#abline(v=151+365+366+365+365+365,col="red")
#abline(v=273+365+366+365+365+365,col="red")

#************
#Calculate the number of fatalities that occur on the same day from 2010-2015
#************
count<-rep(0,2191)
for(i in 1:nrow(all_merged_accidents)){
  for(j in 1:2191){
    if(day[i]==j){
      count[j]<-count[j]+all_merged_accidents$FATALS[i]
    }
  }
}
#count
#These values should be equal
#sum(count) #199989
#sum(all_merged_accidents$FATALS) #199989

#Look at plots
#plot(day_number, count)
#boxplot(day_number, count)
#hist(count)

#************
#Counting the number of DUIs that occur on every day
#************
#First make the DUI variable dicotomous
all_merged_accidents$DRUNK_DR<-ifelse(all_merged_accidents$DRUNK_DR>=1,1,0)

#Count the Number of Drunk Drivers
#length(which(all_merged_accidents$DRUNK_DR==1)) #5592
drunk<-rep(0,2191)
for(i in 1:nrow(all_merged_accidents)){
  for(j in 1:2191){
    if(day[i]==j){
      drunk[j]<-drunk[j]+all_merged_accidents$DRUNK_DR[i]
    }
  }
}
#drunk
#These values should be equal
#sum(drunk) #55262
#sum(all_merged_accidents$DRUNK_DR) #55262




## @knitr Plot
#****************************************************************************************************
#PLOTTING ALL THREE PLOTS
#Create plots on top of one another
par(mfrow=c(2,1),
    mar = c(3,1.2,1,1) + 1)
?par

#************************************************

#First-- number of accidents on a given day
plot(day_number, total,
     xlab="Day Number (01/01/10-12/31/15)",
     ylab="Total Number of Accidents",
     main="Total Number Fatal Traffic Accidents on Each Day in 2010-2015",
     col="blue", pch=".")
lines(lowess(total ~ day_number, f = 0.08), col = "black", lwd = 2)
#Account for the summer months
abline(v=151,col="red")
abline(v=273,col="red")
abline(v=151+365,col="red")
abline(v=273+365,col="red")
abline(v=151+365+366,col="red")
abline(v=273+365+366,col="red")
abline(v=151+365+366+365,col="red")
abline(v=273+365+366+365,col="red")
abline(v=151+365+366+365+365,col="red")
abline(v=273+365+366+365+365,col="red")
abline(v=151+365+366+365+365+365,col="red")
abline(v=273+365+366+365+365+365,col="red")

#Evaluate NYEs 12/31-1/1 each year
#2010 1/1 -- day1
#2010 12/31-- day365
NYE_day<-c(365,365+365, 366+(365*2), 366+(365*3), 366+(365*4), 366+(365*5))
#NY_day<-c(1,1+365, 1+(365*2), 1+366+(365*2), 1+366+(365*3), 1+366+(365*4))
#NYE_day
#NY_day
abline(v=1,col="cyan")
abline(v=365,col="cyan")
abline(v=366,col="cyan")
abline(v=730,col="cyan")
abline(v=731,col="cyan")
abline(v=1096,col="cyan")
abline(v=1097,col="cyan")
abline(v=1461,col="cyan")
abline(v=1462,col="cyan")
abline(v=1826,col="cyan")
abline(v=1827,col="cyan")
abline(v=1461,col="cyan")
abline(v=2191,col="cyan")

#Evaluate 4th of July
#2010 7/4 -- day185
#2010 7/5-- day186
July4_before<-c(183,183+365, 183+(365+366), 183+366+(365*2), 183+366+(365*3), 183+366+(365*4))
July4_day<-c(185,185+365, 185+(365+366), 185+366+(365*2), 185+366+(365*3), 185+366+(365*4))
July4_dayafter<-c(186,186+365, 186+(365+366), 186+366+(365*2), 186+366+(365*3), 186+366+(365*4))
#July4_before
#July4_day
#July4_dayafter
abline(v=183,col="purple")
abline(v=186,col="purple")
abline(v=548,col="purple")
abline(v=551,col="purple")
abline(v=914,col="purple")
abline(v=917,col="purple")
abline(v=1279,col="purple")
abline(v=1282,col="purple")
abline(v=1644,col="purple")
abline(v=1647,col="purple")
abline(v=2009,col="purple")
abline(v=2012,col="purple")


#Evaluate Thanksgiving
#2010 11/24 -- day328
#2010 11/25 -- day329
#2010 11/26-- day330
TGeve<-c(328,328+365, 328+(365+366), 328+366+(365*2), 328+366+(365*3), 328+366+(365*4))
TGdayafter<-c(330,330+365, 330+(365+366), 330+366+(365*2), 330+366+(365*3), 330+366+(365*4))
#TGeve
#TGdayafter
abline(v=328,col="yellow")
abline(v=330,col="yellow")
abline(v=693,col="yellow")
abline(v=695,col="yellow")
abline(v=1059,col="yellow")
abline(v=1061,col="yellow")
abline(v=1424,col="yellow")
abline(v=1426,col="yellow")
abline(v=1789,col="yellow")
abline(v=1791,col="yellow")
abline(v=2154,col="yellow")
abline(v=2156,col="yellow")


#Evaluate Christmas
#2010 12/24 -- day358
#2010 11/25 -- day359
#2010 11/26-- day360
XmasEve<-c(358,358+365, 358+(365+366), 358+366+(365*2), 358+366+(365*3), 358+366+(365*4))
XmasAfter<-c(360,360+365, 360+(365+366), 360+366+(365*2), 360+366+(365*3), 360+366+(365*4))
#XmanEve
#XmasAfter
abline(v=358,col="green")
abline(v=360,col="green")
abline(v=723,col="green")
abline(v=725,col="green")
abline(v=1089,col="green")
abline(v=1091,col="green")
abline(v=1454,col="green")
abline(v=1456,col="green")
abline(v=1819,col="green")
abline(v=1821,col="green")
abline(v=2184,col="green")
abline(v=2186,col="green")

#Evaluate Labor Day
#2010 9/6 -- day249
#2010 9/7 -- day250
LDBefore<-c(247,247+365, 247+(365+366), 247+366+(365*2), 247+366+(365*3), 247+366+(365*4))
LD<-c(249,249+365, 249+(365+366), 249+366+(365*2), 249+366+(365*3), 249+366+(365*4))
LDAfter<-c(250,250+365, 250+(365+366), 250+366+(365*2), 250+366+(365*3), 250+366+(365*4))
#LDBefore
#LD
#LDAfter
abline(v=247,col="pink")
abline(v=250,col="pink")
abline(v=612,col="pink")
abline(v=615,col="pink")
abline(v=978,col="pink")
abline(v=981,col="pink")
abline(v=1343,col="pink")
abline(v=1346,col="pink")
abline(v=1708,col="pink")
abline(v=1711,col="pink")
abline(v=2073,col="pink")
abline(v=2076,col="pink")
data<-data.frame(NYE_day,July4_before,July4_day,LDAfter,TGeve, XmasEve)
names<-c("SummerInt", "July4th", "LaborDay", "ThxGvng", "Xmas","NYE")
colnames(data)<-names
legend('topleft', names(data), 
       lty=1, col=c('red', 'purple','pink','yellow', 'green','cyan'), bty='n', cex=.4)
?legend

#************************************************
#Second-- number of fatalities on a given day
plot(day_number, count,
     xlab="Day Number (01/01/10-12/31/15)",
     ylab="Total Number of Fatalities",
     main="Number Traffic Fatalities on Each Day in 2010-2015",
     col="blue",pch=".")
lines(lowess(count ~ day_number, f = 0.08), col = "black", lwd = 2)
#Account for the summer months
abline(v=151,col="red")
abline(v=273,col="red")
abline(v=151+365,col="red")
abline(v=273+365,col="red")
abline(v=151+365+366,col="red")
abline(v=273+365+366,col="red")
abline(v=151+365+366+365,col="red")
abline(v=273+365+366+365,col="red")
abline(v=151+365+366+365+365,col="red")
abline(v=273+365+366+365+365,col="red")
abline(v=151+365+366+365+365+365,col="red")
abline(v=273+365+366+365+365+365,col="red")

#Evaluate NYEs 12/31-1/1 each year
#2010 1/1 -- day1
#2010 12/31-- day365
#NYE_day<-c(365,365+365, 366+(365*2), 366+(365*3), 366+(365*4), 366+(365*5))
#NY_day<-c(1,1+365, 1+(365*2), 1+366+(365*2), 1+366+(365*3), 1+366+(365*4))
#NYE_day
#NY_day
abline(v=1,col="cyan")
abline(v=365,col="cyan")
abline(v=366,col="cyan")
abline(v=730,col="cyan")
abline(v=731,col="cyan")
abline(v=1096,col="cyan")
abline(v=1097,col="cyan")
abline(v=1461,col="cyan")
abline(v=1462,col="cyan")
abline(v=1826,col="cyan")
abline(v=1827,col="cyan")
abline(v=1461,col="cyan")
abline(v=2191,col="cyan")

#Evaluate 4th of July
#2010 7/4 -- day185
#2010 7/5-- day186
#July4_before<-c(183,183+365, 183+(365+366), 183+366+(365*2), 183+366+(365*3), 183+366+(365*4))
July4_day<-c(185,185+365, 185+(365+366), 185+366+(365*2), 185+366+(365*3), 185+366+(365*4))
#July4_dayafter<-c(186,186+365, 186+(365+366), 186+366+(365*2), 186+366+(365*3), 186+366+(365*4))
#July4_before
#July4_day
#July4_dayafter
abline(v=183,col="purple")
abline(v=186,col="purple")
abline(v=548,col="purple")
abline(v=551,col="purple")
abline(v=914,col="purple")
abline(v=917,col="purple")
abline(v=1279,col="purple")
abline(v=1282,col="purple")
abline(v=1644,col="purple")
abline(v=1647,col="purple")
abline(v=2009,col="purple")
abline(v=2012,col="purple")


#Evaluate Thanksgiving
#2010 11/24 -- day328
#2010 11/25 -- day329
#2010 11/26-- day330
TGeve<-c(328,328+365, 328+(365+366), 328+366+(365*2), 328+366+(365*3), 328+366+(365*4))
TGdayafter<-c(330,330+365, 330+(365+366), 330+366+(365*2), 330+366+(365*3), 330+366+(365*4))
#TGeve
#TGdayafter
abline(v=328,col="yellow")
abline(v=330,col="yellow")
abline(v=693,col="yellow")
abline(v=695,col="yellow")
abline(v=1059,col="yellow")
abline(v=1061,col="yellow")
abline(v=1424,col="yellow")
abline(v=1426,col="yellow")
abline(v=1789,col="yellow")
abline(v=1791,col="yellow")
abline(v=2154,col="yellow")
abline(v=2156,col="yellow")


#Evaluate Christmas
#2010 12/24 -- day358
#2010 11/25 -- day359
#2010 11/26-- day360
XmasEve<-c(358,358+365, 358+(365+366), 358+366+(365*2), 358+366+(365*3), 358+366+(365*4))
XmasAfter<-c(360,360+365, 360+(365+366), 360+366+(365*2), 360+366+(365*3), 360+366+(365*4))
#XmanEve
#XmasAfter
abline(v=358,col="green")
abline(v=360,col="green")
abline(v=723,col="green")
abline(v=725,col="green")
abline(v=1089,col="green")
abline(v=1091,col="green")
abline(v=1454,col="green")
abline(v=1456,col="green")
abline(v=1819,col="green")
abline(v=1821,col="green")
abline(v=2184,col="green")
abline(v=2186,col="green")

#Evaluate Labor Day
#2010 9/6 -- day249
#2010 9/7 -- day250
LDBefore<-c(247,247+365, 247+(365+366), 247+366+(365*2), 247+366+(365*3), 247+366+(365*4))
#LD<-c(249,249+365, 249+(365+366), 249+366+(365*2), 249+366+(365*3), 249+366+(365*4))
LDAfter<-c(250,250+365, 250+(365+366), 250+366+(365*2), 250+366+(365*3), 250+366+(365*4))
#LDBefore
#LD
#LDAfter
abline(v=247,col="pink")
abline(v=250,col="pink")
abline(v=612,col="pink")
abline(v=615,col="pink")
abline(v=978,col="pink")
abline(v=981,col="pink")
abline(v=1343,col="pink")
abline(v=1346,col="pink")
abline(v=1708,col="pink")
abline(v=1711,col="pink")
abline(v=2073,col="pink")
abline(v=2076,col="pink")
data<-data.frame(NYE_day,July4_before,July4_day,LDAfter,TGeve, XmasEve)
names<-c("SummerInt", "July4th", "LaborDay", "ThxGvng", "Xmas","NYE")
colnames(data)<-names
legend('topleft', names(data), 
       lty=1, col=c('red', 'purple','pink','yellow', 'green','cyan'), bty='n', cex=.4)


