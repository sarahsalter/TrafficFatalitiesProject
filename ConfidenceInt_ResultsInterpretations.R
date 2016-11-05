#**********************************************************************************************************************
#**********************************************************************************************************************
#Here we evaluate the significant results & their confidence intervals for each model.
#**********************************************************************************************************************
#**********************************************************************************************************************

#********************************************
#(1): CellPhones
#********************************************
#A. Cell_AllBan Estimate 
  #1. Log Scale: -2.103e-01
  #2. Regular Scale: exp(-2.103e-01) #0.8103411
  #3. Interpretation: When observing states with similar population growth, young population percentage, median unemployment rate, vehicle miles traveled,
#                     income per capita, average temperature, and yearly snowfall indicator, we see that there is a 0.8103411 multiplicative change 
#                     in the number of occuring traffic fatalities in 2015 (for every 100,000 people in a given states population)
#                     for states that have a ban against hand-held devices and texting compared to states that do not ban both.  
#                     
#                     In other words, in 2015 we see that states that do not have a ban against both (when controlling for all other variables in the model),
#                     experience roughly 430.09 fatalities per every 100,000 people in the population.  On the other hand, in 2015 states that ban against both
#                     texting and handheld cellphone use (when controlling for all other variables in the model), experience roughly 348.5215 fatalities per every
#                     100,000 people in the population.  Thus, assuming all confounders are accounted for and that other laws do not affect the at-risk population  
#                     pertaining to cellphone laws, we see that there is an assocation between traffic fatalitiy reduction when a state observes and enforces the
#                     'strictest' cellphone laws-- banning both handheld cellphone use and texting while driving.  

#B. Cell_YoungAllBan Estimate
  #1. Log Scale: -2.694e-01
  #2. Regular Scale: exp(-2.694e-01) #0.7638377
  #3. Interpretation: When observing states with similar population growth, young population percentage, median unemployment rate, vehicle miles traveled,
#                     income per capita, average temperature, and yearly snowfall indicator, we see that there is a 0.7638377 multiplicative change 
#                     in the number of occuring traffic fatalities in 2015 (for every 100,000 people in a given states population)
#                     for states that have some type of cellphone ban for 'young drivers', in which young is defined uniquely for each states but often
#                     represents (learners permit age - restricted license age).
#                     
#                     In other words, in 2015 we see that states that do not have a some type of cellphone ban in place for young drivers
#                     (when controlling for all other variables in the model), experience roughly 430 fatalities per every 100,000 people in the population.  
#                     On the other hand, in 2015 states that have a cellphone ban for young drivers (when controlling for all other variables in the model), 
#                     experience roughly 329 fatalities per every 100,000 people in the population.  Thus, assuming all confounders are accounted for 
#                     and that other laws do not affect the at-risk population pertaining to cellphone laws in the young population, we see that there is an 
#                     assocation between traffic fatalitiy reduction when a state has this law in effect.  

#C. Cell_AllBan Confidence Interval (Regular Scale) 
  #1. exp((-2.103e-01)+c(-1,1)*qt(0.975,49)*1.219e-01) #[0.6342785, 1.0352751]
  #2. Note: Because the 95% confidence interval contains 1, there is a chance there is no change at all. Since the effects here are multiplicative.

#D. Cell_YoungAllBan Confidence Interval (Regular Scale)
  #1. exp((-2.694e-01)+c(-1,1)*qt(0.975,49)*1.132e-01) #[0.6084236, 0.9589502]
  #2. Recall this is a dicotomous variable. 1 is coded as a state has the law in place.

#E. Additional comments: Intuitively it makes sense that states that ban cell-phone use while driving and enforce reprecussions for violator against this
#   law would have less fatal traffic accidents than states than do not hold their citizens as accountable.  

#********************************************
#(2): Seatbelts
#********************************************
#A. SB_Primary_Factor Estimate 
  #1. Log Scale: -2.539e-01
  #2. Regular Scale: exp(-2.539e-01) #0.7757694
  #3. Interpretation: When observing states with similar population growth, young population percentage, median unemployment rate, vehicle miles traveled,
#                     income per capita, average temperature, and yearly snowfall indicator, we see that there is a 0.7757694 multiplicative change 
#                     in the number of occuring traffic fatalities in 2015 (for every 100,000 people in a given states population)
#                     for states that have some a primary seatbelt law, in which officers can issue a ticket solely for a citizen not wearing their seatbelt,
#                     rather than just a secondary offense.
#                     
#                     In other words, in 2015 we see that states that do not have a primary seatbelt law 
#                     (when controlling for all other variables in the model), experience roughly 726 fatalities per every 100,000 people in the population.  
#                     On the other hand, in 2015 states that have a primary seatbelt law (when controlling for all other variables in the model), 
#                     experience roughly 563 fatalities per every 100,000 people in the population.  Thus, assuming all confounders are accounted for 
#                     and that other laws do not affect the at-risk population pertaining to primary seatbelt laws, we see that there is an 
#                     assocation between traffic fatalitiy reduction when a state has this law in effect.  


#B. SB_Primary_Factor Confidence Interval (Regular Scale) 
  #1. exp((-2.539e-01)+c(-1,1)*qt(0.975,49)*1.059e-01) #[0.6270594, 0.9597466]


#********************************************
#(3): Child Restraints--- NOTHING SIGNIFICANT HERE!
#********************************************



#********************************************
#(4): Young Driver
#********************************************
#A. LP_Age Estimate 
  #1. Log Scale: -1.831e-01
  #2. Regular Scale: exp(-1.831e-01) #0.8326849
  #3. Interpretation: When observing states with similar population growth, young population percentage, median unemployment rate, vehicle miles traveled,
#                     income per capita, average temperature, and yearly snowfall indicator, we see that there is a 0.8326849 multiplicative change 
#                     in the number of occuring traffic fatalities in 2015 in (for every 100,000 people in a given states population)
#                     for every 1 unit (1 year) increase in a state's license permit age. 
#                     
#                     In other words, in 2015 we see that states require young drivers young drivers to be older 
#                     (when controlling for all other variables in the model), experience roughly -.183 times less fatalities per every 100,000 people in the population.  
#                     Thus, assuming all confounders are accounted for and that other laws do not affect the at-risk population pertaining to learner's permit ages, we see that there is an 
#                     assocation between traffic fatalitiy reduction when states require young drivers to wait until at least age 16 to obtain their learners permits compared to states that 
#                     allow their young drivers to get their learner's permit as young as 14-15 years of age.  


#B. LP_Age Confidence Interval (Regular Scale) 
  #1. exp((-1.831e-01)+c(-1,1)*qt(0.975,49)*9.693e-02) #[0.6853072 1.0117567]


#********************************************
#(5): Restricted License Driver--- NOTHING IS SIGNIFICANT HERE!
#********************************************




#********************************************
#(6): Older Driver--- NOTHING IS SIGNIFICANT HERE!
#********************************************



#********************************************
#(7): Speedlimits & Aggressive Driving
#********************************************
#A. MaxSL_Urban Estimate 
  #1. Log Scale: 1.081e-02
  #2. Regular Scale: exp(1.081e-02) #1.010869
#3. Interpretation: When observing states with similar population growth, young population percentage, median unemployment rate, vehicle miles traveled,
#                     income per capita, average temperature, and yearly snowfall indicator, we see that there is a 1.010869 multiplicative increase 
#                     in the number of occuring traffic fatalities in 2015 (for every 100,000 people in a given states population)
#                     for every 1 unit (1mph) increase in a state's maximum speedlimit for urban roadways. Although this increase is fairly small, every potential life 
#                     that can be saved matters. Thus, assuming all confounders are accounted for and that other laws do not affect the at-risk population pertaining to maximum urban road speed limits, 
#                     we see that there is an assocation between traffic fatalitiy increase for urban roadways with higher maximum speedlimits. This could be contributed to the fact that
#                     urban roads have the potential to be very congested, thus at greater risk for potential traffic related fatalties.  
                    
#                     In other words, states that have lower maximum speed limits for their 'urban' roads there exists an association of less traffic related
#                     fatalities occuring in those states.



#B. MaxSL_OtherRd Estimate 
  #1. Log Scale: 2.060e-02
  #2. Regular Scale: exp(2.060e-02) #1.020814
#3. Interpretation:   Similarly, when observing states with similar population growth, young population percentage, median unemployment rate, vehicle miles traveled,
#                     income per capita, average temperature, and yearly snowfall indicator, we see that there is a 1.020814 times more traffic fatalities in 2015 
#                     (for every 100,000 people in a given states population) for every 1 unit (1mph) increase in a state's maximum speedlimit for other roadways (all roads except urban, rural, and access roads). 
#                     
#                     In other words, states that have lower maximum speed limits for their 'other roads' there exists an association of less traffic related
#                     fatalities occuring in those states.

#                     Thus, assuming all confounders are accounted for and that other laws do not affect the at-risk population pertaining to maximum 'other' road speed limits, 
#                     we see that there is an assocation between traffic fatalitiy increase for roadways (that are not urban, rural, or access roads) with higher maximum speedlimits. 
#                     This could be contributed to the fact that drivers traveling at higher speeds are at greater risk for severe injuries compare to drivers that travel at lower speeds. 
#               

#C. MaxSL_Urban Confidence Interval (Regular Scale) 
  #1. exp((1.081e-02)+c(-1,1)*qt(0.975,49)*5.717e-03) #[0.9993215, 1.0225492]

#D. MaxSL_OtherRd Confidence Interval (Regular Scale) 
  #1. exp((2.060e-02)+c(-1,1)*qt(0.975,49)*7.569e-03) #[1.005404, 1.036459]


#********************************************
#(8): Helmets
#********************************************
#A. BikeHelmetAge Estimate 
  #1. Log Scale: -2.084e-02 
  #2. Regular Scale: exp(-2.084e-02) #0.9793757
  #3. Interpretation: When observing states with similar population growth, young population percentage, median unemployment rate, vehicle miles traveled,
#                     income per capita, average temperature, and yearly snowfall indicator, we see that there is a 0.9793757 times less traffic fatalities in 2015 
#                     (for every 100,000 people in a given states population) for each 1 unit (1 year) increase in a state's maximum required age that bicycle helmets must be worn. 
#                     
#                     In other words, states that require children to wear bicycle helmets for longer periods of time (until older ages) have an association of less traffic related
#                     fatalities occuring in those states.  For instance, states that enfore the law that children wear bicycle helmets until age 16 have an association of less traffic 
#                     related deaths than states that only enfore children wear bicycle helmets up until age 14.  


#B. BikeHelmetAge Confidence Interval (Regular Scale) 
  #1. exp((-2.084e-02)+c(-1,1)*qt(0.975,49)*9.265e-03) #[0.9613096 0.9977812]



#********************************************
#(9): DUI
#********************************************
#A. DUI_IgnitionFO_Factor Estimate 
  #1. Log Scale: -1.577e-01
  #2. Regular Scale: exp(-1.577e-01) #0.854106
  #3. Interpretation: When observing states with similar population growth, young population percentage, median unemployment rate, vehicle miles traveled,
#                     income per capita, average temperature, and yearly snowfall indicator, we see that there is a 0.854106 multiplicative change 
#                     in the number of occuring traffic fatalities in 2015 (for every 100,000 people in a given states population)
#                     for states that require an ignition interlock device after a citizen's first DUI offense, compared to states that do not.  

#                     In other words, in 2015 we see that states that do not have requires an ignition interlock device afer a citizen's first DUI offense
#                     (when controlling for all other variables in the model), experience roughly 596 fatalities per every 100,000 people in the population.  
#                     On the other hand, in 2015 states that require ignition interlocks after a first DUI offense (when controlling for all other variables in the model), 
#                     experience roughly 509 fatalities per every 100,000 people in the population.  Thus, assuming all confounders are accounted for 
#                     and that other laws do not affect the at-risk population pertaining to DUI offenders, we see that there is an 
#                     assocation between traffic fatalitiy reduction when a state has this law in effect.  


#B. MarjMedicalUse Estimate 
  #1. Log Scale: -2.148e-01
  #2. Regular Scale: exp(-2.148e-01) #0.8067028
  #3. Interpretation: When observing states with similar population growth, young population percentage, median unemployment rate, vehicle miles traveled,
#                     income per capita, average temperature, and yearly snowfall indicator, we see that there is a 0.8067028 multiplicative change 
#                     in the number of occuring traffic fatalities in 2015 (for every 100,000 people in a given states population)
#                     for states that have legalized the use of medical marijuana.
#                     
#                     In other words, in 2015 we see that states that have legalized marijuana for recreational purpose or not at all
#                     (when controlling for all other variables in the model), experience roughly 596 fatalities for every 100,000 people in the population.  
#                     On the other hand, in 2015 states that legalized marijuana (when controlling for all other variables in the model), 
#                     experience roughly 481 fatalities per every 100,000 people in the population.  Thus, assuming all confounders are accounted for 
#                     and that other laws do not affect the at-risk population pertaining to primary marijuana laws, we see that there is an 
#                     assocation between traffic fatalitiy reduction when a state has this law in effect.  This could possibly be contributed to the fact that
#                     maybe citizens who need medical marijuana and use it in a mature and physician approved manner have better overall spirits (when sober) and
#                     thus drive in a more responsible and less irritable and distracted way. Many states with medical marijuana prescribed it to people
#                     that suffer with high anxiety and insomnia. Tired driving is believed to be just as dangerous as driving impaired.  Thus, states with 
#                     legalized marijuana for medicinal purposes may have less tired drivers on the road and thus less risk for fatal motor-vehicles accidents.  


#C. DUI_IgnitionFO_Factor Confidence Interval (Regular Scale) 
  #1. exp((-1.577e-01)+c(-1,1)*qt(0.975,49)*8.693e-02) #[0.7172059, 1.0171376]

#D. MarjMedicalUse Confidence Interval (Regular Scale) 
  #1. exp(( -2.148e-01)+c(-1,1)*qt(0.975,49)*1.000e-01) #[0.6598402, 0.9862529]



#********************************************
#(9): Cameras--- NOTHING IS SIGNIFICANT!
#********************************************
