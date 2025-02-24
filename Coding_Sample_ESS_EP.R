
#################################################################
################    READING IN THE DATA     #####################
#################################################################
EP2019 <- read_dta('/Users/christine/Dropbox/ESS/ZA7581_v2-0-1.dta')

# a copy for myself 
PBsub_EP2019 <- EP2019

# renaming Birth Yr column to be usable in subset 
colnames(PBsub_EP2019)[colnames(PBsub_EP2019) == "D4_1"] <- "D4"

# renaming native & foreign-born 
PBsub_EP2019 <- PBsub_EP2019 %>% 
  mutate(D4a = case_when (
    D4a == 2 ~ "1",
    D4a == 5 ~ "1",
    D4a == 12 ~ "1",
    D4a == 18 ~ "1",
    D4a == 29 ~ "0",))



#################################################################
# subset data for ALL COMPULSORY COUNTRIES (5 in total)
# BE (1056); REMOVED-(not Compulsory in 2019)-BG (1110); CY (1196); GR (1300); LX (1442)   
# for questions on demographics of voters in EP 2019
#################################################################

comp_PBsub_EP2019 <- PBsub_EP2019[PBsub_EP2019$countrycode %in% c("1056", "1110","1196", "1300", "1442"), c("countrycode", "D3", "EDU", "hAge","D4a", "D4a_96_other", "D6", "D7", "D8", "D9", "Q18_2","Q4", "Q5", "Q6", "Q7", "Q9", "Q15", "WGT4")]

# cleaning data up
comp_PBsub_EP2019[comp_PBsub_EP2019 == 98] <- NA
comp_PBsub_EP2019[comp_PBsub_EP2019 == 99] <- NA

#dropped rows with genders labeled as 'other"
comp_PBsub_EP2019$D3[comp_PBsub_EP2019$D3==3] <- NA

#dropped rows with "97" which stands for REFUSED to give info 
comp_PBsub_EP2019$EDU[comp_PBsub_EP2019$EDU==97] <- NA

# cleaning up "D6" variable *D6 = employment status* ::: turning refusals, idk, and no answers into NAs 
comp_PBsub_EP2019 <- comp_PBsub_EP2019 %>%
  mutate(D6 = case_when (
    D6 == 1 ~ 1,
    D6 == 2 ~ 1,
    D6 == 3 ~ 1,
    D6 == 4 ~ 0,
    D6 == 5 ~ 0,
    D6 == 6 ~ 0,
    D6 == 7 ~ 0,))

# FOREIGN BORN CITIZENS WHO SPOILED THEIR BALLOT = total population of 12 
# clean for Q7 - make it into a binary where 96 = spoiled votes (1); everything else = NOT (0)
comp_PBsub_EP2019$Q7 <- ifelse(comp_PBsub_EP2019$Q7 ==96, 1, 0)

# clean for those who did vote!!!  dummy variable for voting (1 = did vote & 0 = didn't vote)    *dummy indicates binary*
comp_PBsub_EP2019$Q6 <- ifelse(comp_PBsub_EP2019$Q6 ==1, 1, 0)

# CLEAN for ONLY FOREIGN BORN CITIZENS  = total population of 1284
im_comp_PBsub_EP2019 <- comp_PBsub_EP2019[comp_PBsub_EP2019$D4a != 1, ]

# CLEAN for ONLY NATIVE BORN CITIZENS
n_comp_PBsub_EP2019 <- comp_PBsub_EP2019[comp_PBsub_EP2019$D4a != 0, ]



#################################################################
# subset data for NON-COMPULSORY COUNTRIES (23 in total)
#################################################################
noncomp_PBsub_EP2019 <- PBsub_EP2019[PBsub_EP2019$countrycode %in% c("1040", "1119","1203", "1276", "1208", "1620","1642", "1705", "1703", "1724","1752", "1826", "1440","1442","1470", "1528", "1616","1428", "1380","1372", "1348", "1250", "1246", "1233"), c("countrycode", "D3", "EDU", "hAge","D4a", "D4a_96_other", "D6", "D7", "D8", "D9", "Q18_2", "Q4", "Q5", "Q6", "Q7", "Q9", "Q15", "WGT4")]

# cleaning data up
noncomp_PBsub_EP2019[noncomp_PBsub_EP2019 == 98] <- NA
noncomp_PBsub_EP2019[noncomp_PBsub_EP2019 == 99] <- NA

#dropped rows with genders labeled as 'other" 
noncomp_PBsub_EP2019$D3[noncomp_PBsub_EP2019$D3==3] <- NA

#dropped rows with "97" which stands for REFUSED to give info 
noncomp_PBsub_EP2019$EDU[noncomp_PBsub_EP2019$EDU==97] <- NA

# cleaning up "D6" variable *D6 = employment status* ::: turning refusals, idk, and no answers into NAs 
noncomp_PBsub_EP2019 <- noncomp_PBsub_EP2019 %>%
  mutate(D6 = case_when (
    D6 == 1 ~ 1,
    D6 == 2 ~ 1,
    D6 == 3 ~ 1,
    D6 == 4 ~ 0,
    D6 == 5 ~ 0,
    D6 == 6 ~ 0,
    D6 == 7 ~ 0,))

# CITIZENS WHO SPOILED THEIR BALLOT 
# clean for Q7 - make it into a binary where 96 = spoiled votes (1); everything else = NOT (0)
noncomp_PBsub_EP2019$Q7 <- ifelse(noncomp_PBsub_EP2019$Q7 ==96, 1, 0)

# clean for those who did vote!!!  dummy variable for voting (1 = did vote & 0 = didn't vote)  *dummy indicates binary*
noncomp_PBsub_EP2019$Q6 <- ifelse(noncomp_PBsub_EP2019$Q6 ==1, 1, 0)

# CLEAN for ONLY FOREIGN BORN CITIZENS 
im_noncomp_PBsub_EP2019 <- noncomp_PBsub_EP2019[noncomp_PBsub_EP2019$D4a != 1, ]

# CLEAN for ONLY NATIVE BORN CITIZENS 
n_noncomp_PBsub_EP2019 <- noncomp_PBsub_EP2019[noncomp_PBsub_EP2019$D4a != 0, ]
```



###############################################
##### MAKING MODELS & INTERACTION TERMS #######
###############################################

#FINAL: foreign born voters (Y= trust in EP ; x = age of voter )
m_im_comp_PBsub_EP2019 <- glm(Q18_2 ~ hAge + D3 + D6 + D8, data = im_comp_PBsub_EP2019, 
                              family = "gaussian", weight = WGT4)

# interaction between education and age among foreign born spoiled ballots in non comp
m1_im_noncomp_PBsub_EP2019 <- glm(Q18_2 ~ EDU*hAge + D3 + D6 + D8, data = im_noncomp_PBsub_EP2019, 
                                  family = "gaussian", weight = WGT4)

# interaction between education and age among foreign born spoiled ballots in compulsory 
m2_im_comp_PBsub_EP2019 <- glm(Q18_2 ~ EDU*hAge + D3 + D6 + D8, data = im_comp_PBsub_EP2019, 
                               family = "gaussian", weight = WGT4)


m3_im_comp_PBsub_EP2019 <- glm(Q18_2 ~ hAge*EDU + D3 + D6 + D8, data = im_comp_PBsub_EP2019, 
                               family = "gaussian", weight = WGT4)

# interaction between education and age among native born spoiled ballots in non compulsory 
m3_n_noncomp_PBsub_EP2019 <- glm(Q18_2 ~ EDU*hAge + D3 + D6 + D8, data = n_noncomp_PBsub_EP2019, 
                                 family = "gaussian", weight = WGT4)

# interaction between education and age among native born spoiled ballots in non compulsory 
m4_n_comp_PBsub_EP2019 <- glm(Q18_2 ~ EDU*hAge + D3 + D6 + D8, data = n_comp_PBsub_EP2019, 
                              family = "gaussian", weight = WGT4)


# Plotting Interaction Plots for all 4 models 
interact_plot(m1_im_noncomp_PBsub_EP2019, pred = "EDU", modx = "hAge", interval = TRUE,
              legend.main = "Education Level",
              main.title = "Spoiled Ballots by Foreign-born Voters in Non-Compulsory States",
              x.label = "Age Values", 
              y.label = "Predicted Probability of Trust in EP",
              x.lim = c(1,5), #need to figure out how to set X axis from 1-5
              colors = c('steelblue4', 'lightseagreen', 'deepskyblue2'))

interact_plot(m2_im_comp_PBsub_EP2019, pred = "EDU", modx = "hAge", interval = TRUE,
              legend.main = "Education Level",
              main.title = "Spoiled Ballots by Foreign-born Voters in Compulsory States",
              x.label = "Age Values",
              y.label = "Predicted Probability of Trust in EP",
              colors = c('steelblue4', 'lightseagreen', 'deepskyblue2'))

interact_plot(m3_n_noncomp_PBsub_EP2019, pred = "EDU", modx = "hAge", interval = TRUE,
              legend.main = "Education Level",
              main.title = "Spoiled Ballots by Native Voters in Non-Compulsory States",
              x.label = "Age Values",
              y.label = "Predicted Probability of Trust in EP",
              colors = c('steelblue4', 'lightseagreen', 'deepskyblue2'))

interact_plot(m4_n_comp_PBsub_EP2019, pred = "EDU", modx = "hAge", interval = TRUE,
              legend.main = "Education Level",
              main.title = "Spoiled Ballots by Native Voters in Compulsory States",
              x.label = "Age Values",
              y.label = "Predicted Probability of Trust in EP",
              colors = c('steelblue4', 'lightseagreen', 'deepskyblue2'))


# Regression tables to transfer over to Overleaf 
stargazer(m_im_comp_PBsub_EP2019, m1_im_noncomp_PBsub_EP2019)

stargazer(m1_im_noncomp_PBsub_EP2019, m3_n_noncomp_PBsub_EP2019)
stargazer(m2_im_comp_PBsub_EP2019, m4_n_comp_PBsub_EP2019)



