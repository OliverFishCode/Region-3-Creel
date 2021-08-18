# Title     : 2019 creel data clean up
# Objective : Creates descriptive statistics and creel calculation  based on 2019 Goldwater Report and department protocols
# Created by: doliver and aloubere
# Created on: 12/10/2020
setwd("C:\\Users\\doliver\\Desktop\\region_3_creel") # set file directory to folder where data is located
library(tidyverse) # Used for data manipulation, correction, and plotting (general QC utilities)
library(xlsx)

### User defined values ####
shift_length <- 3 # specify shift length in hours
Prob_2nd_SU <- .25 # probability that secondary sampling unit is selected
days_per_period <- data.frame(c( 18,12,
                                 18.5,12.5,
                                 20.5,10.5)) # listed in alphabetical order of day type within month based on 2019 goldwater report
colnames(days_per_period) <- "days_per_period" # labels column name

avg_trip_length <- 5.5 # based on average AZ angling trip from standard sampling manual

#### Run the data script if not done yet ####

if(!exists('Creel_Catch_Data')) source('data_merge_clean.R') # must clear environment to run this line

#### Drop fish, interviews and waterbodies that are not of interest ####

Creel_Catch_clean <- data.frame(Creel_Catch_Data[ Creel_Catch_Data$water_body %in% c("GL"), ]) # keeps listed waterbodies
Creel_Catch_clean <- data.frame(Creel_Catch_clean[ Creel_Catch_clean$species %in% c("onmy","lema","misa","icpu"), ]) # Keeps desired species
Creel_Catch_clean <- droplevels(subset(Creel_Catch_clean,
                                       !ind_id %in% c("43507bea-9f04-4ee5-a296-9b87c6ae461f","c264555d-97db-4523-b078-7197687e55f5",
                                                      "6e893a79-6231-46ba-97fe-3881544aa900","9d80eaf1-eb2b-4938-af8b-2e110debe0b4",
                                                      "cd86e216-88a0-4224-acb1-d572f3cfcd54","ce7350a1-3ebc-4c7e-9503-985790ba29d6",
                                                      "4afa4b4e-fbfd-465a-9a93-4b391d3b0ed1","8c7c156a-2ed6-40b1-a0df-f9bcf4203b07",
                                                      "8e036855-7697-48f5-93dd-c555aa7a369e","aaf3165e-a277-4b89-9a8b-4f5fa6007d02",
                                                      "f8a6c2cb-6cfc-4451-bcdf-86bd0ad1ac25","27a40112-94cb-455f-ba6c-595f2996a63e",
                                                      "cacb8938-e45f-4311-9d99-9eabdcde7dc1","5452ec29-e1b8-43ea-8245-925758873825",
                                                      "5f9e1de2-7ac7-43e6-84bf-c3ac5441d2c5","497f0a1f-8118-466a-b00d-cad81a6daab2",
                                                      "c81ffcf2-98f0-463e-805c-dd95642c8876","aa52747f-da03-453d-95de-2a1132f7bc7a",
                                                      "15fd957d-f7aa-49e0-8c6b-ff88307c9134","803930be-56da-4fdf-8225-e05dcebf7807",
                                                      "1fec8a9c-1799-4e98-b8b1-8be398fcf7eb","cbae633d-3551-4c25-bca1-7f666aa0d709"))) # drop unwanted interviews based on ID, currently has place holder values.

#### Descriptive Statistics based on Goldwater 2019 Report ####

# Summerise satisfaction scores
Satisfaction_Scores <- Creel_Catch_clean[!duplicated(Creel_Catch_clean$ind_id), ] #removes duplicates created from having multiple species per interview
Satisfaction_Scores <- summarise(group_by(Satisfaction_Scores,sat),
                          raw_count = n()) # get count by satisfaction score
Satisfaction_Scores <- droplevels(Satisfaction_Scores[-which(Satisfaction_Scores$sat == "none"),]) %>% 
  mutate(percent = (raw_count/sum(raw_count))*100) # summarise satisfaction scores and drop non-respones
colnames(Satisfaction_Scores) <- c("Satisfaction", "Raw_Count", "Percent") # relabels column headers for satisfaction dataframe

# summerise catch preference of anglers
Preference <- Creel_Catch_clean[!duplicated(Creel_Catch_clean$ind_id), ] #removes duplicates created from having multiple species per interview
Preference <- summarise(group_by(Preference,pref),
                                 raw_count = n()) # get count by satisfaction score
Preference <- droplevels(Preference[-which(Preference$pref == "missing"),]) %>% 
  mutate(percent = (raw_count/sum(raw_count))*100) # summarise preference and drop non-respones
colnames(Preference) <- c("Preference", "Raw_Count", "Percent") # relabels column headers for satisfaction dataframe

# summerise county of origin
County <- Creel_Catch_clean[!duplicated(Creel_Catch_clean$ind_id), ] #removes duplicates created from having multiple species per interview
County <- summarise(group_by(County,county,az_resid),
                        raw_count = n()) # get count by satisfaction score
County$county <- ifelse(County$az_resid == 'no', 'out_of_state', County$county)# combines county and out of state designation under same variable
County$county[is.na(County$county)] <- 'none' # replace NA with none category
County <- data.frame(subset(County, select = -c(az_resid))) # # drops az_resid variable
County <- droplevels(County[-which(County == 'none'),]) %>% 
  mutate(percent = (raw_count/sum(raw_count))*100) # summarise county of origin countand drop non-respones
colnames(County) <- c("County", "Raw_Count", "Percent") # relabels column headers for satisfaction dataframe

# Summerise number of surveys by sampling units
Days_By_2ndUnit <- Creel_Catch_clean[!duplicated(Creel_Catch_clean$date), ] #removes duplicates created from having multiple species per interview per day
Days_By_2ndUnit <- data.frame(summarise(group_by(Days_By_2ndUnit,day,time_period),
                                 raw_count = n())%>% 
  mutate(percent = (raw_count/sum(raw_count))*100))# get count of surveys by secondary unit and turn to percent
Days_by_primeUnit <- Creel_Catch_clean[!duplicated(Creel_Catch_clean$date), ] # removes multiple surveys per day
Days_by_primeUnit <- data.frame(summarise(group_by(Days_by_primeUnit,day),
                             raw_count = n())%>% 
  mutate(percent = (raw_count/sum(raw_count))*100)) # gets counts and percents

# Summerise mean number of anglers by sampling unit
anglers_By_2ndUnit <- Creel_Catch_clean[!duplicated(Creel_Catch_clean$date), ] # removes multiple surveys per day
anglers_By_2ndUnit <- data.frame(summarise(group_by(anglers_By_2ndUnit,day,time_period),
                                           mean_count = mean(mean_daily_angler)))%>% 
  mutate(percent = (mean_count/sum(mean_count))*100) # gets counts and percents
anglers_By_primeUnit <- Creel_Catch_clean[!duplicated(Creel_Catch_clean$date), ] # removes multiple surveys per day
anglers_By_primeUnit <- data.frame(summarise(group_by(anglers_By_primeUnit,day),
                                           mean_count = mean(mean_daily_angler)))%>% 
  mutate(percent = (mean_count/sum(mean_count))*100)# gets counts and percents

# Estimate effort in angling hours and days
effort <- Creel_Catch_clean[!duplicated(Creel_Catch_clean$date), ] # removes multiple surveys per day
effort$shift_length <- shift_length # shift length value defined above
effort$period_effort <- effort$shift_length*effort$mean_daily_angler # estimates period effort
effort$daily_effort <- effort$period_effort/Prob_2nd_SU # daily effort calculation for AZ standardized fish sampling manual
effort <- data.frame(summarise(group_by(effort,month,day),
                                        mean_daily_effort = mean(daily_effort))) # gets counts
effort <- cbind(effort,days_per_period)
effort$total_effort_hour <- effort$mean_daily_effort*effort$days_per_period # total effort in angling hours
effort$total_effort_day <- effort$total_effort_hour/avg_trip_length # angler use days as defined by standard methods

# SUmmerises the number of fish caught during surrveys
Caughtkept_counts_species <- data.frame(summarise(group_by(Creel_Catch_clean,month,species),
                                          catch_count = sum(caught),
                                          kept_count = sum(kept))) # gets counts of caught and kept by species by month


#### Estimate catch and harvest per unit effort and total catch and kept by primary unit within month ####

# estimate daily hpue and cpue by day across all anglers and species
ind_CatchNHarvest_rate <- data.frame(drop_na(unique(summarise(group_by(Creel_Catch_clean,ind_id,month, day),
                                                              catch_count = sum(caught),
                                                              kept_count = sum(kept),
                                                              hours = hours,
                                                              date = date)))) %>% 
  mutate(cpue = catch_count/hours, hpue = kept_count/hours)# calculates daily cpue and hpue by angler across species

daily_CatchNHarvest_rate <- data.frame(drop_na(unique(summarise(group_by(ind_CatchNHarvest_rate,date,month, day),
                                                              mean_daily_cpue = mean(cpue),
                                                              mean_daily_hpue = mean(hpue),
                                                              date = date))))# calculates cpue and hpue by date across species and anglers

# esimate effort
CatchNHarvest_effort <- Creel_Catch_clean[!duplicated(Creel_Catch_clean$date), ] # removes multiple surveys per day
CatchNHarvest_effort$shift_length <- shift_length # shift length value defined above
CatchNHarvest_effort$period_effort <- CatchNHarvest_effort$shift_length*CatchNHarvest_effort$mean_daily_angler # estimates period effort
CatchNHarvest_effort$daily_effort <- CatchNHarvest_effort$period_effort/Prob_2nd_SU #estimates daily effort

# combine effort dataframe and daily hpue/cpue dataframe and make final creel calculations
Daily_CatchNHarvest <- merge(daily_CatchNHarvest_rate, CatchNHarvest_effort[,c('date','daily_effort')], by = c("date"), all.x = TRUE, all.y = FALSE) # merge daily effort and cpue and hpue values
Daily_CatchNHarvest$daily_catch <- Daily_CatchNHarvest$mean_daily_cpue*Daily_CatchNHarvest$daily_effort # Estimates daily total catch
Daily_CatchNHarvest$daily_kept <- Daily_CatchNHarvest$mean_daily_hpue*Daily_CatchNHarvest$daily_effort  # Estimates daily total kept
HPUE_CPUE <- summarise(group_by(Daily_CatchNHarvest,month, day),
                       mean_caught = mean(daily_catch),
                       mean_kept = mean(daily_kept),
                       hpue = mean(mean_daily_hpue),
                       cpue = mean(mean_daily_cpue)) # estimate mean caught, mean kept, cpue and hpue by primary sampling unit within month

HPUE_CPUE$days_per_period <- days_per_period$days_per_period # adds total days in period
HPUE_CPUE$total_harvest <- HPUE_CPUE$mean_kept*HPUE_CPUE$days_per_period  # estimates total harvest
HPUE_CPUE$total_caught <- HPUE_CPUE$mean_caught*HPUE_CPUE$days_per_period # estimates total caught   
HPUE_CPUE <- data.frame(HPUE_CPUE)

### write out relevant dataframes ####
write.xlsx(HPUE_CPUE, file = "Creel_analysis_output.xlsx",
           sheetName = "HPUE_CPUE_total_return", append = FALSE) 
write.xlsx(Satisfaction_Scores, file = "Creel_analysis_output.xlsx",
           sheetName = "Satisfaction_Scores", append = TRUE)
write.xlsx(Preference, file = "Creel_analysis_output.xlsx",
           sheetName = "Preference", append = TRUE)  
write.xlsx(Preference, file = "Creel_analysis_output.xlsx",
           sheetName = "County", append = TRUE)  
write.xlsx(Days_By_2ndUnit, file = "Creel_analysis_output.xlsx",
           sheetName = "surveys_per_2nd", append = TRUE)  
write.xlsx(Days_by_primeUnit, file = "Creel_analysis_output.xlsx",
           sheetName = "surveys_per_prime", append = TRUE)  
write.xlsx(anglers_By_2ndUnit, file = "Creel_analysis_output.xlsx",
           sheetName = "anglers_per_2nd", append = TRUE)  
write.xlsx(anglers_By_primeUnit, file = "Creel_analysis_output.xlsx",
           sheetName = "anglers_per_prime", append = TRUE) 
write.xlsx(effort, file = "Creel_analysis_output.xlsx",
           sheetName = "effort", append = TRUE) 
write.xlsx(Caughtkept_counts_species, file = "Creel_analysis_output.xlsx",
           sheetName = "Caughtkept_counts", append = TRUE) 


   
  