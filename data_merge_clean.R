# Title     : 2019 creel data clean up
# Objective : Clean, merge and structure 2019 survey 123 creel data
# Created by: doliver and aloubere
# Created on: 12/10/2020
setwd("C:\\Users\\doliver\\Desktop\\region_3_creel") # set file directory to folder where data is located
library(tidyverse) # Used for data manipulation, correction, and plotting (general QC utilities)
library(readxl) # Used to read excel files
library(StandardizeText) # Utilities for text manipulation (i.e., remove or adding capital lettering easily)
library(reshape2) # used for manipulating data set from wide to long format

#### User defined Functions ####
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
} # used to replace blanks with NAs

#### Import data ####
survey123_global <- read.csv("data\\Region_3_Creel_0.csv") # Import daily common data across interviews, .csv file name
survey123_interview <- read.csv("data\\CreelSurvey_1.csv") # Import interviews, .csv file name

#### Remove unwanted variables and clean up names ####
survey123_global <- survey123_global[, which(names(survey123_global) %in% c("GlobalID", "Body.of.Water.",
                                                                            "Date.", "Start.Time.", "Shore.Count.1.",
                                                                            "Boat.Count.1.", "Day.Type.", "Time.of.Day.",
                                                                            "Days.Following.Stocking.", "Shore.Count.2.",
                                                                            "Boat.Count.2.", "End.Time.", "x", "y"))] # keep only wanted variables
colnames(survey123_global) <- c("globalid", "water_body", "date", "start_time", "shore_count_one", "boat_count_one",
                                "day", "time_period", "dps", "shore_count_two", "boat_count_two", "end_time", "x", "y") # rename columns

survey123_interview <- survey123_interview[, -which(names(survey123_interview) %in% c("ï..ObjectID", "PONI_YN", "LEMA_YN",
                                                                                      "SAFO_YN", "SATR_YN", "AMNA_YN",
                                                                                      "CYCA_YN", "ICPU_YN", "PYOL_YN",
                                                                                      "LECY_YN", "MISA_YN", "ONMY_YN",
                                                                                      "LEMI_YN", "MIDO_YN", "MOSA_YN",
                                                                                      "OTHER_YN", "NONE_YN", "CreationDate",
                                                                                      "Creator", "EditDate", "Editor",
                                                                                      "Species.Identification.", "Other.Caught.",
                                                                                      "Other.Kept."))] # removes unwanted variables
colnames(survey123_interview) <- c("obj_id","ind_id","fishing_loc", "hours", "trip_complete", "az_resid", "county", "sat", "pref", "success",
                                   "c_poni", "k_poni", "c_lema", "k_lema", "c_safo", "k_safo", "c_satr", "k_satr", "c_amna",
                                   "k_amna", "c_cyca", "k_cyca", "c_icpu", "k_icpu", "c_pyol", "k_pyol", "c_lecy", "k_lecy",
                                    "c_misa", "k_misa", "c_onmy", "k_onmy", "c_lemi", "k_lemi", "c_mido", "k_mido", "c_mosa",
                                    "k_mosa", "globalid") # rename columns

#### Merge creel datasets ####

Creel_data <- data.frame(merge(x = survey123_global, y = survey123_interview, by = "globalid", all.y = TRUE))
Creel_data <- Creel_data %>% mutate_all(empty_as_na)
#### Covert catch data from wide to long format and make new variable for month and catch/kept and relabel value as count ####
Creel_Catch_Data <- melt(Creel_data, id.vars = c("obj_id","ind_id","fishing_loc", "hours", "trip_complete", "az_resid", "county", "sat",
                                     "pref", "success", "globalid", "water_body", "date", "start_time", "shore_count_one",
                                     "boat_count_one", "day", "time_period", "dps", "shore_count_two", "boat_count_two",
                                     "end_time", "x", "y")) # convert wide to long
Creel_Catch_Data <- Creel_Catch_Data %>% separate(variable, c("catch_kept", "species"), "_") # create catch/kept variable
colnames(Creel_Catch_Data)[colnames(Creel_Catch_Data) == 'value'] <- 'count' # relabel value as count
Creel_Catch_Data$catch_kept <- recode(Creel_Catch_Data$catch_kept, c = "caught", k = "kept") # relabel levels of catch_kept
Creel_Catch_Data$date <- as.Date(Creel_Catch_Data$date, format = "%m/%d/%Y") # convert date from factor to date
Creel_Catch_Data$month <- months(Creel_Catch_Data$date) # extract month from date and make variable
Creel_Catch_Data$angler_count_one <- Creel_Catch_Data$boat_count_one + Creel_Catch_Data$shore_count_one
Creel_Catch_Data$angler_count_two <- Creel_Catch_Data$boat_count_two + Creel_Catch_Data$shore_count_two
Creel_Catch_Data$mean_daily_angler <- (Creel_Catch_Data$angler_count_one + Creel_Catch_Data$angler_count_two)/2
#### Fix Blanks and NA counts ####
Temp_dataframe <- Creel_Catch_Data %>% mutate_all(empty_as_na) # empty values give NA
Temp_dataframe$count[is.na(Temp_dataframe$count)] <- 0 # replace NA in catch with 0
Temp_dataframe <- summarise(group_by(Temp_dataframe, month, time_period,
                                        day,species,ind_id,catch_kept,county, water_body),
                               count = count,
                               hours = mean(as.numeric(hours)))
Temp_dataframe <- data.frame(spread(Temp_dataframe, key = catch_kept, value = count))# creates seperate columns for catch and kept within a species by interview
Creel_Catch_Data$sat[is.na(Creel_Catch_Data$sat)] <- 'none' # replace NA in catch with none cant match by ID if SAT is an NA
Creel_Catch_Data$pref[is.na(Creel_Catch_Data$pref)] <- 'missing' # replace NA in catch with missing cant match by ID if pref is an NA
Creel_Catch_Data <- unique(inner_join(Temp_dataframe, Creel_Catch_Data[, c('ind_id','sat', 'pref',
                                                               'az_resid', 'date',
                                                               'mean_daily_angler')], by = c('ind_id')))# adds  back in variables of interest

#### Fix Data types if needed ###
Creel_Catch_Data$sat = as.factor(Creel_Catch_Data$sat)
Creel_Catch_Data$date = as.character(Creel_Catch_Data$date)
### remove junk ###
remove(Temp_dataframe, survey123_interview,survey123_global,Creel_data)


