## Processing of Demographic and COVID data for Shiny App ##
# Code by David Gottlieb (@datatodavid) #

#### Libraries ####
library(Hmisc)
library(tidyverse)
library(magrittr)
library(rsconnect)
library(stringi)
library(lubridate)


#### Functions for Label Text Modification ####
to_string <- function(string_text) {
  deparse(substitute(string_text))
}
no_periods <- function(string_text) {
  gsub(".", " ", string_text, fixed=T)
}
yes_periods <- function(string_text) {
  gsub(" ", ".", string_text, fixed=T)
}
no_underscore <- function(string_text) {
  gsub("_", " ", string_text, fixed=T)
}
to_underscore <- function(string_text) {
  gsub(" ", "_", string_text, fixed=T)
}
no_CO <- function(string_text) {
  gsub("CO ", "COLORADO ", string_text, fixed=T)
}
no_Co <- function(string_text) {
  gsub("Co ", "Colorado ", string_text, fixed=T)
}
upper_Co <- function(string_text) {
  gsub("Co ", "CO ", string_text, fixed=T)
}
no_perc <- function(string_text) {
  gsub("Perc", "%", string_text, fixed=T)
}
long_perc <- function(string_text) {
  gsub("Perc", "Percentage", string_text, fixed=T)
}
long_avg <- function(string_text) {
  gsub("Avg", "Average", string_text, fixed=T)
}
avg_parentheses <- function(string_text) {
  gsub("5 Day Avg", "(5 Day Avg)", string_text, fixed=T)
}
per_parentheses <- function(string_text) {
  gsub("Per 100000", "Rate (Per 100000)", string_text, fixed=T)
}
rev_perc_mort <- function(string_text) {
  gsub("Mortality Perc", "% Mortality", string_text, fixed=T)
}
rev_perc_pos <- function(string_text) {
  gsub("Positive Tests Perc", "% Positive Tests", string_text, fixed=T)
}
frontspace <- function(string_text) {
  gsub("COVID", " COVID", string_text, fixed=T)
}
long_map_avg <- function(string_text) {
  gsub("Mean", "Average Value in Date Range", string_text, fixed=T)
}
long_map_max <- function(string_text) {
  gsub("Maximum", "Highest Value in Date Range", string_text, fixed=T)
}
long_map_latest <- function(string_text) {
  gsub("Latest", "Most Recent Value", string_text, fixed=T)
}


# This function helps restore clicking attributes for sidebar that are lost when adding subMenu items. #
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}
# Date functions for the County vs. State COVID Trend Analysis (Time Series)
bimonthly <- function(x) {
  x_range <- range(x, na.rm = TRUE)
  
  date_range <- c(
    floor_date(x_range[1], "month"),
    ceiling_date(x_range[2], "month")
  )
  monthly <- seq(date_range[1], date_range[2], by = "1 month")
  
  sort(c(monthly, monthly + days(14)))
}
weekly <- function(x) {
  x_range <- range(x, na.rm = TRUE)
  
  date_range <- c(
    floor_date(x_range[1], "month"),
    ceiling_date(x_range[2], "month")
  )
  monthly <- seq(date_range[1], date_range[2], by = "1 month")
  
  sort(c(monthly, monthly + days(7), monthly + days(14), monthly + days(21)))
}
rotate <- function(x) {
  t(apply(x, 2, rev))
}

setwd("C:/Users/gottl/Dropbox/Data Science/NYCDSA/Projects/Project 1 Shiny App/CO_COVID")


#######################  DEMOGRAPHIC DATA  #######################  

#### RWJ Demographic Data ####
COdataRWJ2020 = read.csv("2020 County Health Rankings Colorado Data Life Measures CLEANED 2 col header.csv", stringsAsFactors = F, header = T)

## Fixing the column names ##
names(COdataRWJ2020) = COdataRWJ2020[1, ]

COdataRWJ2020 = COdataRWJ2020[-1, ]

names(COdataRWJ2020) %<>%  
  gsub("(", "", ., fixed=T) %>% 
  gsub(")", "", ., fixed=T) %>% 
  make.names(.) %>% 
  gsub("...", ".", ., fixed=T) %>% 
  gsub("..", ".", ., fixed=T)

CORWJnames = names(COdataRWJ2020)

## These functions help add Demographic Measure to beginning of column name ##
for (i in c(44,50,63,91,95,97,104,112,122,135,144,147,169,171,184)) {
CORWJnames[i+1] = paste(CORWJnames[i], CORWJnames[i+1], sep=".")  }

for (j in c(173,205,227,231,250)) {
  for (i in c(j+1, j+2)) {
    CORWJnames[i] = paste(CORWJnames[j], CORWJnames[i], sep=".")  }}

for (j in c(5,24,28,32,36,40,46,52,58,65,85,116,124,139,150,186)) {
  for (i in c(j+1, j+2, j+3)) {
  CORWJnames[i] = paste(CORWJnames[j], CORWJnames[i], sep=".")  }}

for (i in c(176:185)) {
CORWJnames[i] = paste("Perc", CORWJnames[i], sep=".")  }

for (i in c(114:116)) {
CORWJnames[i] = paste(CORWJnames[i], "Age.25.to.44", sep=".")  }

CORWJnames %<>% 
  gsub("95Perc.CI.Low", "95LCI", .) %>% 
  gsub("95Perc.CI.High", "95HCI", .) %>% 
  gsub("X95LCI", "95LCI", .) %>% 
  gsub(".X95HCI", ".95HCI", ., fixed=T) %>% 
  gsub("Num.Associations", "Num.Social.Associations", ., fixed=T) %>% 
  gsub("Years.of.P", "YPLL.Years.of.P", ., fixed=T) %>% 
  gsub(".Hosp.", ".Hospitalization.", ., fixed=T) %>% 
  gsub("Median.Household", "Household", ., fixed=T) %>% 
  gsub("Household.In", "Median.Household.In", ., fixed=T) %>% 
  gsub("X20th.Percentile.Income", "Income.20th.Percentile" , ., fixed=T) %>% 
  gsub("X80th.Percentile.Income", "Income.80th.Percentile" , ., fixed=T) %>% 
  gsub("American.Indian", "AIAN.American.Indian" , ., fixed=T) %>%   
  gsub("Perc.Vaccinated", "Perc.Flu.Vaccinated", ., fixed=T) %>%  
  gsub("Segregation.Index", "Nonwhite.v.White.Segregation.Index", ., fixed=T) %>%
  gsub("Segregation.index", "Black.v.White.Segregation.Index", ., fixed=T) %>%  
  gsub("Average.Daily.PM2.5", "Average.Daily.Air.Pollution", ., fixed=T) %>% 
  gsub("Primary.Care.Physicians.Ratio", "Primary.Care.Patients.per.Physician", ., fixed=T) %>% 
  gsub("Mental.Health.Provider.Ratio", "Mental.Health.Patients.per.Provider", ., fixed=T) %>% 
  gsub("County", "COUNTY", .)

## Reassigning the cleaned names to the Dataset ##
names(COdataRWJ2020) = CORWJnames

## Cleaning up Data ##
COdataRWJ2020$Primary.Care.Patients.per.Physician = 
  gsub(":.*", "", COdataRWJ2020$Primary.Care.Patients.per.Physician)
COdataRWJ2020$Mental.Health.Patients.per.Provider = 
  gsub(":.*", "", COdataRWJ2020$Mental.Health.Patients.per.Provider)
COdataRWJ2020$Length.of.Life.Rank = 
  gsub("NR", NA, COdataRWJ2020$Length.of.Life.Rank)
COdataRWJ2020$Quality.of.Life.Rank = 
  gsub("NR", NA, COdataRWJ2020$Quality.of.Life.Rank)
COdataRWJ2020$Health.Behaviors.Rank = 
  gsub("NR", NA, COdataRWJ2020$Health.Behaviors.Rank)
COdataRWJ2020$Clinical.Care.Rank = 
  gsub("NR", NA, COdataRWJ2020$Clinical.Care.Rank)
COdataRWJ2020$Socio.Economic.Factors.Rank = 
  gsub("NR", NA, COdataRWJ2020$Socio.Economic.Factors.Rank)
COdataRWJ2020$Physical.Environment.Rank = 
  gsub("NR", NA, COdataRWJ2020$Physical.Environment.Rank)

## Selecting / Organizing Database Columns ##
COdataRWJ2020 = COdataRWJ2020[,-231:-233]

COdataRWJ2020 = COdataRWJ2020 %>% 
  select(., -State) %>% 
  mutate_at(., "COUNTY", toupper) 

COdataRWJ2020 = COdataRWJ2020 %>% 
  mutate(., 
         County.Overall.Sum.Rank =
           as.numeric(Length.of.Life.Rank) +
           as.numeric(Quality.of.Life.Rank) +
           as.numeric(Health.Behaviors.Rank) +
           as.numeric(Clinical.Care.Rank) +
           as.numeric(Socio.Economic.Factors.Rank) +
           as.numeric(Physical.Environment.Rank),
         County.Overall.Rank = 
           rank(County.Overall.Sum.Rank, 
                ties.method="min", na.last="keep"))

COdataRWJ2020[ ,-c(2,170)] = 
  lapply(COdataRWJ2020[ ,-c(2,170)], as.numeric)


# ####  ******OPTIONAL****** COdataRWJ2020 Subtables ####
# ## Rankings Only Table ##
# COdataRWJRank = COdataRWJ2020 %>% 
#   select(., COUNTY, ends_with("Rank"))
# #write.csv(names(COdataRWJRank), "names_COdataRWJRank.csv", row.names = F)
# 
# ## Simplified (Measures-only) Table ##
# COdataRWJ2020simp = COdataRWJ2020 %>% 
#   select(., -contains("95"), -contains("Num."), -FIPS, 
#          -matches("(Perc.*Score)"), -matches("(Rate.*Score)"), 
#          -matches("(Ratio.*Score)"), -matches("(Water.*Score)"), 
#          -ends_with("Rank"), -ends_with("Unhealthy.Days"), 
#          -Food.Environment.Index)
# 
# COdataRWJ2020simp = COdataRWJ2020simp %>% 
#   select(., COUNTY, Population, Population.Age.25.to.44, 
#          everything())
# #write.csv(names(COdataRWJ2020simp), "names_COdataRWJ2020simp.csv", row.names = F)
# 
# ## Simplified (Measures-only) + No Demo Breakdown Table ##
# COdataRWJ2020simp_no_demo = COdataRWJ2020simp %>% 
#   select(.,   -contains("e.AIAN"), -contains("e.Asian"), 
#          -contains("e.Black"), -contains("e.Hispanic"), 
#          -contains("e.White"), -contains("d.AIAN"), 
#          -contains("d.Asian"), -contains("d.Black"), 
#          -contains("d.Hispanic"), -contains("d.White"), 
#          -contains("y.AIAN"), -contains("y.Asian"), 
#          -contains("y.Black"), -contains("y.Hispanic"), 
#          -contains("y.White"))
# #names(COdataRWJ2020simp_no_demo)
# COdataRWJ2020simp_no_demo[,43:44]
# write.csv(names(COdataRWJ2020simp_no_demo), "names_COdataRWJ2020simp_no_demo.csv",row.names = F)
# 
# ## Z-Scores-only Table ##
# COdataRWJ2020_Zscores = COdataRWJ2020 %>% 
#   select(., COUNTY, contains("Z.Score"))



#### Race/Ethnicity Demographics Sub-Table ####

## Selecting Columns with Race/Ethnicity Breakdowns ##
COdataRWJ2020raceFULL = COdataRWJ2020 %>% 
  select(., COUNTY, contains("YPLL"), contains("Teen."), 
         contains("Preventable.H"), contains("Vaccinated"), 
         contains("in.Poverty"), contains("Injury.Death"), 
         contains("Drive.Alone"), contains("Household.Income"), 
         contains("AIAN"), contains("Asian"), contains("Black"), 
         contains("Hispanic"), contains("White"), 
         contains("Native"))

## Fixing Column Names ##
names(COdataRWJ2020raceFULL) %<>% 
  gsub(".95LCI", "_95LCI", ., fixed=T) %>%
  gsub(".95HCI", "_95HCI", ., fixed=T) %>%
  gsub(".Z.Score", "_Z.Score", ., fixed=T) %>%
  gsub("AIAN.American.Indian.Alaska.Native", "AIAN", ., fixed=T) %>% 
  gsub("Native.Hawaiian.Other.Pacific.Islander", "NHPI", ., fixed=T) %>% 
  gsub("YPLL.Years.of.Potential.Life.Lost.", "YPLL.", ., fixed=T) %>%
  gsub("Alone.to.Work", "Alone", ., fixed=T) %>% 
  gsub("Rate_95LCI", "Rate.ALL_95LCI", ., fixed=T) %>%
  gsub("Rate_95HCI", "Rate.ALL_95HCI", ., fixed=T) %>% 
  gsub("Rate_Z.Score", "Rate.ALL_Z.Score", ., fixed=T) %>% 
  gsub("ated_Z.Score", "ated.ALL_Z.Score", ., fixed=T) %>% 
  gsub("Poverty_95LCI", "Poverty.ALL_95LCI", ., fixed=T) %>%
  gsub("Poverty_95HCI", "Poverty.ALL_95HCI", ., fixed=T) %>% 
  gsub("Poverty_Z.Score", "Poverty.ALL_Z.Score", ., fixed=T) %>% 
  gsub("Alone_95LCI", "Alone.ALL_95LCI", ., fixed=T) %>%
  gsub("Alone_95HCI", "Alone.ALL_95HCI", ., fixed=T) %>% 
  gsub("Alone_Z.Score", "Alone.ALL_Z.Score", ., fixed=T) %>% 
  gsub("Income_95LCI", "Income.ALL_95LCI", ., fixed=T) %>%
  gsub("Income_95HCI", "Income.ALL_95HCI", ., fixed=T) %>% 
  gsub("Income_Z.Score", "Income.ALL_Z.Score", ., fixed=T)

## These functions help add "ALL" category and which are measures to column names ##
for (i in c(2,21,40,47,54,64,83,102)) {
  colnames(COdataRWJ2020raceFULL)[i] = paste(colnames(COdataRWJ2020raceFULL)[i], "ALL", sep=".") }


for (i in c(2,6,9,12,15,18,21,25,28,31,34,37,40, 42:47, 
            49:54, 58:64, 68,71,74,77,80,83,87,90,93,
            96,99,102,105,108,111,114,117, 120:133)) {
  colnames(COdataRWJ2020raceFULL)[i] = 
    paste(colnames(COdataRWJ2020raceFULL)[i], "Measure", sep="_") }

for (i in 120:133) {
  colnames(COdataRWJ2020raceFULL)[i] = 
    paste("Demographics", colnames(COdataRWJ2020raceFULL)[i], sep=".") }

COdataRWJ2020raceFULL[,-1] = lapply(COdataRWJ2020raceFULL[,-1], as.numeric)

COdataRWJ2020race = 
  COdataRWJ2020raceFULL %>%  select(., -contains("Num."))

## Expanding Dataset ##
COdataRWJ2020race = COdataRWJ2020race %>%
  pivot_longer(
   cols = contains("_"), 
    names_to = c("Category", "Test.Type"),
    names_sep = "_",
    values_to = "Value",
    values_drop_na = TRUE)

## Cleaning Category Names ##
COdataRWJ2020race$Category %<>%    
  gsub(".ALL", "_ALL", ., fixed=T) %>% 
  gsub(".AIAN", "_AIAN", ., fixed=T) %>% 
  gsub(".Asian", "_Asian", ., fixed=T) %>% 
  gsub(".Black", "_Black", ., fixed=T) %>% 
  gsub(".Hispanic", "_Hispanic", ., fixed=T) %>% 
  gsub(".White", "_White", ., fixed=T) %>% 
  gsub(".NHPI", "_NHPI", ., fixed=T) %>% 
  gsub(".Non_Hispanic_White", "_White", ., fixed=T) %>% 
  gsub("_Black.v_White.Segregation.Index", ".Segregation.Index_Black", ., fixed=T) %>% 
  gsub("Nonwhite.v_White.Segregation.Index", "Segregation.Index_Non-White", ., fixed=T) 

COdataRWJ2020race = COdataRWJ2020race %>% 
  separate(., Category, c("Category", "Demographic"), "_", remove = T)

COdataRWJ2020race = COdataRWJ2020race %>%
  pivot_wider(
   names_from = Category, 
    values_from = Value)

names(COdataRWJ2020race) %<>%  
  gsub("YPLL", "Years.of.Potential.Life.Lost", ., fixed=T) %>% 
  gsub("Demographics.S", "S", ., fixed=T)

#write.csv(COdataRWJ2020race, "CO_Demographic_Race_Data_by_County_Expanded.csv", row.names = F)

#### Health Mortality / Hospitalizations Outcomes Master CDPHE ####
Health_Outcomes_Master =     
  read.csv("CDPHE_Composite_Selected_Health_Outcome_Dataset_(County).csv", stringsAsFactors = F, header=T)

names(Health_Outcomes_Master) %<>%  
  gsub("_", ".", .) %>% 
  tolower(.) %>% 
  capitalize(.) %>% 
  gsub("County.name", "COUNTY", .) %>% 
  gsub("l95ci", "95LCI", .) %>% 
  gsub("u95ci", "95HCI", .) %>% 
  gsub("adjrate", "Rate", .) %>% 
  gsub(".s", ".S", ., fixed=T) %>% 
  gsub(".d", ".D", ., fixed=T) %>% 
  gsub("Asthma.", "Asthma.Hospitalizations.", ., fixed=T) %>% 
  gsub("Diabetes.", "Diabetes.Hospitalizations.", ., fixed=T) %>% 
  gsub("Influenza.", "Flu.Hospitalizations.", ., fixed=T) %>% 
  gsub("Hd.", "Heart.Disease.Mortality.", ., fixed=T) %>%
  gsub("Suicide.", "Suicide.Mortality.", ., fixed=T) %>%
  gsub("Lwb.", "Low.Weight.Birth.", ., fixed=T) %>% 
  gsub("Tf.", "Teen.Fertility.", ., fixed=T) %>% 
  gsub("Mva.", "Motor.Vehicle.Accident.Mortality.", ., fixed=T) %>% 
  gsub("Pod.", "Opioid.or.Heroin.Mortality.", ., fixed=T)

Health_Outcomes_Master = Health_Outcomes_Master[ ,-1]

CO_COUNTY = left_join(COdataRWJ2020, Health_Outcomes_Master, by = ("COUNTY"))

#### Non-Hospitalization Health Rates BRFSS (Several Health Measure Datasets) ####

#### Asthma Rates ####
Asthma_Rates = read.csv(
  "Asthma_Prevalence_in_Adults_-_Colorado_BRFSS_2014-2017_(County).csv", 
      stringsAsFactors = F, header=T)

Asthma_Rates = Asthma_Rates %>% 
  select(., -ï..OBJECTID, -LABEL) %>% 
  mutate(., Asthma_Rate_CIL95 = Asthma_Confidence_Interval, 
         Asthma_Rate_CIH95 = Asthma_Confidence_Interval, 
         Asthma_Colorado_Estimate = as.numeric("8.9"), 
         Asthma_Colorado_CIL95 = as.numeric("8.5"), 
         Asthma_Colorado_CIH95 = as.numeric("9.2"))

names(Asthma_Rates) %<>%  
  gsub("Asthma_Confidence_Interval", "Asthma_County_Rate", .) %>% 
  gsub("Per_Adults_Asthma", "Perc_Adults_Asthma", .)

Asthma_Rates$Asthma_County_Rate %<>%  
  gsub("County/Regional Estimate ", "", .) %>% 
  gsub("%.*", "", .) %>% 
  as.numeric(.)

Asthma_Rates$Asthma_Rate_CIL95 %<>% 
  gsub(".*: ", "", .) %>% 
  gsub(" -.*", "", .) %>% 
  as.numeric(.) %>% 

Asthma_Rates$Asthma_Rate_CIH95  %<>%  
  gsub(".*- ", "", .) %>% 
  gsub(")", "", .) %>% 
  as.numeric(.)

#### Cig Rates ####
Cig_Rates = read.csv(
  "Cigarette_Smoking_in_Adults_-_Colorado_BRFSS_2014-2017_(County).csv", 
      stringsAsFactors = F, header=T)

Cig_Rates = Cig_Rates %>% 
  select(., -ï..OBJECTID, -LABEL) %>% 
  mutate(., Cig_Rate_CIL95 = Cigarette_Smoking_Confidence_Interval, 
         Cig_Rate_CIH95 = Cigarette_Smoking_Confidence_Interval, 
         Cigarette_Smoking_Colorado_Estimate = as.numeric("15.4"), 
         Cig_Colorado_CIL95 = as.numeric("14.9"), 
         Cig_Colorado_CIH95 = as.numeric("15.9"))

names(Cig_Rates) %<>%  
  gsub("Cigarette_Smoking_Colorado_Estimate", "Cig_Colorado_Estimate", .) %>% 
  gsub("Cigarette_Smoking_Confidence_Interval", "Cig_County_Rate", .) %>% 
  gsub("Per_Adults_Currently_Smoking_Cigarettes", "Perc_Adults_Cig", .)

Cig_Rates$Cig_County_Rate %<>%  
  gsub("County/Regional Estimate ", "", .) %>% 
  gsub("%.*", "", .) %>% 
  as.numeric(.)

Cig_Rates$Cig_Rate_CIL95 %<>% 
  gsub(".*: ", "", .) %>% 
  gsub(" -.*", "", .) %>% 
  as.numeric(.)

Cig_Rates$Cig_Rate_CIH95  %<>%
  gsub(".*- ", "", .) %>% 
  gsub(")", "", .) %>% 
  as.numeric(.)

#### Diabetes Rates ####
Diabetes_Rates = read.csv(
  "Diabetes_in_Adults_-_Colorado_BRFSS_2014-2017_(County).csv", 
      stringsAsFactors = F, header=T)

Diabetes_Rates = Diabetes_Rates %>% 
  select(., -ï..OBJECTID, -LABEL) %>% 
  mutate(., Diabetes_Rate_CIL95 = Diabetes_Confidence_Interval, 
         Diabetes_Rate_CIH95 = Diabetes_Confidence_Interval, 
         Diabetes_Colorado_Estimate = as.numeric("7.0"), 
         Diabetes_Colorado_CIL95 = as.numeric("6.8"), 
         Diabetes_Colorado_CIH95 = as.numeric("7.3"))

names(Diabetes_Rates) %<>%  
  gsub("Diabetes_Confidence_Interval", "Diabetes_County_Rate", .) %>% 
  gsub("Per_Adults_Diabetes", "Perc_Adults_Diabetes", .)

Diabetes_Rates$Diabetes_County_Rate %<>%  
  gsub("County/Regional Estimate ", "", .) %>% 
  gsub("%.*", "", .) %>% 
  as.numeric(.)

Diabetes_Rates$Diabetes_Rate_CIL95 %<>% 
  gsub(".*: ", "", .) %>% 
  gsub(" -.*", "", .) %>% 
  as.numeric(.)

Diabetes_Rates$Diabetes_Rate_CIH95 %<>%  
  gsub(".*- ", "", .) %>% 
  gsub(")", "", .) %>% 
  as.numeric(.)
 
#### Heart Disease Rates ####
Heart_Disease_Rates = read.csv(
  "Heart_Disease_in_Adults_-_Colorado_BRFSS_2014-2017_(County).csv", 
      stringsAsFactors = F, header=T)

Heart_Disease_Rates = Heart_Disease_Rates %>% 
  select(., -ï..OBJECTID, -LABEL) %>% 
  mutate(., Heart_Disease_Rate_CIL95 = Heart_Disease_Confidence_Interval,
         Heart_Disease_Rate_CIH95 = Heart_Disease_Confidence_Interval, 
         Heart_Disease_Colorado_Estimate = as.numeric("2.9"), 
         Heart_Disease_Colorado_CIL95 = as.numeric("2.7"), 
         Heart_Disease_Colorado_CIH95 = as.numeric("3.0"))

names(Heart_Disease_Rates) %<>%  
  gsub("Heart_Disease_Confidence_Interval", "Heart_Disease_County_Rate", .) %>% 
  gsub("Per_Adults_CoronaryHeartDisease", "Perc_Adults_Heart_Disease", .)

Heart_Disease_Rates$Heart_Disease_County_Rate %<>% 
  gsub("County/Regional Estimate ", "", .) %>%  
  gsub("%.*", "", .) %>% 
  as.numeric(.)

Heart_Disease_Rates$Heart_Disease_Rate_CIL95 %<>%  
  gsub(".*: ", "", .) %>% 
  gsub(" -.*", "", .) %>% 
  as.numeric(.)

Heart_Disease_Rates$Heart_Disease_Rate_CIH95 %<>%  
  gsub(".*- ", "", .) %>% 
  gsub(")", "", .) %>% 
  as.numeric(.)

#### MJ Rates #####
MJ_Rates = read.csv("Marijuana_Use_in_Adults_-_Colorado_BRFSS_2014-2017_(County).csv", stringsAsFactors = F, header=T)

MJ_Rates = MJ_Rates %>% 
  select(., -ï..OBJECTID, -LABEL) %>% 
  mutate(., MJ_Rate_CIL95 = MJ_Use_Confidence_Interval, 
         MJ_Rate_CIH95 = MJ_Use_Confidence_Interval, 
         MJ_Use_Colorado_Estimate = as.numeric("14.0"), 
         MJ_Colorado_CIL95 = as.numeric("13.5"), 
         MJ_Colorado_CIH95 = as.numeric("14.5"))

names(MJ_Rates) %<>%  
  gsub("MJ_Use_Confidence_Interval", "MJ_County_Rate", .) %>% 
  gsub("Per_Adults_MJ_30day", "Perc_Adults_MJ_30day", .)

MJ_Rates$MJ_County_Rate %<>% 
  gsub("County/Regional Estimate ", "", .) %>% 
  gsub("%.*", "", .) %>% 
  as.numeric(.)

MJ_Rates$MJ_Rate_CIL95 %<>% 
  gsub(".*: ", "", .) %>% 
  gsub(" -.*", "", .) %>% 
  as.numeric(.)

MJ_Rates$MJ_Rate_CIH95 %<>% 
  gsub(".*- ", "", .) %>% 
  gsub(")", "", .) %>% 
  as.numeric(.)

#### Delayed Care Rates ####
Delayed_Care_Rates = read.csv("Delayed_Medical_Care_in_Adults_($)_-_Colorado_BRFSS_2014-2017_(County).csv", stringsAsFactors = F, header = T)

Delayed_Care_Rates = Delayed_Care_Rates %>% 
  select(., -ï..OBJECTID, -LABEL) %>% 
  mutate(., Delayed_Care_Rate_CIL95 = 
           Delayed_Medical_Care_Cost_Confidence_Interval, 
         Delayed_Care_Rate_CIH95 = 
           Delayed_Medical_Care_Cost_Confidence_Interval, 
         Delayed_Medical_Care_Cost_Colorado_Estimate = as.numeric("12.4"), 
         Delayed_Care_Colorado_CIL95 = as.numeric("12.0"), 
         Delayed_Care_Colorado_CIH95 = as.numeric("12.8"))

names(Delayed_Care_Rates) %<>% 
  gsub("Delayed_Medical_Care_Cost_Confidence_Interval", 
       "Delayed_Care_County_Rate", .) %>% 
  gsub("Per_Adults_Delayed_Medical_Care_DuetoCosts", 
       "Perc_Adults_Delayed_Care", .)

Delayed_Care_Rates$Delayed_Care_County_Rate %<>%  
  gsub("County/Regional Estimate ", "", .) %>% 
  gsub("%.*", "", .) %>% 
  as.numeric(.)

Delayed_Care_Rates$Delayed_Care_Rate_CIL95 %<>%  
  gsub(".*: ", "", .) %>% 
  gsub(" -.*", "", .) %>% 
  as.numeric(.)

Delayed_Care_Rates$Delayed_Care_Rate_CIH95 %<>%  
  gsub(".*- ", "", .) %>% 
  gsub(")", "", .) %>% 
  as.numeric(.)

#### No Reg Care Rates ####
No_Reg_Care_Rates =  read.csv("No_Regular_Medical_Checkup_in_Adults_-_Colorado_BRFSS_2014-2017_(County).csv", stringsAsFactors = F, header=T)

No_Reg_Care_Rates = No_Reg_Care_Rates %>% 
  select(., -ï..OBJECTID, -LABEL) %>% 
  mutate(., No_Reg_Care_Rate_CIL95 = 
           No_Checkup_12mos_Confidence_Interval, 
         No_Reg_Care_Rate_CIH95 = No_Checkup_12mos_Confidence_Interval, 
         No_Checkup_12mos_Colorado_Estimate = as.numeric("37.4"), 
         No_Reg_Care_Colorado_CIL95 = as.numeric("36.8"), 
         No_Reg_Care_Colorado_CIH95 = as.numeric("38.0"))

names(No_Reg_Care_Rates) %<>% 
  gsub("No_Checkup_12mos_Confidence_Interval", "No_Reg_Care_County_Rate", .) %>% 
  gsub("Per_Adults_NoCheckup_12months", "Perc_Adults_No_Reg_Care_12mos", .)

No_Reg_Care_Rates$No_Reg_Care_County_Rate %<>%  
  gsub("County/Regional Estimate ", "", .) %>% 
  gsub("%.*", "", .) %>% 
  as.numeric(.)

No_Reg_Care_Rates$No_Reg_Care_Rate_CIL95 %<>%  
  gsub(".*: ", "", .) %>% 
  gsub(" -.*", "", .) %>% 
  as.numeric(.)

No_Reg_Care_Rates$No_Reg_Care_Rate_CIH95 %<>% 
  gsub(".*- ", "", .) %>% 
  gsub(")", "", .) %>% 
  as.numeric(.)

#### Disability ####
Disability =  read.csv("Disability_(Census_Tracts).csv", stringsAsFactors = F, header=T)

## For Census Tracts Data, need to aggregate values by County ##
Disability = Disability %>% 
      select(-starts_with("Percent"), 
             -starts_with("Population_Density"), 
             -starts_with("Tract"), -contains("OBJECTID"), -FIPS) %>%
      group_by(County) %>% 
      summarise_if(., is.numeric, funs(sum))
      
names(Disability) = gsub("County", "COUNTY", names(Disability))

Disability = Disability %>% 
  mutate(., 
    Perc_Disability_TCNPop_With_A_Self_Care_Difficulty_Age_Over_4 = 100*Disability_TCNPop_With_A_Self_Care_Difficulty_Age_Over_4/Population_Total,
    Perc_Disability_TCNPop_With_A_Self_Care_Difficulty_Age517 = 100*Disability_TCNPop_With_A_Self_Care_Difficulty_Age517/Population_Total,
    Perc_Disability_TCNPop_AgeOver17_With_An_IndLiving_Difficulty = 100*Disability_TCNPop_AgeOver17_With_An_IndLiving_Difficulty/Population_Total, 
    Perc_Disability_TCNPop_With_A_Disability = 100*Disability_TCNPop_With_A_Disability/Population_Total)

#### Life Expectancy 2010-2015 ####
Life_Expectancy2015 =  read.csv("Colorado_Life_Expectancy_by_Census_Tract_Published_by_NAPHSIS-USALEEP_(2010-2015).csv", stringsAsFactors = F, header=T)

## For Census Tracts Data, need to aggregate values by County ##
Life_Expectancy2015 = Life_Expectancy2015 %>% 
      select(starts_with("LE"), COUNTY) %>%
      group_by(., COUNTY) %>% 
      summarise_if(., is.numeric, funs(mean), na.rm=T) %>% 
      mutate(., LE_STATE = as.numeric("80.5"), 
             LE_VARIANCE = LE_20102015 - LE_STATE)

#### Disability Providers / Beds####
Disability_Providers = read.csv("Disability_Resource_Providers__Colorado_Community_Inclusion.csv", stringsAsFactors = F, header=T)

Disability_Providers = Disability_Providers %>% 
  select(., MAP_NAME, RESOURCE_NAME, CITY, ZIP, COUNTY)

Disability_Providers$COUNTY = 
  gsub(" COUNTY", "", Disability_Providers$COUNTY)

Disability_Providers_Sum = Disability_Providers %>% 
  group_by(., COUNTY) %>% 
  summarise(., Types_Of_Dis_Providers = n_distinct(MAP_NAME), 
            Total_Dis_Providers = n())

#### Health / Assisted Living Facilities ####
Health_Facilities = read.csv("CDPHE_Health_Facilities.csv",
                            stringsAsFactors = F, header=T)

Health_Facilities = Health_Facilities %>% 
  select(., FAC_NAME, NAME, CITY, ZIP, COUNTY, OPERATING_STATUS, 
         MEDICARE, MEDICAID, CERTIFIED_TOTAL_BEDS, COUNTY, FACTYPE, 
         SYMBOL, TYPE)

Health_Facilities = separate(Health_Facilities, CERTIFIED_TOTAL_BEDS, 
      c('CERTIFIED_AVAILABLE_BEDS', 'CERTIFIED_TOTAL_BEDS'), sep='/')

Health_Facilities[c("CERTIFIED_AVAILABLE_BEDS", "CERTIFIED_TOTAL_BEDS")] = sapply(Health_Facilities[c("CERTIFIED_AVAILABLE_BEDS", "CERTIFIED_TOTAL_BEDS")], as.numeric)

Health_Facilities_Sum = Health_Facilities %>% 
  filter(., OPERATING_STATUS == "01-ACTIVE") %>% 
  group_by(., COUNTY) %>% 
  summarise(., Available_Beds = sum(CERTIFIED_AVAILABLE_BEDS),
            Total_Beds = sum(CERTIFIED_TOTAL_BEDS),
            Assisted_Living_Facs = sum(SYMBOL == "Assisted Living Residence/Nursing Home"), Total_Health_Facilities = n())

#### Non-Hospitalization Join ####
Non_Hospitalization_Health =left_join(Life_Expectancy2015,          
                            left_join(Asthma_Rates, 
                            left_join(Cig_Rates, 
                            left_join(Diabetes_Rates,
                            left_join(Heart_Disease_Rates,
                            left_join(MJ_Rates,
                            left_join(Delayed_Care_Rates,
                            left_join(No_Reg_Care_Rates,
                            left_join(Disability,
                            left_join(Disability_Providers_Sum,
                            Health_Facilities_Sum,
by="COUNTY"), by="COUNTY"), by="COUNTY"), by="COUNTY"), by="COUNTY")
, by="COUNTY"), by="COUNTY"), by="COUNTY"), by="COUNTY"), by="COUNTY")

## Standardize Expressions to match Hospitalization Data / CO_COUNTY ##
names(Non_Hospitalization_Health) %<>%  
  gsub("_", ".", ., fixed=T) %>% 
  gsub("CIL95", "95LCI", .) %>% 
  gsub("CIH95", "95HCI", .) %>% 
  gsub("Colorado", "State", .) %>% 
  gsub("L95CI", "95LCI", .) %>% 
  gsub("U95CI", "95HCI", .) %>% 
  gsub("LE.", "Life.Expectancy.", ., fixed=T) %>% 
  gsub("STATE", "State", .) %>% 
  gsub("VARIANCE", "Variance", .) %>% 
  gsub("MJ.", "Marijuana.", ., fixed=T) %>% 
  gsub("Cig.", "Cigarette.", ., fixed=T) 


#### ADDING NON-HOSPITALIZATION HEALTH TO CO_COUNTY####

CO_COUNTY_ALL = left_join(CO_COUNTY, Non_Hospitalization_Health, by = "COUNTY")


CO_COUNTY_ALL = CO_COUNTY_ALL %>% 
  mutate(., Perc.100000.Total.Dis.Providers = 
           100000*Total.Dis.Providers/Population.Total,
Perc.100000.Available.Beds = 100000*Available.Beds/Population.Total,
Perc.100000.Total.Beds = 100000*Total.Beds/Population.Total,
Perc.100000.Assisted.Living.Facs = 100000*Assisted.Living.Facs/Population.Total,
Perc.100000.Total.Health.Facilities = 100000*Total.Health.Facilities/Population.Total)


######################## COVID ##############################
####  Daily Data refreshes start here ####

## COVID DATASET #1 - COUNTY LEVEL OPEN REPOSITORY ##
COVID19County = read.csv("CDPHE_COVID19_County-Level_Open_Data_Repository.csv", stringsAsFactors = F, header=T)

COVID19County$Metric %<>%  
  gsub("Rate per 100,000", "Rate Per 100,000", .) 

COVID19County$Desc_ %<>%  
  gsub("Total Testing Rate Per 100,000 People in Colorado by County", "Testing Rates Per 100000", .) %>% 
  gsub("Case Rates Per 100,000 People in Colorado by County", "Case Rates Per 100000", .)

COVID19County$Value = COVID19County$Value %>% replace_na(., 0)
COVID19County$Rate = COVID19County$Rate %>% replace_na(., 0)

names(COVID19County) %<>%  
  gsub("ï..COUNTY", "COUNTY", ., fixed=T)

## Creating single numerical list & Eliminating Notes from Data ##
COVID19County = COVID19County %>% 
  mutate(., Number = (Value + Rate)) %>% 
  filter(., Metric !="Some cases may still be under investigation and county not assigned yet." & 
            Metric != "Rates are not shown for counties with fewer than 5 cases." & 
            Metric != "Individuals with serology-positive tests are not included in daily case counts until they are confirmed to have had COVID-like symptoms." & 
            Metric != "Includes only tests from labs that participate in electronic lab reporting." & 
            Metric != "County rates per 100,000 are calculated using 2018 population estimates from the Demography Section, Colorado Division of Local Government." & 
            Metric != "Caution should be used when interpreting rates in counties with small populations." & 
            Metric != "Percent of tests by Serology" & 
            Metric != "Percent of tests by PCR" &
            COUNTY != "Note")

COVID19County = COVID19County %>% 
  select(., -Rate, -FIPS, -FULL_, -LABEL, -Value, -ObjectId) %>% 
  pivot_wider(., id_cols=c(COUNTY, POP, Date),
              names_from=Desc_, 
              values_from=Number,
              values_fn = list(Number = mean)
              )

colnames(COVID19County) = make.names(colnames(COVID19County))

COVID19County = COVID19County %>% 
  arrange(COUNTY, Date)

COVID19County$Date = 
  as.Date(COVID19County$Date, format="%m/%d/%Y")


## COVID DATASET #2 - Positive Cases and Rates of Infection by County ##
COVID19Positive = read.csv("Colorado_COVID-19_Positive_Cases_and_Rates_of_Infection_by_County_of_Identification.csv", header=T, stringsAsFactors = F)

names(COVID19Positive) %<>% gsub("__", "_", ., fixed=T) 

COVID19Positive$County_Deaths[is.na(COVID19Positive$County_Deaths)] <- 0

## Creating Per Capita (Rate per 100000) Columns ##
COVID19Positive = COVID19Positive %>% 
  select(., COUNTY, County_Population, State_Population, 
         County_Pos_Cases,
         COVID_County_Pos_Case_Rate_Per_100000 = 
           County_Rate_Per_100_000,
         COVID_County_Total_Deaths = County_Deaths, State_Deaths,
         State_Number_Hospitalizations, State_Number_Tested, Shape_Area,
         Shape_Length) %>% 
  mutate(., COVID_County_Pos_Case_Rate_Per_100000 = 
           100000*County_Pos_Cases/County_Population,
         COVID_County_Death_Rate_Per_100000 =
           100000*COVID_County_Total_Deaths/County_Population,
         COVID_State_Death_Rate_Per_100000 =
           100000*State_Deaths/State_Population,
         COVID_State_HOSP_Rate_Per_100000 =
           100000*State_Number_Hospitalizations/State_Population,
         COVID_State_Testing_Rate_Per_100000 =
           100000*State_Number_Tested/State_Population, 
  )


#### COMBINING COVID 1 and 2 DATASETS ####

COVID19CountySummarise = COVID19County %>% 
  filter(., COUNTY != "Unknown Or Pending County" & 
            COUNTY != "Out Of State County" &
            COUNTY != "International") %>% 
  group_by(., COUNTY) %>% 
  summarise(., COVID_First_Case_Date = min(Date),
             COVID_Num_Days_Since_First_Case = 
               (as.numeric(Sys.Date()) - 
                  as.numeric(COVID_First_Case_Date)),
             COVID_Perc_Days_with_Cases = 
               100*(COVID_Num_Days_Since_First_Case/
                      (as.numeric(Sys.Date()) - 
                  as.numeric(as.Date('2020-03-17')))),
             COVID_Cases_Max = max(Cases.of.COVID.19.in.Colorado.by.County, na.rm = TRUE),
             COVID_Max_Testing_Rates_Per_100000 =
               max(Total.COVID.19.Testing.Rate.per.100.000.People.in.Colorado.by.County, na.rm = TRUE),
             COVID_Mean_Testing_Rates_Per_100000 =
               mean(Total.COVID.19.Testing.Rate.per.100.000.People.in.Colorado.by.County, na.rm = TRUE),
             COVID_Mean_Case_Rates_Per_100000 =
               mean(Case.Rates.Per.100000, na.rm = TRUE),
             COVID_Mean_Cases_per_Day =
               COVID_Cases_Max/
               as.numeric(COVID_Num_Days_Since_First_Case),
             COVID_Max_Case_Rates_Per_100000 =
               max(Case.Rates.Per.100000, na.rm = TRUE),
            COVID_Tests_Max = max(Total.COVID.19.Tests.Performed.in.Colorado.by.County, na.rm=T))

COVID19CountyANALYSIS = left_join(COVID19CountySummarise, 
                                  COVID19Positive, by="COUNTY")

## Eliminating divide by 0 error notation ##
COVID19CountyANALYSIS$COVID_Max_Case_Rates_Per_100000[COVID19CountyANALYSIS$COVID_Max_Case_Rates_Per_100000=="-Inf"]=0
COVID19CountyANALYSIS$COVID_Mean_Case_Rates_Per_100000[COVID19CountyANALYSIS$COVID_Mean_Case_Rates_Per_100000=="NaN"]=0

## Creating Calculated Percentages ##
COVID19CountyANALYSIS = COVID19CountyANALYSIS %>% 
  mutate(.,
         COVID.Positive.Tests.Perc = ifelse(COVID_Tests_Max!=0, 
                                        100*COVID_Cases_Max/COVID_Tests_Max, NA),
         COVID_Death_Rate_if_Pos_Case_Perc = 
           ifelse(COVID_County_Pos_Case_Rate_Per_100000!=0,
                  100*COVID_County_Death_Rate_Per_100000/
           COVID_County_Pos_Case_Rate_Per_100000, NA)
         )


#### COVID DATASET #3 -- STATE & COUNTY DAILY DATA  ####
COVID19StateData = read.csv("CDPHE_COVID19_Daily_State_Statistics.csv", header=T, stringsAsFactors = F)

COVID19StateData$Date = as.Date(COVID19StateData$Date, format="%m/%d/%Y")

COVID19StateData = COVID19StateData %>% 
  select(., COUNTY = ï..Name, POP = Population, Date, Tested, Cases, Deaths, 
         Total.State.Hospitalizations = Hosp, Outbreaks, Counties, COVID.Cases.Per.100000=Rate) %>% 
  filter(., COUNTY != "Note") %>% 
  mutate(., COVID.Tests.Per.100000 = 100000*Tested/POP,
            COVID.Deaths.Per.100000 = 100000*Deaths/POP,
            COVID.Positive.Tests.Perc = ifelse(Tested!=0, 100*Cases/Tested, NA),
            COVID.Mortality.Perc = ifelse(Cases!=0, 100*Deaths/Cases, NA),
            State.Hospitalizations.Per.100000 = 100000*Total.State.Hospitalizations/POP
         )
COVID19State_MERGE = COVID19StateData %>% 
  select(., COUNTY, Date, POP, Total.Tests = Tested, Total.Cases = Cases, 
         Total.Deaths = Deaths, Total.State.Hospitalizations, COVID.Tests.Per.100000, 
         COVID.Cases.Per.100000, COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
         State.Hospitalizations.Per.100000)

COVID19County_MERGE = COVID19County %>% 
  mutate(., COVID.Deaths.Per.100000 = 100000*Deaths.Among.COVID.19.Cases.in.Colorado.by.County/POP,
         COVID.Positive.Tests.Perc = ifelse(Total.COVID.19.Tests.Performed.in.Colorado.by.County!=0,
                                        100*Cases.of.COVID.19.in.Colorado.by.County/
           Total.COVID.19.Tests.Performed.in.Colorado.by.County, NA),
         COVID.Mortality.Perc = ifelse(Cases.of.COVID.19.in.Colorado.by.County!=0,
                                       100*Deaths.Among.COVID.19.Cases.in.Colorado.by.County/Cases.of.COVID.19.in.Colorado.by.County,
                                       NA),
         Total.State.Hospitalizations = NA, State.Hospitalizations.Per.100000 = NA) %>% 
  select(., COUNTY, Date, POP, 
         Total.Tests = Total.COVID.19.Tests.Performed.in.Colorado.by.County, 
         Total.Cases = Cases.of.COVID.19.in.Colorado.by.County, 
         Total.Deaths = Deaths.Among.COVID.19.Cases.in.Colorado.by.County, 
         Total.State.Hospitalizations,
         COVID.Tests.Per.100000 = Total.COVID.19.Testing.Rate.per.100.000.People.in.Colorado.by.County, 
         COVID.Cases.Per.100000 = Case.Rates.Per.100000, 
         COVID.Deaths.Per.100000, 
         COVID.Positive.Tests.Perc, 
         COVID.Mortality.Perc, 
         State.Hospitalizations.Per.100000)


####  COVID NEW COUNT, LAST WEEK SUM, AND 5 DAY AVERAGES ####
COVID19ALL_MERGE = union_all(COVID19State_MERGE, COVID19County_MERGE)

COVID19County_YESTERDAY = COVID19ALL_MERGE %>% 
  select(COUNTY, Date, 
         YEST.Total.Tests = Total.Tests,
         YEST.Total.Cases = Total.Cases,
         YEST.Total.Deaths = Total.Deaths,
         YEST.COVID.Tests.Per.100000=COVID.Tests.Per.100000,
         YEST.COVID.Cases.Per.100000=COVID.Cases.Per.100000,
         YEST.COVID.Deaths.Per.100000=COVID.Deaths.Per.100000,
         YEST.COVID.Positive.Tests.Perc=COVID.Positive.Tests.Perc,
         YEST.COVID.Mortality.Perc=COVID.Mortality.Perc,
         YEST.Total.State.Hospitalizations=Total.State.Hospitalizations,
         YEST.State.Hospitalizations.Per.100000=State.Hospitalizations.Per.100000) %>% 
  mutate(Yest = Date+1) %>% 
  select(., -Date)

COVID19County_TWODAY = COVID19ALL_MERGE %>% 
  select(COUNTY, Date, 
         TWODAY.Total.Tests = Total.Tests,
         TWODAY.Total.Cases = Total.Cases,
         TWODAY.Total.Deaths = Total.Deaths,
         TWODAY.COVID.Tests.Per.100000=COVID.Tests.Per.100000,
         TWODAY.COVID.Cases.Per.100000=COVID.Cases.Per.100000,
         TWODAY.COVID.Deaths.Per.100000=COVID.Deaths.Per.100000,
         TWODAY.COVID.Positive.Tests.Perc=COVID.Positive.Tests.Perc,
         TWODAY.COVID.Mortality.Perc=COVID.Mortality.Perc,
         TWODAY.Total.State.Hospitalizations=Total.State.Hospitalizations,
         TWODAY.State.Hospitalizations.Per.100000=State.Hospitalizations.Per.100000) %>% 
  mutate(TwoDay = Date+2) %>% 
  select(., -Date)

COVID19County_TOM = COVID19ALL_MERGE %>% 
  select(COUNTY, Date, 
         TOM.Total.Tests = Total.Tests,
         TOM.Total.Cases = Total.Cases,
         TOM.Total.Deaths = Total.Deaths,
         TOM.COVID.Tests.Per.100000=COVID.Tests.Per.100000,
         TOM.COVID.Cases.Per.100000=COVID.Cases.Per.100000,
         TOM.COVID.Deaths.Per.100000=COVID.Deaths.Per.100000,
         TOM.COVID.Positive.Tests.Perc=COVID.Positive.Tests.Perc,
         TOM.COVID.Mortality.Perc=COVID.Mortality.Perc,
         TOM.Total.State.Hospitalizations=Total.State.Hospitalizations,
         TOM.State.Hospitalizations.Per.100000=State.Hospitalizations.Per.100000) %>% 
  mutate(Tom = Date-1) %>% 
  select(., -Date)

COVID19County_AFTERNEXT = COVID19ALL_MERGE %>% 
  select(COUNTY, Date, 
         AFTERNEXT.Total.Tests = Total.Tests,
         AFTERNEXT.Total.Cases = Total.Cases,
         AFTERNEXT.Total.Deaths = Total.Deaths,
         AFTERNEXT.COVID.Tests.Per.100000=COVID.Tests.Per.100000,
         AFTERNEXT.COVID.Cases.Per.100000=COVID.Cases.Per.100000,
         AFTERNEXT.COVID.Deaths.Per.100000=COVID.Deaths.Per.100000,
         AFTERNEXT.COVID.Positive.Tests.Perc=COVID.Positive.Tests.Perc,
         AFTERNEXT.COVID.Mortality.Perc=COVID.Mortality.Perc,
         AFTERNEXT.Total.State.Hospitalizations=Total.State.Hospitalizations,
         AFTERNEXT.State.Hospitalizations.Per.100000=State.Hospitalizations.Per.100000) %>% 
  mutate(AfterNext = Date-2) %>% 
  select(., -Date)

COVID19County_LASTWEEK = COVID19ALL_MERGE %>% 
  select(COUNTY, Date, 
         LASTWEEK.Total.Tests = Total.Tests,
         LASTWEEK.Total.Cases = Total.Cases,
         LASTWEEK.Total.Deaths = Total.Deaths,
         LASTWEEK.COVID.Tests.Per.100000=COVID.Tests.Per.100000,
         LASTWEEK.COVID.Cases.Per.100000=COVID.Cases.Per.100000,
         LASTWEEK.COVID.Deaths.Per.100000=COVID.Deaths.Per.100000,
         LASTWEEK.COVID.Positive.Tests.Perc=COVID.Positive.Tests.Perc,
         LASTWEEK.COVID.Mortality.Perc=COVID.Mortality.Perc,
         LASTWEEK.Total.State.Hospitalizations=Total.State.Hospitalizations,
         LASTWEEK.State.Hospitalizations.Per.100000=State.Hospitalizations.Per.100000) %>% 
  mutate(LastWeek = Date+7) %>% 
  select(., -Date)

COVID19ALL_MERGE =
  left_join(COVID19ALL_MERGE,COVID19County_TWODAY,
            by=c("COUNTY"="COUNTY", "Date"="TwoDay"))
COVID19ALL_MERGE =
  left_join(COVID19ALL_MERGE,COVID19County_YESTERDAY,
            by=c("COUNTY"="COUNTY", "Date"="Yest"))
COVID19ALL_MERGE =
  left_join(COVID19ALL_MERGE,COVID19County_TOM,
            by=c("COUNTY"="COUNTY", "Date"="Tom"))
COVID19ALL_MERGE =
  left_join(COVID19ALL_MERGE,COVID19County_AFTERNEXT,
            by=c("COUNTY"="COUNTY", "Date"="AfterNext"))
COVID19ALL_MERGE =
  left_join(COVID19ALL_MERGE,COVID19County_LASTWEEK,
            by=c("COUNTY"="COUNTY", "Date"="LastWeek"))


COVID19ALL_MERGE = COVID19ALL_MERGE %>%
  mutate(., New.Tests = Total.Tests - YEST.Total.Tests,
         New.Cases = Total.Cases - YEST.Total.Cases,
         New.Deaths = Total.Deaths - YEST.Total.Deaths,
         New.State.Hospitalizations = 
           Total.State.Hospitalizations - YEST.Total.State.Hospitalizations,
         New.Tests.Last.Week = Total.Tests - LASTWEEK.Total.Tests,
         New.Cases.Last.Week = Total.Cases - LASTWEEK.Total.Cases,
         New.Deaths.Last.Week = Total.Deaths - LASTWEEK.Total.Deaths,
         New.State.Hospitalizations.Last.Week = 
           Total.State.Hospitalizations - LASTWEEK.Total.State.Hospitalizations,
         Change.in.Tests.Per.100000=
           COVID.Tests.Per.100000-YEST.COVID.Tests.Per.100000,
         Change.in.Cases.Per.100000=
           COVID.Cases.Per.100000-YEST.COVID.Cases.Per.100000,
         Change.in.Deaths.Per.100000=
           COVID.Deaths.Per.100000-YEST.COVID.Deaths.Per.100000,
         Change.in.State.Hospitalizations.Per.100000 = 
           State.Hospitalizations.Per.100000 - YEST.State.Hospitalizations.Per.100000)

COVID19County_THREEDAY2 = COVID19ALL_MERGE %>% 
  select(COUNTY, Date, 
         THREEDAY.New.Tests = New.Tests,
         THREEDAY.New.Cases = New.Cases,
         THREEDAY.New.Deaths = New.Deaths,
         THREEDAY.Change.in.Tests.Per.100000=Change.in.Tests.Per.100000,
         THREEDAY.Change.in.Cases.Per.100000=Change.in.Cases.Per.100000,
         THREEDAY.Change.in.Deaths.Per.100000=Change.in.Deaths.Per.100000,
         THREEDAY.COVID.Positive.Tests.Perc=COVID.Positive.Tests.Perc,
         THREEDAY.COVID.Mortality.Perc=COVID.Mortality.Perc,
         THREEDAY.New.State.Hospitalizations = New.State.Hospitalizations,
         THREEDAY.Change.in.State.Hospitalizations.Per.100000 = 
           Change.in.State.Hospitalizations.Per.100000) %>% 
  mutate(ThreeDay = Date+3) %>% 
  select(., -Date)    

COVID19County_TWODAY2 = COVID19ALL_MERGE %>% 
  select(COUNTY, Date, 
         TWODAY.New.Tests = New.Tests,
         TWODAY.New.Cases = New.Cases,
         TWODAY.New.Deaths = New.Deaths,
         TWODAY.Change.in.Tests.Per.100000=Change.in.Tests.Per.100000,
         TWODAY.Change.in.Cases.Per.100000=Change.in.Cases.Per.100000,
         TWODAY.Change.in.Deaths.Per.100000=Change.in.Deaths.Per.100000,
         TWODAY.New.State.Hospitalizations = New.State.Hospitalizations,
         TWODAY.Change.in.State.Hospitalizations.Per.100000 = 
           Change.in.State.Hospitalizations.Per.100000) %>% 
  mutate(TwoDay = Date+2) %>% 
  select(., -Date)

COVID19County_YESTERDAY2 = COVID19ALL_MERGE %>% 
  select(COUNTY, Date, 
         YEST.New.Tests = New.Tests,
         YEST.New.Cases = New.Cases,
         YEST.New.Deaths = New.Deaths,
         YEST.Change.in.Tests.Per.100000=Change.in.Tests.Per.100000,
         YEST.Change.in.Cases.Per.100000=Change.in.Cases.Per.100000,
         YEST.Change.in.Deaths.Per.100000=Change.in.Deaths.Per.100000,
         YEST.New.State.Hospitalizations = New.State.Hospitalizations,
         YEST.Change.in.State.Hospitalizations.Per.100000 = 
           Change.in.State.Hospitalizations.Per.100000) %>% 
  mutate(Yest = Date+1) %>% 
  select(., -Date)

COVID19County_TOM2 = COVID19ALL_MERGE %>% 
  select(COUNTY, Date, 
         TOM.New.Tests = New.Tests,
         TOM.New.Cases = New.Cases,
         TOM.New.Deaths = New.Deaths,
         TOM.Change.in.Tests.Per.100000=Change.in.Tests.Per.100000,
         TOM.Change.in.Cases.Per.100000=Change.in.Cases.Per.100000,
         TOM.Change.in.Deaths.Per.100000=Change.in.Deaths.Per.100000,
         TOM.New.State.Hospitalizations = New.State.Hospitalizations,
         TOM.Change.in.State.Hospitalizations.Per.100000 = 
           Change.in.State.Hospitalizations.Per.100000) %>% 
  mutate(Tom = Date-1) %>% 
  select(., -Date)

COVID19County_AFTERNEXT2 = COVID19ALL_MERGE %>% 
  select(COUNTY, Date, 
         AFTERNEXT.New.Tests = New.Tests,
         AFTERNEXT.New.Cases = New.Cases,
         AFTERNEXT.New.Deaths = New.Deaths,
         AFTERNEXT.Change.in.Tests.Per.100000=Change.in.Tests.Per.100000,
         AFTERNEXT.Change.in.Cases.Per.100000=Change.in.Cases.Per.100000,
         AFTERNEXT.Change.in.Deaths.Per.100000=Change.in.Deaths.Per.100000,
         AFTERNEXT.New.State.Hospitalizations = New.State.Hospitalizations,
         AFTERNEXT.Change.in.State.Hospitalizations.Per.100000 = 
           Change.in.State.Hospitalizations.Per.100000) %>% 
  mutate(AfterNext = Date-2) %>% 
  select(., -Date)

COVID19ALL_MERGE =
  left_join(COVID19ALL_MERGE,COVID19County_THREEDAY2,
            by=c("COUNTY"="COUNTY", "Date"="ThreeDay"))
COVID19ALL_MERGE =
  left_join(COVID19ALL_MERGE,COVID19County_TWODAY2,
            by=c("COUNTY"="COUNTY", "Date"="TwoDay"))
COVID19ALL_MERGE =
  left_join(COVID19ALL_MERGE,COVID19County_YESTERDAY2,
            by=c("COUNTY"="COUNTY", "Date"="Yest"))
COVID19ALL_MERGE =
  left_join(COVID19ALL_MERGE,COVID19County_TOM2,
            by=c("COUNTY"="COUNTY", "Date"="Tom"))
COVID19ALL_MERGE =
  left_join(COVID19ALL_MERGE,COVID19County_AFTERNEXT2,
            by=c("COUNTY"="COUNTY", "Date"="AfterNext"))

COVID19ALL_MERGE = COVID19ALL_MERGE %>% 
  mutate(., 
         New.Tests.5.Day.Avg = rowMeans(select(., TWODAY.New.Tests,YEST.New.Tests, New.Tests, TOM.New.Tests, AFTERNEXT.New.Tests)),
         New.Cases.5.Day.Avg = rowMeans(select(., TWODAY.New.Cases,YEST.New.Cases, New.Cases, TOM.New.Cases, AFTERNEXT.New.Cases)),
         New.Deaths.5.Day.Avg = rowMeans(select(., TWODAY.New.Deaths,YEST.New.Deaths, New.Deaths, TOM.New.Deaths, AFTERNEXT.New.Deaths)),
         New.State.Hospitalizations.5.Day.Avg = rowMeans(select(., TWODAY.New.State.Hospitalizations,
                                                                YEST.New.State.Hospitalizations, New.State.Hospitalizations,
                                                                TOM.New.State.Hospitalizations, AFTERNEXT.New.State.Hospitalizations)),
         COVID.Positive.Tests.Perc.5.Day.Avg = rowMeans(select(., TWODAY.COVID.Positive.Tests.Perc,YEST.COVID.Positive.Tests.Perc, COVID.Positive.Tests.Perc, TOM.COVID.Positive.Tests.Perc, AFTERNEXT.COVID.Positive.Tests.Perc)),
         COVID.Mortality.Perc.5.Day.Avg = rowMeans(select(., TWODAY.COVID.Mortality.Perc,YEST.COVID.Mortality.Perc, COVID.Mortality.Perc, TOM.COVID.Mortality.Perc, AFTERNEXT.COVID.Mortality.Perc))
)

COVID19ALL_MERGE$COUNTY %<>% gsub("Colorado", "COLORADO", .)

colnames(COVID19ALL_MERGE) %<>% 
  gsub("COVID.", "", ., fixed=T) 

COVID19ALL_MERGE = COVID19ALL_MERGE %>% 
  select(., -starts_with("YEST"), -starts_with("TOM"), -starts_with("TWO"),
         -starts_with("THREE"), -starts_with("AFTER"), -starts_with("LASTWEEK"), 
         -starts_with("Change")) %>% 
  filter(., COUNTY != "Unknown Or Pending County" & 
           COUNTY != "Out Of State County" &
           COUNTY != "International") 

COVID19DATA = COVID19ALL_MERGE %>% 
  select(., COUNTY, Date, POP, starts_with("Total"), 
         New.Tests, New.Cases, New.Deaths, New.State.Hospitalizations,
         contains("Last"),contains("Perc"), 
         contains("100"), contains("5.Day"))

COVID19ALL_MERGE = COVID19ALL_MERGE %>% 
  select(., -Positive.Tests.Perc, -Mortality.Perc, -New.Tests, -New.Cases, -New.Deaths,
         -New.State.Hospitalizations,
         COUNTY, Date, POP, starts_with("Total"), contains("5.Day"), contains("100"), 
         starts_with("Total"))

## Making it so COLORADO always appears last on County Timeline (keeps same color) ##
COVID19ALL_MERGE = COVID19ALL_MERGE %>% 
  mutate(COUNTY = fct_relevel(COUNTY, "ADAMS","ALAMOSA","ARAPAHOE","ARCHULETA","BACA",
         "BENT","BOULDER","BROOMFIELD","CHAFFEE","CHEYENNE","CLEAR CREEK","CONEJOS",
         "COSTILLA","CROWLEY","CUSTER","DELTA","DENVER","DOLORES","DOUGLAS","EAGLE",
         "EL PASO","ELBERT","FREMONT","GARFIELD","GILPIN","GRAND","GUNNISON","HINSDALE",
         "HUERFANO","JACKSON","JEFFERSON","KIOWA","KIT CARSON",
         "LA PLATA","LAKE","LARIMER",
         "LAS ANIMAS","LINCOLN","LOGAN","MESA","MINERAL","MOFFAT","MONTEZUMA","MONTROSE",
         "MORGAN","OTERO","OURAY","PARK","PHILLIPS","PITKIN","PROWERS","PUEBLO",
         "RIO BLANCO","RIO GRANDE","ROUTT","SAGUACHE","SAN JUAN","SAN MIGUEL",
         "SEDGWICK","SUMMIT","TELLER","WASHINGTON","WELD","YUMA",
         "COLORADO"))

#### JOIN CoVID to established Datasets ####

## Race Measures Wide Table ##
CO_Race_Measures_COVID19 = inner_join(COdataRWJ2020race, COVID19CountyANALYSIS, by="COUNTY")
names(CO_Race_Measures_COVID19) %<>%  
  gsub("COVID_Mean_Testing_Rates_Per_100000", 
       "COVID.Tests.Per.100000", .) %>% 
  gsub("COVID_County_Pos_Case_Rate_Per_100000", 
       "COVID.Cases.Per.100000", .) %>% 
  gsub("COVID_County_Death_Rate_Per_100000", "COVID.Deaths.Per.100000", .) %>% 
  gsub("COVID_Death_Rate_if_Pos_Case_Perc", "COVID.Mortality.Perc", .) %>% 
  gsub("_", ".", .)

## All Data Join ##
CO_COUNTY_COVID_ALL = 
  left_join(COVID19CountyANALYSIS, CO_COUNTY_ALL, by="COUNTY")

colnames(CO_COUNTY_COVID_ALL) %<>%  
  gsub("Perc.100000", "Per.100000", .) %>% 
  gsub("Average.Daily.PM2.5", "Average.Daily.Air.Pollution", .)

#### Create Smaller Table Groups Here ####
CO_COUNTY_NAMES = names(CO_COUNTY_COVID_ALL)
#write.csv(CO_COUNTY_NAMES, "CO_COUNTY_NAMES.csv", row.names = T)

# names(CO_COUNTY_COVID_ALL)
CO_COUNTY_COVID_FILTER = CO_COUNTY_COVID_ALL %>% 
  filter(., !is.na(COUNTY)) %>% 
  select(., COUNTY, Population = County_Population, starts_with("COVID"), starts_with("Per"), contains("Perc", ignore.case = T), contains("Rate", ignore.case = T), Life.Expectancy.20102015, Life.Expectancy.State, starts_with("Median.Household.Income"), contains(".State.", ignore.case = T), contains("Score"), -matches("(Perc.*Score)"), -matches("(Rate.*Score)"), -matches("(Ratio.*Score)"), -matches("(Water.*Score)"), -ends_with("Rank"), -ends_with("Unhealthy.Days"), Nonwhite.v.White.Segregation.Index, Black.v.White.Segregation.Index)

colnames(CO_COUNTY_COVID_FILTER) %<>%
  gsub("COVID_Mean_Testing_Rates_Per_100000", 
       "COVID.Tests.Per.100000", .) %>% 
  gsub("COVID_County_Pos_Case_Rate_Per_100000", 
       "COVID.Cases.Per.100000", .) %>% 
  gsub("COVID_County_Death_Rate_Per_100000", "COVID.Deaths.Per.100000", .) %>% 
  gsub("COVID_Death_Rate_if_Pos_Case_Perc", "COVID.Mortality.Perc", .) %>% 
  gsub("Delayed.Medical.Care.Cost.State.Estimate", "Delayed.Care.State", .) %>% 
  gsub("No.Checkup.12mos.State.Estimate", "No.Reg.Care.State", .) %>% 
  gsub("State.Estimate", "State.Rate", .) %>% 
  gsub("Marijuana.Use.", "Marijuana.", .) %>% 
  gsub("Perc.Overcrowding", "Perc.Housing.Overcrowding", .) %>% 
  gsub("Perc.Inadequate.Facilities", 
       "Perc.Inadequate.Housing.Facilities", .) %>% 
  gsub("Perc.Disability.TCNPop.With.A.Self.Care.Difficulty.Age.Over.4", 
       "Perc.Self.Care.Disability", .) %>% 
  gsub("Perc.Disability.TCNPop.With.A.Self.Care.Difficulty.Age517",
       "Perc.Self.Care.Disability.Age.5.to.17", .) %>% 
  gsub("Perc.Disability.TCNPop.AgeOver17.With.An.IndLiving.Difficulty",
       "Perc.Indp.Living.Disability.Over.17", .) %>% 
  gsub("Perc.Disability.TCNPop.With.A.Disability", "Perc.Disability", .) %>% 
  gsub("StateRate", "State.Rate", .) %>% 
  gsub("Primary.Care.Physicians.Rate", 
       "Primary.Care.Patients.per.Physician", .) %>%
  gsub("Mental.Health.Provider.Rate", 
       "Mental.Health.Patients.per.Provider", .) %>% 
  gsub("Care.State", "Care.State.Rate", .) %>% 
  gsub("State.Rate.95", "State.95", .) %>% 
  gsub("20102015", "2010.2015.Rate", .) %>%
  gsub("Life.Expectancy.State", "Life.Expectancy.2010.2015.State.Rate", .) %>% 
  gsub("Average.Number.of.", "Avg.", .) %>% 
  gsub("Social.Association.Rate", "Social.Orgs.Association.Rate", .) %>% 
  gsub("Total.Dis", "Disability", .) %>% 
  gsub("Total.Health", "Health", .) %>% 
  gsub("YPLL", "Years.of.Potential.Life.Lost", .) %>% 
  gsub("Years.of.Potential.Life.Lost.Years.of.Potential.Life.Lost", "Years.of.Potential.Life.Lost", .) %>% 
  gsub("_", ".", .)
 
CO_COUNTY_COVID_SIMPLEST = CO_COUNTY_COVID_FILTER %>% select(.,
-contains("95"), -contains("State", ignore.case = T),
-contains("e.AIAN"), -contains("e.Asian"), -contains("e.Black"), -contains("e.Hispanic"), -contains("e.White"), -contains("d.AIAN"), -contains("d.Asian"), -contains("d.Black"), -contains("d.Hispanic"), -contains("d.White"), -contains("y.AIAN"), -contains("y.Asian"), -contains("y.Black"), -contains("y.Hispanic"), -contains("y.White"))

#### write.csv FILES ####

#write.csv(CO_COUNTY_COVID_ALL, "CO_COUNTY_COVID_ALL.csv", row.names = F)
#write.csv(CO_COUNTY_COVID_FILTER, "CO_COUNTY_COVID_FILTER.csv", row.names = F)

#### SUBTABLE BREAKDOWNS -- SIMPLEST ####
CO_MORTALITY_MORBIDITY = CO_COUNTY_COVID_SIMPLEST %>% 
  select(., COUNTY, 
         COVID.Tests.Per.100000, COVID.Cases.Per.100000,
         COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
         starts_with("Life.Expectancy.2010"), contains("Life.Lost"),
         contains("Hospitalizations"),
         contains("Mortality"), Violent.Crime.Rate,
         starts_with("Injury.Death.Rate"),
         starts_with("Perc.Driving.Deaths"))

CO_PRIOR_MEDICAL = CO_COUNTY_COVID_SIMPLEST  %>% 
  select(., COUNTY, 
         COVID.Tests.Per.100000, COVID.Cases.Per.100000,
         COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
         contains("Asthma"), contains("Diabetes"), 
         contains("Heart.Disease"),
         Perc.Disability, Perc.Indp.Living.Disability.Over.17, 
         Perc.Self.Care.Disability, starts_with("Low.Weight.Birth"), 
         -contains("Hospitalizations"), -contains("Mortality.Rate"),
         -contains("Mortality.Death"))
  
CO_HEALTHCARE_ACCESS = CO_COUNTY_COVID_SIMPLEST  %>% select(., COUNTY, 
  COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
  COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc, 
  starts_with("Perc.Uninsured"), starts_with("Perc.Flu.Vaccinated"), 
  starts_with("Perc.Adults.with.Obesity"), 
  contains("Delayed.Care"), contains("No.Reg.Care"),
  starts_with("Preventable.Hospitalization"),
  Primary.Care.Patients.per.Physician, Mental.Health.Patients.per.Provider,
  starts_with("Per.100000"))
                                           
CO_LIFESTYLE = CO_COUNTY_COVID_SIMPLEST  %>% select(., COUNTY, 
  COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
  COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
  starts_with("Perc.Fair.or.Poor.Health"),
  contains("Unhealthy"), Food.Environment.Index.Z.Score,
  starts_with("Perc.Physically.Inactive"), 
  Perc.With.Access.to.Exercise.Opportunities, Social.Orgs.Association.Rate,
  starts_with("Perc.Excessive.Drinking"), 
  starts_with("Perc.Smokers"), contains("Marijuana"), Perc.Adults.Cig, 
  contains("Cigarette"), Chlamydia.Rate,
  starts_with("Teen.Fertility"), starts_with("Teen.Birth.Rate"))

CO_INCOME_EDUCATION = CO_COUNTY_COVID_SIMPLEST  %>% select(., COUNTY, 
  COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
  COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
  High.School.Graduation.Rate, starts_with("Perc.Some.College"), 
  starts_with("Median.Household"), 
  Income.80th.Percentile, Income.20th.Percentile, starts_with("Perc.Unemployed"), starts_with("Perc.Children.in.Poverty"), 
  contains("Poverty"), Perc.Enrolled.in.Free.or.Reduced.Lunch,
  starts_with("Perc.Drive.Alone"))
                                           
CO_DEMOGRAPHICS = CO_COUNTY_COVID_SIMPLEST  %>% select(., COUNTY, 
  COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
  COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
  Perc.less.than.18.years.of.age, Perc.65.and.over, Perc.Female, Perc.Hispanic, 
  Perc.Non.Hispanic.White, Perc.Black, Perc.Asian, 
  Perc.AIAN.American.Indian.Alaska.Native, 
  Perc.Native.Hawaiian.Other.Pacific.Islander, Perc.Not.Proficient.in.English, 
  contains("Segregation"))           
  
CO_HOME = CO_COUNTY_COVID_SIMPLEST  %>% select(., COUNTY, 
  COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
  COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
  starts_with("Perc.Homeowners"), starts_with("Perc.Single.Parent.Households"), 
  starts_with("Perc.Severe.Housing"), 
  starts_with("Perc.Housing.Overcrowding"), 
  starts_with("Perc.Inadequate"), Average.Daily.Air.Pollution.Z.Score, 
  Perc.Rural)

CO_SUMMARY_SCORES = CO_COUNTY_COVID_SIMPLEST  %>% select(., COUNTY, 
  COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
  COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
  Years.of.Potential.Life.Lost.Rate, Life.Expectancy.2010.2015.Rate,
  ends_with("Z.Score"), -contains("Unhealthy"), -contains("Air"))

## These lists will form dropdown options in sidebar of Shiny App ##
NAMES_CO_COMPLETE_SUBLISTS = 
  c("CO_MORTALITY_MORBIDITY", "CO_PRIOR_MEDICAL", "CO_HEALTHCARE_ACCESS",
    "CO_LIFESTYLE", "CO_INCOME_EDUCATION", "CO_DEMOGRAPHICS", "CO_HOME",
    "CO_SUMMARY_SCORES")

CO_COMPLETE_SUBLISTS = 
  c("ALL LISTS", names(CO_MORTALITY_MORBIDITY)[1:6], "", "",
    "CO_MORTALITY_MORBIDITY", names(CO_MORTALITY_MORBIDITY)[-1:-6], "", "",
    "CO_PRIOR_MEDICAL", names(CO_PRIOR_MEDICAL)[-1:-6], "", "",
    "CO_HEALTHCARE_ACCESS", names(CO_HEALTHCARE_ACCESS)[-1:-6], "", "",
    "CO_LIFESTYLE", names(CO_LIFESTYLE)[-1:-6], "", "",
    "CO_INCOME_EDUCATION", names(CO_INCOME_EDUCATION)[-1:-6], "", "",
    "CO_DEMOGRAPHICS", names(CO_DEMOGRAPHICS)[-1:-6], "", "",
    "CO_HOME", names(CO_HOME)[-1:-6], "", "",
    "CO_SUMMARY_SCORES", names(CO_SUMMARY_SCORES)[-1:-6]
    )

#write.csv(CO_COMPLETE_SUBLISTS, "CO_COMPLETE_SUBLISTS.csv", row.names = F)

COVID19DATA_NAMES = names(COVID19DATA) 

#write.csv(COVID19DATA_NAMES, "COVID19DATA_NAMES.csv", row.names = F)

#### MAP PREWORK ####
CO_county_map = map_data('county', 'colorado')
CO_county_map$subregion = toupper(CO_county_map$subregion)
names(CO_county_map) = gsub("subregion", "COUNTY", 
                            names(CO_county_map))

CO_MAP_COVID = merge(CO_county_map, CO_COUNTY_COVID_FILTER, 
                     by="COUNTY")

CO_COUNTY_JUST_RURAL = CO_COUNTY_COVID_FILTER %>% 
  select(., COUNTY, Perc.Rural)

CO_Race_Measures_COVID19 = left_join(CO_Race_Measures_COVID19, 
                                     CO_COUNTY_JUST_RURAL, by="COUNTY")

#### Factors for Race/Ethnicity Ordering ####
CO_Race_Measures_COVID19$Demographic = 
  factor(CO_Race_Measures_COVID19$Demographic, 
         levels = c("ALL", "AIAN", "Asian", "Black",
                    "Hispanic", "NHPI", "Non-White", "White"))

## Creating Sub-Groups for Important Filters ##
CO_COUNTY_BAL_SLIDERS = CO_COUNTY_COVID_FILTER %>%
                mutate(., 
                       COVID.Groups = ntile(COVID.Cases.Max, 3),
                       Income.Groups = ntile(Median.Household.Income, 3),
                       # RURAL_var = ntile(Perc.Rural, 3)
                       Rural.Groups = ifelse(Perc.Rural<25,1,
                                          ifelse((Perc.Rural>=25 & Perc.Rural<50),2,
                                                 ifelse((Perc.Rural>=50 & Perc.Rural<100),3,
                                                        ifelse(Perc.Rural==100,4,NA))))) %>% 
                           select(., COUNTY, COVID.Groups, Income.Groups, Rural.Groups)
                
CO_COUNTY_BAL_SLIDERS$COVID.Groups %<>%
    gsub(1, "Low COVID", .) %>% 
    gsub(2, "Med COVID", .) %>% 
    gsub(3, "High COVID", .)
CO_COUNTY_BAL_SLIDERS$Income.Groups %<>%
    gsub(1, "Low Income", .) %>% 
    gsub(2, "Med Income", .) %>% 
    gsub(3, "High Income", .)
CO_COUNTY_BAL_SLIDERS$Rural.Groups %<>%
    gsub(1, "Urban/Suburban", .) %>% 
    gsub(2, "Somewhat Rural", .) %>% 
    gsub(3, "Mostly Rural", .) %>% 
    gsub(4, "100% Rural", .)
            
CO_MAP_COVID_BAL = left_join(CO_MAP_COVID, CO_COUNTY_BAL_SLIDERS, by="COUNTY") 

CO_MAP_COVID_BAL$Income.Groups = 
  factor(CO_MAP_COVID_BAL$Income.Groups, 
         levels = c("High Income", "Med Income", "Low Income"))  

CO_MAP_COVID_BAL$COVID.Groups = 
  factor(CO_MAP_COVID_BAL$COVID.Groups, 
         levels = c("High COVID", "Med COVID", "Low COVID")) 

## WIKIPEDIA COUNTY PAGES BY CLICKING ON MAP ##
CO_MAP_COVID_BAL$WIKI <- sprintf("window.open(\"%s%s\")",
                                 "http://en.wikipedia.org/wiki/", paste0(as.character(to_underscore(stri_trans_totitle(CO_MAP_COVID_BAL$COUNTY))), "_County,_Colorado"))
CO_MAP_COVID$WIKI <- sprintf("window.open(\"%s%s\")",
                             "http://en.wikipedia.org/wiki/", paste0(as.character(to_underscore(stri_trans_totitle(CO_MAP_COVID$COUNTY))), "_County,_Colorado"))







#####################################################################
#### ******OPTIONAL****** ADDITIONAL BREAKDOWNS BY FILTER ####
#colnames(CO_COUNTY_COVID_FILTER)
# CO_MORTALITY_MORBIDITY_FULL = CO_COUNTY_COVID_FILTER %>% select(., COUNTY, 
#    COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
#    COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
#    starts_with("Life.Expectancy.2010"), contains("Life.Lost"), contains("Hospitalizations"),
#    contains("Mortality"), Violent.Crime.Rate, starts_with("Injury.Death.Rate"), 
#    starts_with("Perc.Driving.Deaths"))
# #names(CO_MORTALITY_MORBIDITY_FULL)  
# 
# CO_PRIOR_MEDICAL_FULL = CO_COUNTY_COVID_FILTER  %>% select(., COUNTY, 
#   COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
#   COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
#   #Perc.Adults.Asthma, Perc.Adults.Diabetes, Perc.Adults.Heart.Disease, 
#   contains("Asthma"), contains("Diabetes"), contains("Heart.Disease"),
#   Perc.Disability, Perc.Indp.Living.Disability.Over.17, 
#   Perc.Self.Care.Disability, starts_with("Low.Weight.Birth"), 
#   -contains("Hospitalizations"), -contains("Mortality"))
# #names(CO_PRIOR_MEDICAL_FULL)
# 
# CO_HEALTHCARE_ACCESS_FULL = CO_COUNTY_COVID_FILTER  %>% select(., COUNTY, 
#   COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
#   COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
#   starts_with("Perc.Uninsured"), starts_with("Perc.Flu.Vaccinated"), 
#   #Perc.Adults.Delayed.Care, Perc.Adults.No.Reg.Care.12mos,
#   starts_with("Perc.Adults.with.Obesity"), 
#   contains("Delayed.Care"), contains("No.Reg.Care"),
#   starts_with("Preventable.Hospitalization"),
#   Primary.Care.Patients.per.Physician, Mental.Health.Patients.per.Provider,
#   starts_with("Per.100000"))
# #names(CO_HEALTHCARE_ACCESS_FULL)
# 
# CO_LIFESTYLE_FULL = CO_COUNTY_COVID_FILTER  %>% select(., COUNTY, 
#   COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
#   COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
#   starts_with("Perc.Fair.or.Poor.Health"),
#   contains("Unhealthy"), Food.Environment.Index.Z.Score,
#   starts_with("Perc.Physically.Inactive"), 
#   Perc.With.Access.to.Exercise.Opportunities, Social.Orgs.Association.Rate,
#   starts_with("Perc.Excessive.Drinking"), 
#   starts_with("Perc.Smokers"), contains("Marijuana"), Perc.Adults.Cig, 
#   contains("Cigarette"), Chlamydia.Rate,
#   starts_with("Teen.Fertility"), starts_with("Teen.Birth.Rate"))
# #names(CO_LIFESTYLE_FULL)
# 
# CO_INCOME_EDUCATION_FULL = CO_COUNTY_COVID_FILTER  %>% select(., COUNTY, 
#   COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
#   COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
#   High.School.Graduation.Rate, starts_with("Perc.Some.College"), 
#   starts_with("Median.Household"), 
#   Income.80th.Percentile, Income.20th.Percentile, starts_with("Perc.Unemployed"), starts_with("Perc.Children.in.Poverty"), 
#   contains("Poverty"), Perc.Enrolled.in.Free.or.Reduced.Lunch,
#   starts_with("Perc.Drive.Alone"))
# #names(CO_INCOME_EDUCATION_FULL)
# 
# CO_DEMOGRAPHICS_FULL = CO_COUNTY_COVID_FILTER  %>% select(., COUNTY, 
#   COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
#   COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
#   Perc.less.than.18.years.of.age, Perc.65.and.over, Perc.Female, Perc.Hispanic, 
#   Perc.Non.Hispanic.White, Perc.Black, Perc.Asian, 
#   Perc.AIAN.American.Indian.Alaska.Native, 
#   Perc.Native.Hawaiian.Other.Pacific.Islander, Perc.Not.Proficient.in.English, 
#   contains("Segregation"))           
# #names(CO_DEMOGRAPHICS_FULL)
# 
# CO_HOME_FULL = CO_COUNTY_COVID_FILTER  %>% select(., COUNTY, 
#   COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
#   COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
#   starts_with("Perc.Homeowners"), starts_with("Perc.Single.Parent.Households"), 
#   starts_with("Perc.Severe.Housing"), 
#   starts_with("Perc.Housing.Overcrowding"), 
#   starts_with("Perc.Inadequate"), Average.Daily.Air.Pollution.Z.Score, 
#   Perc.Rural)
# #names(CO_HOME_FULL)
# 
# CO_SUMMARY_SCORES_FULL = CO_COUNTY_COVID_FILTER  %>% select(., COUNTY, 
#   COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
#   COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
#   Years.of.Potential.Life.Lost.Rate, Life.Expectancy.2010.2015.Rate,
#   ends_with("Z.Score"), -contains("Unhealthy"), -contains("Air"))
# #names(CO_SUMMARY_SCORES_FULL)
# 
# CO_COMPLETE_SUBLISTS_FULL = 
#   c("ALL LISTS", names(CO_MORTALITY_MORBIDITY_FULL)[1:6], "", "",
#     "CO_MORTALITY_MORBIDITY_FULL", names(CO_MORTALITY_MORBIDITY_FULL)[-1:-6], "", "",
#     "CO_PRIOR_MEDICAL_FULL", names(CO_PRIOR_MEDICAL_FULL)[-1:-6], "", "",
#     "CO_HEALTHCARE_ACCESS_FULL", names(CO_HEALTHCARE_ACCESS_FULL)[-1:-6], "", "",
#     "CO_LIFESTYLE_FULL", names(CO_LIFESTYLE_FULL)[-1:-6], "", "",
#     "CO_INCOME_EDUCATION_FULL", names(CO_INCOME_EDUCATION_FULL)[-1:-6], "", "",
#     "CO_DEMOGRAPHICS_FULL", names(CO_DEMOGRAPHICS_FULL)[-1:-6], "", "",
#     "CO_HOME_FULL", names(CO_HOME_FULL)[-1:-6], "", "",
#     "CO_SUMMARY_SCORES_FULL", names(CO_SUMMARY_SCORES_FULL)[-1:-6]
#   )
# #CO_COMPLETE_SUBLISTS_FULL
# write.csv(CO_COMPLETE_SUBLISTS_FULL, "CO_COMPLETE_SUBLISTS_FULL.csv", 
#           row.names = F)

# #### SUBTABLE BREAKDOWNS -- NO DEMO ####
# #colnames(CO_COUNTY_COVID_RATES_NO_DEMO)
# CO_MORTALITY_MORBIDITY_NO_DEMO = CO_COUNTY_COVID_RATES_NO_DEMO %>% select(., COUNTY, 
#     COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
#     COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
#     starts_with("Life.Expectancy.2010"), contains("Life.Lost"), contains("Hospitalizations"),
#     contains("Mortality"), Violent.Crime.Rate, starts_with("Injury.Death.Rate"), 
#     starts_with("Perc.Driving.Deaths"))
# #names(CO_MORTALITY_MORBIDITY_NO_DEMO)  
# 
# CO_PRIOR_MEDICAL_NO_DEMO = CO_COUNTY_COVID_RATES_NO_DEMO  %>% select(., COUNTY, 
#    COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
#    COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
#    #Perc.Adults.Asthma, Perc.Adults.Diabetes, Perc.Adults.Heart.Disease, 
#    contains("Asthma"), contains("Diabetes"), contains("Heart.Disease"),
#    Perc.Disability, Perc.Indp.Living.Disability.Over.17, 
#    Perc.Self.Care.Disability, starts_with("Low.Weight.Birth"), 
#    -contains("Hospitalizations"), -contains("Mortality"))
# #names(CO_PRIOR_MEDICAL_NO_DEMO)
# 
# CO_HEALTHCARE_ACCESS_NO_DEMO = CO_COUNTY_COVID_RATES_NO_DEMO  %>% select(., COUNTY, 
#    COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
#    COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
#    starts_with("Perc.Uninsured"), starts_with("Perc.Flu.Vaccinated"), 
#    #Perc.Adults.Delayed.Care, Perc.Adults.No.Reg.Care.12mos,
#    starts_with("Perc.Adults.with.Obesity"), 
#    contains("Delayed.Care"), contains("No.Reg.Care"),
#    starts_with("Preventable.Hospitalization"),
#    Primary.Care.Patients.per.Physician, Mental.Health.Patients.per.Provider,
#    starts_with("Per.100000"))
# #names(CO_HEALTHCARE_ACCESS_NO_DEMO)
# 
# CO_LIFESTYLE_NO_DEMO = CO_COUNTY_COVID_RATES_NO_DEMO  %>% select(., COUNTY, 
#    COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
#    COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
#    starts_with("Perc.Fair.or.Poor.Health"),
#    contains("Unhealthy"), Food.Environment.Index.Z.Score,
#    starts_with("Perc.Physically.Inactive"), 
#    Perc.With.Access.to.Exercise.Opportunities, Social.Orgs.Association.Rate,
#    starts_with("Perc.Excessive.Drinking"), 
#    starts_with("Perc.Smokers"), contains("Marijuana"), Perc.Adults.Cig, 
#    contains("Cigarette"), Chlamydia.Rate,
#    starts_with("Teen.Fertility"), starts_with("Teen.Birth.Rate"))
# #names(CO_LIFESTYLE_NO_DEMO)
# 
# CO_INCOME_EDUCATION_NO_DEMO = CO_COUNTY_COVID_RATES_NO_DEMO  %>% select(., COUNTY, 
#   COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
#   COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
#   High.School.Graduation.Rate, starts_with("Perc.Some.College"), 
#   starts_with("Median.Household"), 
#   Income.80th.Percentile, Income.20th.Percentile, starts_with("Perc.Unemployed"), starts_with("Perc.Children.in.Poverty"), 
#   contains("Poverty"), Perc.Enrolled.in.Free.or.Reduced.Lunch,
#   starts_with("Perc.Drive.Alone"))
# #names(CO_INCOME_EDUCATION_NO_DEMO)
# 
# CO_DEMOGRAPHICS_NO_DEMO = CO_COUNTY_COVID_RATES_NO_DEMO  %>% select(., COUNTY, 
#   COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
#   COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
#   Perc.less.than.18.years.of.age, Perc.65.and.over, Perc.Female, Perc.Hispanic, 
#   Perc.Non.Hispanic.White, Perc.Black, Perc.Asian, 
#   Perc.AIAN.American.Indian.Alaska.Native, 
#   Perc.Native.Hawaiian.Other.Pacific.Islander, Perc.Not.Proficient.in.English, 
#   contains("Segregation"))           
# #names(CO_DEMOGRAPHICS_NO_DEMO)
# 
# CO_HOME_NO_DEMO = CO_COUNTY_COVID_RATES_NO_DEMO  %>% select(., COUNTY, 
#   COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
#   COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
#   starts_with("Perc.Homeowners"), starts_with("Perc.Single.Parent.Households"), 
#   starts_with("Perc.Severe.Housing"), 
#   starts_with("Perc.Housing.Overcrowding"), 
#   starts_with("Perc.Inadequate"), Average.Daily.Air.Pollution.Z.Score, 
#   Perc.Rural)
# #names(CO_HOME_NO_DEMO)
# 
# CO_SUMMARY_SCORES_NO_DEMO = CO_COUNTY_COVID_RATES_NO_DEMO  %>% select(., COUNTY, 
#   COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
#   COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
#   Years.of.Potential.Life.Lost.Rate, Life.Expectancy.2010.2015.Rate,
#   ends_with("Z.Score"), -contains("Unhealthy"), -contains("Air"))
# #names(CO_SUMMARY_SCORES_NO_DEMO)
# 
# CO_COMPLETE_SUBLISTS_NO_DEMO = 
#   c("ALL LISTS", names(CO_MORTALITY_MORBIDITY_NO_DEMO)[1:6], "", "",
#     "CO_MORTALITY_MORBIDITY_NO_DEMO", names(CO_MORTALITY_MORBIDITY_NO_DEMO)[-1:-6], "", "",
#     "CO_PRIOR_MEDICAL_NO_DEMO", names(CO_PRIOR_MEDICAL_NO_DEMO)[-1:-6], "", "",
#     "CO_HEALTHCARE_ACCESS_NO_DEMO", names(CO_HEALTHCARE_ACCESS_NO_DEMO)[-1:-6], "", "",
#     "CO_LIFESTYLE_NO_DEMO", names(CO_LIFESTYLE_NO_DEMO)[-1:-6], "", "",
#     "CO_INCOME_EDUCATION_NO_DEMO", names(CO_INCOME_EDUCATION_NO_DEMO)[-1:-6], "", "",
#     "CO_DEMOGRAPHICS_NO_DEMO", names(CO_DEMOGRAPHICS_NO_DEMO)[-1:-6], "", "",
#     "CO_HOME_NO_DEMO", names(CO_HOME_NO_DEMO)[-1:-6], "", "",
#     "CO_SUMMARY_SCORES_NO_DEMO", names(CO_SUMMARY_SCORES_NO_DEMO)[-1:-6]
#   )
# #CO_COMPLETE_SUBLISTS_NO_DEMO
# write.csv(CO_COMPLETE_SUBLISTS_NO_DEMO, "CO_COMPLETE_SUBLISTS_NO_DEMO.csv", 
#           row.names = F)
