---
title: "CO CoVID"
author: "David Gottlieb"
date: "5/13/2020"
output: html_document
runtime: shiny
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(Hmisc)
library(tidyverse)
library(magrittr)
library(rsconnect)
devtools::install_version("mnormt", version = "1.5-7", repos = "http://cran.us.r-project.org")

setwd("C:/Users/gottl/Dropbox/Data Science/NYCDSA/Projects/Project 1 Shiny App/CO_COVID")

#### RWJ Demographic Data ####
COdataRWJ2020 = read.csv("2020 County Health Rankings Colorado Data Life Measures CLEANED 2 col header.csv", stringsAsFactors = F, header = T)
names(COdataRWJ2020) = COdataRWJ2020[1, ]
COdataRWJ2020 = COdataRWJ2020[-1, ]
names(COdataRWJ2020) %<>%  
  gsub("(", "", ., fixed=T) %>% 
  gsub(")", "", ., fixed=T) %>% 
  make.names(.) %>% 
  gsub("...", ".", ., fixed=T) %>% 
  gsub("..", ".", ., fixed=T)

CORWJnames = names(COdataRWJ2020)

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
  gsub("Average.Daily.PM2.5", "Average.Daily.Air.Pollution", ., fixed=T)


#CORWJnames
names(COdataRWJ2020) = CORWJnames
names(COdataRWJ2020) = 
  gsub("County", "COUNTY", names(COdataRWJ2020))
COdataRWJ2020$Primary.Care.Physicians.Ratio = 
  gsub(":.*", "", COdataRWJ2020$Primary.Care.Physicians.Ratio)
names(COdataRWJ2020) = 
  gsub("Primary.Care.Physicians.Ratio", "Primary.Care.Patients.per.Physician", names(COdataRWJ2020))
COdataRWJ2020$Mental.Health.Provider.Ratio = 
  gsub(":.*", "", COdataRWJ2020$Mental.Health.Provider.Ratio)
names(COdataRWJ2020) = 
  gsub("Mental.Health.Provider.Ratio", "Mental.Health.Patients.per.Provider", names(COdataRWJ2020))
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
#names(COdataRWJ2020)
COdataRWJ2020 = COdataRWJ2020[,-231:-233]
#names(COdataRWJ2020)
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
#names(COdataRWJ2020)
#COdataRWJ2020[,264:265]
COdataRWJ2020[ ,-c(2,170)] = 
  lapply(COdataRWJ2020[ ,-c(2,170)], as.numeric)
#names(COdataRWJ2020)
#lapply(COdataRWJ2020, class)




#### COdataRWJ2020 Subtables ####
## Rankings Only Table ##
COdataRWJRank = COdataRWJ2020 %>% 
  select(., COUNTY, ends_with("Rank"))
#names(COdataRWJRank)
#write.csv(names(COdataRWJRank), "names_COdataRWJRank.csv", row.names = F)

## Simplified (Measures-only) Table ##
COdataRWJ2020simp = COdataRWJ2020 %>% 
  select(., -contains("95"), -contains("Num."), -FIPS, 
         -matches("(Perc.*Score)"), -matches("(Rate.*Score)"), 
         -matches("(Ratio.*Score)"), -matches("(Water.*Score)"), 
         -ends_with("Rank"), -ends_with("Unhealthy.Days"), 
         -Food.Environment.Index)
COdataRWJ2020simp = COdataRWJ2020simp %>% 
  select(., COUNTY, Population, Population.Age.25.to.44, 
         everything())
#names(COdataRWJ2020simp)
write.csv(names(COdataRWJ2020simp), "names_COdataRWJ2020simp.csv", row.names = F)

## Simplified (Measures-only) + No Demo Breakdown Table ##
COdataRWJ2020simp_no_demo = COdataRWJ2020simp %>% 
  select(.,   -contains("e.AIAN"), -contains("e.Asian"), 
         -contains("e.Black"), -contains("e.Hispanic"), 
         -contains("e.White"), -contains("d.AIAN"), 
         -contains("d.Asian"), -contains("d.Black"), 
         -contains("d.Hispanic"), -contains("d.White"), 
         -contains("y.AIAN"), -contains("y.Asian"), 
         -contains("y.Black"), -contains("y.Hispanic"), 
         -contains("y.White"))
#names(COdataRWJ2020simp_no_demo)
COdataRWJ2020simp_no_demo[,43:44]
write.csv(names(COdataRWJ2020simp_no_demo), "names_COdataRWJ2020simp_no_demo.csv",row.names = F)

## Z-Scores-only Table ##
COdataRWJ2020_Zscores = COdataRWJ2020 %>% 
  select(., COUNTY, contains("Z.Score"))



#### Race Demographics Sub-Table ####
COdataRWJ2020raceFULL = COdataRWJ2020 %>% 
  select(., COUNTY, contains("YPLL"), contains("Teen."), 
         contains("Preventable.H"), contains("Vaccinated"), 
         contains("in.Poverty"), contains("Injury.Death"), 
         contains("Drive.Alone"), contains("Household.Income"), 
         contains("AIAN"), contains("Asian"), contains("Black"), 
         contains("Hispanic"), contains("White"), 
         contains("Native"))

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
#names(COdataRWJ2020raceFULL) 

COdataRWJ2020race = 
  COdataRWJ2020raceFULL %>%  select(., -contains("Num."))

COdataRWJ2020race = COdataRWJ2020race %>%
  pivot_longer(
   cols = contains("_"), 
    names_to = c("Category", "Test.Type"),
    names_sep = "_",
    values_to = "Value",
    values_drop_na = TRUE  )
#head(COdataRWJ2020race)

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
#names(COdataRWJ2020race)

write.csv(COdataRWJ2020race, "CO_Demographic_Race_Data_by_County_Expanded.csv", row.names = F)

#CO_COUNTY = left_join(COdataRWJ2020, Zscores2020RWJ, by = ("COUNTY"))

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
# names(Health_Outcomes_Master)
# lapply(Health_Outcomes_Master, class)

CO_COUNTY = left_join(COdataRWJ2020, Health_Outcomes_Master, by = ("COUNTY"))
#names(CO_COUNTY)

#### Non-Hospitalization Health Rates BRFSS ####

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

names(Asthma_Rates) = 
  gsub("Asthma_Confidence_Interval", "Asthma_County_Rate", 
       names(Asthma_Rates))
names(Asthma_Rates) = 
  gsub("Per_Adults_Asthma", "Perc_Adults_Asthma", names(Asthma_Rates))
Asthma_Rates$Asthma_County_Rate = 
  gsub("County/Regional Estimate ", "", Asthma_Rates$Asthma_County_Rate)
Asthma_Rates$Asthma_County_Rate = 
  gsub("%.*", "", Asthma_Rates$Asthma_County_Rate)
Asthma_Rates$Asthma_County_Rate =
  as.numeric(Asthma_Rates$Asthma_County_Rate)

Asthma_Rates$Asthma_Rate_CIL95 = 
  gsub(".*: ", "", Asthma_Rates$Asthma_Rate_CIL95)
Asthma_Rates$Asthma_Rate_CIL95 = 
  gsub(" -.*", "", Asthma_Rates$Asthma_Rate_CIL95)
Asthma_Rates$Asthma_Rate_CIL95 = 
  as.numeric(Asthma_Rates$Asthma_Rate_CIL95)

Asthma_Rates$Asthma_Rate_CIH95 = 
  gsub(".*- ", "", Asthma_Rates$Asthma_Rate_CIH95)
Asthma_Rates$Asthma_Rate_CIH95 = 
  gsub(")", "", Asthma_Rates$Asthma_Rate_CIH95)
Asthma_Rates$Asthma_Rate_CIH95 = 
  as.numeric(Asthma_Rates$Asthma_Rate_CIH95)
####

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

names(Cig_Rates) = 
  gsub("Cigarette_Smoking_Colorado_Estimate", "Cig_Colorado_Estimate", names(Cig_Rates))
names(Cig_Rates) = 
  gsub("Cigarette_Smoking_Confidence_Interval", "Cig_County_Rate", names(Cig_Rates))
names(Cig_Rates) = 
  gsub("Per_Adults_Currently_Smoking_Cigarettes", "Perc_Adults_Cig", names(Cig_Rates))
Cig_Rates$Cig_County_Rate = 
  gsub("County/Regional Estimate ", "", Cig_Rates$Cig_County_Rate)
Cig_Rates$Cig_County_Rate = 
  gsub("%.*", "", Cig_Rates$Cig_County_Rate)
Cig_Rates$Cig_County_Rate = 
  as.numeric(Cig_Rates$Cig_County_Rate)

Cig_Rates$Cig_Rate_CIL95 = gsub(".*: ", "", Cig_Rates$Cig_Rate_CIL95)
Cig_Rates$Cig_Rate_CIL95 = gsub(" -.*", "", Cig_Rates$Cig_Rate_CIL95)
Cig_Rates$Cig_Rate_CIL95 = as.numeric(Cig_Rates$Cig_Rate_CIL95)

Cig_Rates$Cig_Rate_CIH95 = gsub(".*- ", "", Cig_Rates$Cig_Rate_CIH95)
Cig_Rates$Cig_Rate_CIH95 = gsub(")", "", Cig_Rates$Cig_Rate_CIH95)
Cig_Rates$Cig_Rate_CIH95 = as.numeric(Cig_Rates$Cig_Rate_CIH95)
####

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
names(Diabetes_Rates) = 
  gsub("Diabetes_Confidence_Interval", "Diabetes_County_Rate", 
       names(Diabetes_Rates))
names(Diabetes_Rates) = 
  gsub("Per_Adults_Diabetes", "Perc_Adults_Diabetes", 
       names(Diabetes_Rates))
Diabetes_Rates$Diabetes_County_Rate = 
  gsub("County/Regional Estimate ", "", 
       Diabetes_Rates$Diabetes_County_Rate)
Diabetes_Rates$Diabetes_County_Rate = 
  gsub("%.*", "", Diabetes_Rates$Diabetes_County_Rate)
Diabetes_Rates$Diabetes_County_Rate = 
  as.numeric(Diabetes_Rates$Diabetes_County_Rate)

Diabetes_Rates$Diabetes_Rate_CIL95 = 
  gsub(".*: ", "", Diabetes_Rates$Diabetes_Rate_CIL95)
Diabetes_Rates$Diabetes_Rate_CIL95 = 
  gsub(" -.*", "", Diabetes_Rates$Diabetes_Rate_CIL95)
Diabetes_Rates$Diabetes_Rate_CIL95 = 
  as.numeric(Diabetes_Rates$Diabetes_Rate_CIL95)

Diabetes_Rates$Diabetes_Rate_CIH95 = 
  gsub(".*- ", "", Diabetes_Rates$Diabetes_Rate_CIH95)
Diabetes_Rates$Diabetes_Rate_CIH95 = 
  gsub(")", "", Diabetes_Rates$Diabetes_Rate_CIH95)
Diabetes_Rates$Diabetes_Rate_CIH95 = 
  as.numeric(Diabetes_Rates$Diabetes_Rate_CIH95)
####
 
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
names(Heart_Disease_Rates) = 
  gsub("Heart_Disease_Confidence_Interval", "Heart_Disease_County_Rate", 
       names(Heart_Disease_Rates))
names(Heart_Disease_Rates) = 
  gsub("Per_Adults_CoronaryHeartDisease", "Perc_Adults_Heart_Disease", 
       names(Heart_Disease_Rates))
Heart_Disease_Rates$Heart_Disease_County_Rate = 
  gsub("County/Regional Estimate ", "", 
       Heart_Disease_Rates$Heart_Disease_County_Rate)
Heart_Disease_Rates$Heart_Disease_County_Rate = 
  gsub("%.*", "", Heart_Disease_Rates$Heart_Disease_County_Rate)
Heart_Disease_Rates$Heart_Disease_County_Rate = 
  as.numeric(Heart_Disease_Rates$Heart_Disease_County_Rate)

Heart_Disease_Rates$Heart_Disease_Rate_CIL95 = 
  gsub(".*: ", "", Heart_Disease_Rates$Heart_Disease_Rate_CIL95)
Heart_Disease_Rates$Heart_Disease_Rate_CIL95 = 
  gsub(" -.*", "", Heart_Disease_Rates$Heart_Disease_Rate_CIL95)
Heart_Disease_Rates$Heart_Disease_Rate_CIL95 = 
  as.numeric(Heart_Disease_Rates$Heart_Disease_Rate_CIL95)

Heart_Disease_Rates$Heart_Disease_Rate_CIH95 = 
  gsub(".*- ", "", Heart_Disease_Rates$Heart_Disease_Rate_CIH95)
Heart_Disease_Rates$Heart_Disease_Rate_CIH95 = 
  gsub(")", "", Heart_Disease_Rates$Heart_Disease_Rate_CIH95)
Heart_Disease_Rates$Heart_Disease_Rate_CIH95 = 
  as.numeric(Heart_Disease_Rates$Heart_Disease_Rate_CIH95)
####

#### MJ Rates #####
MJ_Rates = read.csv("Marijuana_Use_in_Adults_-_Colorado_BRFSS_2014-2017_(County).csv", stringsAsFactors = F, header=T)
#unique(colnames(MJ_Rates))
MJ_Rates = MJ_Rates %>% 
  select(., -ï..OBJECTID, -LABEL) %>% 
  mutate(., MJ_Rate_CIL95 = MJ_Use_Confidence_Interval, 
         MJ_Rate_CIH95 = MJ_Use_Confidence_Interval, 
         MJ_Use_Colorado_Estimate = as.numeric("14.0"), 
         MJ_Colorado_CIL95 = as.numeric("13.5"), 
         MJ_Colorado_CIH95 = as.numeric("14.5"))
names(MJ_Rates) = 
  gsub("MJ_Use_Confidence_Interval", "MJ_County_Rate", names(MJ_Rates))
names(MJ_Rates) = 
  gsub("Per_Adults_MJ_30day", "Perc_Adults_MJ_30day", names(MJ_Rates))
MJ_Rates$MJ_County_Rate = 
  gsub("County/Regional Estimate ", "", MJ_Rates$MJ_County_Rate)
MJ_Rates$MJ_County_Rate = 
  gsub("%.*", "", MJ_Rates$MJ_County_Rate)
MJ_Rates$MJ_County_Rate = as.numeric(MJ_Rates$MJ_County_Rate)

MJ_Rates$MJ_Rate_CIL95 = gsub(".*: ", "", MJ_Rates$MJ_Rate_CIL95)
MJ_Rates$MJ_Rate_CIL95 = gsub(" -.*", "", MJ_Rates$MJ_Rate_CIL95)
MJ_Rates$MJ_Rate_CIL95 = as.numeric(MJ_Rates$MJ_Rate_CIL95)

MJ_Rates$MJ_Rate_CIH95 = gsub(".*- ", "", MJ_Rates$MJ_Rate_CIH95)
MJ_Rates$MJ_Rate_CIH95 = gsub(")", "", MJ_Rates$MJ_Rate_CIH95)
MJ_Rates$MJ_Rate_CIH95 = as.numeric(MJ_Rates$MJ_Rate_CIH95)
####

#### Delayed Care Rates ####
Delayed_Care_Rates = read.csv("Delayed_Medical_Care_in_Adults_($)_-_Colorado_BRFSS_2014-2017_(County).csv", stringsAsFactors = F, header = T)

Delayed_Care_Rates = Delayed_Care_Rates %>% 
  select(., -ï..OBJECTID, -LABEL) %>% 
  mutate(., Delayed_Care_Rate_CIL95 = 
           Delayed_Medical_Care_Cost_Confidence_Interval, 
         Delayed_Care_Rate_CIH95 = 
           Delayed_Medical_Care_Cost_Confidence_Interval, 
         Delayed_Medical_Care_Cost_Colorado_Estimate = 
           as.numeric("12.4"), 
         Delayed_Care_Colorado_CIL95 = as.numeric("12.0"), 
         Delayed_Care_Colorado_CIH95 = as.numeric("12.8"))
names(Delayed_Care_Rates) = 
  gsub("Delayed_Medical_Care_Cost_Confidence_Interval", 
       "Delayed_Care_County_Rate", names(Delayed_Care_Rates))
names(Delayed_Care_Rates) = 
  gsub("Per_Adults_Delayed_Medical_Care_DuetoCosts", 
       "Perc_Adults_Delayed_Care", names(Delayed_Care_Rates))

Delayed_Care_Rates$Delayed_Care_County_Rate = 
  gsub("County/Regional Estimate ", "", 
       Delayed_Care_Rates$Delayed_Care_County_Rate)
Delayed_Care_Rates$Delayed_Care_County_Rate = 
  gsub("%.*", "", Delayed_Care_Rates$Delayed_Care_County_Rate)
Delayed_Care_Rates$Delayed_Care_County_Rate = 
  as.numeric(Delayed_Care_Rates$Delayed_Care_County_Rate)

Delayed_Care_Rates$Delayed_Care_Rate_CIL95 = 
  gsub(".*: ", "", Delayed_Care_Rates$Delayed_Care_Rate_CIL95)
Delayed_Care_Rates$Delayed_Care_Rate_CIL95 = 
  gsub(" -.*", "", Delayed_Care_Rates$Delayed_Care_Rate_CIL95)
Delayed_Care_Rates$Delayed_Care_Rate_CIL95 = 
  as.numeric(Delayed_Care_Rates$Delayed_Care_Rate_CIL95)

Delayed_Care_Rates$Delayed_Care_Rate_CIH95 = 
  gsub(".*- ", "", Delayed_Care_Rates$Delayed_Care_Rate_CIH95)
Delayed_Care_Rates$Delayed_Care_Rate_CIH95 = 
  gsub(")", "", Delayed_Care_Rates$Delayed_Care_Rate_CIH95)
Delayed_Care_Rates$Delayed_Care_Rate_CIH95 = 
  as.numeric(Delayed_Care_Rates$Delayed_Care_Rate_CIH95)
####

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
names(No_Reg_Care_Rates) = 
  gsub("No_Checkup_12mos_Confidence_Interval", "No_Reg_Care_County_Rate",
       names(No_Reg_Care_Rates))
names(No_Reg_Care_Rates) = 
  gsub("Per_Adults_NoCheckup_12months", "Perc_Adults_No_Reg_Care_12mos", 
       names(No_Reg_Care_Rates))

No_Reg_Care_Rates$No_Reg_Care_County_Rate = 
  gsub("County/Regional Estimate ", "", 
       No_Reg_Care_Rates$No_Reg_Care_County_Rate)
No_Reg_Care_Rates$No_Reg_Care_County_Rate = 
  gsub("%.*", "", No_Reg_Care_Rates$No_Reg_Care_County_Rate)
No_Reg_Care_Rates$No_Reg_Care_County_Rate = 
  as.numeric(No_Reg_Care_Rates$No_Reg_Care_County_Rate)

No_Reg_Care_Rates$No_Reg_Care_Rate_CIL95 = 
  gsub(".*: ", "", No_Reg_Care_Rates$No_Reg_Care_Rate_CIL95)
No_Reg_Care_Rates$No_Reg_Care_Rate_CIL95 = 
  gsub(" -.*", "", No_Reg_Care_Rates$No_Reg_Care_Rate_CIL95)
No_Reg_Care_Rates$No_Reg_Care_Rate_CIL95 = 
  as.numeric(No_Reg_Care_Rates$No_Reg_Care_Rate_CIL95)

No_Reg_Care_Rates$No_Reg_Care_Rate_CIH95 = 
  gsub(".*- ", "", No_Reg_Care_Rates$No_Reg_Care_Rate_CIH95)
No_Reg_Care_Rates$No_Reg_Care_Rate_CIH95 = 
  gsub(")", "", No_Reg_Care_Rates$No_Reg_Care_Rate_CIH95)
No_Reg_Care_Rates$No_Reg_Care_Rate_CIH95 = 
  as.numeric(No_Reg_Care_Rates$No_Reg_Care_Rate_CIH95)
####

#### Disability ####
Disability =  read.csv("Disability_(Census_Tracts).csv", stringsAsFactors = F, header=T)
    # For Census, Data, need to aggregate values by County - 
Disability = Disability %>% 
      select(-starts_with("Percent"), 
             -starts_with("Population_Density"), 
             -starts_with("Tract"), -contains("OBJECTID"), -FIPS) %>%
      group_by(County) %>% 
      summarise_if(., is.numeric, funs(sum))
      
names(Disability) = gsub("County", "COUNTY", names(Disability))

Disability = Disability %>% 
  mutate(., Perc_Disability_TCNPop_With_A_Self_Care_Difficulty_Age_Over_4 = 100*Disability_TCNPop_With_A_Self_Care_Difficulty_Age_Over_4/Population_Total,
Perc_Disability_TCNPop_With_A_Self_Care_Difficulty_Age517 = 100*Disability_TCNPop_With_A_Self_Care_Difficulty_Age517/Population_Total,
Perc_Disability_TCNPop_AgeOver17_With_An_IndLiving_Difficulty = 100*Disability_TCNPop_AgeOver17_With_An_IndLiving_Difficulty/Population_Total, 
Perc_Disability_TCNPop_With_A_Disability = 100*Disability_TCNPop_With_A_Disability/Population_Total)

#### Life Expectancy 2010-2015 ####
Life_Expectancy2015 =  read.csv("Colorado_Life_Expectancy_by_Census_Tract_Published_by_NAPHSIS-USALEEP_(2010-2015).csv", stringsAsFactors = F, header=T)

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

#names(Non_Hospitalization_Health)

#### ADDING NON-HOSPITALIZATION HEALTH ####

CO_COUNTY_ALL = left_join(CO_COUNTY, Non_Hospitalization_Health, by = "COUNTY")


CO_COUNTY_ALL = CO_COUNTY_ALL %>% 
  mutate(., Perc.100000.Total.Dis.Providers = 
           100000*Total.Dis.Providers/Population.Total,
Perc.100000.Available.Beds = 100000*Available.Beds/Population.Total,
Perc.100000.Total.Beds = 100000*Total.Beds/Population.Total,
Perc.100000.Assisted.Living.Facs = 100000*Assisted.Living.Facs/Population.Total,
Perc.100000.Total.Health.Facilities = 100000*Total.Health.Facilities/Population.Total)

#### Politics ####
# Affiliation = read.csv("2018GeneralResults.csv", stringsAsFactors = F)
# Affiliation$COUNTY %<>% gsub("TOTAL", "COLORADO")
# Affiliation = Affiliation %>% 
#   select(COUNTY, everything()) 

##### COVID ######
## COVID DATASET #1 ##
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
COVID19County = COVID19County %>% 
  mutate(., Number = (Value + Rate)) %>% 
  filter(., Metric !="Some cases may still be under investigation and county not assigned yet." & 
           Metric != "Rates are not shown for counties with fewer than 5 cases." & 
           Metric != "Individuals with serology-positive tests are not included in daily case counts until they are confirmed to have had COVID-like symptoms." & 
           Metric != "Includes only tests from labs that participate in electronic lab reporting." & 
           Metric != "County rates per 100,000 are calculated using 2018 population estimates from the Demography Section, Colorado Division of Local Government." & 
           Metric != "Caution should be used when interpreting rates in counties with small populations." & 
           Metric != "Percent of tests by Serology" & Metric != "Percent of tests by PCR" 
         & COUNTY != "Note")
#unique(COVID19County$Desc_)

#names(COVID19County)
#unique(COVID19County$COUNTY)
COVID19County = COVID19County %>% 
  select(., -Rate, -FIPS, -FULL_, -LABEL, -Value, -ObjectId) %>% 
  pivot_wider(., id_cols=c(COUNTY, POP, Date),
              names_from=Desc_, 
              values_from=Number,
              values_fn = list(Number = mean)
              )

#lapply(COVID19County, class)
colnames(COVID19County) = make.names(colnames(COVID19County))
COVID19County = COVID19County %>% 
  arrange(COUNTY, Date)
COVID19County$Date = 
  as.Date(COVID19County$Date, format="%m/%d/%Y")


#lapply(COVID19County[,-1:-3], as.numeric)
#lapply(COVID19County, class)

## COVID DATASET #2 ##
COVID19Positive = read.csv("Colorado_COVID-19_Positive_Cases_and_Rates_of_Infection_by_County_of_Identification.csv", header=T, stringsAsFactors = F)

names(COVID19Positive) %<>% gsub("__", "_", ., fixed=T) 
COVID19Positive$County_Deaths[is.na(COVID19Positive$County_Deaths)] <- 0
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
#names(COVID19County)
#COVID19County$Testing.Rates.Per.100000
# colnames(COVID19County)

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
             COVID_Cases_Max = max(Colorado.Case.Counts.by.County, na.rm = TRUE),
             COVID_Max_Testing_Rates_Per_100000 =
               max(Testing.Rates.Per.100000, na.rm = TRUE),
             COVID_Mean_Testing_Rates_Per_100000 =
               mean(Testing.Rates.Per.100000, na.rm = TRUE),
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
COVID19CountyANALYSIS$COVID_Max_Case_Rates_Per_100000[COVID19CountyANALYSIS$COVID_Max_Case_Rates_Per_100000=="-Inf"]=0
COVID19CountyANALYSIS$COVID_Mean_Case_Rates_Per_100000[COVID19CountyANALYSIS$COVID_Mean_Case_Rates_Per_100000=="NaN"]=0

# colnames(COVID19CountyANALYSIS)
COVID19CountyANALYSIS = COVID19CountyANALYSIS %>% 
  mutate(.,
         COVID.Positive.Tests.Perc = ifelse(COVID_Tests_Max!=0, 
                                        100*COVID_Cases_Max/COVID_Tests_Max, NA),
         COVID_Death_Rate_if_Pos_Case_Perc = 
           ifelse(COVID_County_Pos_Case_Rate_Per_100000!=0,
                  100*COVID_County_Death_Rate_Per_100000/
           COVID_County_Pos_Case_Rate_Per_100000, NA)
         )


  

#### COVID DATASET #3 STATE & COUNTY DAILY DATA  ####
COVID19StateData = read.csv("CDPHE_COVID19_Daily_State_Statistics.csv", header=T, stringsAsFactors = F)
COVID19StateData$Date = as.Date(COVID19StateData$Date, format="%m/%d/%Y")
#colnames(COVID19StateData)
COVID19StateData = COVID19StateData %>% 
  select(., COUNTY = ï..Name, POP = Population, Date, Tested, Cases, Deaths, 
         Total.State.Hospitalizations = Hosp, Outbreaks, Counties, COVID.Cases.Per.100000=Rate) %>% 
  filter(., COUNTY != "Note") %>% 
  mutate(., COVID.Tests.Per.100000 = 100000*Tested/POP,
            COVID.Deaths.Per.100000 = 100000*Deaths/POP,
            COVID.Positive.Tests.Perc = ifelse(Tested!=0, 100*Cases/Tested, NA),
            COVID.Mortality.Perc = ifelse(Cases!=0, 100*Deaths/Cases, NA),
         State.Hospitalizations.Per.100000 = 100000*Total.State.Hospitalizations/POP
            # County.Avg.COVID.Cases.Per.100000 = COVID.Cases.Per.100000/Counties,
            # County.Avg.COVID.Tests.Per.100000 = COVID.Deaths.Per.100000/Counties,
            # County.Avg.COVID.Deaths.Per.100000 = COVID.Deaths.Per.100000/Counties,
            # County.Avg.COVID.Mortality.Perc = COVID.Mortality.Perc/Counties
         )
COVID19State_MERGE = COVID19StateData %>% 
  select(., COUNTY, Date, POP, Total.Tests = Tested, Total.Cases = Cases, 
         Total.Deaths = Deaths, Total.State.Hospitalizations, COVID.Tests.Per.100000, 
         COVID.Cases.Per.100000, COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
         State.Hospitalizations.Per.100000)
# colnames(COVID19County)
# COVID19StateData$Tested
# COVID19County$Testing.Rates.Per.100000
# COVID19County$Case.Rates.Per.100000
# COVID19County_MERGE$COVID.Mortality.Perc
COVID19County_MERGE = COVID19County %>% 
  mutate(., COVID.Deaths.Per.100000 = 100000*Number.of.Deaths.by.County/POP,
         COVID.Positive.Tests.Perc = ifelse(Total.COVID.19.Tests.Performed.in.Colorado.by.County!=0,
                                        100*Colorado.Case.Counts.by.County/
           Total.COVID.19.Tests.Performed.in.Colorado.by.County, NA),
         COVID.Mortality.Perc = ifelse(Colorado.Case.Counts.by.County!=0,
                                       100*Number.of.Deaths.by.County/Colorado.Case.Counts.by.County,
                                       NA),
         Total.State.Hospitalizations = NA, State.Hospitalizations.Per.100000 = NA) %>% 
  select(., COUNTY, Date, POP, 
         Total.Tests = Total.COVID.19.Tests.Performed.in.Colorado.by.County, 
         Total.Cases = Colorado.Case.Counts.by.County, 
         Total.Deaths = Number.of.Deaths.by.County, 
         Total.State.Hospitalizations,
         COVID.Tests.Per.100000 = Testing.Rates.Per.100000, 
         COVID.Cases.Per.100000 = Case.Rates.Per.100000, 
         COVID.Deaths.Per.100000, 
         COVID.Positive.Tests.Perc, 
         COVID.Mortality.Perc, 
         State.Hospitalizations.Per.100000)


###########  RELOAD FROM HERE  ###################
# RELOAD FROM HERE #
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
         
         # Perc.Positive.Tests = 
         #   ifelse(New.Tests!=0, 100*New.Cases/New.Tests, NA),
         # Perc.Change.in.Tests =
         #   ifelse(YEST.New.Tests!=0, 100*(New.Tests-YEST.New.Tests)/YEST.New.Tests, NA),
         # Perc.Change.in.Cases =
         #   ifelse(YEST.New.Cases!=0, 100*(New.Cases-YEST.New.Cases)/YEST.New.Cases, NA),
         # Perc.Change.in.Deaths =
         #   ifelse(YEST.New.Deaths!=0, 100*(New.Deaths-YEST.New.Deaths)/YEST.New.Deaths, NA),
         # Perc.Change.in.Tests.Per.100000 =
         #   ifelse(YEST.Change.in.Tests.Per.100000!=0, 100*(Change.in.Tests.Per.100000-YEST.Change.in.Tests.Per.100000)/YEST.Change.in.Tests.Per.100000, NA),
         # Perc.Change.in.Cases.Per.100000 =
         #   ifelse(YEST.Change.in.Cases.Per.100000!=0, 100*(Change.in.Cases.Per.100000-YEST.Change.in.Cases.Per.100000)/YEST.Change.in.Cases.Per.100000, NA),
         # Perc.Change.in.Deaths.Per.100000 =
         #   ifelse(YEST.Change.in.Deaths.Per.100000!=0, 100*(Change.in.Deaths.Per.100000-YEST.Change.in.Deaths.Per.100000)/YEST.Change.in.Deaths.Per.100000, NA),
         
         
         New.Tests.5.Day.Avg = rowMeans(select(., TWODAY.New.Tests,YEST.New.Tests, New.Tests, TOM.New.Tests, AFTERNEXT.New.Tests)),
         New.Cases.5.Day.Avg = rowMeans(select(., TWODAY.New.Cases,YEST.New.Cases, New.Cases, TOM.New.Cases, AFTERNEXT.New.Cases)),
         New.Deaths.5.Day.Avg = rowMeans(select(., TWODAY.New.Deaths,YEST.New.Deaths, New.Deaths, TOM.New.Deaths, AFTERNEXT.New.Deaths)),
         New.State.Hospitalizations.5.Day.Avg = rowMeans(select(., TWODAY.New.State.Hospitalizations,
                                                                YEST.New.State.Hospitalizations, New.State.Hospitalizations,
                                                                TOM.New.State.Hospitalizations, AFTERNEXT.New.State.Hospitalizations)),
         # YEST.New.Tests.5.Day.Avg = rowMeans(select(., TWODAY.New.Tests,YEST.New.Tests, New.Tests, TOM.New.Tests, THREEDAY.New.Tests)),
         # YEST.New.Cases.5.Day.Avg = rowMeans(select(., TWODAY.New.Cases,YEST.New.Cases, New.Cases, TOM.New.Cases, THREEDAY.New.Cases)),
         # YEST.New.Deaths.5.Day.Avg = rowMeans(select(., TWODAY.New.Deaths,YEST.New.Deaths, New.Deaths, TOM.New.Deaths, THREEDAY.New.Deaths)),
        
         # Perc.Change.in.Tests.5.Day.Avg =
         #   ifelse(YEST.New.Tests.5.Day.Avg!=0, 100*(New.Tests.5.Day.Avg-YEST.New.Tests.5.Day.Avg)/YEST.New.Tests.5.Day.Avg, NA),
         # Perc.Change.in.Cases.5.Day.Avg =
         #   ifelse(YEST.New.Cases.5.Day.Avg!=0, 100*(New.Cases.5.Day.Avg-YEST.New.Cases.5.Day.Avg)/YEST.New.Cases.5.Day.Avg, NA),
         # Perc.Change.in.Deaths.5.Day.Avg =
         #   ifelse(YEST.New.Deaths.5.Day.Avg!=0, 100*(New.Deaths.5.Day.Avg-YEST.New.Deaths.5.Day.Avg)/YEST.New.Deaths.5.Day.Avg, NA),

  #        Change.in.Tests.Per.100000.5.Day.Avg = rowMeans(select(., TWODAY.Change.in.Tests.Per.100000,YEST.Change.in.Tests.Per.100000, Change.in.Tests.Per.100000, TOM.Change.in.Tests.Per.100000, AFTERNEXT.Change.in.Tests.Per.100000)),
  #        Change.in.Cases.Per.100000.5.Day.Avg = 
  # rowMeans(select(., TWODAY.Change.in.Cases.Per.100000,YEST.Change.in.Cases.Per.100000, Change.in.Cases.Per.100000, TOM.Change.in.Cases.Per.100000, AFTERNEXT.Change.in.Cases.Per.100000)),
  #        Change.in.Deaths.Per.100000.5.Day.Avg = rowMeans(select(., TWODAY.Change.in.Deaths.Per.100000,YEST.Change.in.Deaths.Per.100000, Change.in.Deaths.Per.100000, TOM.Change.in.Deaths.Per.100000, AFTERNEXT.Change.in.Deaths.Per.100000)),
         # YEST.Change.in.Tests.Per.100000.5.Day.Avg = rowMeans(select(., TWODAY.Change.in.Tests.Per.100000,YEST.Change.in.Tests.Per.100000, Change.in.Tests.Per.100000, TOM.Change.in.Tests.Per.100000, THREEDAY.Change.in.Tests.Per.100000)),
         # YEST.Change.in.Cases.Per.100000.5.Day.Avg = rowMeans(select(., TWODAY.Change.in.Cases.Per.100000,YEST.Change.in.Cases.Per.100000, Change.in.Cases.Per.100000, TOM.Change.in.Cases.Per.100000, THREEDAY.Change.in.Cases.Per.100000)),
         # YEST.Change.in.Deaths.Per.100000.5.Day.Avg = rowMeans(select(., TWODAY.Change.in.Deaths.Per.100000,YEST.Change.in.Deaths.Per.100000, Change.in.Deaths.Per.100000, TOM.Change.in.Deaths.Per.100000, THREEDAY.Change.in.Deaths.Per.100000)),

         #  Perc.Change.in.Tests.Per.100000.5.Day.Avg =
         #   ifelse(YEST.Change.in.Tests.Per.100000.5.Day.Avg!=0, 100*(Change.in.Tests.Per.100000.5.Day.Avg-YEST.Change.in.Tests.Per.100000.5.Day.Avg)/YEST.Change.in.Tests.Per.100000.5.Day.Avg, NA),
         # Perc.Change.in.Cases.Per.100000.5.Day.Avg =
         #   ifelse(YEST.Change.in.Cases.Per.100000.5.Day.Avg!=0, 100*(Change.in.Cases.Per.100000.5.Day.Avg-YEST.Change.in.Cases.Per.100000.5.Day.Avg)/YEST.Change.in.Cases.Per.100000.5.Day.Avg, NA),
         # Perc.Change.in.Deaths.Per.100000.5.Day.Avg =
           # ifelse(YEST.Change.in.Deaths.Per.100000.5.Day.Avg!=0, 100*(Change.in.Deaths.Per.100000.5.Day.Avg-YEST.Change.in.Deaths.Per.100000.5.Day.Avg)/YEST.Change.in.Deaths.Per.100000.5.Day.Avg, NA),

      COVID.Positive.Tests.Perc.5.Day.Avg = rowMeans(select(., TWODAY.COVID.Positive.Tests.Perc,YEST.COVID.Positive.Tests.Perc, COVID.Positive.Tests.Perc, TOM.COVID.Positive.Tests.Perc, AFTERNEXT.COVID.Positive.Tests.Perc)),
        COVID.Mortality.Perc.5.Day.Avg = rowMeans(select(., TWODAY.COVID.Mortality.Perc,YEST.COVID.Mortality.Perc, COVID.Mortality.Perc, TOM.COVID.Mortality.Perc, AFTERNEXT.COVID.Mortality.Perc))
  # ,
#          YEST.COVID.Mortality.Perc.5.Day.Avg = rowMeans(select(., TWODAY.COVID.Mortality.Perc,YEST.COVID.Mortality.Perc, COVID.Mortality.Perc, TOM.COVID.Mortality.Perc, THREEDAY.COVID.Mortality.Perc)),
# Perc.Change.in.Mortality =
#            COVID.Mortality.Perc-YEST.COVID.Mortality.Perc,
#          Perc.Change.in.Mortality.5.Day.Avg =
#            COVID.Mortality.Perc.5.Day.Avg-YEST.COVID.Mortality.Perc.5.Day.Avg
)
         # 
# ,
         # Perc.Change.in.Mortality =
         #   COVID.Mortality.Perc-YEST.COVID.Mortality.Perc)
# COVID19ALL_MERGE$Change.in.Tests.Per.100000

COVID19ALL_MERGE$COUNTY %<>% gsub("Colorado", "COLORADO", .)
colnames(COVID19ALL_MERGE) %<>% 
  gsub("COVID.", "", ., fixed=T) 
# COVID19ALL_MERGE$New.Tests.Last.Week %<>% gsub(NA, 0, .)
# COVID19ALL_MERGE$New.Cases.Last.Week %<>% gsub(NA, 0, .)
# COVID19ALL_MERGE$New.Deaths.Last.Week %<>% gsub(NA, 0, .)
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
         contains("100"), contains("5.Day")
         )

COVID19ALL_MERGE = COVID19ALL_MERGE %>% 
  select(., -Positive.Tests.Perc, -Mortality.Perc, -New.Tests, -New.Cases, -New.Deaths,
         -New.State.Hospitalizations,
         COUNTY, Date, POP, starts_with("Total"), contains("5.Day"), contains("100"), 
         starts_with("Total"))
         
         
#unique(COVID19ALL_MERGE$COUNTY)
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
# COVID19ALL_MERGE$COUNTY =
#   factor(c,
#          levels = c("ADAMS","ALAMOSA","ARAPAHOE","ARCHULETA","BACA",
#          "BENT","BOULDER","BROOMFIELD","CHAFFEE","CHEYENNE","CLEAR CREEK","CONEJOS",
#          "COSTILLA","CROWLEY","CUSTER","DELTA","DENVER","DOLORES","DOUGLAS","EAGLE",
#          "EL PASO","ELBERT","FREMONT","GARFIELD","GILPIN","GRAND","GUNNISON","HINSDALE",
#          "HUERFANO","JACKSON","JEFFERSON","KIOWA","KIT CARSON",
#          "LA PLATA","LAKE","LARIMER",
#          "LAS ANIMAS","LINCOLN","LOGAN","MESA","MINERAL","MOFFAT","MONTEZUMA","MONTROSE",
#          "MORGAN","OTERO","OURAY","PARK","PHILLIPS","PITKIN","PROWERS","PUEBLO",
#          "RIO BLANCO","RIO GRANDE","ROUTT","SAGUACHE","SAN JUAN","SAN MIGUEL",
#          "SEDGWICK","SUMMIT","TELLER","WASHINGTON","WELD","YUMA",
#          "COLORADO"))
# 

# COVID19ALL_MERGE$WIKI <- sprintf("window.open(\"%s%s\")",
#   "http://en.wikipedia.org/wiki/", paste0(as.character(to_underscore(stri_trans_totitle(COVID19ALL_MERGE$COUNTY))), "_County,_Colorado"))

#### COVID DATASET #4 -- not using as far as know ####
# COVID19State = read.csv("COVID19_Positivity_Data_from_Clinical_Laboratories.csv", stringsAsFactors = F, header=T)
# #unique(COVID19State$Metric)
# #names(COVID19State)
# #lapply(COVID19State, class)
# COVID19State$Attr_Date =
#   as.Date(COVID19State$Attr_Date, format="%m/%d/%Y")
# COVID19State = COVID19State %>%
#   select(., -ObjectId, -Rep_Date) %>%
#   filter(., Metric !="Count of people tested by CDPHE lab" & Metric != "Count of people tested by non-CDPHE (commercial) lab" & Metric != "Individuals with serology-positive tests are not included in daily case counts until they are confirmed to have had COVID-like symptoms." & Metric != "Includes only tests from labs that participate in electronic lab reporting." & Metric != "May not include all negative results.")
# #unique(COVID19State$Desc_)
# names(COVID19State) %<>%
#   gsub("ï..Name", "Name", ., fixed=T) %>%
#   gsub("Pop", "POP", .) %>%
#   gsub("Attr_Date", "Date", .)
# #names(COVID19State)
# COVID19State = COVID19State %>%
#   select(., -Desc_) %>%
#   pivot_wider(., id_cols=c(Name, POP, Date),
#               names_from=Metric,
#               values_from=Value,
#               values_fn = list(Value = mean)
#               )
# colnames(COVID19State) = make.names(colnames(COVID19State))
# COVID19State = COVID19State %>%
#   arrange(Date)
# names(COVID19State) %<>% gsub("Total.Tests.per.100k.People", 
#                                  "State.COVID.Tests.Per.100000", .)
# #names(COVID19State)
# #lapply(COVID19State, class)
# 
# COVID19County_to_State = COVID19County %>% 
#   filter(., COUNTY != "Unknown Or Pending County" & 
#            COUNTY != "Note" &
#            COUNTY != "Out Of State County" &
#            COUNTY != "International")  %>% 
#   select(., Date, Cases, Deaths) %>%
#   group_by(., Date) %>% 
#   summarise(., COVID.Total.Cases.Per.100000 =
#               100000*sum(Cases, na.rm=T)/5694287,
#             COVID.Total.Deaths.Per.100000 =
#               100000*sum(Deaths, na.rm=T)/5694287,
#             COVID.Mortality.Perc = 
#               100*sum(Deaths, na.rm=T)/sum(Cases, na.rm=T))

####
#### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

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
#colnames(CO_COUNTY_COVID_ALL)

#### Create Smaller Table Groups Here ####
CO_COUNTY_NAMES = names(CO_COUNTY_COVID_ALL)
write.csv(CO_COUNTY_NAMES, "CO_COUNTY_NAMES.csv", row.names = T)

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
#names(CO_COUNTY_COVID_FILTER)
# 
# CO_COUNTY_COVID_RATES_NO_95 = CO_COUNTY_COVID_FILTER %>% select(., -contains("95")) 
# #colnames(CO_COUNTY_COVID_RATES_NO_95)
# 
# CO_COUNTY_COVID_RATES_NO_STATE = CO_COUNTY_COVID_FILTER %>% select(., -contains("State", ignore.case = T))
# #colnames(CO_COUNTY_COVID_RATES_NO_STATE)
# 
# CO_COUNTY_COVID_RATES_NO_DEMO = CO_COUNTY_COVID_FILTER %>% select(., -contains("e.AIAN"), -contains("e.Asian"), -contains("e.Black"), -contains("e.Hispanic"), -contains("e.White"), -contains("d.AIAN"), -contains("d.Asian"), -contains("d.Black"), -contains("d.Hispanic"), -contains("d.White"), -contains("y.AIAN"), -contains("y.Asian"), -contains("y.Black"), -contains("y.Hispanic"), -contains("y.White")) 
# #colnames(CO_COUNTY_COVID_RATES_NO_DEMO)
# 
# CO_COUNTY_COVID_RATES_NO_STATE_NO_DEMO = CO_COUNTY_COVID_FILTER %>% select(., -contains("State", ignore.case = T), -contains("e.AIAN"), -contains("e.Asian"), -contains("e.Black"), -contains("e.Hispanic"), -contains("e.White"), -contains("d.AIAN"), -contains("d.Asian"), -contains("d.Black"), -contains("d.Hispanic"), -contains("d.White"), -contains("y.AIAN"), -contains("y.Asian"), -contains("y.Black"), -contains("y.Hispanic"), -contains("y.White"))  
# #colnames(CO_COUNTY_COVID_RATES_NO_STATE_NO_DEMO)
# 
# CO_COUNTY_COVID_RATES_NO_95_NO_DEMO = CO_COUNTY_COVID_FILTER %>% select(., -contains("95"), -contains("e.AIAN"), -contains("e.Asian"), -contains("e.Black"), -contains("e.Hispanic"), -contains("e.White"), -contains("d.AIAN"), -contains("d.Asian"), -contains("d.Black"), -contains("d.Hispanic"), -contains("d.White"), -contains("y.AIAN"), -contains("y.Asian"), -contains("y.Black"), -contains("y.Hispanic"), -contains("y.White"))  
# #colnames(CO_COUNTY_COVID_RATES_NO_95_NO_DEMO)
# 
# CO_COUNTY_COVID_RATES_NO_STATE_NO_95 = CO_COUNTY_COVID_FILTER %>% select(., -contains("State", ignore.case = T), -contains("95")) 
# #colnames(CO_COUNTY_COVID_RATES_NO_STATE_NO_95)
# 
CO_COUNTY_COVID_SIMPLEST = CO_COUNTY_COVID_FILTER %>% select(.,
-contains("95"), -contains("State", ignore.case = T),
-contains("e.AIAN"), -contains("e.Asian"), -contains("e.Black"), -contains("e.Hispanic"), -contains("e.White"), -contains("d.AIAN"), -contains("d.Asian"), -contains("d.Black"), -contains("d.Hispanic"), -contains("d.White"), -contains("y.AIAN"), -contains("y.Asian"), -contains("y.Black"), -contains("y.Hispanic"), -contains("y.White"))
# 
# #names(CO_COUNTY_COVID_SIMPLEST)

#### write.csv FILES ####

write.csv(CO_COUNTY_COVID_ALL, "CO_COUNTY_COVID_ALL.csv", row.names = F)
write.csv(CO_COUNTY_COVID_FILTER, "CO_COUNTY_COVID_FILTER.csv", row.names = F)

#### SUBTABLE BREAKDOWNS -- SIMPLEST ####
#colnames(CO_COUNTY_COVID_SIMPLEST)
CO_MORTALITY_MORBIDITY = CO_COUNTY_COVID_SIMPLEST %>% 
  select(., COUNTY, 
         COVID.Tests.Per.100000, COVID.Cases.Per.100000,
         COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
         starts_with("Life.Expectancy.2010"), contains("Life.Lost"),
         contains("Hospitalizations"),
         contains("Mortality"), Violent.Crime.Rate,
         starts_with("Injury.Death.Rate"),
         starts_with("Perc.Driving.Deaths"))
#names(CO_MORTALITY_MORBIDITY)  

CO_PRIOR_MEDICAL = CO_COUNTY_COVID_SIMPLEST  %>% 
  select(., COUNTY, 
         COVID.Tests.Per.100000, COVID.Cases.Per.100000,
         COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
#Perc.Adults.Asthma, Perc.Adults.Diabetes, Perc.Adults.Heart.Disease,  
        contains("Asthma"), contains("Diabetes"), 
        contains("Heart.Disease"),
        Perc.Disability, Perc.Indp.Living.Disability.Over.17, 
        Perc.Self.Care.Disability, starts_with("Low.Weight.Birth"), 
        -contains("Hospitalizations"), -contains("Mortality.Rate"),
        -contains("Mortality.Death"))
#names(CO_PRIOR_MEDICAL)
  
CO_HEALTHCARE_ACCESS = CO_COUNTY_COVID_SIMPLEST  %>% select(., COUNTY, 
  COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
  COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc, 
  starts_with("Perc.Uninsured"), starts_with("Perc.Flu.Vaccinated"), 
  #Perc.Adults.Delayed.Care, Perc.Adults.No.Reg.Care.12mos,
  starts_with("Perc.Adults.with.Obesity"), 
  contains("Delayed.Care"), contains("No.Reg.Care"),
  starts_with("Preventable.Hospitalization"),
  Primary.Care.Patients.per.Physician, Mental.Health.Patients.per.Provider,
  starts_with("Per.100000"))
#names(CO_HEALTHCARE_ACCESS)
                                           
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
#names(CO_LIFESTYLE)

CO_INCOME_EDUCATION = CO_COUNTY_COVID_SIMPLEST  %>% select(., COUNTY, 
  COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
  COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
  High.School.Graduation.Rate, starts_with("Perc.Some.College"), 
  starts_with("Median.Household"), 
  Income.80th.Percentile, Income.20th.Percentile, starts_with("Perc.Unemployed"), starts_with("Perc.Children.in.Poverty"), 
  contains("Poverty"), Perc.Enrolled.in.Free.or.Reduced.Lunch,
  starts_with("Perc.Drive.Alone"))
#names(CO_INCOME_EDUCATION)
                                           
CO_DEMOGRAPHICS = CO_COUNTY_COVID_SIMPLEST  %>% select(., COUNTY, 
  COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
  COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
  Perc.less.than.18.years.of.age, Perc.65.and.over, Perc.Female, Perc.Hispanic, 
  Perc.Non.Hispanic.White, Perc.Black, Perc.Asian, 
  Perc.AIAN.American.Indian.Alaska.Native, 
  Perc.Native.Hawaiian.Other.Pacific.Islander, Perc.Not.Proficient.in.English, 
  contains("Segregation"))           
#names(CO_DEMOGRAPHICS)
  
CO_HOME = CO_COUNTY_COVID_SIMPLEST  %>% select(., COUNTY, 
  COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
  COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
  starts_with("Perc.Homeowners"), starts_with("Perc.Single.Parent.Households"), 
  starts_with("Perc.Severe.Housing"), 
  starts_with("Perc.Housing.Overcrowding"), 
  starts_with("Perc.Inadequate"), Average.Daily.Air.Pollution.Z.Score, 
  Perc.Rural)
#names(CO_HOME)

CO_SUMMARY_SCORES = CO_COUNTY_COVID_SIMPLEST  %>% select(., COUNTY, 
  COVID.Tests.Per.100000, COVID.Cases.Per.100000,  
  COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, COVID.Mortality.Perc,
  Years.of.Potential.Life.Lost.Rate, Life.Expectancy.2010.2015.Rate,
  ends_with("Z.Score"), -contains("Unhealthy"), -contains("Air"))
#names(CO_SUMMARY_SCORES)

NAMES_CO_COMPLETE_SUBLISTS = 
  c("CO_MORTALITY_MORBIDITY", "CO_PRIOR_MEDICAL", "CO_HEALTHCARE_ACCESS",
    "CO_LIFESTYLE", "CO_INCOME_EDUCATION", "CO_DEMOGRAPHICS", "CO_HOME",
    "CO_SUMMARY_SCORES")
#NAMES_CO_COMPLETE_SUBLISTS

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
#CO_COMPLETE_SUBLISTS
write.csv(CO_COMPLETE_SUBLISTS, "CO_COMPLETE_SUBLISTS.csv", row.names = F)

COVID19DATA_NAMES = names(COVID19DATA) 
write.csv(COVID19DATA_NAMES, "COVID19DATA_NAMES.csv", row.names = F)
#### SUBTABLE BREAKDOWNS -- FILTER ####
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

#### MAP PREWORK ####
CO_county_map = map_data('county', 'colorado')
CO_county_map$subregion = toupper(CO_county_map$subregion)
names(CO_county_map) = gsub("subregion", "COUNTY", 
                            names(CO_county_map))
#head(CO_county_map)

# CO_COUNTY_COVID_FILTER$COVID.Deaths.Per.100000[is.na(CO_COUNTY_COVID_FILTER$COVID.Deaths.Per.100000)] <- 0
# CO_COUNTY_COVID_FILTER$COVID.Cases.Max[is.na(CO_COUNTY_COVID_FILTER$COVID.Cases.Max)] <- 0
# CO_COUNTY_COVID_FILTER$COVID.Mortality.Perc[is.na(CO_COUNTY_COVID_FILTER$COVID.Mortality.Perc)] <- 0
# CO_COUNTY_COVID_FILTER$COVID.Positive.Tests.Perc[is.na(CO_COUNTY_COVID_FILTER$COVID.Positive.Tests.Perc)] <- 0
CO_MAP_COVID = merge(CO_county_map, CO_COUNTY_COVID_FILTER, 
                     by="COUNTY")


CO_COUNTY_JUST_RURAL = CO_COUNTY_COVID_FILTER %>% 
  select(., COUNTY, Perc.Rural)

CO_Race_Measures_COVID19 = left_join(CO_Race_Measures_COVID19, 
                                     CO_COUNTY_JUST_RURAL, by="COUNTY")

#### Factors ####
CO_Race_Measures_COVID19$Demographic = 
  factor(CO_Race_Measures_COVID19$Demographic, 
         levels = c("ALL", "AIAN", "Asian", "Black",
                    "Hispanic", "NHPI", "Non-White", "White"))


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

CO_MAP_COVID_BAL$WIKI <- sprintf("window.open(\"%s%s\")",
                                 "http://en.wikipedia.org/wiki/", paste0(as.character(to_underscore(stri_trans_totitle(CO_MAP_COVID_BAL$COUNTY))), "_County,_Colorado"))
CO_MAP_COVID$WIKI <- sprintf("window.open(\"%s%s\")",
                             "http://en.wikipedia.org/wiki/", paste0(as.character(to_underscore(stri_trans_totitle(CO_MAP_COVID$COUNTY))), "_County,_Colorado"))



#names(CO_MAP_COVID)
bi_colors = c("#be64ac","#dfb0d6","#e8e8e8","#8c62aa","#a5add3","#ace4e4",
              "#3b4994","#5698b9","#5ac8c8")




#### Functions for Label Creation ####
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
rev_perc <- function(string_text) {
  gsub(" COVID Mortality Perc", "% COVID Mortality", string_text, fixed=T)
}
frontspace<- function(string_text) {
  gsub("COVID", " COVID", string_text, fixed=T)
}
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





#### Archived --- SUBSET COMBINED TABLES EVERYTHING ####
# COVID19_PER = COVID.Tests.Per.100000, COVID.Cases.Per.100000,
         # COVID.Deaths.Per.100000, COVID.Mortality.Perc, %>% 
#   select(., COUNTY, COVID_Mean_Testing_Rates_Per_100000, 
#          COVID_Max_Testing_Rates_Per_100000, 
#          COVID_County_Pos_Case_Rate_Per_100000, 
#          COVID_Perc_Days_with_Cases, COVID_Mean_Cases_per_Day, 
#          COVID_County_Death_Rate_Per_100000,
#          COVID_Death_Rate_if_Pos_Case_Perc)

#names(CO_COUNTY_COVID_FILTER)
# CO_COUNTY_COVID_RATES = CO_COUNTY_COVID_ALL %>% select(., COUNTY, starts_with("COVID"), starts_with("Per"), contains("Perc", ignore.case = T), contains("Rate", ignore.case = T), Life.Expectancy.20102015, Life.Expectancy.State, starts_with("Median.Household.Income"),  -contains("95"), contains("Score"), -matches("(Perc.*Score)"), -matches("(Rate.*Score)"), -matches("(Ratio.*Score)"), -matches("(Water.*Score)"), -ends_with("Rank"), -ends_with("Unhealthy.Days"), Nonwhite.v.White.Segregation.Index = Segregation.Index, Black.v.White.Segregation.Index = Segregation.index)

# CO_COUNTY_PER = CO_COUNTY_ALL %>% select(., COUNTY, contains("Perc", ignore.case = T), contains("Rate", ignore.case = T), Life.Expectancy.20102015, Life.Expectancy.State, -contains("95"), -contains("Z.score", ignore.case = T))
# 
# CO_ALL_PER = inner_join(COVID19_PER, CO_COUNTY_PER, by="COUNTY")
# lapply(CO_ALL_PER, class)
# CO_ALL_PER_collist =unique(colnames(CO_ALL_PER))
# write.csv(CO_ALL_PER_collist, "CO_ALL_PER_collist.csv", row.names = T)
# ?cor
# CO_ALL_PER_collist
# CO_ALL_PER_NUM = CO_ALL_PER %>% select(., -COUNTY)
# CO_ALL_PER_NUM.cor = cor(CO_ALL_PER_NUM, use = "complete.obs")
#  write.csv(CO_ALL_PER_NUM.cor, "CO_ALL_PER_NUM.cor.csv", row.names = T)
# CO_ALL_PER$COVID_Perc_Days_with_Cases
# # Adult.smoking_Perc.Smokers #                  NADA
# ggplot(CO_ALL_PER, aes(x=Adult.smoking_Perc.Smokers
# , y=COVID_County_Pos_Case_Rate_Per_100000)) + geom_point(aes(alpha=COVID_County_Death_Rate_Per_100000)) 
# 
#### Archived --- Correlation Basic Exploration ####
# library(DataExplorer)
# CO_ALL_PER_NUM = CO_ALL_PER %>% select(., -COUNTY)
# CO_ALL_PER.cor = cor(CO_ALL_PER_NUM)
# write.csv(CO_ALL_PER.cor, "CO_ALL_PER_plotcorrelation.csv", row.names = T)
# 
# 
# 
# 
#### Archived --- OLDER STUFF ####
# # install.packages("Hmisc")
# # install.packages("psych")
# # install.packages("pastecs")
# # install.packages("DataExplorer")
# library(Hmisc)
# library(psych)
# library(pastecs)
# stat.desc(CO_COUNTY_COVID_ALL)
# library(DataExplorer)
# create_report(CO_COUNTY_COVID_ALL)
# DataExplorer::plot_correlation()
# ?plot_correlation
# var(CO_COUNTY_COVID$COVID_Max_Case_Rates_Per_100000, CO_COUNTY_COVID$Diabetes_County_Rate)
# ?cor
# 
# t.test(CO_COUNTY_COVID$COVID_Max_Case_Rates_Per_100000, CO_COUNTY_COVID$Diabetes_County_Rate)
# names(CO_COUNTY_COVID) = gsub(" ", ".", names(CO_COUNTY_COVID))
# names(CO_COUNTY_COVID) = gsub("-", "_", names(CO_COUNTY_COVID))
# names(CO_COUNTY_COVID)
# ?select
# 
# COVID_SIMPLE = CO_COUNTY_COVID %>% 
#   select(., COUNTY, starts_with("COVID_"), Population_Total, LE_20102015, Premature.death_Deaths, Premature.death.1_Years.of.Potential.Life.Lost.Rate, Poor.or.fair.health_Perc.Fair.or.Poor.Health, Poor.physical.health.days_Average.Number.of.Physically.Unhealthy.Days, Poor.mental.health.days_Average.Number.of.Mentally.Unhealthy.Days, Adult.obesity_Perc.Adults.with.Obesity, Physical.inactivity_Perc.Physically.Inactive, Access.to.exercise.opportunities_Perc.With.Access.to.Exercise.Opportunities, Excessive.drinking_Perc.Excessive.Drinking, Alcohol.impaired.driving.deaths.2_Perc.Driving.Deaths.with.Alcohol.Involvement, Sexually.transmitted.infections.1_Chlamydia.Rate, Teen.Birth.Rate_Teen.Birth.Rate, Uninsured.1_Perc.Uninsured, Primary.care.physicians.1_Primary.Care.Physicians.Rate, Mental.health.providers.1_Mental.Health.Provider.Rate, Preventable.Hospitalization.Rate_Preventable.Hospitalization.Rate, Flu.vaccinations_Perc.Vaccinated, High.school.graduation.1_High.School.Graduation.Rate, Some.college.2_Perc.Some.College, Unemployment.2_Perc.Unemployed, Children.in.poverty_Perc.Children.in.Poverty, Income.inequality.2_Income.Ratio, Children.in.single.parent.households.2_Perc.Single_Parent.Households, Social.associations.1_Social.Association.Rate, Violent.crime.1_Violent.Crime.Rate, Injury.deaths.1_Injury.Death.Rate, Severe.housing.problems_Perc.Severe.Housing.Problems, Severe.housing.problems.3_Severe.Housing.Cost.Burden, Severe.housing.problems.6_Overcrowding, Driving.alone.to.work_Perc.Drive.Alone.to.Work, starts_with("Perc"), contains("_ADJRATE"), Disability_TCNPop_AgeOver17_With_An_IndLiving_Difficulty, Available_Beds, Total_Beds, Assisted_Living_Facs, Total_Health_Facilities) %>% 
#   mutate(., Perc_Over17_With_An_IndLiving_Difficulty = 100*Disability_TCNPop_AgeOver17_With_An_IndLiving_Difficulty / Population_Total, Available_Beds_per_100000 = 100000*Available_Beds/Population_Total, Total_Beds_per_100000 = 100000*Total_Beds/Population_Total, Assisted_Living_Facs_per_100000 = 100000*Assisted_Living_Facs/Population_Total, Health_Facilities_per_100000 = 100000*Total_Health_Facilities/Population_Total)
# 
# COVID_PREEXISTING = COVID_SIMPLE %>% 
#   select(., COUNTY, starts_with("COVID"), starts_with("Perc"), contains("_ADJRATE"), LE_20102015, Premature.death_Deaths, Premature.death.1_Years.of.Potential.Life.Lost.Rate)
# 
# COVID_PT_OTHER_HEALTH = COVID_SIMPLE %>% 
#   select(., COUNTY, starts_with("COVID"), Poor.or.fair.health_Perc.Fair.or.Poor.Health, Poor.physical.health.days_Average.Number.of.Physically.Unhealthy.Days, Poor.mental.health.days_Average.Number.of.Mentally.Unhealthy.Days, Adult.obesity_Perc.Adults.with.Obesity, Physical.inactivity_Perc.Physically.Inactive, Access.to.exercise.opportunities_Perc.With.Access.to.Exercise.Opportunities, Excessive.drinking_Perc.Excessive.Drinking, Alcohol.impaired.driving.deaths.2_Perc.Driving.Deaths.with.Alcohol.Involvement, Sexually.transmitted.infections.1_Chlamydia.Rate, Teen.Birth.Rate_Teen.Birth.Rate, Uninsured.1_Perc.Uninsured, Primary.care.physicians.1_Primary.Care.Physicians.Rate, Mental.health.providers.1_Mental.Health.Provider.Rate, Preventable.Hospitalization.Rate_Preventable.Hospitalization.Rate, Flu.vaccinations_Perc.Vaccinated)
# 
# COVID_HOME_ENVIRONMENT = COVID_SIMPLE %>% 
#   select(., COUNTY, starts_with("COVID"),  High.school.graduation.1_High.School.Graduation.Rate, Some.college.2_Perc.Some.College, Unemployment.2_Perc.Unemployed, Children.in.poverty_Perc.Children.in.Poverty, Income.inequality.2_Income.Ratio, Children.in.single.parent.households.2_Perc.Single_Parent.Households, Social.associations.1_Social.Association.Rate, Violent.crime.1_Violent.Crime.Rate, Injury.deaths.1_Injury.Death.Rate, Severe.housing.problems_Perc.Severe.Housing.Problems, Severe.housing.problems.3_Severe.Housing.Cost.Burden, Severe.housing.problems.6_Overcrowding, Driving.alone.to.work_Perc.Drive.Alone.to.Work, Available_Beds_per_100000, Total_Beds_per_100000, Assisted_Living_Facs_per_100000, Health_Facilities_per_100000)
# 
# # COVID_POOR_DECISIONS = COVID_SIMPLE %>% 
# #   select(., COUNTY, starts_with("COVID_"), Population_Total, )
# # 
# # COVID_HEALTH_RESOURCES = COVID_SIMPLE %>% 
# #   select(., COUNTY, starts_with("COVID_"), )
# 
# 
# 
# 
# colnames(COVID_SIMPLE)
# lapply(COVID_PREEXISTING_NUM, class)
# ncol(COVID_SIMPLE)
# plot_correlation(COVID_PREEXISTING)
# plot_correlation(COVID_PT_OTHER_HEALTH)
# plot_correlation(COVID_HOME_ENVIRONMENT)
# create_report(COVID_PREEXISTING)
# create_report(COVID_PT_OTHER_HEALTH)
# create_report(COVID_HOME_ENVIRONMENT)
# ?rcorr
# COVID_PREEXISTING_NUM = COVID_PREEXISTING %>% select(., -COVID_First_Case_Date, -COUNTY)
# COVID_PREEXISTING.cor = cor(COVID_PREEXISTING_NUM, na.rm = F)
# COVID_PREEXISTING.rcorr = rcorr(as.matrix(COVID_PREEXISTING_NUM))
# COVID_PREEXISTING.rcorr
# head(COVID_PREEXISTING)
# write.csv(COVID_PREEXISTING.cor, "COVID_PREEXISTING_cor_narm.csv", row.names = T)
# 
# COVID_SIMPLE_NUM = COVID_SIMPLE %>% select(., -COVID_First_Case_Date, -COUNTY)
# COVID_SIMPLE.cor = cor(COVID_SIMPLE_NUM)
# COVID_SIMPLE.rcorr = rcorr(as.matrix(COVID_SIMPLE_NUM))
# COVID_PREEXISTING.rcorr
# head(COVID_PREEXISTING)
# write.csv(COVID_SIMPLE.cor, "COVID_SIMPLE_NUM_cor.csv", row.names = T)
# 
# CO_COUNTY_COVID_NUM = CO_COUNTY_COVID %>% select_if(., is.numeric)
# CO_COUNTY_COVID_NUM.cor = cor(CO_COUNTY_COVID_NUM)
# CO_COUNTY_COVID_NUM.cov = cov(CO_COUNTY_COVID_NUM)
# ?cor
# write.csv(CO_COUNTY_COVID_NUM.cor, "CO_COUNTY_COVID_NUM_cor.csv", row.names = T)
# write.csv(CO_COUNTY_COVID_NUM.cov, "CO_COUNTY_COVID_NUM_cov.csv", row.names = T)
# ?select_if
# ?cor
# ```
# 
# This R Markdown document is made interactive using Shiny. Unlike the more traditional workflow of creating static reports, you can now create documents that allow your readers to change the assumptions underlying your analysis and see the results immediately. 
# 
# To learn more, see [Interactive Documents](http://rmarkdown.rstudio.com/authoring_shiny.html).
# 
# ## Inputs and Outputs
# 
# You can embed Shiny inputs and outputs in your document. Outputs are automatically updated whenever inputs change.  This demonstrates how a standard R plot can be made interactive by wrapping it in the Shiny `renderPlot` function. The `selectInput` and `sliderInput` functions create the input widgets used to drive the plot.
# 
# ```{r eruptions, echo=FALSE}
# inputPanel(
#   selectInput("n_breaks", label = "Number of bins:",
#               choices = c(10, 20, 35, 50), selected = 20),
#   
#   sliderInput("bw_adjust", label = "Bandwidth adjustment:",
#               min = 0.2, max = 2, value = 1, step = 0.2)
# )
# 
# renderPlot({
#   hist(faithful$eruptions, probability = TRUE, breaks = as.numeric(input$n_breaks),
#        xlab = "Duration (minutes)", main = "Geyser eruption duration")
#   
#   dens <- density(faithful$eruptions, adjust = input$bw_adjust)
#   lines(dens, col = "blue")
# })
# ```
# 
# ## Embedded Application
# 
# It's also possible to embed an entire Shiny application within an R Markdown document using the `shinyAppDir` function. This example embeds a Shiny application located in another directory:
# 
# ```{r tabsets, echo=FALSE}
# shinyAppDir(
#   system.file("examples/06_tabsets", package = "shiny"),
#   options = list(
#     width = "100%", height = 550
#   )
# )
# ```
# 
# Note the use of the `height` parameter to determine how much vertical space the embedded application should occupy.
# 
# You can also use the `shinyApp` function to define an application inline rather then in an external directory.
# 
# In all of R code chunks above the `echo = FALSE` attribute is used. This is to prevent the R code within the chunk from rendering in the document alongside the Shiny components.
# 
# 
# 
