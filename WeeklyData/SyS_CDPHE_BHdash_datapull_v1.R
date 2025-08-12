##SyS CDPHE ##
### Wastewater Surveillance - EV-68, AFM-related ###

## Load Packages ##
library(lubridate)
library(httr)
library(tidytext) 
library(janitor) 
library(DT)
library(jsonlite)
library(broom)
library(Rnssp)
library(tidyverse)
library(tidytext)
library(tidyr)
library(plyr)
library(widyr)
library(dplyr)
library(MMWRweek)
library(xlsx)
library(writexl)
library(scales)

Sys.getenv(c("JAVA_HOME", "LD_LIBRARY_PATH"))
setwd("~/WeeklyData")

## Set NSSP user profile ----
myProfile <- readRDS("myProfile.rds")

# API DATA #
#Leave start date as 12/29/2019, adjust end date to reflect most recent complete week's end date (Saturday)

startdate <- "1Oct2024"
enddate <- "2Aug2025"
date <- "TEST"
week <- "31"
year <- "2025"

#set counties

counties <- "co_adams&geography=co_arapahoe&geography=co_boulder&geography=co_broomfield&geography=co_denver&geography=co_douglas&geography=co_el%20paso&geography=co_jefferson&geography=co_la%20plata&geography=co_larimer&geography=co_mesa&geography=co_montezuma&geography=co_rio%20blanco&geography=co_pueblo&geography=co_weld"

## Behavioral Health Dashboard Indicators by County

url_bhcounty <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?geography=",counties,"&datasource=va_er&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=ccddCategory&aqtTarget=TableBuilder&ccddCategory=cdc%20alcohol%20v1&ccddCategory=cdc%20all%20drug%20overdose%20v3%20parsed&ccddCategory=cdc%20intimate%20partner%20violence%20v2&ccddCategory=cdc%20mental%20health%20v1&ccddCategory=cdc%20sexual%20violence%20v3&ccddCategory=cdc%20suicidal%20ideation%20v1&ccddCategory=cdc%20suicide%20attempt%20v2&ccddCategory=cdc%20suspected%20child%20abuse%20and%20neglect%20v1&geographySystem=region&detector=nodetectordetector&timeResolution=weekly&hasBeenE=1&rowFields=timeResolution&rowFields=geographyregion&columnField=ccddCategory")

bhcounty <- myProfile$get_api_data(url_bhcounty, fromCSV = TRUE) %>%
  dplyr::select(week = timeResolution,
         county = geographyregion,
         mentalhealth_pct = "CDC Mental Health v1",
         mentalhealth_ct = "CDC Mental Health v1 Data Count",
         suicidalideation_pct = "CDC Suicidal Ideation v1",
         suicidalideation_ct = "CDC Suicidal Ideation v1 Data Count",
         suicideattempt_pct = "CDC Suicide Attempt v2",
         suicideattempt_ct = "CDC Suicide Attempt v2 Data Count",
         childabuse_pct = "CDC Suspected Child Abuse and Neglect v1",
         childabuse_ct = "CDC Suspected Child Abuse and Neglect v1 Data Count",
         ipv_pct = "CDC Intimate Partner Violence v2",
         ipv_ct = "CDC Intimate Partner Violence v2 Data Count",
         sexualviolence_pct = "CDC Sexual Violence v3",
         sexualviolence_ct = "CDC Sexual Violence v3 Data Count",
         alcohol_pct = "CDC Alcohol v1",
         alcohol_ct = "CDC Alcohol v1 Data Count",
         alldrug_pct = "CDC All Drug Overdose v3 Parsed",
         alldrug_ct = "CDC All Drug Overdose v3 Parsed Data Count",
         totalED = "CDC All Drug Overdose v3 Parsed All Count")

# Behavioral Health Indicators by Age Groups (Age Group 6)

url_bhagegroup <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?geography=",counties,"&datasource=va_er&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=ccddCategory&aqtTarget=TableBuilder&ccddCategory=cdc%20alcohol%20v1&ccddCategory=cdc%20all%20drug%20overdose%20v3%20parsed&ccddCategory=cdc%20intimate%20partner%20violence%20v2&ccddCategory=cdc%20mental%20health%20v1&ccddCategory=cdc%20sexual%20violence%20v3&ccddCategory=cdc%20suicidal%20ideation%20v1&ccddCategory=cdc%20suicide%20attempt%20v2&ccddCategory=cdc%20suspected%20child%20abuse%20and%20neglect%20v1&geographySystem=region&detector=nodetectordetector&timeResolution=weekly&hasBeenE=1&rowFields=timeResolution&rowFields=ageGroup6&columnField=ccddCategory")

bhagegroup <- myProfile$get_api_data(url_bhagegroup, fromCSV = TRUE) %>%
  dplyr::select(week = timeResolution,
         agegroup = ageGroup6,
         mentalhealth_ct = "CDC Mental Health v1 Data Count",
         suicidalideation_ct = "CDC Suicidal Ideation v1 Data Count",
         suicideattempt_ct = "CDC Suicide Attempt v2 Data Count",
         childabuse_ct = "CDC Suspected Child Abuse and Neglect v1 Data Count",
         ipv_ct = "CDC Intimate Partner Violence v2 Data Count",
         sexualviolence_ct = "CDC Sexual Violence v3 Data Count",
         alcohol_ct = "CDC Alcohol v1 Data Count",
         alldrug_ct = "CDC All Drug Overdose v3 Parsed Data Count",
         totalED = "CDC All Drug Overdose v3 Parsed All Count")

bhagegroup <- bhagegroup %>% 
  mutate(agegroup = case_when((agegroup == "00-02" | 
                                 agegroup == "03-05" | 
                                 agegroup == "06-17") ~  "00-17",
                              TRUE ~ agegroup)) %>% 
  group_by(week, agegroup) %>% 
  summarise_all(list(sum)) %>%
  ungroup() %>%
  mutate(mentalhealth_pct = (mentalhealth_ct/totalED)*100,
            suicidalideation_pct = (suicidalideation_ct/totalED)*100,
            suicideattempt_pct = (suicideattempt_ct/totalED)*100,
            childabuse_pct = (childabuse_ct/totalED)*100,
            ipv_pct = (ipv_ct/totalED)*100,
            sexualviolence_pct = (sexualviolence_ct/totalED)*100,
            alcohol_pct = (alcohol_ct/totalED)*100,
            alldrug_pct = (alldrug_ct/totalED)*100)

# CLEAN DATA #

bhcounty <- bhcounty %>% 
  separate(col = week, into = c("MMWRyear", "MMWRweek"), sep = "-", convert = TRUE) %>%
  mutate(weekStart = MMWRweek2Date(MMWRyear, MMWRweek)) %>%
  mutate(county = gsub("CO_", "", county))
bhcounty <- bhcounty[, c(1,2,21,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]

bhagegroup <- bhagegroup %>% 
  separate(col = week, into = c("MMWRyear", "MMWRweek"), sep = "-", convert = TRUE) %>%
  mutate(weekStart = MMWRweek2Date(MMWRyear, MMWRweek))
bhagegroup <- bhagegroup[, c(1,2,21,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10,20,11,12)]

# Write file

filename <- paste0("SyStable_BehavioralHealthDash_v1.xlsx")
filepath <- paste0("Output/",filename)

x <- list(bhcounty, bhagegroup)

write_xlsx(setNames(x, c("County", "Age Group")), 
           path = filepath, 
           col_names = T)

rm(list = ls())
