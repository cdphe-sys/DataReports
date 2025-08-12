##SyS Indicators##

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
library(dplyr)
library(widyr)
library(MMWRweek)
library(xlsx)
library(openxlsx)
library(writexl)

setwd("~/WeeklyData")

## Set NSSP user profile ----
myProfile <- readRDS("myProfile.rds")

# API DATA #
#Leave start date as 12/29/2019, adjust end date to reflect most recent complete week's end date (Saturday)

startdate <- "1Jan2024"
enddate <- "31Jul2025"
date <- "TEST"
week <- "31"
year <- "2025"

counties <- "co_adams&geography=co_arapahoe&geography=co_boulder&geography=co_broomfield&geography=co_denver&geography=co_douglas&geography=co_el%20paso&geography=co_jefferson&geography=co_la%20plata&geography=co_larimer&geography=co_mesa&geography=co_pueblo&geography=co_weld&geography=co_rio%20blanco&geography=co_montezuma&geography=co_sedgwick&geography=co_yuma&geography=co_garfield&geography=co_phillips"

categories <- "cdc%20all%20drug%20overdose%20v3%20parsed&ccddCategory=cdc%20heroin%20overdose%20v5%20parsed&ccddCategory=cdc%20opioid%20overdose%20v4%20parsed&ccddCategory=cdc%20methamphetamine%20overdose%20v1%20parsed&ccddCategory=cdc%20stimulant%20overdose%20v4%20parsed&ccddCategory=cdc%20fentanyl%20overdose%20v2%20parsed&ccddCategory=cdc%20cocaine%20overdose%20v2%20parsed&ccddCategory=cdc%20benzodiazepine%20overdose%20v2%20parsed"

##Weekly OD x County

url_county <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?geography=",counties,"&datasource=va_er&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=ccddCategory&aqtTarget=TableBuilder&ccddCategory=",categories,"&geographySystem=region&detector=probewmapoissonswitch&timeResolution=monthly&hasBeenE=1&rowFields=timeResolution&rowFields=geographyregion&columnField=ccddCategory")

county <- myProfile$get_api_data(url_county, fromCSV = TRUE) %>%
  dplyr::select(timeResolution,
         geographyregion,
         alldrug_ct = "CDC All Drug Overdose v3 Parsed Data Count",
         benzo_ct = "CDC Benzodiazepine Overdose v2 Parsed Data Count",
         coca_ct = "CDC Cocaine Overdose v2 Parsed Data Count",
         fent_ct = "CDC Fentanyl Overdose v2 Parsed Data Count",
         heroin_ct = "CDC Heroin Overdose v5 Parsed Data Count",
         meth_ct = "CDC Methamphetamine Overdose v1 Parsed Data Count",
         opioid_ct = "CDC Opioid Overdose v4 Parsed Data Count",
         stim_ct = "CDC Stimulant Overdose v4 Parsed Data Count",
         totalED = "CDC Stimulant Overdose v4 Parsed All Count") 


##Weekly OD x County, Sex

url_sex <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?geography=",counties,"&datasource=va_er&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=ccddCategory&aqtTarget=TableBuilder&ccddCategory=",categories,"&geographySystem=region&detector=probewmapoissonswitch&timeResolution=monthly&hasBeenE=1&rowFields=timeResolution&rowFields=geographyregion&rowFields=sex&columnField=ccddCategory")

sex <- myProfile$get_api_data(url_sex, fromCSV = TRUE) %>%
  dplyr::select(timeResolution,
         geographyregion,
         sex,
         alldrug_ct = "CDC All Drug Overdose v3 Parsed Data Count",
         benzo_ct = "CDC Benzodiazepine Overdose v2 Parsed Data Count",
         coca_ct = "CDC Cocaine Overdose v2 Parsed Data Count",
         fent_ct = "CDC Fentanyl Overdose v2 Parsed Data Count",
         heroin_ct = "CDC Heroin Overdose v5 Parsed Data Count",
         meth_ct = "CDC Methamphetamine Overdose v1 Parsed Data Count",
         opioid_ct = "CDC Opioid Overdose v4 Parsed Data Count",
         stim_ct = "CDC Stimulant Overdose v4 Parsed Data Count",
         totalED = "CDC Stimulant Overdose v4 Parsed All Count") 


##Weekly x County, Age Group (Pediatric)

url_pedsage <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?geography=",counties,"&datasource=va_er&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=ccddCategory&aqtTarget=TableBuilder&ccddCategory=",categories,"&geographySystem=region&detector=probewmapoissonswitch&timeResolution=monthly&hasBeenE=1&rowFields=timeResolution&rowFields=geographyregion&rowFields=ageGroupPediatric&columnField=ccddCategory")

pedsage <- myProfile$get_api_data(url_pedsage, fromCSV = TRUE) %>%
  dplyr::select(timeResolution,
         geographyregion,
         ageGroup = ageGroupPediatric,
         alldrug_ct = "CDC All Drug Overdose v3 Parsed Data Count",
         benzo_ct = "CDC Benzodiazepine Overdose v2 Parsed Data Count",
         coca_ct = "CDC Cocaine Overdose v2 Parsed Data Count",
         fent_ct = "CDC Fentanyl Overdose v2 Parsed Data Count",
         heroin_ct = "CDC Heroin Overdose v5 Parsed Data Count",
         meth_ct = "CDC Methamphetamine Overdose v1 Parsed Data Count",
         opioid_ct = "CDC Opioid Overdose v4 Parsed Data Count",
         stim_ct = "CDC Stimulant Overdose v4 Parsed Data Count",
         totalED = "CDC Stimulant Overdose v4 Parsed All Count") 

##Weekly x County, Age Group (NCHS)

url_NCHSage <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?geography=",counties,"&datasource=va_er&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=ccddCategory&aqtTarget=TableBuilder&ccddCategory=",categories,"&geographySystem=region&detector=probewmapoissonswitch&timeResolution=monthly&hasBeenE=1&rowFields=timeResolution&rowFields=geographyregion&rowFields=ageNCHS&columnField=ccddCategory")

NCHSage <- myProfile$get_api_data(url_NCHSage, fromCSV = TRUE) %>%
  dplyr::select(timeResolution,
         geographyregion,
         ageGroup = ageNCHS,
         alldrug_ct = "CDC All Drug Overdose v3 Parsed Data Count",
         benzo_ct = "CDC Benzodiazepine Overdose v2 Parsed Data Count",
         coca_ct = "CDC Cocaine Overdose v2 Parsed Data Count",
         fent_ct = "CDC Fentanyl Overdose v2 Parsed Data Count",
         heroin_ct = "CDC Heroin Overdose v5 Parsed Data Count",
         meth_ct = "CDC Methamphetamine Overdose v1 Parsed Data Count",
         opioid_ct = "CDC Opioid Overdose v4 Parsed Data Count",
         stim_ct = "CDC Stimulant Overdose v4 Parsed Data Count",
         totalED = "CDC Stimulant Overdose v4 Parsed All Count") 


##Weekly x County, Race

url_race <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?geography=",counties,"&datasource=va_er&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=ccddCategory&aqtTarget=TableBuilder&ccddCategory=",categories,"&geographySystem=region&detector=probewmapoissonswitch&timeResolution=monthly&hasBeenE=1&rowFields=timeResolution&rowFields=geographyregion&rowFields=race&columnField=ccddCategory")


race <- myProfile$get_api_data(url_race, fromCSV = TRUE) %>%
  dplyr::select(timeResolution,
         geographyregion,
         race,
         alldrug_ct = "CDC All Drug Overdose v3 Parsed Data Count",
         benzo_ct = "CDC Benzodiazepine Overdose v2 Parsed Data Count",
         coca_ct = "CDC Cocaine Overdose v2 Parsed Data Count",
         fent_ct = "CDC Fentanyl Overdose v2 Parsed Data Count",
         heroin_ct = "CDC Heroin Overdose v5 Parsed Data Count",
         meth_ct = "CDC Methamphetamine Overdose v1 Parsed Data Count",
         opioid_ct = "CDC Opioid Overdose v4 Parsed Data Count",
         stim_ct = "CDC Stimulant Overdose v4 Parsed Data Count",
         totalED = "CDC Stimulant Overdose v4 Parsed All Count") 

##Weekly x County, Ethnicity

url_ethnicity <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?geography=",counties,"&datasource=va_er&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=ccddCategory&aqtTarget=TableBuilder&ccddCategory=",categories,"&geographySystem=region&detector=probewmapoissonswitch&timeResolution=monthly&hasBeenE=1&rowFields=timeResolution&rowFields=geographyregion&rowFields=ethnicity&columnField=ccddCategory")


ethnicity <- myProfile$get_api_data(url_ethnicity, fromCSV = TRUE) %>%
  dplyr::select(timeResolution,
         geographyregion,
         ethnicity,
         alldrug_ct = "CDC All Drug Overdose v3 Parsed Data Count",
         benzo_ct = "CDC Benzodiazepine Overdose v2 Parsed Data Count",
         coca_ct = "CDC Cocaine Overdose v2 Parsed Data Count",
         fent_ct = "CDC Fentanyl Overdose v2 Parsed Data Count",
         heroin_ct = "CDC Heroin Overdose v5 Parsed Data Count",
         meth_ct = "CDC Methamphetamine Overdose v1 Parsed Data Count",
         opioid_ct = "CDC Opioid Overdose v4 Parsed Data Count",
         stim_ct = "CDC Stimulant Overdose v4 Parsed Data Count",
         totalED = "CDC Stimulant Overdose v4 Parsed All Count") 

## OD Sex

sex_df <- sex %>%
  dplyr::mutate(sex = recode(sex,
                              "Not Reported" = "Unknown",
                              "Unknown" = "Unknown")) %>%
  dplyr::group_by(timeResolution, geographyregion, sex) %>%
  dplyr::summarize(across(everything(), ~sum(.x))) %>%
  dplyr::select(timeResolution,
         geographyregion,
         sex,
         alldrug_ct,
         benzo_ct,
         coca_ct,
         fent_ct,
         heroin_ct,
         meth_ct,
         opioid_ct,
         stim_ct,
         totalED)


## OD Age Groups

agePeds_subset <- pedsage %>%
  dplyr::filter(ageGroup == "00-04"|
                ageGroup == "05-09"|
                ageGroup == "10-14") %>%
  dplyr::mutate(ageGroup = ifelse(ageGroup == "05-09"|ageGroup == "10-14", "05-14", "00-04")) %>%
  dplyr::group_by(timeResolution, geographyregion, ageGroup) %>%
  dplyr::summarize(across(everything(), ~sum(.x))) %>%
  dplyr::select(timeResolution,
         geographyregion,
         ageGroup,
         alldrug_ct,
         benzo_ct,
         coca_ct,
         fent_ct,
         heroin_ct,
         meth_ct,
         opioid_ct,
         stim_ct,
         totalED)

ageNCHS_subset <- NCHSage %>%
  dplyr::filter(ageGroup == "15-24"|
                ageGroup == "25-34"|
                ageGroup == "35-44"|
                ageGroup == "45-54"|
                ageGroup == "55-64"|
                ageGroup == "65-74"|
                ageGroup == "75-84"|
                ageGroup == "85+") %>%
  dplyr::group_by(timeResolution,geographyregion,ageGroup) %>%
  dplyr::summarize(across(everything(), ~sum(.x))) %>%
  dplyr::select(timeResolution,
         geographyregion,
         ageGroup,
         alldrug_ct,
         benzo_ct,
         coca_ct,
         fent_ct,
         heroin_ct,
         meth_ct,
         opioid_ct,
         stim_ct,
         totalED)

age_df <- rbind(agePeds_subset, ageNCHS_subset) %>%
  arrange(timeResolution, geographyregion, ageGroup)

# OD Race & Ethnicity

race_df <- race %>%
  dplyr::mutate(race = recode(race,
                              "Not Reported" = "Unknown",
                              "Refused to answer" = "Unknown",
                              "Unknown" = "Unknown")) %>%
  dplyr::group_by(timeResolution, geographyregion, race) %>%
  dplyr::summarize(across(everything(), ~sum(.x))) %>%
  dplyr::select(timeResolution,
         geographyregion,
         race,
         alldrug_ct,
         benzo_ct,
         coca_ct,
         fent_ct,
         heroin_ct,
         meth_ct,
         opioid_ct,
         stim_ct,
         totalED)


ethn_df <- ethnicity %>%
  dplyr::mutate(ethnicity = recode(ethnicity,
                              "Not Reported" = "Unknown",
                              "Unknown" = "Unknown")) %>%
  dplyr::group_by(timeResolution, geographyregion, ethnicity) %>%
  dplyr::summarize(across(everything(), ~sum(.x))) %>%
  dplyr::select(timeResolution,
         geographyregion,
         ethnicity,
         alldrug_ct,
         benzo_ct,
         coca_ct,
         fent_ct,
         heroin_ct,
         meth_ct,
         opioid_ct,
         stim_ct,
         totalED)



# CLEAN DATA #

county_df <- as.data.frame(county) %>% 
  mutate(geographyregion = gsub("CO_", "", geographyregion)) %>%
  dplyr::rename("county" = geographyregion)

sex_df <- as.data.frame(sex_df) %>% 
  mutate(geographyregion = gsub("CO_", "", geographyregion)) %>%
  dplyr::rename("county" = geographyregion)

age_df <- as.data.frame(age_df) %>% 
  mutate(geographyregion = gsub("CO_", "", geographyregion)) %>%
  dplyr::rename("county" = geographyregion)

race_df <- as.data.frame(race_df) %>% 
  mutate(geographyregion = gsub("CO_", "", geographyregion)) %>%
  dplyr::rename("county" = geographyregion)

ethn_df <- as.data.frame(ethn_df) %>% 
  mutate(geographyregion = gsub("CO_", "", geographyregion)) %>%
  dplyr::rename("county" = geographyregion)


rm(ageNCHS_subset, agePeds_subset, NCHSage, pedsage, totalage, overall, race, sex, ethnicity, county)

# Write to XLSX

filename <- paste0("SyStable_OD2Apublicdash_v1.xlsx")
filepath <- paste0("Output/",filename)
sheets <- list("county" = county_df,
               "sex" = sex_df, 
               "age" = age_df, 
               "race" = race_df, 
               "ethn" = ethn_df)

writexl::write_xlsx(sheets, path = filepath, format_headers = FALSE)

rm(list = ls())