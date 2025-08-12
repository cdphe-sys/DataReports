# SyS OD2A Data Dashboard RMD - Data Pull #----------------------------------------------

# Set up ------------------------------------------------------------------
### Load libraries 
library(lubridate)
library(tidyverse)
library(knitr)
library(httr)
library(Rnssp)
library(ggplot2)
library(tidytext)
library(janitor) 
library(DT)
library(ggformula)
library(purrr)
library(wesanderson)
library(sf)
library(dplyr)
library(plyr)
library(rgdal)
library(ggrepel)
library(ggpubr)
library(ggthemes)
library(grid)
library(gridExtra)
library(jsonlite)
library(data.table)
library(MMWRweek)
library(pacman)

#pacman::p_install_version("plotly", version = "4.8.0")

### Run the myProfile.R file each time your ESSENCE password changes before using this code
myProfile <- readRDS("~/OD2A_RMD/myProfile.rds")

### Set dates

startdate = "2Jun2024"
enddate = "5Jul2025"

### Set counties 

counties <- "co_adams&geography=co_arapahoe&geography=co_archuleta&geography=co_boulder&geography=co_broomfield&geography=co_denver&geography=co_douglas&geography=co_el%20paso&geography=co_garfield&geography=co_jefferson&geography=co_la%20plata&geography=co_larimer&geography=co_mesa&geography=co_moffat&geography=co_montezuma&geography=co_phillips&geography=co_pueblo&geography=co_rio%20blanco&geography=co_sedgwick&geography=co_weld&geography=co_yuma"

# Data Pull -----------------

## category list

### CDC All Drug v3 Parsed
### CDC Benzodiazepine Overdose v2 Parsed
### CDC Cocaine Overdose v2 Parsed
### CDC Fentanyl Overdose v2 Parsed
### CDC Heroin Overdose v5 Parsed
### CDC Methamphetamine Overdose v1 Parsed
### CDC Opioid Overdose v4 Parsed
### CDC Stimulants v4 Parsed


cat <- "cdc%20all%20drug%20overdose%20v3%20parsed&ccddCategory=cdc%20heroin%20overdose%20v5%20parsed&ccddCategory=cdc%20opioid%20overdose%20v4%20parsed&ccddCategory=cdc%20methamphetamine%20overdose%20v1%20parsed&ccddCategory=cdc%20stimulant%20overdose%20v4%20parsed&ccddCategory=cdc%20fentanyl%20overdose%20v2%20parsed&ccddCategory=cdc%20cocaine%20overdose%20v2%20parsed&ccddCategory=cdc%20benzodiazepine%20overdose%20v2%20parsed"

## API pull

url_cat <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?geography=",counties,"&datasource=va_er&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=ccddCategory&aqtTarget=TableBuilder&ccddCategory=",cat,"&geographySystem=region&detector=probewmapoissonswitch&timeResolution=weekly&hasBeenE=1&rowFields=timeResolution&rowFields=geographyregion&columnField=ccddCategory")

cat_df <- get_api_data(url_cat, fromCSV = TRUE) %>%
  dplyr::select(MMWR = timeResolution,
         county = geographyregion,
         alldrug_pct = "CDC All Drug Overdose v3 Parsed",
         alldrug_ct = "CDC All Drug Overdose v3 Parsed Data Count",
         benzo_pct = "CDC Benzodiazepine Overdose v2 Parsed",
         benzo_ct = "CDC Benzodiazepine Overdose v2 Parsed Data Count",
         coca_pct = "CDC Cocaine Overdose v2 Parsed",
         coca_ct = "CDC Cocaine Overdose v2 Parsed Data Count",
         fent_pct = "CDC Fentanyl Overdose v2 Parsed",
         fent_ct = "CDC Fentanyl Overdose v2 Parsed Data Count",
         heroin_pct = "CDC Heroin Overdose v5 Parsed",
         heroin_ct = "CDC Heroin Overdose v5 Parsed Data Count",
         meth_pct = "CDC Methamphetamine Overdose v1 Parsed",
         meth_ct = "CDC Methamphetamine Overdose v1 Parsed Data Count",
         opioid_pct = "CDC Opioid Overdose v4 Parsed",
         opioid_ct = "CDC Opioid Overdose v4 Parsed Data Count",
         stim_pct = "CDC Stimulant Overdose v4 Parsed",
         stim_ct = "CDC Stimulant Overdose v4 Parsed Data Count",
         totalED = "CDC Stimulant Overdose v4 Parsed All Count")

## Format Cat_DF to serve as AllDrugOverdoseED_df2 for table in RMD

# OD x Sex ---------------------------------------------------

## API Pull

url_sex <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?geography=",counties,"&datasource=va_er&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=sex&aqtTarget=TableBuilder&ccddCategory=",cat,"&geographySystem=region&detector=probewmapoissonswitch&timeResolution=weekly&hasBeenE=1&rowFields=timeResolution&rowFields=geographyregion&columnField=sex")

sex_df <- get_api_data(url_sex, fromCSV = TRUE) %>%
  dplyr::select(MMWR = timeResolution,
         county = geographyregion,
         sex_f_pct = "Female",
         sex_f_ct = "Female Data Count",
         sex_m_pct = "Male",
         sex_m_ct = "Male Data Count",
         sex_nr_ct = "Not Reported Data Count",
         sex_inv_ct = "Invalid Data Count",
         sex_other_ct = "Other Data Count",
         sex_unk_ct = "Unknown Data Count",
         sex_totalOD = "Male All Count") %>%
  mutate(sex_unk_ct = (sex_nr_ct + sex_inv_ct + sex_other_ct + sex_unk_ct),
         sex_unk_pct = (sex_unk_ct/sex_totalOD*100)) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::select(-c(sex_nr_ct, sex_inv_ct, sex_other_ct))

# OD x Age ---------------------------------------------------

## API Pull

url_age <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?geography=",counties,"&datasource=va_er&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=ageschool&aqtTarget=TableBuilder&ccddCategory=",cat,"&geographySystem=region&detector=probewmapoissonswitch&timeResolution=weekly&hasBeenE=1&rowFields=timeResolution&rowFields=geographyregion&columnField=ageschool")

age_df <- get_api_data(url_age, fromCSV = TRUE) %>%
  dplyr::select(MMWR = timeResolution,
         county = geographyregion,
         age_0004_ct = "00-04 Data Count",
         age_0511_ct = "05-11 Data Count",
         age_1217_ct = "12-17 Data Count",
         age_1825_ct = "18-25 Data Count",
         age_2634_ct = "26-34 Data Count",
         age_3544_ct = "35-44 Data Count",
         age_4554_ct = "45-54 Data Count",
         age_5564_ct = "55-64 Data Count",
         age_65_ct = "65+ Data Count",
         age_totalOD = "65+ All Count") %>%
  mutate(age_0017_ct = (age_0004_ct + age_0511_ct + age_1217_ct),
         age_0017_pct = (age_0017_ct/age_totalOD*100),
         age_1825_pct = (age_1825_ct/age_totalOD*100),
         age_2634_pct = (age_2634_ct/age_totalOD*100),
         age_3544_pct = (age_3544_ct/age_totalOD*100),
         age_4554_pct = (age_4554_ct/age_totalOD*100),
         age_5564_pct = (age_5564_ct/age_totalOD*100),
         age_65_pct = (age_65_ct/age_totalOD*100)) %>%
  dplyr::select(-c(age_0004_ct, age_0511_ct, age_1217_ct))


# OD x Race/Ethnicity ---------------------------------------------------

## API Pull

url_race <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?geography=",counties,"&datasource=va_er&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=race&aqtTarget=TableBuilder&ccddCategory=",cat,"&geographySystem=region&detector=probewmapoissonswitch&timeResolution=weekly&hasBeenE=1&rowFields=timeResolution&rowFields=geographyregion&columnField=race")

race_df <- get_api_data(url_race, fromCSV = TRUE) %>%
  dplyr::select(MMWR = timeResolution,
         county = geographyregion,
         race_AIAN_pct = "American Indian or Alaska Native",
         race_AIAN_ct = "American Indian or Alaska Native Data Count",
         race_asian_pct = "Asian",
         race_asian_ct = "Asian Data Count",
         race_black_pct = "Black or African American",
         race_black_ct = "Black or African American Data Count",
         race_NHPI_pct = "Native Hawaiian or Other Pacific Islander",
         race_NHPI_ct = "Native Hawaiian or Other Pacific Islander Data Count",
         race_white_pct = "White",
         race_white_ct = "White Data Count",
         race_other_pct = "Other Race",
         race_other_ct = "Other Race Data Count",
         race_nr_ct = "Not Reported Data Count",
         race_unk_ct = "Unknown Data Count",
         race_rta_ct = "Refused to answer Data Count",
         race_totalOD = "Other Race All Count") %>%
  mutate(race_unk_ct = (race_nr_ct + race_unk_ct + race_rta_ct),
         race_unk_pct = (race_unk_ct/race_totalOD*100)) %>%
  dplyr::select(-c(race_nr_ct, race_rta_ct))

url_ethn <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?geography=",counties,"&datasource=va_er&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=ethnicity&aqtTarget=TableBuilder&ccddCategory=",cat,"&geographySystem=region&detector=probewmapoissonswitch&timeResolution=weekly&hasBeenE=1&rowFields=timeResolution&rowFields=geographyregion&columnField=ethnicity")

ethn_df <- get_api_data(url_ethn, fromCSV = TRUE) %>%
  dplyr::select(MMWR = timeResolution,
         county = geographyregion,
         ethn_hisp_pct = "Hispanic or Latino",
         ethn_hisp_ct = "Hispanic or Latino Data Count",
         ethn_nothisp_pct = "Not Hispanic or Latino",
         ethn_nothisp_ct = "Not Hispanic or Latino Data Count",
         ethn_nr_ct = "Not Reported Data Count",
         ethn_unk_ct = "Unknown Data Count",
         ethn_totalOD = "Hispanic or Latino All Count") %>%
  mutate(ethn_unk_ct = (ethn_nr_ct + ethn_unk_ct),
         ethn_unk_pct = (ethn_unk_ct/ethn_totalOD*100)) %>%
  dplyr::select(-ethn_nr_ct)


# Merge Tables ---------------------------------------------
completeOD_df <- merge(cat_df, sex_df, by = c('MMWR', 'county'))
completeOD_df <- merge(completeOD_df, age_df, by = c('MMWR', 'county'))
completeOD_df <- merge(completeOD_df, race_df, by = c('MMWR', 'county'))
completeOD_df <- merge(completeOD_df, ethn_df, by = c('MMWR', 'county'))

completeOD_df <-  completeOD_df %>% 
  separate(col = MMWR, into = c("MMWRyear", "MMWRweek"), sep = "-", convert = TRUE) %>%
  mutate(WeekStart = MMWRweek2Date(MMWRyear, MMWRweek)) %>%
  mutate(county = gsub("CO_", "", county),
         WeekStart = as.Date(WeekStart))

completeOD_df <- completeOD_df %>% mutate(across(where(is.numeric), round, 2))

#save(completeOD_df, file = "~/OD2A_RMD/RMDOD2Adata.Rdata")

save.image(file = "~/OD2A_RMD/RMDOD2Adatacomplete.Rdata")

rm(list = ls())