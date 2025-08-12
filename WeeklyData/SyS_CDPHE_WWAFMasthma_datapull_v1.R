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
library(dplyr)
library(widyr)
library(MMWRweek)
library(xlsx)
library(writexl)

Sys.getenv(c("JAVA_HOME", "LD_LIBRARY_PATH"))
setwd("~/WeeklyData")

## Set NSSP user profile ----
myProfile <- readRDS("myProfile.rds")

# API DATA #
#Leave start date as 12/29/2019, adjust end date to reflect most recent complete week's end date (Saturday)

startdate <- "31Dec2023"
enddate <- "2Aug2025"
date <- "TEST"
week <- "31"
year <- "2025"

## Pediatric AFM & Asthma-related ED Visits x school age groups

url_overall <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?geography=co&datasource=va_er&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&ageschool=00-04&ageschool=05-11&ageschool=12-17&percentParam=ccddCategory&aqtTarget=TableBuilder&ccddCategory=cdc%20afm%20broad%20v1-limit%20to%20pediatric&ccddCategory=cdc%20afm%20narrow%20v1-limit%20to%20pediatric&ccddCategory=cdc%20asthma%20ccdd%20v1&geographySystem=state&detector=nodetectordetector&timeResolution=weekly&hasBeenE=1&rowFields=timeResolution&rowFields=ageschool&columnField=ccddCategory")

overall <- myProfile$get_api_data(url_overall, fromCSV = TRUE) %>%
  dplyr::select(timeResolution,
         AgeGroup = ageschool,
         "Ped AFM broad (%)" = "CDC AFM Broad v1-Limit to Pediatric",
         "Ped AFM broad (Counts)" = "CDC AFM Broad v1-Limit to Pediatric Data Count",
         "Ped AFM narrow (%)" = "CDC AFM Narrow v1-Limit to Pediatric",
         "Ped AFM narrow (Counts)" = "CDC AFM Narrow v1-Limit to Pediatric Data Count",
         "Ped Asthma (%)" = "CDC Asthma CCDD v1",
         "Ped Asthma (Counts)" = "CDC Asthma CCDD v1 Data Count",
         "Total ED Visits" = "CDC Asthma CCDD v1 All Count")

url_totalage <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?geography=co&datasource=va_er&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&ageschool=00-04&ageschool=05-11&ageschool=12-17&percentParam=ccddCategory&aqtTarget=TableBuilder&ccddCategory=cdc%20afm%20broad%20v1-limit%20to%20pediatric&ccddCategory=cdc%20afm%20narrow%20v1-limit%20to%20pediatric&ccddCategory=cdc%20asthma%20ccdd%20v1&geographySystem=state&detector=nodetectordetector&timeResolution=weekly&hasBeenE=1&rowFields=timeResolution&columnField=ccddCategory")

totalage <- myProfile$get_api_data(url_totalage, fromCSV = TRUE) %>%
  dplyr::select(timeResolution,
         "Ped AFM broad (%)" = "CDC AFM Broad v1-Limit to Pediatric",
         "Ped AFM broad (Counts)" = "CDC AFM Broad v1-Limit to Pediatric Data Count",
         "Ped AFM narrow (%)" = "CDC AFM Narrow v1-Limit to Pediatric",
         "Ped AFM narrow (Counts)" = "CDC AFM Narrow v1-Limit to Pediatric Data Count",
         "Ped Asthma (%)" = "CDC Asthma CCDD v1",
         "Ped Asthma (Counts)" = "CDC Asthma CCDD v1 Data Count",
         "Total ED Visits" = "CDC Asthma CCDD v1 All Count") %>%
  add_column(AgeGroup = "totalage", .after = "timeResolution")

overall <- rbind(overall, totalage) %>%
  arrange(timeResolution, AgeGroup)


# CLEAN DATA #

overall <- overall %>% 
  separate(col = timeResolution, into = c("MMWRyear", "MMWRweek"), sep = "-", convert = TRUE) %>%
  mutate(WeekStart = MMWRweek2Date(MMWRyear, MMWRweek))
overall <- overall[, c(11,3,4,5,6,7,8,9,10)]

date <- gsub("_", "", date)
filename <- paste0(year, date, "_SyStable_WWpedAFM_v1.xlsx")
filepath <- paste0("Output/",filename)


write_xlsx(overall, 
           path = filepath, 
           col_names = T)

save.image(file = "RMD_wwpedafm_data.Rdata")

rm(list = ls())
