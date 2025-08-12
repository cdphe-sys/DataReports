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
library(writexl)

Sys.getenv(c("JAVA_HOME", "LD_LIBRARY_PATH"))
setwd("~/WeeklyData")
  
## Set NSSP user profile ----
myProfile <- readRDS("myProfile.rds")

# API DATA #
#Leave start date as 12/29/2019, adjust end date to reflect most recent complete week's end date (Saturday)

startdate <- "29Dec2019"
enddate <- "2Aug2025"
date <- "TEST"
week <- "31"
year <- "2025"

# Set geography: ILI list of facilities

counties <- "co_adams&geography=co_arapahoe&geography=co_douglas&geography=co_boulder&geography=co_denver&geography=co_el%20paso&geography=co_jefferson&geography=co_larimer&geography=co_broomfield&geography=co_la%20plata&geography=co_mesa&geography=co_montezuma&geography=co_pueblo&geography=co_weld"

facilities <- "1345&geography=1337&geography=1338&geography=31467&geography=31466&geography=1340&geography=31461&geography=1327&geography=1333&geography=1339&geography=1332&geography=19862&geography=1336&geography=1325&geography=31465&geography=1329&geography=1335&geography=1334&geography=1324&geography=1328&geography=24996&geography=24994&geography=24995&geography=1330&geography=1343&geography=1344&geography=31464&geography=1342&geography=1326&geography=1331&geography=1352&geography=1348&geography=16434&geography=1353&geography=1354&geography=1346&geography=1349&geography=1347&geography=1351&geography=1350&geography=31455&geography=31456&geography=31462&geography=31358&geography=31410&geography=31460&geography=31457&geography=31458&geography=31459&geography=1341&geography=32311&geography=32052"


##CLI Overall - Weekly Diagnosed Flu, New COVID Related, Diagnosed COVID, ILI, All Visits Percent & Counts

url_overall <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?percentParam=ccddCategory&patientClass=e&displayTotals=false&displayTotals=false&displayTotals=false&displayTotals=false&medicalGroupingSystem=essencesyndromes&dateconfig=15&userId=583&geographySystem=hospital&displayZeroCountRows=true&displayZeroCountRows=true&ccddCategory=cdc%20influenza%20dd%20v1&ccddCategory=ili%20ccdd%20neg%20coronavirus%20dd%20v1&ccddCategory=cli%20cc%20with%20cli%20dd%20and%20coronavirus%20dd%20v2&ccddCategory=cdc%20covid-specific%20dd%20v1&ccddCategory=cdc%20respiratory%20syncytial%20virus%20v1&ccddCategory=cdc%20respiratory%20syncytial%20virus%20dd%20v1&ccddCategory=cdc%20broad%20acute%20respiratory%20dd%20v1&geography=",facilities,"&datasource=va_hosp&timeResolution=weekly&portletId=190591&aqtTarget=TableBuilder&detector=nodetectordetector&graphWidth=499&fieldIDs=timeResolution&fieldIDs=ccddCategory&fieldLabels=Week&fieldLabels=CC%20and%20DD%20Category&startDate=",startdate,"&endDate=",enddate,"&rowFields=timeResolution&columnField=ccddCategory")

overall <- myProfile$get_api_data(url_overall, fromCSV = TRUE) %>%
  dplyr::select(timeResolution,
         "ILI neg COVID (%)" = "ILI CCDD Neg Coronavirus DD v1",
         "ILI neg COVID (Counts)" = "ILI CCDD Neg Coronavirus DD v1 Data Count",
         "Diagnosed Flu (%)" = "CDC Influenza DD v1",
         "Diagnosed Flu (Counts)" = "CDC Influenza DD v1 Data Count",
         "New COVID Related (%)" = "CLI CC with CLI DD and Coronavirus DD v2",
         "New COVID Related (Counts)" = "CLI CC with CLI DD and Coronavirus DD v2 Data Count",
         "Diagnosed COVID (%)" = "CDC COVID-Specific DD v1",
         "Diagnosed COVID (Counts)" = "CDC COVID-Specific DD v1 Data Count",
         "RSV Related (%)" = "CDC Respiratory Syncytial Virus v1",
         "RSV Related (Counts)" = "CDC Respiratory Syncytial Virus v1 Data Count",
         "Diagnosed RSV (%)" = "CDC Respiratory Syncytial Virus DD v1",
         "Diagnosed RSV (Counts)" = "CDC Respiratory Syncytial Virus DD v1 Data Count",
         "Broad Acute Resp (%)" = "CDC Broad Acute Respiratory DD v1",
         "Broad Acute Resp (Counts)" = "CDC Broad Acute Respiratory DD v1 Data Count",
         "All Hospital Visits" = "CLI CC with CLI DD and Coronavirus DD v2 All Count")

##CLI x County

url_county <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?endDate=",enddate, "&geography=",counties,"&percentParam=ccddCategory&patientClass=e&datasource=va_er&startDate=",startdate, "&medicalGroupingSystem=essencesyndromes&userId=3942&aqtTarget=TableBuilder&ccddCategory=cdc%20influenza%20dd%20v1&ccddCategory=cli%20cc%20with%20cli%20dd%20and%20coronavirus%20dd%20v2&ccddCategory=cdc%20covid-specific%20dd%20v1&ccddCategory=ili%20ccdd%20neg%20coronavirus%20dd%20v1&ccddCategory=cdc%20respiratory%20syncytial%20virus%20v1&ccddCategory=cdc%20respiratory%20syncytial%20virus%20dd%20v1&ccddCategory=cdc%20broad%20acute%20respiratory%20dd%20v1&geographySystem=region&detector=nodetectordetector&dateconfig=15&timeResolution=weekly&rowFields=timeResolution&rowFields=geographyregion&columnField=ccddCategory")


county <- myProfile$get_api_data(url_county, fromCSV = TRUE) %>%
  dplyr::select(timeResolution,
         geographyregion,
         "ILI neg COVID" = "ILI CCDD Neg Coronavirus DD v1 Data Count",
         "Diagnosed Flu" = "CDC Influenza DD v1 Data Count",
         "New COVID Related" = "CLI CC with CLI DD and Coronavirus DD v2 Data Count",
         "Diagnosed COVID" = "CDC COVID-Specific DD v1 Data Count",
         "RSV Related" = "CDC Respiratory Syncytial Virus v1 Data Count",
         "Diagnosed RSV" = "CDC Respiratory Syncytial Virus DD v1 Data Count",
         "Broad Acute Resp" = "CDC Broad Acute Respiratory DD v1 Data Count",
         "Total ED Visits" = "CLI CC with CLI DD and Coronavirus DD v2 All Count")

##Age - ILI Rept

url_ageILI <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?percentParam=ccddCategory&patientClass=e&displayTotals=false&displayTotals=false&displayTotals=false&displayTotals=false&displayTotals=false&displayTotals=false&medicalGroupingSystem=essencesyndromes&dateconfig=15&userId=583&geographySystem=hospital&displayZeroCountRows=true&displayZeroCountRows=true&ccddCategory=cdc%20influenza%20dd%20v1&ccddCategory=ili%20ccdd%20neg%20coronavirus%20dd%20v1&ccddCategory=cli%20cc%20with%20cli%20dd%20and%20coronavirus%20dd%20v2&ccddCategory=cdc%20covid-specific%20dd%20v1&ccddCategory=cdc%20respiratory%20syncytial%20virus%20v1&ccddCategory=cdc%20respiratory%20syncytial%20virus%20dd%20v1&ccddCategory=cdc%20broad%20acute%20respiratory%20dd%20v1&geography=",facilities,"&datasource=va_hosp&timeResolution=weekly&portletId=190594&aqtTarget=TableBuilder&detector=nodetectordetector&graphWidth=499&fieldIDs=timeResolution&fieldIDs=ageCDCILI&fieldIDs=ccddCategory&fieldLabels=Week&fieldLabels=CDC%20ILI%20Reporting%20Age%20Group&fieldLabels=CC%20and%20DD%20Category&startDate=",startdate, "&endDate=",enddate, "&rowFields=timeResolution&rowFields=ageCDCILI&columnField=ccddCategory")


age_ili <- myProfile$get_api_data(url_ageILI, fromCSV = TRUE) %>%
  dplyr::select(timeResolution,
         AgeGroup = ageCDCILI,
         "ILI neg COVID (%)" = "ILI CCDD Neg Coronavirus DD v1",
         "ILI neg COVID (Counts)" = "ILI CCDD Neg Coronavirus DD v1 Data Count",
         "Diagnosed Flu (%)" = "CDC Influenza DD v1",
         "Diagnosed Flu (Counts)" = "CDC Influenza DD v1 Data Count",
         "New COVID Related (%)" = "CLI CC with CLI DD and Coronavirus DD v2",
         "New COVID Related (Counts)" = "CLI CC with CLI DD and Coronavirus DD v2 Data Count",
         "Diagnosed COVID (%)" = "CDC COVID-Specific DD v1",
         "Diagnosed COVID (Counts)" = "CDC COVID-Specific DD v1 Data Count",
         "RSV Related (%)" = "CDC Respiratory Syncytial Virus v1",
         "RSV Related (Counts)" = "CDC Respiratory Syncytial Virus v1 Data Count",
         "Diagnosed RSV (%)" = "CDC Respiratory Syncytial Virus DD v1",
         "Diagnosed RSV (Counts)" = "CDC Respiratory Syncytial Virus DD v1 Data Count",
         "Broad Acute Resp (%)" = "CDC Broad Acute Respiratory DD v1",
         "Broad Acute Resp (Counts)" = "CDC Broad Acute Respiratory DD v1 Data Count",
         "All Hospital Visits" = "CLI CC with CLI DD and Coronavirus DD v2 All Count")


##Age - NCHS

url_ageNCHS <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?percentParam=ccddCategory&patientClass=e&displayTotals=false&displayTotals=false&displayTotals=false&displayTotals=false&displayTotals=false&displayTotals=false&displayTotals=false&displayTotals=false&displayTotals=false&medicalGroupingSystem=essencesyndromes&dateconfig=15&userId=583&geographySystem=hospital&displayZeroCountRows=true&displayZeroCountRows=true&displayZeroCountRows=true&ccddCategory=cdc%20influenza%20dd%20v1&ccddCategory=ili%20ccdd%20neg%20coronavirus%20dd%20v1&ccddCategory=cli%20cc%20with%20cli%20dd%20and%20coronavirus%20dd%20v2&ccddCategory=cdc%20covid-specific%20dd%20v1&ccddCategory=cdc%20respiratory%20syncytial%20virus%20v1&ccddCategory=cdc%20respiratory%20syncytial%20virus%20dd%20v1&ccddCategory=cdc%20broad%20acute%20respiratory%20dd%20v1&geography=",facilities,"&datasource=va_hosp&timeResolution=weekly&portletId=190595&aqtTarget=TableBuilder&detector=nodetectordetector&graphWidth=499&fieldIDs=timeResolution&fieldIDs=ageNCHS&fieldIDs=ccddCategory&fieldLabels=Week&fieldLabels=NCHS%20Age%20Group&fieldLabels=CC%20and%20DD%20Category&startDate=",startdate, "&endDate=",enddate, "&rowFields=timeResolution&rowFields=ageNCHS&columnField=ccddCategory")


age_NCHS <- myProfile$get_api_data(url_ageNCHS, fromCSV = TRUE) %>%
  dplyr::select(timeResolution,
         AgeGroup = ageNCHS,
         "ILI neg COVID (%)" = "ILI CCDD Neg Coronavirus DD v1",
         "ILI neg COVID (Counts)" = "ILI CCDD Neg Coronavirus DD v1 Data Count",
         "Diagnosed Flu (%)" = "CDC Influenza DD v1",
         "Diagnosed Flu (Counts)" = "CDC Influenza DD v1 Data Count",
         "New COVID Related (%)" = "CLI CC with CLI DD and Coronavirus DD v2",
         "New COVID Related (Counts)" = "CLI CC with CLI DD and Coronavirus DD v2 Data Count",
         "Diagnosed COVID (%)" = "CDC COVID-Specific DD v1",
         "Diagnosed COVID (Counts)" = "CDC COVID-Specific DD v1 Data Count",
         "RSV Related (%)" = "CDC Respiratory Syncytial Virus v1",
         "RSV Related (Counts)" = "CDC Respiratory Syncytial Virus v1 Data Count",
         "Diagnosed RSV (%)" = "CDC Respiratory Syncytial Virus DD v1",
         "Diagnosed RSV (Counts)" = "CDC Respiratory Syncytial Virus DD v1 Data Count",
         "Broad Acute Resp (%)" = "CDC Broad Acute Respiratory DD v1",
         "Broad Acute Resp (Counts)" = "CDC Broad Acute Respiratory DD v1 Data Count",
         "All Hospital Visits" = "CLI CC with CLI DD and Coronavirus DD v2 All Count")



## Age - 5-10

url_age510 <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?endDate=",enddate, "&geography=",facilities,"&percentParam=ccddCategory&patientClass=e&datasource=va_hosp&startDate=",startdate, "&agerange=5&medicalGroupingSystem=essencesyndromes&userId=3942&aqtTarget=TableBuilder&ccddCategory=cdc%20influenza%20dd%20v1&ccddCategory=ili%20ccdd%20neg%20coronavirus%20dd%20v1&ccddCategory=cli%20cc%20with%20cli%20dd%20and%20coronavirus%20dd%20v2&ccddCategory=cdc%20covid-specific%20dd%20v1&ccddCategory=cdc%20respiratory%20syncytial%20virus%20v1&ccddCategory=cdc%20respiratory%20syncytial%20virus%20dd%20v1&ccddCategory=cdc%20broad%20acute%20respiratory%20dd%20v1&agerangeMax=10&geographySystem=hospital&detector=nodetectordetector&dateconfig=15&timeResolution=weekly&agerangeOperator=btw&rowFields=timeResolution&columnField=ccddCategory")

age_510 <- myProfile$get_api_data(url_age510, fromCSV = TRUE) %>%
  dplyr::select(timeResolution,
         "ILI neg COVID (Counts)" = "ILI CCDD Neg Coronavirus DD v1 Data Count",
         "Diagnosed Flu (Counts)" = "CDC Influenza DD v1 Data Count",
         "New COVID Related (Counts)" = "CLI CC with CLI DD and Coronavirus DD v2 Data Count",
         "Diagnosed COVID (Counts)" = "CDC COVID-Specific DD v1 Data Count",
         "RSV Related (Counts)" = "CDC Respiratory Syncytial Virus v1 Data Count",
         "Diagnosed RSV (Counts)" = "CDC Respiratory Syncytial Virus DD v1 Data Count",
         "Broad Acute Resp (Counts)" = "CDC Broad Acute Respiratory DD v1 Data Count",
         "All Hospital Visits" = "CLI CC with CLI DD and Coronavirus DD v2 All Count")

age_510 <- add_column(age_510, AgeGroup = "05-10", .after = "timeResolution")

## Age - Special

ageILI_subset <- age_ili %>%
  dplyr::filter(AgeGroup == "00-04") %>%
  dplyr::select(timeResolution,
         AgeGroup,
         "ILI neg COVID (Counts)",
         "Diagnosed Flu (Counts)",
         "New COVID Related (Counts)",
         "Diagnosed COVID (Counts)",
         "RSV Related (Counts)",
         "Diagnosed RSV (Counts)",
         "Broad Acute Resp (Counts)",
         "All Hospital Visits")

ageNCHS_subset <- age_NCHS %>%
  dplyr::filter(AgeGroup != "00-10") %>%
  dplyr::select(timeResolution,
         AgeGroup,
         "ILI neg COVID (Counts)",
         "Diagnosed Flu (Counts)",
         "New COVID Related (Counts)",
         "Diagnosed COVID (Counts)",
         "RSV Related (Counts)",
         "Diagnosed RSV (Counts)",
         "Broad Acute Resp (Counts)",
         "All Hospital Visits")

age_special <- rbind(ageILI_subset, age_510, ageNCHS_subset) %>%
  arrange(timeResolution, AgeGroup)

# CLEAN DATA #
overall <- as.data.frame(overall) %>% 
  separate(col = timeResolution, into = c("Year", "Week"), sep = "-", convert = TRUE) 

county <- as.data.frame(county) %>% 
  separate(col = timeResolution, into = c("Year", "Week"), sep = "-", convert = TRUE) %>%
  mutate(geographyregion = gsub("CO_", "", geographyregion)) %>%
  dplyr::rename("County" = geographyregion)

age_special <- as.data.frame(age_special) %>% 
  separate(col = timeResolution, into = c("Year", "Week"), sep = "-", convert = TRUE) %>%
  dplyr::rename("Age Group" = AgeGroup)

age_ili <- as.data.frame(age_ili) %>% 
  separate(col = timeResolution, into = c("Year", "Week"), sep = "-", convert = TRUE) %>%
  dplyr::rename("ILI Age Group" = AgeGroup)


age_NCHS <- as.data.frame(age_NCHS) %>% 
  separate(col = timeResolution, into = c("Year", "Week"), sep = "-", convert = TRUE) %>%
  dplyr::rename("NCHS Age Group" = AgeGroup)

filename <- paste0(year, "_", date, "_WK", week, "_CLI V2 CDPHE.xlsx")
filepath <- paste0("Output/",filename)

x <- list(overall, county, age_special, age_ili, age_NCHS)

write_xlsx(setNames(list(overall,county,age_special,age_ili,age_NCHS), 
                    c("Overall", "County", "Special Age", "Age-ILI Rept", "Age-NCHS")), 
           path = filepath, 
           col_names = T)

rm(list = ls())
