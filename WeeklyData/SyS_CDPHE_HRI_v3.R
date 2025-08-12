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

startdate <- "27Apr2025"
enddate <- "2Aug2025"
date <- "TEST"
week <- "31"
year <- "2025"

start <- format((as.Date(startdate, "%d%b%Y")), "%m/%d/%Y")
end <- format((as.Date(enddate, "%d%b%Y")), "%m/%d/%Y")

# Set counties

counties <- "co_adams&geography=co_arapahoe&geography=co_archuleta&geography=co_boulder&geography=co_broomfield&geography=co_denver&geography=co_douglas&geography=co_el%20paso&geography=co_jefferson&geography=co_la%20plata&geography=co_larimer&geography=co_mesa&geography=co_pueblo&geography=co_weld&geography=co_rio%20blanco&geography=co_montezuma&geography=co_sedgwick&geography=co_phillips&geography=co_yuma&geography=co_garfield"

##Weekly HRI x County, Sex

url_HRIxsex <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?geography=",counties,"&datasource=va_hosp&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=ccddCategory&aqtTarget=TableBuilder&ccddCategory=heat%20related%20illness%20v2&geographySystem=hospitalregion&detector=probewmapoissonswitch&timeResolution=weekly&rowFields=timeResolution&rowFields=geographyhospitalregion&rowFields=sex&columnField=patientClass")

HRIxsex <- myProfile$get_api_data(url_HRIxsex, fromCSV = TRUE) %>%
  dplyr::mutate(TotalVisits = rowSums(across(c("Emergency All Count","Inpatient All Count","Outpatient All Count")))) %>%
  dplyr::select(timeResolution,
                geographyhospitalregion,
         sex,
         "HRI_ED" = "Emergency Data Count",
         "TotalED" = "Emergency All Count",
         "HRI_IP" = "Inpatient Data Count",
         "TotalIP" = "Inpatient All Count",
         "HRI_OP" = "Outpatient Data Count",
         "TotalOP" = "Outpatient All Count",
         TotalVisits) 


##Weekly HRI x County, Age Group (Pediatric)

url_HRIxpedsage <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?geography=",counties,"&datasource=va_hosp&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=ccddCategory&aqtTarget=TableBuilder&ccddCategory=heat%20related%20illness%20v2&geographySystem=hospitalregion&detector=probewmapoissonswitch&timeResolution=weekly&rowFields=timeResolution&rowFields=geographyhospitalregion&rowFields=ageGroupPediatric&columnField=patientClass")

HRIxpedsage <- myProfile$get_api_data(url_HRIxpedsage, fromCSV = TRUE) %>%
  dplyr::mutate(TotalVisits = rowSums(across(c("Emergency All Count","Inpatient All Count","Outpatient All Count")))) %>%
  dplyr::select(timeResolution,
                geographyhospitalregion,
         ageGroup = ageGroupPediatric,
         "HRI_ED" = "Emergency Data Count",
         "TotalED" = "Emergency All Count",
         "HRI_IP" = "Inpatient Data Count",
         "TotalIP" = "Inpatient All Count",
         "HRI_OP" = "Outpatient Data Count",
         "TotalOP" = "Outpatient All Count",
         TotalVisits) 

##Weekly HRI x County, Age Group (NCHS)

url_HRIxNCHSage <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?geography=",counties,"&datasource=va_hosp&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=ccddCategory&aqtTarget=TableBuilder&ccddCategory=heat%20related%20illness%20v2&geographySystem=hospitalregion&detector=probewmapoissonswitch&timeResolution=weekly&rowFields=timeResolution&rowFields=geographyhospitalregion&rowFields=ageNCHS&columnField=patientClass")

HRIxNCHSage <- myProfile$get_api_data(url_HRIxNCHSage, fromCSV = TRUE) %>%
  dplyr::mutate(TotalVisits = rowSums(across(c("Emergency All Count","Inpatient All Count","Outpatient All Count")))) %>%
  dplyr::select(timeResolution,
                geographyhospitalregion,
         ageGroup = ageNCHS,
         "HRI_ED" = "Emergency Data Count",
         "TotalED" = "Emergency All Count",
         "HRI_IP" = "Inpatient Data Count",
         "TotalIP" = "Inpatient All Count",
         "HRI_OP" = "Outpatient Data Count",
         "TotalOP" = "Outpatient All Count",
         TotalVisits) 


##Weekly HRI x County, Race

url_HRIxrace <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?geography=",counties,"&datasource=va_hosp&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=ccddCategory&aqtTarget=TableBuilder&ccddCategory=heat%20related%20illness%20v2&geographySystem=hospitalregion&detector=probewmapoissonswitch&timeResolution=weekly&rowFields=timeResolution&rowFields=geographyhospitalregion&rowFields=crace&columnField=patientClass")


HRIxrace <- myProfile$get_api_data(url_HRIxrace, fromCSV = TRUE) %>%
  dplyr::mutate(TotalVisits = rowSums(across(c("Emergency All Count","Inpatient All Count","Outpatient All Count")))) %>%
  dplyr::select(timeResolution,
                geographyhospitalregion,
         crace,
         "HRI_ED" = "Emergency Data Count",
         "TotalED" = "Emergency All Count",
         "HRI_IP" = "Inpatient Data Count",
         "TotalIP" = "Inpatient All Count",
         "HRI_OP" = "Outpatient Data Count",
         "TotalOP" = "Outpatient All Count",
         TotalVisits) 

##Weekly HRI x County, Ethnicity

url_HRIxethnicity <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?geography=",counties,"&datasource=va_hosp&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=ccddCategory&aqtTarget=TableBuilder&ccddCategory=heat%20related%20illness%20v2&geographySystem=hospitalregion&detector=probewmapoissonswitch&timeResolution=weekly&rowFields=timeResolution&rowFields=geographyhospitalregion&rowFields=cethnicity&columnField=patientClass")


HRIxethnicity <- myProfile$get_api_data(url_HRIxethnicity, fromCSV = TRUE) %>%
  dplyr::mutate(TotalVisits = rowSums(across(c("Emergency All Count","Inpatient All Count","Outpatient All Count")))) %>%
  dplyr::select(timeResolution,
                geographyhospitalregion,
         cethnicity,
         "HRI_ED" = "Emergency Data Count",
         "TotalED" = "Emergency All Count",
         "HRI_IP" = "Inpatient Data Count",
         "TotalIP" = "Inpatient All Count",
         "HRI_OP" = "Outpatient Data Count",
         "TotalOP" = "Outpatient All Count",
         TotalVisits) 

# Data Details HRI pull

url_HRIdetails <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails/csv?geography=",counties,"&datasource=va_hosp&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=noPercent&aqtTarget=DataDetails&ccddCategory=heat%20related%20illness%20v2&geographySystem=hospitalregion&detector=nodetectordetector&timeResolution=weekly")

HRIdetails <- myProfile$get_api_data(url_HRIdetails, fromCSV = TRUE) 

peh_age <- HRIdetails %>%
  dplyr::mutate(peh_ct = case_when(grepl("Homeless", CCDDCategory_flat) ~ 1,
                  ZipCode == "00003" ~ 1,
                  TRUE ~ 0),
         agegroup = case_when(Age >= 0 & Age <= 4 ~ "00-04",
                              Age >= 5 & Age <= 9 ~ "05-09", 
                              Age >= 10 & Age <= 14 ~ "10-14",
                              Age >= 15 & Age <= 34 ~ "15-34",
                              Age >= 35 & Age <= 64 ~ "35-64",
                              Age >= 65 ~ "65+",
                              TRUE ~ "Unknown"),
         HospitalRegion = gsub("CO_", "", HospitalRegion),
         date = MMWRweek(as.Date(Date, "%m/%d/%Y")),
         weekStart = MMWRweek2Date(date$MMWRyear,date$MMWRweek),
         weekStart = format(as.Date(weekStart,"%Y-%m-%d"), "%m/%d/%Y")) %>%
  dplyr::select(weekStart,
         county = HospitalRegion,
         agegroup,
         peh_ct) %>%
  group_by(weekStart, county, agegroup) %>%
  summarise_if(is.numeric, sum)

peh_sex <- HRIdetails %>%
  dplyr::mutate(peh_ct = case_when(grepl("Homeless", CCDDCategory_flat) ~ 1,
                            ZipCode == "00003" ~ 1,
                            TRUE ~ 0),
         HospitalRegion = gsub("CO_", "", HospitalRegion),
         date = MMWRweek(as.Date(Date, "%m/%d/%Y")),
         weekStart = MMWRweek2Date(date$MMWRyear,date$MMWRweek),
         weekStart = format(as.Date(weekStart,"%Y-%m-%d"), "%m/%d/%Y")) %>%
  dplyr::mutate(Sex = recode(Sex,
                             "N" = "Unknown",
                             "U" = "Unknown",
                             "M" = "Male",
                             "F" = "Female")) %>%
  dplyr::select(weekStart,
         county = HospitalRegion,
         Sex,
         peh_ct) %>%
  group_by(weekStart, county, Sex) %>%
  summarise_if(is.numeric, sum)


peh_race <- HRIdetails %>%
  dplyr::mutate(peh_ct = case_when(grepl("Homeless", CCDDCategory_flat) ~ 1,
                            ZipCode == "00003" ~ 1,
                            TRUE ~ 0),
         HospitalRegion = gsub("CO_", "", HospitalRegion),
         date = MMWRweek(as.Date(Date, "%m/%d/%Y")),
         weekStart = MMWRweek2Date(date$MMWRyear,date$MMWRweek),
         weekStart = format(as.Date(weekStart,"%Y-%m-%d"), "%m/%d/%Y")) %>%
  dplyr::mutate(c_race = recode(c_race,
                              "Not Reported or Null" = "Unknown",
                              "Not Categorized" = "Unknown",
                              "Refused to answer" = "Unknown",
                              "Unknown" = "Unknown")) %>%
  dplyr::select(weekStart,
         county = HospitalRegion,
         c_race,
         peh_ct) %>%
  group_by(weekStart, county, c_race) %>%
  summarise_if(is.numeric, sum)

peh_ethn <- HRIdetails %>%
  dplyr::mutate(peh_ct = case_when(grepl("Homeless", CCDDCategory_flat) ~ 1,
                                   ZipCode == "00003" ~ 1,
                                   TRUE ~ 0),
                HospitalRegion = gsub("CO_", "", HospitalRegion),
                date = MMWRweek(as.Date(Date, "%m/%d/%Y")),
                weekStart = MMWRweek2Date(date$MMWRyear,date$MMWRweek),
                weekStart = format(as.Date(weekStart,"%Y-%m-%d"), "%m/%d/%Y")) %>%
  dplyr::mutate(c_ethnicity = recode(c_ethnicity,
                                   "Not Reported or Null" = "Unknown",
                                   "Not Categorized" = "Unknown",
                                   "Refused to answer" = "Unknown",
                                   "Unknown" = "Unknown")) %>%
  dplyr::select(weekStart,
         county = HospitalRegion,
         c_ethnicity,
         peh_ct) %>%
  group_by(weekStart, county, c_ethnicity) %>%
  summarise_if(is.numeric, sum)


##Daily High Ambient Temp x County

url_temp <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails/csv?percentParam=noPercent&endDate=",enddate,"&userId=3942&weatherFactor=maxtemp&datasource=va_weather_aggr&stationAggregateFunc=max&timeResolution=daily&aqtTarget=DataDetails&detector=probrepswitch&timeAggregateFunc=max&startDate=",startdate,"&stationID=bou-ako&stationID=pub-als&stationID=gjt-ase&stationID=bou-bdu&stationID=gld-itr&stationID=bou-apa&stationID=pub-cos&stationID=bou-ccu&stationID=gjt-cez&stationID=gjt-cag&stationID=bou-den&stationID=gjt-dro&stationID=bou-mnh&stationID=bou-fnl&stationID=gjt-gjt&stationID=bou-gxy&stationID=bou-20v&stationID=pub-lhx&stationID=pub-laa&stationID=pub-lxv&stationID=bou-lic&stationID=gjt-eeo&stationID=gjt-mtj&stationID=pub-pub&stationID=gjt-ril&stationID=pub-spd&stationID=pub-tad&stationID=bou-33v")


temp <- myProfile$get_api_data(url_temp, fromCSV = TRUE) %>%
  dplyr::select(Location,
         Date,
         MaxTemp,
         AvgTemp)

#station codes

stations <- read_csv("COweatherstations.csv")
temp2 <- merge(temp, stations, by = "Location") %>%
  dplyr::select(Date,
         Station,
         MaxTemp,
         AvgTemp) %>%
  arrange(Date)

## HRI Sex

sex <- HRIxsex %>%
  dplyr::mutate(sex = recode(sex,
                              "Not Reported" = "Unknown",
                              "Unknown" = "Unknown")) %>%
  dplyr::group_by(timeResolution, geographyhospitalregion, sex) %>%
  dplyr::summarize(across(everything(), ~sum(.x))) %>%
  dplyr::select(timeResolution,
                geographyhospitalregion,
         sex,
         "HRI_ED",
         "TotalED",
         "HRI_IP",
         "TotalIP",
         "HRI_OP",
         "TotalOP",
         TotalVisits)


## HRI Age Groups

agePeds_subset <- HRIxpedsage %>%
  dplyr::filter(ageGroup == "00-04"|
                ageGroup == "05-09"|
                ageGroup == "10-14") %>%
  dplyr::mutate(ageGroup = ifelse(ageGroup == "05-09"|ageGroup == "10-14", "05-14", "00-04")) %>%
  dplyr::group_by(timeResolution, geographyhospitalregion, ageGroup) %>%
  dplyr::summarize(across(everything(), ~sum(.x))) %>%
  dplyr::select(timeResolution,
                geographyhospitalregion,
         ageGroup,
         "HRI_ED",
         "TotalED",
         "HRI_IP",
         "TotalIP",
         "HRI_OP",
         "TotalOP",
         TotalVisits)

ageNCHS_subset <- HRIxNCHSage %>%
  dplyr::filter(ageGroup == "15-24"|
                ageGroup == "25-34"|
                ageGroup == "35-44"|
                ageGroup == "45-54"|
                ageGroup == "55-64"|
                ageGroup == "65-74"|
                ageGroup == "75-84"|
                ageGroup == "85+") %>%
  dplyr::mutate(ageGroup = 
                  ifelse(ageGroup == "15-24"|ageGroup == "25-34", "15-34",
                         ifelse(ageGroup=="35-44"|ageGroup=="45-54"|ageGroup=="55-64","35-64","65+"))) %>%
  dplyr::group_by(timeResolution,geographyhospitalregion,ageGroup) %>%
  dplyr::summarize(across(everything(), ~sum(.x))) %>%
  dplyr::select(timeResolution,
                geographyhospitalregion,
         ageGroup,
         "HRI_ED",
         "TotalED",
         "HRI_IP",
         "TotalIP",
         "HRI_OP",
         "TotalOP",
         TotalVisits)

HRIage <- rbind(agePeds_subset, ageNCHS_subset) %>%
  arrange(timeResolution, geographyhospitalregion, ageGroup) 

# HRI Race & Ethnicity

race <- HRIxrace %>%
  dplyr::mutate(crace = recode(crace,
                              "Not Reported or Null" = "Unknown",
                              "Not Categorized" = "Unknown",
                              "Refused to answer" = "Unknown",
                              "Unknown" = "Unknown")) %>%
  dplyr::group_by(timeResolution, geographyhospitalregion, crace) %>%
  dplyr::summarize(across(everything(), ~sum(.x))) %>%
  dplyr::select(timeResolution,
                geographyhospitalregion,
         race = crace,
         "HRI_ED",
         "TotalED",
         "HRI_IP",
         "TotalIP",
         "HRI_OP",
         "TotalOP",
         TotalVisits)


ethn <- HRIxethnicity %>%
  dplyr::mutate(cethnicity = recode(cethnicity,
                              "Not Reported" = "Unknown",
                              "Unknown" = "Unknown")) %>%
  dplyr::group_by(timeResolution, geographyhospitalregion, cethnicity) %>%
  dplyr::summarize(across(everything(), ~sum(.x))) %>%
  dplyr::select(timeResolution,
                geographyhospitalregion,
         ethnicity = cethnicity,
         "HRI_ED",
         "TotalED",
         "HRI_IP",
         "TotalIP",
         "HRI_OP",
         "TotalOP",
         TotalVisits)



# CLEAN DATA #
HRIsex <- as.data.frame(sex) %>% 
  separate(col = timeResolution, into = c("year", "MMWRweek"), sep = "-", convert = TRUE) %>%
  dplyr::mutate(weekStart = MMWRweek2Date(year, MMWRweek),
                geographyhospitalregion = gsub("CO_", "", geographyhospitalregion)) %>%
  dplyr::rename("county" = geographyhospitalregion) %>%
  dplyr::select(1,2,12,3,4,5,6,7,8,9,10,11) %>%
  dplyr::mutate(weekStart = format(as.Date(weekStart,"%Y-%m-%d"), "%m/%d/%Y"))

sex <- left_join(HRIsex, peh_sex, by = c("weekStart" = "weekStart", "county" = "county", "sex" = "Sex")) %>%
  dplyr::mutate(peh_ct = tidyr::replace_na(peh_ct, 0))

HRIagegroups <- as.data.frame(HRIage) %>% 
  separate(col = timeResolution, into = c("year", "MMWRweek"), sep = "-", convert = TRUE) %>%
  dplyr::mutate(weekStart = MMWRweek2Date(year, MMWRweek),
                geographyhospitalregion = gsub("CO_", "", geographyhospitalregion)) %>%
  dplyr::rename("county" = geographyhospitalregion) %>%
  dplyr::select(1,2,12,3,4,5,6,7,8,9,10,11) %>%
  dplyr::mutate(weekStart = format(as.Date(weekStart,"%Y-%m-%d"), "%m/%d/%Y"))

age <- left_join(HRIagegroups, peh_age, by = c("weekStart" = "weekStart", "county" = "county", "ageGroup" = "agegroup")) %>%
  mutate(peh_ct = tidyr::replace_na(peh_ct, 0))

HRIrace <- as.data.frame(race) %>% 
  separate(col = timeResolution, into = c("year", "MMWRweek"), sep = "-", convert = TRUE) %>%
  dplyr::mutate(weekStart = MMWRweek2Date(year, MMWRweek),
                geographyhospitalregion = gsub("CO_", "", geographyhospitalregion)) %>%
  dplyr::rename("county" = geographyhospitalregion) %>%
  dplyr::select(1,2,12,3,4,5,6,7,8,9,10,11) %>%
  dplyr::mutate(weekStart = format(as.Date(weekStart,"%Y-%m-%d"), "%m/%d/%Y"))

race <- left_join(HRIrace, peh_race, by = c("weekStart" = "weekStart", "county" = "county", "race" = "c_race")) %>%
  mutate(peh_ct = tidyr::replace_na(peh_ct, 0))


HRIethn <- as.data.frame(ethn) %>% 
  separate(col = timeResolution, into = c("year", "MMWRweek"), sep = "-", convert = TRUE) %>%
  dplyr::mutate(weekStart = MMWRweek2Date(year, MMWRweek),
                geographyhospitalregion = gsub("CO_", "", geographyhospitalregion)) %>%
  dplyr::rename("county" = geographyhospitalregion) %>%
  dplyr::select(1,2,12,3,4,5,6,7,8,9,10,11) %>%
  dplyr::mutate(weekStart = format(as.Date(weekStart,"%Y-%m-%d"), "%m/%d/%Y"))

ethn <- left_join(HRIethn, peh_ethn, by = c("weekStart" = "weekStart", "county" = "county", "ethnicity" = "c_ethnicity")) %>%
  mutate(peh_ct = tidyr::replace_na(peh_ct, 0))


Temp <- as.data.frame(temp2) 

rm(ageNCHS_subset, agePeds_subset, HRIagegroups, HRIxethnicity, HRIxNCHSage, HRIxpedsage, HRIxrace, HRIxsex, HRIage, HRIsex, HRIrace, HRIethn, HRIdetails, peh_age, peh_sex, peh_race, peh_ethn, stations, temp, temp2)

# Write to XLSX

filename <- paste0(year, "_", date, "_WK", week, "_HRIv3.xlsx")
filepath <- paste0("Output/",filename)
sheets <- list("HRI by sex" = sex, 
               "HRI by age" = age, 
               "HRI by race" = race, 
               "HRI by ethn" = ethn, 
               "Temps" = Temp)

writexl::write_xlsx(sheets, path = filepath, format_headers = FALSE)

rm(list = ls())