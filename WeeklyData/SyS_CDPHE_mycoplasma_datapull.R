# CDPHE SyS #
# Mycoplasma Data Pull -----

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

setwd("~/WeeklyData")

## Set NSSP user profile ----
myProfile <- readRDS("myProfile.rds")

# API DATA #
#Leave start date as 12/29/2019, adjust end date to reflect most recent complete week's end date (Saturday)

startdate <- "1Jun2023"
enddate <- "2Aug2025"
date <- "TEST"
week <- "31"
year <- "2025"

start <- format((as.Date(startdate, "%d%b%Y")), "%m/%d/%Y")
end <- format((as.Date(enddate, "%d%b%Y")), "%m/%d/%Y")

# Pneumonia Data Details - Denominator
url <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails/csv?geography=co&datasource=va_hosp&DDParsedFreeText=%5EJ09.X1%5E,OR,%5EJ10.0%5B0-1%5D%5E,OR,%5EJ10.08%5E,OR,%5EJ11.00%5E,OR,%5EJ11.08%5E,OR,%5EJ12.%5B0-3%5D%5E,OR,%5EJ12.%5B8-9%5D%5E,OR,%5EJ13%5E,OR,%5EJ14%5E,OR,%5EJ15.%5B0-9%5D%5E%20,OR,%5EJ16.0%5E,OR,%5EJ16.8%5E,OR,%5EJ17.8%5E,OR,%5EJ18.%5B0-2%5D%5E,OR,%5EJ18.%5B8-9%5D%5E,OR,%5EJ80%5E,OR,%5EA48.1%5E,OR,%5EJ20.0%5E,OR,%5EJ09X1%5E,OR,%5EJ100%5B0-1%5D%5E,OR,%5EJ1008%5E,OR,%5EJ1100%5E,OR,%5EJ1108%5E,OR,%5EJ12%5B0-3%5D%5E,OR,%5EJ12%5B8-9%5D%5E,OR,%5EJ15%5B0-9%5D%5E,OR,%5EJ160%5E,OR,%5EJ168%5E,OR,%5EJ178%5E,OR,%5EJ18%5B0-2%5D%5E,OR,%5EJ18%5B8-9%5D%5E,OR,%5EA481%5E,OR,%5EJ200%5E&startDate=",startdate,"&agerange=0&medicalGroupingSystem=essencesyndromes&userId=3942&agerangeMax=17&endDate=",enddate,"&percentParam=noPercent&aqtTarget=DataDetails&geographySystem=hospitalstate&detector=nodetectordetector&timeResolution=weekly&hasBeenE=1&agerangeOperator=btw")

#adjust column selection & renaming
datadetails_pneu <- get_api_data(url, fromCSV = TRUE) %>%
  mutate(date = as.Date(Date, "%m/%d/%Y"),
         pneu_ct = 1,
         agegroup = case_when(Age >= 0 & Age <= 4 ~ "00-04",
                             Age >= 5 & Age <= 9 ~ "05-09", 
                             Age >= 10 & Age <= 14 ~ "10-14",
                             Age >= 15 & Age <= 17 ~ "15-17")) %>%
  dplyr::select(date,
         pneu_ct,
         agegroup)

pneu_ct <- datadetails_pneu %>%
  group_by(date, agegroup) %>%
  summarise_if(is.numeric, sum)


# Mycoplasma Data Details - Numerator
url <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails/csv?geography=co&datasource=va_hosp&DDParsedFreeText=%5EJ20.0%5E,OR,%5EJ200%5E,OR,%5EJ15.7%5E,OR,%5EJ157%5E&startDate=",startdate,"&agerange=0&medicalGroupingSystem=essencesyndromes&userId=3942&agerangeMax=17&endDate=",enddate,"&percentParam=noPercent&aqtTarget=DataDetails&geographySystem=hospitalstate&detector=nodetectordetector&timeResolution=weekly&hasBeenE=1&agerangeOperator=btw")

#adjust column selection & renaming
datadetails_myco <- get_api_data(url, fromCSV = TRUE) %>%
  mutate(date = as.Date(Date, "%m/%d/%Y"),
         myco_ct = 1,
         agegroup = case_when(Age >= 0 & Age <= 4 ~ "00-04",
                              Age >= 5 & Age <= 9 ~ "05-09", 
                              Age >= 10 & Age <= 14 ~ "10-14",
                              Age >= 15 & Age <= 17 ~ "15-17")) %>%
  dplyr::select(date,
         myco_ct,
         agegroup)

myco_ct <- datadetails_myco %>%
  group_by(date, agegroup) %>%
  summarise_if(is.numeric, sum)


# full join date<-pneu<-myco

ts <- seq(as.Date(start, "%m/%d/%Y"), as.Date(end, "%m/%d/%Y"), "days")
dates <- data.frame(date = as.Date(ts))

dates <- dates %>%
  mutate(MMWR = data.frame(MMWRweek(date))) %>%
  unnest(cols = c(MMWR)) %>%
  dplyr::select(date,
         MMWRyear,
         MMWRweek) %>%
  mutate(WeekStart = MMWRweek2Date(MMWRyear, MMWRweek))

df_ct <- merge(x = dates, y = pneu_ct, by = "date", all = TRUE)
df_ct <- merge(x = df_ct, y = myco_ct, by = c("date","agegroup"), all = TRUE)
df_ct <- df_ct %>%
  mutate_all(~replace(., is.na(.), 0))

df_ct <- df_ct %>%
  group_by(WeekStart, agegroup) %>%
  summarise_if(is.numeric, sum) %>%
  dplyr::select(-MMWRyear,
         -MMWRweek)

# Add overall age group 00-17
df_totals <- df_ct %>%
  group_by(WeekStart) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(agegroup = "total")

df_ct <- rbind(df_ct, df_totals) %>%
  arrange(WeekStart, agegroup)


# National Pediatric Mycoplasma Pneumonia 00-17 ------------------------------------------------

url_national <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=",enddate,"&geography=region%20i&geography=region%20ii&geography=region%20iii&geography=region%20iv&geography=region%20v&geography=region%20vi&geography=region%20vii&geography=region%20viii&geography=region%20ix&geography=region%20x&percentParam=DDParsedFreeText&datasource=va_hospdreg&DDParsedFreeText=%5EJ20.0%5E,OR,%5EJ200%5E,OR,%5EJ15.7%5E,OR,%5EJ157%5E&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=5874&aqtTarget=TimeSeries&geographySystem=hospitaldhhsregion&detector=nodetectordetector&timeResolution=weekly&hasBeenE=1&age=00-04&age=05-17")

national <- get_api_response(url_national) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  data.frame() %>%
  dplyr::select(date = timeSeriesData.date,
         percent = timeSeriesData.count)%>%
  mutate(date = as.Date(date)) %>%
  add_column(geo = "National")

url_colorado <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?geography=co&datasource=va_hosp&DDParsedFreeText=%5EJ20.0%5E,OR,%5EJ200%5E,OR,%5EJ15.7%5E,OR,%5EJ157%5E&startDate=",startdate,"&agerange=0&medicalGroupingSystem=essencesyndromes&userId=5874&agerangeMax=17&endDate=",enddate,"&percentParam=DDParsedFreeText&aqtTarget=TimeSeries&geographySystem=hospitalstate&detector=nodetectordetector&timeResolution=weekly&hasBeenE=1&agerangeOperator=btw")

colorado <- get_api_response(url_colorado) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  data.frame() %>%
  dplyr::select(date = timeSeriesData.date,
         percent = timeSeriesData.count)%>%
  mutate(date = as.Date(date)) %>%
  add_column(geo = "Colorado")

## Bind Rows -----

overall_mycoplasma <- rbind(national, colorado)


# calculate percent

df_final <- df_ct %>%
  mutate(myco_pct = (myco_ct/pneu_ct)*100)

save(df_final, file = "RMD_mycoplasmareport_data.Rdata")
save(overall_mycoplasma, file = "RMD_mycoplasmareport_national.Rdata")

rm(list = ls())