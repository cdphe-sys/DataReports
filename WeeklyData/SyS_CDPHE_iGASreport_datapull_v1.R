##SyS CDPHE ##
### iGAS Report - Data Pull ###

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

startdate <- "31Dec2023"
enddate <- "2Aug2025"
date <- "TEST"
week <- "31"
year <- "2025"

# PERCENT

##Strep - Total Pop Percent

url_strep_total_pct <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?geography=co&datasource=va_hosp&dischargeDiagnosis=%5E;J02%5E&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=dischargeDiagnosis&aqtTarget=TimeSeries&geographySystem=hospitalstate&detector=probewmapoissonswitch&timeResolution=weekly&hasBeenE=1&graphOnly=true&numSeries=0&startMonth=1&nonZeroComposite=false")

#adjust column selection & renaming
strep_total_pct <- get_api_response(url_strep_total_pct) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  data.frame() %>%
  dplyr::select(date = timeSeriesData.date,
         percent = timeSeriesData.count,
         pvalue = timeSeriesData.levels,
         alert = timeSeriesData.color) %>%
  mutate(date = as.Date(date)) %>%
  add_column(category = "Streptococcal Pharyngitis", .after = "date") %>%
  add_column(agegroup = "total", .after = "category")

##iGAS - Total Pop Percent

url_igas_total_pct <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?geography=co&datasource=va_hosp&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=dischargeDiagnosis&aqtTarget=TimeSeries&geographySystem=hospitalstate&detector=probewmapoissonswitch&timeResolution=weekly&hasBeenE=1&dischargeDiagnosis=%5E;A400%5E,or,%5E;A40.0%5E,or,(,%5E;B950%5E,or,%5E;B95.0%5E,AND,(,%5E;A483%5E,or,%5E;A48.3%5E,or,%5E;G002%5E,or,%5E;G00.2%5E,or,%5E;G060%5E,or,%5E;G06.0%5E,or,%5E;J154%5E,or,%5E;J15.4%5E,or,%5E;J17%5E,or,%5E;J3%5B69%5D%5E,or,%5E;J851%5E,or,%5E;J85.1%5E,or,%5E;J86%5B09%5D%5E,or,%5E;M726%5E,or,%5E;M72.6%5E,or,%5E;R652%5E,or,%5E;R65.2%5E,or,%5E;R7881%5E,or,%5E;R78.81%5E,),)")

#adjust column selection & renaming
igas_total_pct <- get_api_response(url_igas_total_pct) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  data.frame() %>%
  dplyr::select(date = timeSeriesData.date,
         percent = timeSeriesData.count,
         pvalue = timeSeriesData.levels,
         alert = timeSeriesData.color) %>%
  mutate(date = as.Date(date)) %>%
  add_column(category = "Invasive Group A Streptococcus", .after = "date") %>%
  add_column(agegroup = "total", .after = "category")

## Scarlet Fever - Total Pop Percent

url_scarlet_total_pct <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?geography=co&datasource=va_hosp&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=dischargeDiagnosis&aqtTarget=TimeSeries&geographySystem=hospitalstate&detector=probewmapoissonswitch&timeResolution=weekly&hasBeenE=1&dischargeDiagnosis=%5E;A38%5E")

#adjust column selection & renaming
scarlet_total_pct <- get_api_response(url_scarlet_total_pct) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  data.frame() %>%
  dplyr::select(date = timeSeriesData.date,
         percent = timeSeriesData.count,
         pvalue = timeSeriesData.levels,
         alert = timeSeriesData.color) %>%
  mutate(date = as.Date(date)) %>%
  add_column(category = "Scarlet Fever", .after = "date") %>%
  add_column(agegroup = "total", .after = "category")

##Strep - Pediatric Percent

url_strep_ped_pct <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?geography=co&datasource=va_hosp&startDate=",startdate,"&agerange=0&medicalGroupingSystem=essencesyndromes&userId=3942&agerangeMax=17&endDate=",enddate,"&percentParam=dischargeDiagnosis&aqtTarget=TimeSeries&geographySystem=hospitalstate&detector=probewmapoissonswitch&timeResolution=weekly&hasBeenE=1&dischargeDiagnosis=%5E;J02%5E&agerangeOperator=btw")

#adjust column selection & renaming
strep_ped_pct <- get_api_response(url_strep_ped_pct) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  data.frame() %>%
  dplyr::select(date = timeSeriesData.date,
         percent = timeSeriesData.count,
         pvalue = timeSeriesData.levels,
         alert = timeSeriesData.color) %>%
  mutate(date = as.Date(date)) %>%
  add_column(category = "Streptococcal Pharyngitis", .after = "date") %>%
  add_column(agegroup = "pediatric", .after = "category")

##iGAS - Pediatric Percent

url_igas_ped_pct <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?geography=co&datasource=va_hosp&startDate=",startdate,"&agerange=0&medicalGroupingSystem=essencesyndromes&userId=3942&agerangeMax=17&endDate=",enddate,"&percentParam=dischargeDiagnosis&aqtTarget=TimeSeries&geographySystem=hospitalstate&detector=probewmapoissonswitch&timeResolution=weekly&hasBeenE=1&dischargeDiagnosis=%5E;A400%5E,or,%5E;A40.0%5E,or,(,%5E;B950%5E,or,%5E;B95.0%5E,AND,(,%5E;A483%5E,or,%5E;A48.3%5E,or,%5E;G002%5E,or,%5E;G00.2%5E,or,%5E;G060%5E,or,%5E;G06.0%5E,or,%5E;J154%5E,or,%5E;J15.4%5E,or,%5E;J17%5E,or,%5E;J3%5B69%5D%5E,or,%5E;J851%5E,or,%5E;J85.1%5E,or,%5E;J86%5B09%5D%5E,or,%5E;M726%5E,or,%5E;M72.6%5E,or,%5E;R652%5E,or,%5E;R65.2%5E,or,%5E;R7881%5E,or,%5E;R78.81%5E,),)&agerangeOperator=btw")

#adjust column selection & renaming
igas_ped_pct <- get_api_response(url_igas_ped_pct) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  data.frame() %>%
  dplyr::select(date = timeSeriesData.date,
         percent = timeSeriesData.count,
         pvalue = timeSeriesData.levels,
         alert = timeSeriesData.color) %>%
  mutate(date = as.Date(date)) %>%
  add_column(category = "Invasive Group A Streptococcus", .after = "date") %>%
  add_column(agegroup = "pediatric", .after = "category")

## Scarlet Fever - Pediatric Percent

url_scarlet_ped_pct <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?geography=co&datasource=va_hosp&startDate=",startdate,"&agerange=0&medicalGroupingSystem=essencesyndromes&userId=3942&agerangeMax=17&endDate=",enddate,"&percentParam=dischargeDiagnosis&aqtTarget=TimeSeries&geographySystem=hospitalstate&detector=probewmapoissonswitch&timeResolution=weekly&hasBeenE=1&dischargeDiagnosis=%5E;A400%5E,or,%5E;A40.0%5E,or,(,%5E;B950%5E,or,%5E;B95.0%5E,AND,(,%5E;A483%5E,or,%5E;A48.3%5E,or,%5E;G002%5E,or,%5E;G00.2%5E,or,%5E;G060%5E,or,%5E;G06.0%5E,or,%5E;J154%5E,or,%5E;J15.4%5E,or,%5E;J17%5E,or,%5E;J3%5B69%5D%5E,or,%5E;J851%5E,or,%5E;J85.1%5E,or,%5E;J86%5B09%5D%5E,or,%5E;M726%5E,or,%5E;M72.6%5E,or,%5E;R652%5E,or,%5E;R65.2%5E,or,%5E;R7881%5E,or,%5E;R78.81%5E,),)&agerangeOperator=btw")

#adjust column selection & renaming
scarlet_ped_pct <- get_api_response(url_scarlet_ped_pct) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  data.frame() %>%
  dplyr::select(date = timeSeriesData.date,
         percent = timeSeriesData.count,
         pvalue = timeSeriesData.levels,
         alert = timeSeriesData.color) %>%
  mutate(date = as.Date(date)) %>%
  add_column(category = "Scarlet Fever", .after = "date") %>%
  add_column(agegroup = "pediatric", .after = "category")

##bind rows

igasreport_percent <- rbind(strep_total_pct, strep_ped_pct, igas_total_pct, igas_ped_pct, scarlet_total_pct, scarlet_ped_pct) %>%
  arrange(category, agegroup, date)


# COUNTS

##Strep - Total Pop Count

url_strep_total_ct <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?geography=co&datasource=va_hosp&dischargeDiagnosis=%5E;J02%5E&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=noPercent&aqtTarget=TimeSeries&geographySystem=hospitalstate&detector=probewmapoissonswitch&timeResolution=weekly&hasBeenE=1&graphOnly=true&numSeries=0&startMonth=1&nonZeroComposite=false")

#adjust column selection & renaming
strep_total_ct <- get_api_response(url_strep_total_ct) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  data.frame() %>%
  dplyr::select(date = timeSeriesData.date,
         count = timeSeriesData.count,
         pvalue = timeSeriesData.levels,
         alert = timeSeriesData.color) %>%
  mutate(date = as.Date(date)) %>%
  add_column(category = "Streptococcal Pharyngitis", .after = "date") %>%
  add_column(agegroup = "total", .after = "category")

##iGAS - Total Pop Count

url_igas_total_ct <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?geography=co&datasource=va_hosp&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=noPercent&aqtTarget=TimeSeries&geographySystem=hospitalstate&detector=probewmapoissonswitch&timeResolution=weekly&hasBeenE=1&dischargeDiagnosis=%5E;A400%5E,or,%5E;A40.0%5E,or,(,%5E;B950%5E,or,%5E;B95.0%5E,AND,(,%5E;A483%5E,or,%5E;A48.3%5E,or,%5E;G002%5E,or,%5E;G00.2%5E,or,%5E;G060%5E,or,%5E;G06.0%5E,or,%5E;J154%5E,or,%5E;J15.4%5E,or,%5E;J17%5E,or,%5E;J3%5B69%5D%5E,or,%5E;J851%5E,or,%5E;J85.1%5E,or,%5E;J86%5B09%5D%5E,or,%5E;M726%5E,or,%5E;M72.6%5E,or,%5E;R652%5E,or,%5E;R65.2%5E,or,%5E;R7881%5E,or,%5E;R78.81%5E,),)")

#adjust column selection & renaming
igas_total_ct <- get_api_response(url_igas_total_ct) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  data.frame() %>%
  dplyr::select(date = timeSeriesData.date,
         count = timeSeriesData.count,
         pvalue = timeSeriesData.levels,
         alert = timeSeriesData.color) %>%
  mutate(date = as.Date(date)) %>%
  add_column(category = "Invasive Group A Streptococcus", .after = "date") %>%
  add_column(agegroup = "total", .after = "category")

## Scarlet Fever - Total Pop Percent

url_scarlet_total_ct <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?geography=co&datasource=va_hosp&startDate=",startdate,"&medicalGroupingSystem=essencesyndromes&userId=3942&endDate=",enddate,"&percentParam=noPercent&aqtTarget=TimeSeries&geographySystem=hospitalstate&detector=probewmapoissonswitch&timeResolution=weekly&hasBeenE=1&dischargeDiagnosis=%5E;A38%5E")

#adjust column selection & renaming
scarlet_total_ct <- get_api_response(url_scarlet_total_ct) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  data.frame() %>%
  dplyr::select(date = timeSeriesData.date,
         count = timeSeriesData.count,
         pvalue = timeSeriesData.levels,
         alert = timeSeriesData.color) %>%
  mutate(date = as.Date(date)) %>%
  add_column(category = "Scarlet Fever", .after = "date") %>%
  add_column(agegroup = "total", .after = "category")

##Strep - Pediatric Percent

url_strep_ped_ct <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?geography=co&datasource=va_hosp&startDate=",startdate,"&agerange=0&medicalGroupingSystem=essencesyndromes&userId=3942&agerangeMax=17&endDate=",enddate,"&percentParam=noPercent&aqtTarget=TimeSeries&geographySystem=hospitalstate&detector=probewmapoissonswitch&timeResolution=weekly&hasBeenE=1&dischargeDiagnosis=%5E;J02%5E&agerangeOperator=btw")

#adjust column selection & renaming
strep_ped_ct <- get_api_response(url_strep_ped_ct) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  data.frame() %>%
  dplyr::select(date = timeSeriesData.date,
         count = timeSeriesData.count,
         pvalue = timeSeriesData.levels,
         alert = timeSeriesData.color) %>%
  mutate(date = as.Date(date)) %>%
  add_column(category = "Streptococcal Pharyngitis", .after = "date") %>%
  add_column(agegroup = "pediatric", .after = "category")

##iGAS - Pediatric Percent

url_igas_ped_ct <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?geography=co&datasource=va_hosp&startDate=",startdate,"&agerange=0&medicalGroupingSystem=essencesyndromes&userId=3942&agerangeMax=17&endDate=",enddate,"&percentParam=noPercent&aqtTarget=TimeSeries&geographySystem=hospitalstate&detector=probewmapoissonswitch&timeResolution=weekly&hasBeenE=1&dischargeDiagnosis=%5E;A400%5E,or,%5E;A40.0%5E,or,(,%5E;B950%5E,or,%5E;B95.0%5E,AND,(,%5E;A483%5E,or,%5E;A48.3%5E,or,%5E;G002%5E,or,%5E;G00.2%5E,or,%5E;G060%5E,or,%5E;G06.0%5E,or,%5E;J154%5E,or,%5E;J15.4%5E,or,%5E;J17%5E,or,%5E;J3%5B69%5D%5E,or,%5E;J851%5E,or,%5E;J85.1%5E,or,%5E;J86%5B09%5D%5E,or,%5E;M726%5E,or,%5E;M72.6%5E,or,%5E;R652%5E,or,%5E;R65.2%5E,or,%5E;R7881%5E,or,%5E;R78.81%5E,),)&agerangeOperator=btw")

#adjust column selection & renaming
igas_ped_ct <- get_api_response(url_igas_ped_ct) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  data.frame() %>%
  dplyr::select(date = timeSeriesData.date,
         count = timeSeriesData.count,
         pvalue = timeSeriesData.levels,
         alert = timeSeriesData.color) %>%
  mutate(date = as.Date(date)) %>%
  add_column(category = "Invasive Group A Streptococcus", .after = "date") %>%
  add_column(agegroup = "pediatric", .after = "category")

## Scarlet Fever - Pediatric Percent

url_scarlet_ped_ct <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?geography=co&datasource=va_hosp&startDate=",startdate,"&agerange=0&medicalGroupingSystem=essencesyndromes&userId=3942&agerangeMax=17&endDate=",enddate,"&percentParam=noPercent&aqtTarget=TimeSeries&geographySystem=hospitalstate&detector=probewmapoissonswitch&timeResolution=weekly&hasBeenE=1&dischargeDiagnosis=%5E;A400%5E,or,%5E;A40.0%5E,or,(,%5E;B950%5E,or,%5E;B95.0%5E,AND,(,%5E;A483%5E,or,%5E;A48.3%5E,or,%5E;G002%5E,or,%5E;G00.2%5E,or,%5E;G060%5E,or,%5E;G06.0%5E,or,%5E;J154%5E,or,%5E;J15.4%5E,or,%5E;J17%5E,or,%5E;J3%5B69%5D%5E,or,%5E;J851%5E,or,%5E;J85.1%5E,or,%5E;J86%5B09%5D%5E,or,%5E;M726%5E,or,%5E;M72.6%5E,or,%5E;R652%5E,or,%5E;R65.2%5E,or,%5E;R7881%5E,or,%5E;R78.81%5E,),)&agerangeOperator=btw")

#adjust column selection & renaming
scarlet_ped_ct <- get_api_response(url_scarlet_ped_ct) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  data.frame() %>%
  dplyr::select(date = timeSeriesData.date,
         count = timeSeriesData.count,
         pvalue = timeSeriesData.levels,
         alert = timeSeriesData.color) %>%
  mutate(date = as.Date(date)) %>%
  add_column(category = "Scarlet Fever", .after = "date") %>%
  add_column(agegroup = "pediatric", .after = "category")

##bind rows

igasreport_counts <- rbind(strep_total_ct, strep_ped_ct, igas_total_ct, igas_ped_ct, scarlet_total_ct, scarlet_ped_ct) %>%
  arrange(category, agegroup, date)


# CLEAN DATA #

save(igasreport_percent, file = "RMD_igasreport_percent_data.Rdata")
save(igasreport_counts, file = "RMD_igasreport_counts_data.Rdata")

rm(list = ls())