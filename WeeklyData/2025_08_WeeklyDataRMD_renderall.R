# OD2A Render Dashboards

setwd("~/WeeklyData")

# EVERY WEEK 

# Name files & paths ----
iGASfile <- paste0(Sys.Date(),"_TESTSySReport_iGAS_v1.docx")
iGASfilepath <- paste0("Output/", iGASfile)

mycofile <- paste0(Sys.Date(),"_TESTSySReport_mycoplasma_v3.docx")
mycofilepath <- paste0("Output/", mycofile)

# Render RMDs ----
rmarkdown::render('202410_SySReport_iGAS_v1.rmd',
                  output_file = iGASfilepath)

rmarkdown::render('202412_SySReport_mycoplasma_v3.rmd',
                  output_file = mycofilepath)


# EVERY OTHER WEEK

# Name files & paths ----

wwfile <- paste0(Sys.Date(),"_TESTSySReport_WWpedAFM_v1.docx")
wwfilepath <- paste0("Output/", wwfile)

# Render RMDs ----

rmarkdown::render('202409_SySreport_WWpedAFM_v1.rmd',
                  output_file = wwfilepath)

