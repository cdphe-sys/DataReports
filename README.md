# DataReports
These are scripts used by the CDPHE syndromic surveillance epidemiologists for various data reports, tables, or dashboards. They are meant to serve as templates for other ESSENCE users in Colorado or out of state who are looking to replicate any of these reports/processes.

# About WeeklyData
WeeklyData are a set of files used to create various tables (XLSX) or reports (DOCX) for internal teams or public dashboards. 

There are individual scripts for various topics (COVID-like Illness, heat-related illness, behavioral health, and wastewater) that produce tables (XLSX). The HRI script utilizes a static file (COweatherstations) to add more contextual information to the weather data provided by ESSENCE.

There are scripts for various topics (invasive Group-A streptococcus, mycoplasma pneumonia, wastewater) which pull and save Rdata to then be utilized by the RMarkdown (RMD) reports (to create DOCX files). The RMDs utilize the word_style_v1 DOCX to set document styling. The script ending "-renderall" will knit all of the RMDs at once, eliminating the need to run each of them individually.

All files will be saved into the Output folder.

# About OD2A_RMD
This file includes all the information needed to create the OD2A county-specific dashboards. Jurisdictions take the folder and limit it to their own jurisdictions files/maps and render their dashboard.

To run these properly:
- Ensure that your map is updated (in the OD2A_RND_Maps folder)
- In the datapull file, limit to your county and adjust your dates. Run the file.
- Knit your RMD/use the "-renderall" file to render your jurisdiction's data
