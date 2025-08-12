# Colorado SyS Program Map ----

# Set Up ----
## Load libraries ----

#install.packages("mapdata")
install.packages("shadowtext")
install.packages("pak")

library(mapdata)
library(ggplot2)
library(ggmap)
library(sf)
library(tidytext)
library(stringr)
library(dplyr)
library(R.utils)
library(shadowtext)
library(readxl)
library(maps)
library(maptools)
library(rgeos)
library(rgdal)
library(raster)
library(cowplot)
library(tigris)
library(pak)
#pak::pak("walkerke/tigris@ftp-patch")
library(ggrepel)
library(tibble)

# Load map data ----

co_state <- states(cb = TRUE, class = "sf") %>% 
  filter(NAME == "Colorado")
co_state$geometry <- st_transform(co_state$geometry, "WGS84")

co_countylabel <- counties(state = "CO", cb = TRUE, class = "sf")
co_countylabel$geometry <- st_transform(co_countylabel$geometry, "WGS84")

co_labels_base <- co_countylabel %>% 
  filter(NAME %in% c("Adams", "Arapahoe", "Archuleta", "Boulder",
                     "Broomfield", "Denver", "Douglas", 
                     "El Paso", "Garfield", "Jefferson", "La Plata", 
                     "Larimer", "Mesa", "Moffat", "Montezuma", 
                     "Phillips", "Pueblo", "Weld", 
                     "Rio Blanco", "Sedgwick", "Yuma"))
co_labels_base <- add_column(co_labels_base, status = "none", .after = "geometry")

countyname <- "Arapahoe"

co_labels_highlight <- co_countylabel %>% 
  filter(NAME %in% c(countyname))

co_labels_highlight <- add_column(co_labels_highlight, status = "highlight", .after = "geometry")

co_labels <- rbind(co_labels_base, co_labels_highlight)
  
co_labels$geometry <- st_transform(co_labels$geometry, "WGS84")


## Projected map data ----

co_countylabel_2163 <- st_transform(co_countylabel, "EPSG:6427")

co_state_2163 <- st_transform(co_state, "EPSG:6427")

co_labels_2163 <- st_transform(co_labels, "EPSG:6427")



## Projected SyS coverage map ----

co_map_2163 <- co_labels_2163 %>%
  ggplot() +
  geom_sf(data = co_countylabel_2163, aes(geometry = geometry), fill = NA, colour = "gray", size = 1) +
  geom_sf(data = subset (co_labels, status == "none"), aes(geometry = geometry, fill = "navy"), colour = "black", size = 1) +
  geom_sf(data = subset(co_labels, status == "highlight"), aes(geometry = geometry, fill = "lightblue"), colour = "black", size = 1) +
  geom_sf(data = co_state_2163, aes(geometry = geometry), fill = NA , colour = "black", size = 2) +
  geom_text_repel(aes(geometry = geometry, after_stat(x), after_stat(y), label = NAME), 
                  stat = StatSfCoordinates,
                  size =  3.5, 
                  fontface = "bold", 
                  colour = "black", 
                  bg.colour = "white") +
  scale_fill_identity() +
  theme_void()
co_map_2163


## Projected SyS inset map (no labels) ----

co_map_inset <- co_labels_2163 %>%
  ggplot() +
  geom_sf(data = co_countylabel_2163, aes(geometry = geometry), fill = "white", colour = "gray", size = 1) +
  geom_sf(data = subset (co_labels, status == "none"), aes(geometry = geometry, fill = "navy"), colour = "black", size = 1) +
  geom_sf(data = subset(co_labels, status == "highlight"), aes(geometry = geometry, fill = "lightblue"), colour = "black", size = 1) +
  geom_sf(data = co_state_2163, aes(geometry = geometry), fill = NA , colour = "black", size = 2) +
  scale_fill_identity() +
  theme_void()
co_map_inset

ggsave(paste0("~/OD2A_RMD/OD2A_RMD_Maps/2025_COSySmap_",countyname,".png"))
