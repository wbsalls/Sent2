library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(USAboundaries)
library(tidyverse)
library(sen2r)

setwd("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/S2")

# S2 grid
s2grid <- read_sf("geosp/S2A_OPER_GIP_TILPAR_MPC__20151209T095117_V20150622T000000_21000101T000000_B00.kml")

# ** try assigning CRS by ESPG (for lat lon?); look in training for instructions


# get US boundary

#us <- us_boundaries(type = "state", resolution = "high")

conus <- ne_states(country = 'United States of America') %>%
  st_as_sf(states) %>%
  dplyr::filter(!name %in% c('Hawaii','Alaska') & !is.na(name))

#%>% ggplot + geom_sf()

plot(st_geometry(conus))


st_crs(conus)


pr <- st_read("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/s2/PriestRapids/shp/PriestRapids_extent.shp")


# query all S2 images in US
sen2r()

all <- s2_list(spatial_extent = pr,
               time_interval = c(as.Date("2021-11-01"), Sys.Date()),
               level = "L1C",
               server = c("scihub")) # , "gcloud"

alldf <- as.data.frame(all)

attr(all, "sensing_datetime")

safe_attrs <- c("mission", "level", "id_tile", "id_orbit", "sensing_datetime", "ingestion_datetime", "clouds", "footprint", "uuid", "online")

write.table(names(all)[1:3],
            file = "C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/s2test.txt",
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE)

