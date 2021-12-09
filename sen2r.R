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
us <- us_boundaries(type = "state", resolution = "high")


states <- 
states_sf <- st_as_sf(states)
conus <- ne_states(country = 'United States of America') %>%
  st_as_sf(states) %>%
  dplyr::filter(!name %in% c('Hawaii','Alaska') & !is.na(name))

#%>% ggplot + geom_sf()

plot(st_geometry(conus))


st_crs(states)





# query all S2 images in US
sen2r()

all <- s2_list()