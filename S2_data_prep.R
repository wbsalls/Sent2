
library(sp)
library(rgdal)
library(rgeos)
library(rnaturalearth)
library(scales)
library(plotrix)
library(raster)

source("C:/Users/WSALLS/Git/Sent2/algorithms.R")
source("C:/Users/WSALLS/Git/Sent2/error_metrics_220120.R")

opar <- par()

setwd("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/S2")

l2gen <- read.csv("fromNima/Pahlevan_l2gen.csv", stringsAsFactors = FALSE)
acolite <- read.csv("fromNima/Pahlevan_acolite.csv", stringsAsFactors = FALSE)

l2gen$uniqueIDer <- paste(l2gen$Scene.ID, l2gen$In.Situ.datetime, l2gen$In.Situ.chl, sep = "; ")
acolite$uniqueIDer <- paste(acolite$Scene.ID, acolite$In.Situ.datetime, acolite$In.Situ.chl, sep = "; ")

merged_mus <- merge(l2gen, acolite, by = "uniqueIDer")

colnames(merged_mus)

plot(merged_mus$rhot.705..x, merged_mus$rhot.705..y)
points(merged_mus$rhos.705..x, merged_mus$rhos.705..y, pch = 20, col = "blue")

plot(merged_mus$rhos.705..x, merged_mus$rhos.705..y)

plot(merged_mus$Rrs.705..x, merged_mus$Rrs.705..y)
abline(0, 1, col = "red")


## pick data source (change both!)
chldata <- l2gen # l2gen acolite
chldata_source <- "l2gen"

## chlorophyll

# calculate MCI
chldata$MCI_rhot <- calc_mci(R1 = chldata$rhot.665., R2 = chldata$rhot.705., R3 = chldata$rhot.740.)
chldata$NDCI_rhot <- calc_ndci(R1 = chldata$rhot.665., R2 = chldata$rhot.705.)
#chldata$DCI_rhot <- calc_dci(R1 = chldata$rhot.665., R2 = chldata$rhot.705., R3 = chldata$rhot.740.)

chldata$MCI_rhos <- calc_mci(R1 = chldata$rhos.665., R2 = chldata$rhos.705., R3 = chldata$rhos.740.)
chldata$NDCI_rhos <- calc_ndci(R1 = chldata$rhos.665., R2 = chldata$rhos.705.)
#chldata$DCI_rhos <- calc_dci(R1 = chldata$rhos.665., R2 = chldata$rhos.705., R3 = chldata$rhos.740.)

chldata$MCI_Rrs <- calc_mci(R1 = chldata$Rrs.665., R2 = chldata$Rrs.705., R3 = chldata$Rrs.740.)
chldata$NDCI_Rrs <- calc_ndci(R1 = chldata$Rrs.665., R2 = chldata$Rrs.705.)
#chldata$DCI_Rrs <- calc_dci(R1 = chldata$Rrs.665., R2 = chldata$Rrs.705., R3 = chldata$Rrs.740.)


## <<<<<

## cross-check

colnames(acolite)
colnames(l2gen)

sort(colnames(acolite)[!(colnames(acolite) %in% colnames(l2gen))])
sort(colnames(l2gen)[!(colnames(l2gen) %in% colnames(acolite))])
sort(colnames(l2gen)[!(colnames(l2gen) %in% colnames(acolite))])[18:34]


##
# add chl (manually) - rhos MCI, sed not removed
#chldata$chla_rhos <- (chldata$MCI_rhos + 0.00069) / 0.00017 # coefficients from rhos_noSed calibration

# error
#chldata$chla_err_add <- chldata$chla_rhos - chldata$In.Situ.chl
#chldata$chla_err_mult <- chldata$chla_rhos / chldata$In.Situ.chl


#hist(chldata$chla_err_add)
#hist(chldata$chla_err_mult)


## >>>>>>


## timezone correction --------------------------------------------------

library(lubridate)


## correct time and time diff of select data sources

# make list of data sources in each timezone (others are assumed UTC)
ETsources <- c("Boston_Harbor", "CBP", "EOTB", "Lake_Erie08", "St_Johns")
CTsources <- c("Wisconsin")
PTsources <- c(c("sfbay", "sfbay2"))

# new column for native timezone
chldata$In.Situ.datetime_nativeTZ <- NA

# specify native timezone
chldata$In.Situ.datetime_nativeTZ[chldata$Database %in% ETsources] <- "ET"
chldata$In.Situ.datetime_nativeTZ[chldata$Database %in% CTsources] <- "CT"
chldata$In.Situ.datetime_nativeTZ[chldata$Database %in% PTsources] <- "PT"
chldata$In.Situ.datetime_nativeTZ[!(chldata$Database %in% 
                                     c(ETsources, CTsources, PTsources))] <- "UTC"

# new column for standardized UTC in situ times (initiating this way types column as POSIXct)
chldata$In.Situ.datetime_UTC <- as.POSIXct("2000-01-01 00:00:00", tz = "UTC")

# specify posix times for data sources in each timezone
# (and UTC if not in one of those data sources)
chldata$In.Situ.datetime_UTC[chldata$Database %in% ETsources] <- 
  as.POSIXct(chldata$In.Situ.datetime[chldata$Database %in% ETsources], 
             tz = "America/New_York")
chldata$In.Situ.datetime_UTC[chldata$Database %in% CTsources] <- 
  as.POSIXct(chldata$In.Situ.datetime[chldata$Database %in% CTsources], 
             tz = "America/Chicago")
chldata$In.Situ.datetime_UTC[chldata$Database %in% PTsources] <- 
  as.POSIXct(chldata$In.Situ.datetime[chldata$Database %in% PTsources], 
             tz = "America/Los_Angeles")
chldata$In.Situ.datetime_UTC[!(chldata$Database %in% 
                                 c(ETsources, CTsources, PTsources))] <- 
  as.POSIXct(chldata$In.Situ.datetime[!(chldata$Database %in% 
                                          c(ETsources, CTsources, PTsources))], tz = "UTC")


# convert Overpass.datetime to POSIXct
chldata$Overpass.datetime_UTC <- as.POSIXct(chldata$Overpass.datetime, tz = "UTC")

# create new diff col with updated diffs; change from seconds to minutes; convert to numeric
chldata$Overpass.time.difference..minutes.CORR <- chldata$Overpass.datetime_UTC - chldata$In.Situ.datetime_UTC
units(chldata$Overpass.time.difference..minutes.CORR) <- "mins"
chldata$Overpass.time.difference..minutes.CORR <- as.numeric(chldata$Overpass.time.difference..minutes.CORR)


# check for and restrict to same-day (+/-12 hours)
hist(chldata$Overpass.time.difference..minutes.CORR / 60)
sum(abs(chldata$Overpass.time.difference..minutes.CORR / 60) > 12)

chldata <- chldata[abs(chldata$Overpass.time.difference..minutes.CORR / 60) <= 12, ]

# remove those with in situ time at midnight, since ambiguous
chldata$ins_time_raw <- substr(chldata$In.Situ.datetime, 12, 19)
sum(chldata$ins_time_raw == "00:00:00")

chldata <- chldata[chldata$ins_time_raw != "00:00:00", ]


## duplicates ------------------------------------------------------------

chldata$overpass.date <- (substr(chldata$Overpass.datetime, 1, 10))
chldata$insitu.date <- (substr(chldata$In.Situ.datetime, 1, 10))


# checking for duplicates with "In.Situ.lat", "In.Situ.lon", "insitu.date" - NONE
dup_fields <- c("In.Situ.lat", "In.Situ.lon", "insitu.date")
chldata_dupcols <- chldata[, which(colnames(chldata) %in% dup_fields)]
sum(duplicated(chldata_dupcols))

# checking for duplicates with "In.Situ.lat", "In.Situ.lon", "Scene.ID" - NONE (anymore)
dup_fields <- c("In.Situ.lat", "In.Situ.lon", "Scene.ID")
chldata$dupstring <- apply(chldata[, which(colnames(chldata) %in% dup_fields)], 1, paste0, collapse = ",")
sum(duplicated(chldata$dupstring))

chldata_duppairs <- chldata[chldata$dupstring %in% chldata$dupstring[duplicated(chldata$dupstring)], ]
chldata_duppairs <- chldata_duppairs[order(chldata_duppairs$dupstring), ]
chldata_duppairs[, c(dup_fields, "dupstring", "insitu.date", "Overpass.time.difference..minutes.")]

#chldata_duprm <- chldata[!duplicated(chldata_dupcols), ]

##

## geospatial --------------------------------------------------

# make point file

#chlpts <- SpatialPointsDataFrame(coords = chldata[, c("In.Situ.lon", "In.Situ.lat")], data = chldata, proj4string = CRS("+proj=longlat +ellps=WGS84"))
chlpts <- SpatialPointsDataFrame(coords = chldata[, c("In.Situ.lon", "In.Situ.lat")], data = chldata,
                                 proj4string = CRS("+init=epsg:4326"))


world <- ne_countries(scale = "medium", returnclass = "sp")
#us <- world[world$admin == "United States of America", ]

plot(world, border = "gray")
plot(chlpts, col = alpha("red", 0.3), pch = 20, add = TRUE)


## NHD lakes

# read in MERIS resolvable lakes shapefile (revised version from Erin and Blake) - for shore dist.
nhd <- readOGR(dsn = "C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/geospatial_general/NHD_NLA_shoredist",
               layer = "nhd_nla_subset_shore_dist")
nhd_backup <- nhd

# transform CRS of NHD and points 
nhd_aea <- spTransform(nhd, CRS("+init=epsg:5070"))
chlpts_aea <- spTransform(chlpts, CRS("+init=epsg:5070"))
identicalCRS(chlpts_aea, nhd_aea)

# filter points to those within NHD lakes
chlpts_nhd <- chlpts_aea[nhd_aea, ]
plot(chlpts_nhd)
length(chlpts_nhd)

# convert lake polygons to lines for calculating distance from shore
nhd_lines <- as(nhd_aea, "SpatialLines")

# calculate distance to shore ***
chlpts_nhd$dist_shore_m <- NA

for (p in 1:nrow(chlpts_nhd)) {
  chlpts_nhd$dist_shore_m[p] <- gDistance(chlpts_nhd[p, ], nhd_lines)
  
  if (p %% 10 == 0) {
    print(sprintf("%s of %s done", p, nrow(chlpts_nhd)))
  }
}

#writeOGR(chlpts_nhd, "geosp", "chlpts_nhd", driver = "ESRI Shapefile")

#chlpts_nhd <- readOGR("geosp", "chlpts_nhd")

# shore dist filtering
hist(chlpts_nhd$dist_shore_m)
sum(chlpts_nhd$dist_shore_m < 30) # 2
chlpts_nhd$dist_shore_m[chlpts_nhd$dist_shore_m < 30] # 0.3900913 3.9619915
chlpts_nhd <- chlpts_nhd[chlpts_nhd$dist_shore_m > 30, ]


# plot
# load US shp; reproject
us_raw <- readOGR("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/geospatial_general/conus_shp", "cb_2015_us_state_20m")
plot(us_raw)

us <- spTransform(us_raw, CRS("+init=epsg:5070")) # to AEA

# check for points outside CONUS (none since NHD is just CONUS)
plot(us)
plot(chlpts_nhd, col = "red", pch = 20, add = TRUE) # none in AK, HI, PR

# trim US shp to CONUS
conus <- us[!(us$NAME %in% c("Alaska", "Hawaii", "Puerto Rico")), ]
plot(conus)
plot(chlpts_nhd, col = alpha("red", 0.3), pch = 20, add = TRUE)


## set CONUS validation dataset
mu_conus <- chlpts_nhd@data
mu_conus_backup <- chlpts_nhd@data

### --------------------------------------------------------------------------------- ###

## additional filtering

# remove NA in situ
sum(is.na(mu_conus$In.Situ.chl)) #35
mu_conus <- mu_conus[!(is.na(mu_conus$In.Situ.chl)), ]


## sediment

# calc baseline slope for filtering
mu_conus$baseline_slope <- calc_baseline_slope(mu_conus$rhos.665., mu_conus$rhos.740.)
sum(mu_conus$baseline_slope < -0.00015) # count how many contaminated #72

# make new df for plotting sed points vs clear pts
mu_conus_sed <- mu_conus
mu_conus_sed$sed <- NA
mu_conus_sed$sed[mu_conus$baseline_slope < -0.00015] <- "sed"
mu_conus_sed$sed[mu_conus$baseline_slope >= -0.00015] <- "clear"

mu_conus_sed$sedcolor <- NA
mu_conus_sed$sedcolor[mu_conus_sed$sed == "sed"] <- "red"
mu_conus_sed$sedcolor[mu_conus_sed$sed == "clear"] <- "black"

# remove sediment pts
mu_conus <- mu_conus[mu_conus$baseline_slope >= -0.00015, ]

# plotting
plot(mu_conus_sed$In.Situ.chl, mu_conus_sed$MCI_rhos, col = mu_conus_sed$sedcolor, pch = 20)
plot(mu_conus_sed$baseline_slope, mu_conus_sed$MCI_rhos, col = mu_conus_sed$sedcolor, pch = 20)
plot(-mu_conus_sed$baseline_slope, mu_conus_sed$MCI_rhos, col = mu_conus_sed$sedcolor, pch = 20,
     xlab = "~ sediment conc", ylab = "MCI")

# write data
write.csv(mu_conus, sprintf("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/S2/calvalready_%s.csv", chldata_source))

#mu_conus2 <- read.csv("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/S2/calvalready.csv")




