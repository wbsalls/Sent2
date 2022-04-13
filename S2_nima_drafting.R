
library(sp)
library(rgdal)
library(rgeos)
library(rnaturalearth)
library(scales)
library(plotrix)

source("C:/Users/WSALLS/Git/Sent2/algorithms.R")
source("C:/Users/WSALLS/Git/Sent2/error_metrics_220120.R")

opar <- par()

setwd("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/S2")

l2gen <- read.csv("Nima/Pahlevan_l2gen.csv", stringsAsFactors = FALSE)
acolite <- read.csv("Nima/Pahlevan_acolite.csv", stringsAsFactors = FALSE)


## cross-check

colnames(acolite)
colnames(l2gen)

sort(colnames(acolite)[!(colnames(acolite) %in% colnames(l2gen))])
sort(colnames(l2gen)[!(colnames(l2gen) %in% colnames(acolite))])
sort(colnames(l2gen)[!(colnames(l2gen) %in% colnames(acolite))])[18:34]


## pick data source
chldata <- l2gen


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


# add chl (manually) - rhos MCI, sed not removed
chldata$chla_rhos <- (chldata$MCI_rhos + 0.00069) / 0.00017 # coefficients from rhos_noSed calibration

# error
chldata$chla_err_add <- chldata$chla_rhos - chldata$In.Situ.chl
chldata$chla_err_mult <- chldata$chla_rhos / chldata$In.Situ.chl


hist(chldata$chla_err_add)
hist(chldata$chla_err_mult)





## time density --------------------------------------------------
# including timezone checks/correction


## correct time of select data sources


# convert in situ and sat times to posix

ETsources <- c("Boston_Harbor", "CBP", "EOTB", "Lake_Erie08", "St_Johns")
CTsources <- c("Wisconsin")
PTsources <- c(c("sfbay", "sfbay2"))

chldataET <- chldata[chldata$Database %in% ETsources, ]

chldataET$In.Situ.datetime_UTC[chldataET$Database %in% ETsources] <- 
  as.POSIXct(chldataET$In.Situ.datetime[chldataET$Database %in% ETsources], tz = "America/New_York")

chldataET$In.Situ.datetime_UTC[1:10]
as.POSIXct(chldataET$In.Situ.datetime[1:10], tz = "America/New_York")

as_datetime(1438692120)

#
chldata$In.Situ.datetime_UTC <- NA

chldata$In.Situ.datetime_UTC[chldata$Database %in% ETsources] <- 
  as.POSIXct(chldata$In.Situ.datetime[chldata$Database %in% ETsources], tz = "America/New_York")
chldata$In.Situ.datetime_UTC[chldata$Database %in% CTsources] <- 
  as.POSIXct(chldata$In.Situ.datetime[chldata$Database %in% CTsources], tz = "America/Chicago")
chldata$In.Situ.datetime_UTC[chldata$Database %in% PTsources] <- 
  as.POSIXct(chldata$In.Situ.datetime[chldata$Database %in% PTsources], tz = "America/Los_Angeles")
chldata$In.Situ.datetime_UTC[!(chldata$Database %in% c(ETsources, CTsources, PTsources))] <- 
  as.POSIXct(chldata$In.Situ.datetime[!(chldata$Database %in% c(ETsources, CTsources, PTsources))], tz = "UTC")

#
testtime <- as.POSIXct("2016-02-05 15:38:00", tz = "America/New_York")
as.POSIXct(format(testtime, tz="UTC", usetz=TRUE), tz = "UTC")


class(chldata$Overpass.datetime)
as.POSIXct("2015-07-26 16:02:34", tz = "GMT")


# create new in situ time col with adjusted select data sources using tz_corr


# create new diff col with updated diffs


# check for non-same-day



##
summary(chldata$Overpass.time.difference..minutes.)
hist(chldata$Overpass.time.difference..minutes. / 60)



# parse date/time
chldata$sat_year <- as.numeric(substr(chldata$Overpass.datetime, 1, 4))
chldata$sat_month <- as.numeric(substr(chldata$Overpass.datetime, 6, 7))
chldata$sat_day <- as.numeric(substr(chldata$Overpass.datetime, 9, 10))
chldata$sat_hour <- as.numeric(substr(chldata$Overpass.datetime, 12, 13)) +
  round((as.numeric(substr(chldata$Overpass.datetime, 15, 16)) / 60), 2)
chldata$ins_hour <- as.numeric(substr(chldata$In.Situ.datetime, 12, 13)) +
  round((as.numeric(substr(chldata$In.Situ.datetime, 15, 16)) / 60), 2)

# all
par(mfrow = c(2, 1))

hist(chldata$sat_hour)
plot(chldata$In.Situ.lon, chldata$sat_hour,
     xlab = "longitude", ylab = "satellite hour") # NOTE: Overpass.datetime appears to be in UTC

hist(chldata$ins_hour)
plot(chldata$In.Situ.lon, chldata$ins_hour,
     xlab = "longitude", ylab = "in situ hour") # NOTE: In.Situ.datetime appears to be in local time
hist(chldata$ins_hour[chldata$In.Situ.lon < -50], xlab = "in situ hour", main = "lat < -50 (Western Hemi)")
hist(chldata$ins_hour[chldata$In.Situ.lon > -50], xlab = "in situ hour", main = "lat > -50 (Eastern Hemi)")
# *** This likely means Overpass.time.difference..minutes. is incorrect, and that matchups don't fully reflect same-day

plot(chldata$In.Situ.lon, chldata$In.Situ.lat)

hist(chldata$Overpass.time.difference..minutes. / 60)

summary(chldata$In.Situ.datetime - chldata$Overpass.datetime)

sum((chldata$In.Situ.datetime - chldata$Overpass.datetime) == chldata$Overpass.time.difference..minutes.)


# US
chldata_us <- chldata[chldata$In.Situ.lon < -50 & chldata$In.Situ.lat > 25 & chldata$In.Situ.lat < 49, ]

hist(chldata_us$sat_hour)
plot(chldata_us$In.Situ.lon, chldata_us$sat_hour)

hist(chldata_us$ins_hour)
plot(chldata_us$In.Situ.lon, chldata_us$ins_hour)

hist(chldata_us$Overpass.time.difference..minutes. / 60)



mdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
chldata$tseriesday <- (chldata$year - 2015)*365.25 + (chldata$month * mean(mdays)) + chldata$day
hist(chldata$tseriesday)
chldata$tseriesmonth <- substr(chldata$Overpass.datetime, 1, 7)

plot(table(chldata$tseriesmonth))

# diff data sources
table(chldata$Database)
length(unique(chldata$Database))

table(chldata_us$Database)
length(unique(chldata_us$Database))

summary(chldata$In.Situ.lon)

par(mfrow = c(4, 7), mar = c(2, 2, 2, 1))
for (d in unique(chldata$Database)) {
  chldata_d <- chldata[chldata$Database == d, ]
  plot(chldata_d$In.Situ.lon, chldata_d$ins_hour,
       xlim = c(-180, 180),
       ylim = c(0, 24),
       xlab = "longitude", ylab = "in situ hour", main = d)
  #abline(h = c(7, 19)) # rough daylight hours in local time
  #abline(h = c(11, 3), lty = 3) # rough daylight hours in the US, UTC, most liberal window (EST to WDT)
  rect(xleft = -180, ybottom = 7, xright = 180, ytop = 19, 
       col = alpha("orange", 0.3))
  rect(xleft = -180, ybottom = 11, xright = -50, ytop = 24, 
       col = alpha("blue", 0.3))
  rect(xleft = -180, ybottom = 0, xright = -50, ytop = 3, 
       col = alpha("blue", 0.3))
}
#plot.new() # to fill empty plot lattice slots

par(opar)

## duplicates ------------------------------------------------------------

chldata$overpass.date <- (substr(chldata$Overpass.datetime, 1, 10))
chldata$insitu.date <- (substr(chldata$In.Situ.datetime, 1, 10))


# no duplicates with "In.Situ.lat", "In.Situ.lon", "insitu.date"
dup_fields <- c("In.Situ.lat", "In.Situ.lon", "insitu.date")
chldata_dupcols <- chldata[, which(colnames(chldata) %in% dup_fields)]
sum(duplicated(chldata_dupcols))

# 3 duplicates with "In.Situ.lat", "In.Situ.lon", "Scene.ID"
dup_fields <- c("In.Situ.lat", "In.Situ.lon", "Scene.ID")
chldata$dupstring <- apply(chldata[, which(colnames(chldata) %in% dup_fields)], 1, paste0, collapse = ",")
sum(duplicated(chldata$dupstring))

chldata_duppairs <- chldata[chldata$dupstring %in% chldata$dupstring[duplicated(chldata$dupstring)], ]
chldata_duppairs <- chldata_duppairs[order(chldata_duppairs$dupstring), ]
chldata_duppairs[, c(dup_fields, "dupstring", "insitu.date", "Overpass.time.difference..minutes.")]

#chldata_duprm <- chldata[!duplicated(chldata_dupcols), ]

## **being pulled from an adjacent day
#   is that ok? I'd say no... check entire dataset for those. need to get timezones down first.





## geospatial --------------------------------------------------

# make point file

#chlpts <- SpatialPointsDataFrame(coords = chldata[, c("In.Situ.lon", "In.Situ.lat")], data = chldata, proj4string = CRS("+proj=longlat +ellps=WGS84"))
chlpts <- SpatialPointsDataFrame(coords = chldata[, c("In.Situ.lon", "In.Situ.lat")], data = chldata,
                                 proj4string = CRS("+init=epsg:4326"))


world <- ne_countries(scale = "medium", returnclass = "sp")
#us <- world[world$admin == "United States of America", ]

plot(world, border = "gray")
plot(chlpts, col = alpha("red", 0.3), pch = 20, add = TRUE)
plot(chlpts, col = color.scale(chlpts$chla_err_add, c(0, 1, 1), c(1, 1, 0), 0), pch = 20, add = TRUE)
plot(chlpts, col = color.scale(log(abs(chlpts$chla_err_add)), c(0, 1, 1), c(1, 1, 0), 0), pch = 20, add = TRUE)
plot(chlpts, col = color.scale(chlpts$chla_err_mult, c(0, 1, 1), c(1, 1, 0), 0), pch = 20, add = TRUE)
plot(chlpts, col = color.scale(log(abs(chlpts$chla_err_mult)), c(0, 1, 1) ,c(1, 1, 0), 0), pch = 20, add = TRUE)



## NHD lakes

# read in MERIS resolvable lakes shapefile (revised version from Erin and Blake) - for shore dist.
nhd <- readOGR(dsn = "C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/geospatial_general/NHD_NLA_shoredist",
               layer = "nhd_nla_subset_shore_dist")
nhd_backup <- nhd

# transform CRS of NHD and points 
nhd_aea <- spTransform(nhd, proj4string(CRS("+init=epsg:5070")))
chlpts_aea <- spTransform(chlpts, proj4string(CRS("+init=epsg:5070")))
identicalCRS(chlpts_aea, nhd_aea)

# filter points to those within NHD lakes
chlpts_nhd <- chlpts_aea[nhd_aea, ]
plot(chlpts_nhd)

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

# shore dist filtering
sum(chlpts_nhd$dist_shore_m < 30) # 2
chlpts_nhd$dist_shore_m[chlpts_nhd$dist_shore_m < 30] # 0.3900913 3.9619915
chlpts_nhd <- chlpts_nhd[chlpts_nhd$dist_shore_m > 30, ]

# plotting points in NHD lakes
plot(chlpts_nhd, col = color.scale(chlpts_nhd$chla_err_add, c(0, 1, 1), c(1, 1, 0), 0), pch = 20)
plot(chlpts_nhd, col = color.scale(log(abs(chlpts_nhd$chla_err_add)), c(0, 1, 1), c(1, 1, 0), 0), pch = 20, add = TRUE)
plot(chlpts_nhd, col = color.scale(chlpts_nhd$chla_err_mult, c(0, 1, 1), c(1, 1, 0), 0), pch = 20, add = TRUE)
plot(chlpts_nhd, col = color.scale(log(abs(chlpts_nhd$chla_err_mult)), c(0, 1, 1) ,c(1, 1, 0), 0), pch = 20, add = TRUE)



### rough space

par(mfrow = c(1, 2))

plot(chldata$In.Situ.lon, chldata$In.Situ.lat)
abline(h = 25)
abline(h = 49)
abline(v = -50)

nrow(chldata[chldata$In.Situ.lon < -50 & chldata$In.Situ.lat > 25, ]) # US/Canada
nrow(chldata[chldata$In.Situ.lon < -50 & chldata$In.Situ.lat > 25 & chldata$In.Situ.lat < 49, ]) # CONUS
# chldata: 1721/2808 in US/Canada; 1636 in CONUS
# acolite: 2202/3053 in US/Canada; 2050 in CONUS

chldata_conus <- chldata[chldata$In.Situ.lon < -50 & chldata$In.Situ.lat > 25 & chldata$In.Situ.lat < 49, ]
plot(chldata_conus$In.Situ.lon, chldata_conus$In.Situ.lat)
points(c(-92, -87.3, -86.9, -88.3, -122.3), c(46.8, 41.6, 46.2, 46, 38), col = "red")

par(opar)



### MCI validation ---------------------------------------------------------------

## test boostrapping

library(boot)




## automated all

chl_inds <- colnames(chldata)[which(grepl("MCI_", colnames(chldata)))]


par(mfrow = c(3, 2))

pxlim <- c(0, 140)
pylim <- c(-0.03, 0.03)

error_df <- data.frame()

for (c in seq_along(chl_inds)) {
  print(sprintf("%s: %s", c, chl_inds[c]))
  proc_level <- substr(chl_inds[c], 5, 8)
  ra_name <- paste0(proc_level, ".665.")
  rc_name <- paste0(proc_level, ".740.")
  this_data <- chldata
  
  # error metrics
  mc <- lm(this_data[[chl_inds[c]]] ~ this_data$In.Situ.chl)
  n_mc <- nrow(this_data[(!is.na(this_data$In.Situ.chl) & !is.na(this_data[[chl_inds[c]]])), ])
  rsq_mc <- round(summary(mc)$r.squared, 3)
  error_df <- rbind(error_df, cbind(run = paste0(chl_inds[c]),
                                    n = n_mc,
                                    minCI = min(this_data[[chl_inds[c]]], na.rm = TRUE),
                                    maxCI = max(this_data[[chl_inds[c]]], na.rm = TRUE),
                                    meanCI = min(this_data[[chl_inds[c]]], na.rm = TRUE),
                                    medianCI = min(this_data[[chl_inds[c]]], na.rm = TRUE),
                                    r_sq = rsq_mc))
  
  # plot
  plot(this_data$In.Situ.chl, this_data[[chl_inds[c]]], 
       main = paste0(chl_inds[c]), xlim = pxlim, ylim = pylim)
  abline(mc) # , untf=TRUE # if in log space
  abline(h=0)
  text(100, -0.02, sprintf("n = %s \nr-sq = %s", n_mc, rsq_mc), adj = c(0, 1))
  
  
  # filter sediment
  this_data$baseline_slope <- calc_baseline_slope(this_data[[ra_name]], this_data[[rc_name]])
  this_data <- this_data[this_data$baseline_slope > -0.00015, ]
  
  # error metrics
  mc <- lm(this_data[[chl_inds[c]]] ~ this_data$In.Situ.chl)
  n_mc <- nrow(this_data[(!is.na(this_data$In.Situ.chl) & !is.na(this_data[[chl_inds[c]]])), ])
  rsq_mc <- round(summary(mc)$r.squared, 3)
  error_df <- rbind(error_df, cbind(run = paste0(chl_inds[c], "_noSed"),
                                    n = n_mc,
                                    minCI = min(this_data[[chl_inds[c]]], na.rm = TRUE),
                                    maxCI = max(this_data[[chl_inds[c]]], na.rm = TRUE),
                                    meanCI = min(this_data[[chl_inds[c]]], na.rm = TRUE),
                                    medianCI = min(this_data[[chl_inds[c]]], na.rm = TRUE),
                                    r_sq = rsq_mc))
  
  # plot
  plot(this_data$In.Situ.chl, this_data[[chl_inds[c]]], 
       main = paste0(chl_inds[c], " no sed"), xlim = pxlim, ylim = pylim)
  abline(mc) # , untf=TRUE # if in log space
  abline(h=0)
  text(100, -0.02, sprintf("n = %s \nr-sq = %s", n_mc, rsq_mc), adj = c(0, 1))
}

par(opar)

plot(chldata$In.Situ.chl, chldata$mci_rrs, log = "", main = "MCI",
     ylim = c(-0.02, 0.02))
#ylim = c(min(chldata$mci_rrs, chldata$dci_rrs, na.rm = T), max(chldata$mci_rrs, chldata$dci_rrs, na.rm = T)))
plot(chldata$In.Situ.chl, chldata$dci_rrs, log = "", main = "DCI",
     ylim = c(-0.02, 0.02))
#ylim = c(min(chldata$mci_rrs, chldata$dci_rrs, na.rm = T), max(chldata$mci_rrs, chldata$dci_rrs, na.rm = T)))

abline(0.0012, 0.0002) #ontario
abline(0.0021, 0.0004) #erie


slope.mci <- 0.0004 # from Binding et al. 2013 - Erie # 2500
intercept.mci <- 0.0021 # from Binding et al. 2013 - Erie # 10.5


mlm <- lm(chldata$mci_rrs ~ chldata$In.Situ.chl)
summary(mlm)

dlm <- lm(chldata$dci_rrs ~ chldata$In.Situ.chl)
summary(dlm)


val_metrics <- plot_error_metrics(x = chldata$In.Situ.chl, y = chldata$chla_rhos, # export 800 x 860; 600 x 645 for paper
                                  #xname = expression(italic("in situ") * " chl " * italic(a) * " (" * mu * "g " * L^-1 * ")"), 
                                  #yname = expression("S2-derived chl " * italic(a) * " (" * mu * "g " * L^-1 * ")"), 
                                  #yname = "S2-derived chlorophyll a (ug/L)", 
                                  #yname = "S2-derived chlorophyll a (ug/L, from MCI using L1C reflectance)", 
                                  #title = plot_title, 
                                  equal_axes = TRUE, 
                                  log_axes = "xy", # xy, x, y, ""
                                  log_space = TRUE, # T, F
                                  plot_abline = TRUE,
                                  #text_x = min(mu_mci$chla_corr, mu_mci$chla_s2),
                                  #text_y = ,
                                  mape = FALSE,
                                  rand_error = FALSE,
                                  regr_stats = FALSE,
                                  #states = mu_mci$state,
                                  #lakes = mu_mci$comid,
                                  #xlim = c(min(mu_mci$chla_corr, mu_mci$chla_s2, na.rm = T), max(mu_mci$chla_corr, mu_mci$chla_s2, na.rm = T)),
                                  #ylim = c(min(mu_mci$chla_corr, mu_mci$chla_s2, na.rm = T), max(mu_mci$chla_corr, mu_mci$chla_s2, na.rm = T)),
                                  
                                  show_metrics = TRUE, 
                                  #xaxt="n",
                                  #yaxt="n",
                                  col = alpha("black", 0.4), 
                                  #col = mu_mci$sedimentf,
                                  #col = mu_mci$state_col,
                                  pch = 20)

### plot Seegers data

seegers <- read.csv("CyANChlBS_Matchups_2021June23.csv", stringsAsFactors = FALSE)

seegers$MERIS_chl <- 6620 * seegers$MERIS_ci_cyano - 3.1

# add to existing plot
plot(seegers$chl, seegers$MERIS_chl, add = TRUE)

# separate plotting
plot(seegers$chl, seegers$MERIS_chl, 
     xlim = c(0, 150), ylim = c(0, 150))
plot(chldata$In.Situ.chl, chldata$chla_rhos, 
     xlim = c(0, 150), ylim = c(0, 150))

