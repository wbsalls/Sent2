

# validation ----------------------------------------------------------------------------------

library(chron)
library(colorRamps)
library(grDevices)
library(RColorBrewer)
library(sp)
library(rgdal)
library(raster)
library(scales) # for alpha transparency
library(ggplot2)
library(dplyr)

source("C:/Users/WSalls/Desktop/Git/Sent2/error_metrics_1800611.R")
source("/Users/wilsonsalls/Desktop/Git/Sent2/error_metrics_1800611.R")

setwd("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/681_imgs")
setwd("/Users/wilsonsalls/Desktop/EPA/Sentinel2/Validation/681_imgs")

mu_mci_raw <- read.csv("validation_S2_682imgs_MCI_L1C_2018-11-21.csv", stringsAsFactors = FALSE)
mu_mci <- mu_mci_raw

## formatting, pre-filtering -------------------------------------------
# rename MCI column
colnames(mu_mci)[which(colnames(mu_mci) == "MCI_val_1")] <- "MCI_L1C"

#* fix chron
mu_mci$samp_localTime <- chron(dates. = substr(mu_mci$samp_localTime, 2, 9), 
                               times. = substr(mu_mci$samp_localTime, 11, 18))
mu_mci$img_localTime <- chron(dates. = substr(mu_mci$img_localTime, 2, 9), 
                              times. = substr(mu_mci$img_localTime, 11, 18))
mu_mci$samp_localDate <- substr(mu_mci$samp_localTime, 2, 9)

mu_mci$offset_hrs <- as.numeric(mu_mci$samp_localTime - mu_mci$img_localTime) * 24


# remove duplicates: identify based on duplicated chlorophyll-a and MCI (L1C)
# most duplicates have 0 MCI_L1C
val_df <- data.frame(mu_mci$chla_corr, mu_mci$MCI_L1C, mu_mci$LatitudeMeasure, mu_mci$LongitudeMeasure, mu_mci$samp_localDate)
sum(duplicated(val_df))
val_df_dups <- val_df[duplicated(val_df), ]

mu_mci <- mu_mci[!duplicated(val_df), ]

# depth - remove NAs that were left
mu_mci <- mu_mci[-which(is.na(mu_mci$depth_corr) &
                          is.na(mu_mci$topdepth_corr) & 
                          is.na(mu_mci$botdepth_corr) & 
                          is.na(mu_mci$ActivityRelativeDepthName)), ]

# average 9-pixel window
mci_val_colindex <- which(colnames(mu_mci) == "MCI_L1C"):which(colnames(mu_mci) == "MCI_val_9")

mu_mci$mci_mean <- apply(mu_mci[, mci_val_colindex], 1, mean)
mu_mci$mci_single <- mu_mci$MCI_L1C

# choose which MCI to use *******
mu_mci$MCI_L1C <- mu_mci$mci_single # mci_single OR mci_mean

# make copy
mu_mci_prefilter <- mu_mci


## filter ---------------------------------------------------------------------------------------
mu_mci <- mu_mci_prefilter # reset

## remove NA MCI
sum(is.na(mu_mci$MCI_L1C))
mu_mci <- mu_mci[!is.na(mu_mci$MCI_L1C), ]

## remove MCI = 4.999
max(mu_mci$MCI_L1C)
sum(mu_mci$MCI_L1C == max(mu_mci$MCI_L1C))
'mu_mci_499 <- mu_mci[mu_mci$MCI_L1C == max(mu_mci$MCI_L1C), ]
plot(sort(mu_mci_499$chla_corr), ylab = "in situ chlorophyll-a (ug/l)")
plot(sort(mu_mci$chla_corr))
hist(mu_mci_499$chla_corr)'

mu_mci <- mu_mci[mu_mci$MCI_L1C != max(mu_mci$MCI_L1C), ]

# plot raw MCI
#plot(mu_mci$chla_corr, mu_mci$MCI_L1C, xlab = "in situ chlorophyll-a (ug/l)", ylab = "MCI (Level 1C)")

## remove MCI = 0
sum(mu_mci$MCI_L1C == 0)

'sum(mu_mci$MCI_L1C == 0)
mu_mci_0 <- mu_mci[mu_mci$MCI_L1C == 0, ]
mu_mci_0 <- mu_mci_0[mu_mci_0$chla_corr < 200, ]
plot(mu_mci_0$chla_corr, mu_mci_0$chla_s2)
hist(mu_mci_0$chla_corr) # slightly more right-skewed than hist with all data, so 0 values tend to have lower in situ chl (good)
hist(mu_mci$chla_corr)'

mu_mci <- mu_mci[mu_mci$MCI_L1C != 0, ]

## negative MCI
min(mu_mci$MCI_L1C)
#hist(mu_mci$MCI_L1C)
sum(mu_mci$MCI_L1C < -0.01)
#mu_mci <- mu_mci[mu_mci$MCI_L1C > -0.01, ] # not relevant if removing negative S2 chl since intercept is -0.0021

## shore dist
mu_mci <- mu_mci[mu_mci$dist_shore_m > 30, ]

## method
method_sub <- "APHA" # APHA USEPA USGS
mu_mci <- mu_mci[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == method_sub), ]

## in situ chla
summary(mu_mci$chla_corr)
mu_mci <- mu_mci[mu_mci$chla_corr <= 1000, ]

## sediment
# merge raw band values table
raw_bands <- read.csv("mu_rawbands_3day.csv", stringsAsFactors = FALSE)
mu_mci <- merge(mu_mci, raw_bands, by = "X.5", all.x = TRUE)

# calculate slope
mu_mci$mci_baseline_b6_b4 <- mu_mci$b6_1 - mu_mci$b4_1
mu_mci$mci_baseline_slope <- mu_mci$mci_baseline_b6_b4 / (740 - 655)

# assign cutoff
sed_cutoff <- -2 # Binding recommendation: retain only points > -0.15
mu_mci$sediment <- paste0("<= ", sed_cutoff)
mu_mci$sediment[mu_mci$mci_baseline_slope > sed_cutoff] <- paste0("> ", sed_cutoff)
mu_mci$sedimentf <- factor(mu_mci$sediment, levels(factor(mu_mci$sediment))[c(2, 1)])

table(mu_mci$sedimentf)
plot(mu_mci$mci_baseline_slope, rep(1, nrow(mu_mci)))

# apply cutoff
mu_mci <- mu_mci[mu_mci$mci_baseline_slope > sed_cutoff, ]


## remove bad points identified in imagery
length(mu_mci_raw$X.5) == length(unique(mu_mci_raw$X.5)) # checking if unique: yes

rm_imgry <- c(3275, 3656, 3673, 3674, 3678, 3689, 4888, 4889, 4890, 6976, 7680, 10334, 10335, 11768)

# apply removal
mu_mci <- mu_mci[-which(mu_mci$X.5 %in% rm_imgry), ]

# for reset
mu_mci_filtered <- mu_mci

# RESET ----------------------------------------------------------------------------------------
mu_mci <- mu_mci_filtered

# offset time  -----------------------------------
offset_min <- 0
offset_max <- 0
offset_threshold <- offset_min:offset_max
mu_mci <- mu_mci[mu_mci$offset_days %in% offset_threshold, ]

## clouds
#mu_mci <- mu_mci[mu_mci$CLOUDY_PIXEL_PERCENTAGE == 0, ]

## season
mu_mci$month <- as.numeric(substr(mu_mci$samp_localTime, 2, 3))
table(mu_mci$month)
#mu_mci <- mu_mci[mu_mci$month %in% 6:8, ]

# for reset
mu_mci_filtered <- mu_mci


## calc chl a from MCI  ---------------------------------------------------------------------------------------------------

mu_mci <- mu_mci_filtered

# choose 1) binding OR 2) cal-val

s2_calc <- "binding" # <<<<<<<<<<< *** binding / split ***

if (s2_calc == "binding") {
  # 1) binding paper -------
  slope.mci <- 0.0004 # from Binding et al. 2013 - Erie
  intercept.mci <- -0.0021 # from Binding et al. 2013 - Erie
  
  
} else if (s2_calc == "mollaee") {
  # 1b) Mollaee thesis (p 77) -------
  slope.mci <- 0.0001790292 # 1/2158 = 0.00046
  intercept.mci <- -0.0018 # -3.9/2158 = -0.0018
  
  
} else if (s2_calc == "split") {
  
  # 2) cal-val split -------
  set.seed(1)
  index.selected <- sample(1:nrow(mu_mci), size = floor(nrow(mu_mci) * 0.8), replace = FALSE)
  mu_mci_calc <- mu_mci[index.selected, ] # select 80% for cal
  mu_mci <- mu_mci[-index.selected, ] # retain remaining 20% for val
  
  # fit linear model; set coefficients
  model_s2chla <- lm(mu_mci_calc$MCI_L1C ~ mu_mci_calc$chla_corr)
  
  slope.mci <- model_s2chla$coefficients[2]
  intercept.mci <- model_s2chla$coefficients[1]
}

# calculate S2 chla
mu_mci$chla_s2 <- (mu_mci$MCI_L1C - intercept.mci) / slope.mci

# remove negative S2 Chla
sum(mu_mci$chla_s2 < 0)
mu_mci <- mu_mci[mu_mci$chla_s2 >= 0, ]
#mu_mci[mu_mci$chla_s2 < 0, ] <- 0 # set negatives to 0


# for reset
mu_mci_s2chla <- mu_mci

# ----------------------------------------------------------------


## integrate depth duplicates (only done on set with n = 111 (same-day, filtered, bad pts removed))
# creates a new table with integrated records, and comments where integration occurred
# retains the old table with comments

# reset
mu_mci <- mu_mci_s2chla

mu_mci$integration_comment <- ""

mu_mci_integrated <- data.frame()
duplicate_checks <- data.frame()

#
for (r in 1:nrow(mu_mci)) {
  
  # skip if it's a duplicate that has already been handled
  if (mu_mci$integration_comment[r] == "REMOVED") {
    next
  }
  
  # get index of all observations with same location and date
  concurrent_index <- which(mu_mci$LatitudeMeasure == mu_mci$LatitudeMeasure[r] & 
                              mu_mci$LongitudeMeasure == mu_mci$LongitudeMeasure[r] & 
                              mu_mci$samp_localDate == mu_mci$samp_localDate[r])
  concurrent <- mu_mci[concurrent_index, ]
  
  # if only one row (not duplicates), add to output
  if (nrow(concurrent) == 1) {
    mu_mci_integrated <- rbind(mu_mci_integrated, mu_mci[r, ])
    next
  }
  
  # if both satellite and in situ dup, add to output and flag
  if (length(unique(concurrent$GRANULE_ID)) > 1 & length(unique(concurrent$bio_uniqueID)) > 1) {
    mu_mci_integrated <- rbind(mu_mci_integrated, mu_mci[r, ])
    mu_mci$integration_comment[r] <- paste0("in situ AND satellite duplicate with X.5 = ", toString(concurrent$X.5))
    print(paste0("SKIPPING... has in situ AND satellite duplicates: mu_mci row #", r))
    next
  }
  
  # check if depth is the same
  if (length(unique(concurrent$depth_corr)) == 1 &
      length(unique(concurrent$topdepth_corr)) == 1 & 
      length(unique(concurrent$botdepth_corr)) == 1 & 
      length(unique(concurrent$ActivityRelativeDepthName)) == 1) {
    same_depth <- TRUE
  }
  
  ## integrating
  # diff in situ sample, same img
  if (length(unique(concurrent$bio_uniqueID)) > 1) {
    
    # as long as depth is the same, average in situ chl; otherwise, add to list to check
    if (isTRUE(same_depth)) {
      
      mu_mci_integrated$integration_comment[nrow(mu_mci_integrated)] <- 
        sprintf("avged in situ chl; original values: %s; original X.5: %s", 
                toString(concurrent$chla_corr), toString(concurrent$X.5)) # add comment
      
      mu_mci_integrated <- rbind(mu_mci_integrated, mu_mci[r, ]) # add this row to output
      mu_mci_integrated$chla_corr[nrow(mu_mci_integrated)] <- mean(concurrent$chla_corr) # set average chl
      mu_mci$integration_comment[concurrent_index[2:length(concurrent_index)]] <- "REMOVED" # flag future obsevations to be skipped
      
    } else {
      mu_mci_integrated <- rbind(mu_mci_integrated, mu_mci[r, ])
      mu_mci$integration_comment[r] <- paste0("in situ duplicate with different depths - X.5:", toString(concurrent$X.5))
      print(paste0("SKIPPING... in situ duplicate with different depths: mu_mci row #", r))
      next
    }
  }
  
  # diff imgs, same in situ sample
  else if (length(unique(concurrent$GRANULE_ID)) > 1) {
    
    # as long as depth is the same, average in S2 chl; otherwise, add to list to check
    if (isTRUE(same_depth)) {
      
      mu_mci_integrated$integration_comment[nrow(mu_mci_integrated)] <-  
        sprintf("avged in S2 chl; original values: %s; original X.5: %s", 
                toString(concurrent$chla_s2), toString(concurrent$X.5)) # add comment
      
      mu_mci_integrated <- rbind(mu_mci_integrated, mu_mci[r, ]) # add this row to output
      mu_mci_integrated$chla_corr[nrow(mu_mci_integrated)] <- mean(concurrent$chla_s2) # set average chl
      mu_mci$integration_comment[concurrent_index[2:length(concurrent_index)]] <- "REMOVED" # flag future obsevations to be skipped
      
    } else {
      mu_mci_integrated <- rbind(mu_mci_integrated, mu_mci[r, ])
      mu_mci$integration_comment[r] <- paste0("img duplicate with different depths - X.5:", toString(concurrent$X.5))
      print(paste0("SKIPPING... img duplicate with different depths: mu_mci row #", r))
      next
    }
  }
  
  # if doesn't fit into either case, there's a problem
  else {
    mu_mci_integrated <- rbind(mu_mci_integrated, mu_mci[r, ])
    mu_mci$integration_comment[r] <- "ISSUE - CHECK"
    print(paste0("issue: mu_mci row #", r))
  }
}

# rename old and dew tables
mu_mci_preintegrated <- mu_mci
mu_mci <- mu_mci_integrated

#######

'
# get space-time duplicates
dup_fields <- data.frame(mu_mci$LatitudeMeasure, mu_mci$LongitudeMeasure, mu_mci$samp_localDate)
dups <- which(duplicated(dup_fields))
length(dups)
sum(duplicated(mu_mci[dups, ])) # check if any cases have more than one duplicate

# make dataframe of X.5 indices of duplicate pairs
dup_pairs <- data.frame()
for (d in (dups)) {
  for (m in 1:nrow(dup_fields)) {
    if (all(dup_fields[m, ] == dup_fields[d, ])) {
      if (d == m) {
        next
      }
      dup_pairs <- rbind(dup_pairs, data.frame(a = mu_mci$X.5[m], b = mu_mci$X.5[d]))
      print(c(m, d))
    }
  }
}
dup_pairs

# average chl (either in situ or S2) for remaining duplicates (with same depth)
mu_mci$depth_avg_comment <- NA # for recording averaging
for (p in 1:nrow(dup_pairs)) {
  chla_s2_1 <- mu_mci$chla_s2[which(mu_mci$X.5 == dup_pairs[p, 1])]
  chla_s2_2 <- mu_mci$chla_s2[which(mu_mci$X.5 == dup_pairs[p, 2])]
  chla_insitu_1 <- mu_mci$chla_corr[which(mu_mci$X.5 == dup_pairs[p, 1])]
  chla_insitu_2 <- mu_mci$chla_corr[which(mu_mci$X.5 == dup_pairs[p, 2])]
  img_1 <- mu_mci$GRANULE_ID[which(mu_mci$X.5 == dup_pairs[p, 1])]
  img_2 <- mu_mci$GRANULE_ID[which(mu_mci$X.5 == dup_pairs[p, 2])]
  
  # for cases of equal S2 chl: average in situ values, assign to first record, and delete second record
  if (chla_s2_1 == chla_s2_2) {
    mean_chla_corr <- mean(c(chla_insitu_1, chla_insitu_2))
    mu_mci$depth_avg_comment[which(mu_mci$X.5 == dup_pairs[p, 1])] <- 
      sprintf("avged in situ chl; original values: %s from this record, and %s from from X.5 = %s", chla_insitu_1, chla_insitu_2, dup_pairs[p, 2])
    mu_mci$chla_corr[which(mu_mci$X.5 == dup_pairs[p, 1])] <- mean_chla_corr
    mu_mci <- mu_mci[-which(mu_mci$X.5 == dup_pairs[p, 2]), ]
    print(sprintf("averaging in situ values for X.5 = %s and %s", dup_pairs[p, 1], dup_pairs[p, 2]))
  }
  
  # for cases of equal in situ chl: average s2 values, assign to first record, and delete second record
  if (chla_insitu_1 == chla_insitu_2) {
    mean_chla_corr <- mean(c(chla_s2_1, chla_s2_2))
    mu_mci$depth_avg_comment[which(mu_mci$X.5 == dup_pairs[p, 1])] <- 
      sprintf("avged S2 chl; original values: %s from this record, and %s from from X.5 = %s", chla_s2_1, chla_s2_2, dup_pairs[p, 2])
    mu_mci$chla_s2[which(mu_mci$X.5 == dup_pairs[p, 1])] <- mean_chla_corr
    mu_mci <- mu_mci[-which(mu_mci$X.5 == dup_pairs[p, 2]), ]
    print(sprintf("averaging in situ values for X.5 = %s and %s", dup_pairs[p, 1], dup_pairs[p, 2]))
  }
}

# remove duplicate with non-surface depth
mu_mci$depth_corr[which(mu_mci$X.5 == as.numeric(dup_pairs[3, ]))]
dup_pairs[3, ]
mu_mci <- mu_mci[-which(mu_mci$X.5 == dup_pairs[3, 2]), ]
dup_pairs <- dup_pairs[-3, ]
'

### calculate error
mu_mci$residual_chla <- abs(mu_mci$chla_s2 - mu_mci$chla_corr) # residual
mu_mci$pct_error_chla <- (abs(mu_mci$chla_s2 - mu_mci$chla_corr) / mu_mci$chla_corr) * 100 # % error


### validation plot  -----------------------------------------------------------------------------------

#jpeg(sprintf("val_%s_%s.png", offset_min, offset_max), width = 800, height = 860)
if (offset_min == offset_max) {
  plot_title <- sprintf("+/- %s day", offset_min)
} else {
  plot_title <- sprintf("+/- %s-%s days", offset_min, offset_max)
}
plot_error_metrics(x = mu_mci$chla_corr, y = mu_mci$chla_s2, # export 800 x 860
                   xname = "in situ chlorophyll-a (ug/l)", 
                   yname = "S2-derived chlorophyll-a (ug/l, from MCI using L1C reflectance)", 
                   title = plot_title, 
                   #title = paste0(method_sub, ", ", plot_title), # if subsetting by method
                   equal_axes = TRUE, 
                   log_axes = "xy", 
                   log_space = TRUE,
                   plot_abline = FALSE,
                   mape = FALSE,
                   rand_error = FALSE,
                   regr_stats = FALSE,
                   states = mu_mci$state,
                   lakes = mu_mci$comid,
                   xlim = c(0.05, max(mu_mci$chla_corr, mu_mci$chla_s2)),
                   ylim = c(0.05, max(mu_mci$chla_corr, mu_mci$chla_s2)),
                   show_metrics = TRUE, 
                   #xaxt="n",
                   #yaxt="n",
                   col = alpha("black", 0.4), 
                   #col = mu_mci$sedimentf,
                   pch = 20)
#legend("bottomright", legend = unique(mu_mci$sedimentf), col = c("black", "red"), border = NULL)
#dev.off()
cat(sprintf("S2 regression slope = %s; intercept = %s (Binding = 0.0004; -0.0021)\n%s images", 
            signif(slope.mci, digits = 2), signif(intercept.mci, digits = 2),
            length(unique(mu_mci$GRANULE_ID))))

####

threshold_lty <- 2
abline(h = 2, lty = threshold_lty, col = "blue")
abline(v = 2, lty = threshold_lty, col = "blue")
abline(h = 7, lty = threshold_lty, col = "green")
abline(v = 7, lty = threshold_lty, col = "green")
abline(h = 30, lty = threshold_lty, col = "red")
abline(v = 30, lty = threshold_lty, col = "red")


##

#plot(mu_mci$chla_corr, mu_mci$chla_s2, xlim = c(0, 415), ylim = c(0, 415), xlab = "in situ chlorophyll-a (ug/l)", ylab = "S2-derived chlorophyll-a (from MCI L1C)")
#plot(mu_mci$chla_corr, mu_mci$residual_chla)
#plot(mu_mci$chla_corr, mu_mci$pct_error_chla)

# write image list
write.csv(unique(mu_mci[, c("PRODUCT_ID", "GRANULE_ID")]), "O:/PRIV/NERL_ORD_CYAN/Sentinel2/Images/composited/0day/img_list.csv")

# subset by CV  -------------------------------------------
mci_val_colindex <- which(colnames(mu_mci) == "MCI_L1C"):which(colnames(mu_mci) == "MCI_val_9")

# define function for population SD (rather than sample SD)
sd_pop <- function(x) {
  sqrt(sum((x - mean(x, na.rm = TRUE)) ^ 2) / (length(x)))
}

# calculate MCI mean, sd, CV for 3x3 array
mu_mci$mci_mean <- apply(mu_mci[, mci_val_colindex], 1, mean)
mu_mci$mci_sd <- apply(mu_mci[, mci_val_colindex], 1, sd_pop)
mu_mci$mci_cv <- mu_mci$mci_sd / mu_mci$mci_mean

# perform subset
#mu_mci <- mu_mci[abs(mu_mci$mci_cv) <= 0.15, ] # *try adjusting cv threshold



# make map of in situ locations ------------------------------------------------------------------

# read us shp for state names
us <- readOGR("O:/PRIV/NERL_ORD_CYAN/Salls_working/geospatial_general/US", "cb_2015_us_state_20m")

# make spdf of matchups
lon <- mu_mci$LongitudeMeasure # **
lat <- mu_mci$LatitudeMeasure # **
mu_mci_pts <- SpatialPointsDataFrame(coords = matrix(c(lon, lat), ncol = 2), 
                                     mu_mci, proj4string = CRS("+init=epsg:4326"))

# append state name to each point
mu_mci_pts_proj <- spTransform(mu_mci_pts, crs(us))

conus <- us[-which(us$STUSPS %in% c("AK", "HI", "PR")), ]

if (offset_min == offset_max) {
  plot(conus, col = "cornsilk2", border = "grey", main = sprintf("+/- %s-day matchups", offset_max))
} else {
  plot(conus, col = "cornsilk2", border = "grey", main = sprintf("+/- %s-%s-day matchups", offset_min, offset_max))
}
plot(mu_mci_pts_proj, pch = 20, col = alpha("black", 0.2), add=TRUE)

# ------------------------------------------------------------------


# mci vs chl-a
'
l1c <- calc_error_metrics(mu_mci$chla_corr, mu_mci$MCI_L1C)
l1c
#plot_error_metrics(x = mu_mci$chla_corr, y = mu_mci$MCI_L1C, 
xname = "in situ chlorophyll-a (ug/l)", 
yname = "S2 MCI (L1C)", 
title = sprintf("L1C MCI (r-sq = %s)", round(l1c$r.sq, 2)), 
equal_axes = FALSE, 
log_axes = "", 
states = mu_mci$state, 
lakes = mu_mci$comid)

# convert MCI to chlorophyll-a
slope.mci <- l1c$slope # 0.0001579346 from 237 imgs; 0.000222 from 50 imgs
intercept.mci <- l1c$int # -0.0003651727 from 237 imgs; -0.00357 from 50 imgs

slope.mci <- 0.0002 # from Binding et al. 2013 - Ontario
intercept.mci <- -0.0012 # from Binding et al. 2013 - Ontario
'

# color code by day ----------------------------------
library(viridis)

# make table of colors, with factor for plotting
mu_mci$offset_days_factor <- as.factor(mu_mci$offset_days)

jcolors <- data.frame(day = levels(mu_mci$offset_days_factor),
                      color = topo.colors(length(levels(mu_mci$offset_days_factor)), 
                                          alpha = 0.3)) # topo.colors, viridis

# merge color to mu_mci for plotting
mu_mci <- merge(mu_mci, jcolors, by.x = "offset_days", by.y = "day", all.x = TRUE, all.y = FALSE)
mu_mci$color <- as.character(mu_mci$color)

plot(mu_mci$chla_corr, mu_mci$chla_s2, col = mu_mci$color, pch = 16, 
     xlim = c(0, 210), ylim = c(0, 210), 
     xlab = "in situ chlorophyll-a (ug/l)", 
     ylab = "S2-derived chlorophyll-a (from MCI L1C)", 
     main = "Days between in situ and satellite") #[mu_mci$offset_days %in% 0:3]
legend(199, 217, levels(mu_mci$offset_days_factor), col = as.character(jcolors$color), 
       pch = 16, y.intersp = 0.5)

## validation plot: each day
for (d in 10:0) {
  
  offset_threshold <- d
  mu_mci <- mu_mci[mu_mci$offset_days <= offset_threshold, ]
  
  
  ## chl-a vs chl-a
  l1c_chla <- calc_error_metrics(mu_mci$chla_corr, mu_mci$chla_s2)
  print(l1c_chla)
  
  
  # color
  col_plot <- jcolors$color[match(mu_mci$offset_days_factor, jcolors$day)]
  pch_plot <- 21
  plot_error_metrics(x = mu_mci$chla_corr, y = mu_mci$chla_s2, 
                     xname = "in situ chlorophyll-a (ug/l)", 
                     yname = "S2-derived chlorophyll-a (from MCI L1C)", 
                     title = sprintf("+/- %s-day validation of Sentinel-2-derived chlorophyll-a\n(coefficients from Binding et al. [2013], Lake Erie)", offset_threshold), 
                     equal_axes = TRUE, 
                     log_axes = "xy", 
                     plot_abline = FALSE,
                     rsq = FALSE,
                     states = mu_mci$state,
                     lakes = mu_mci$comid,
                     xlim = c(0.05, 200),
                     ylim = c(0.05, 200),
                     bg = col_plot, col = "black", pch = pch_plot) # col = alpha("black", 0.3), pch = 20
  legend(0.03, 10, levels(mu_mci$offset_days_factor), pt.bg = jcolors$color, col = "black", pch = pch_plot)
}


## these don't work....
#
#plot(mu_mci$chla_corr, mu_mci$MCI_L1C, col = mu_mci$offset_days_factor)
#plot(mu_mci$chla_corr, mu_mci$MCI_L1C, col = rainbow(mu_mci$offset_days_factor))
#legend(10, 0.045, levels(mu_mci$offset_days_factor), col = rainbow(1:length(mu_mci$offset_days_factor)), pch=1)

#
library(ggplot2)
qplot(mu_mci$chla_corr, mu_mci$MCI_L1C, col = mu_mci$offset_days_factor)

#
plot(mu_mci$chla_corr, mu_mci$MCI_L1C, col = topo.colors(n = 11, alpha = 0.5), pch = 16, xlim = c(0, 210))
legend(195, 0.045, levels(mu_mci$offset_days_factor), col = topo.colors(11, alpha = 0.5), pch = 16)
plot(mu_mci$chla_corr[mu_mci$offset_days %in% 0:3], mu_mci$MCI_L1C[mu_mci$offset_days %in% 0:3], 
     col = topo.colors(n = 11, alpha = 0.5), pch = 16, xlim = c(0, 210)) # doesn't work


# investigate patterns ---------------------------------------------------------------------------
par()$mfrow
par(mfrow = c(2,1))
par(mfrow = c(1,1))

## residual vs. in situ value - what's happening with points above 1:1 line?
plot(mu_mci$chla_corr, mu_mci$residual_chla, xlab = "in situ chlorophyll-a (ug/l)", ylab = "chl a error (ug/L)")

## check high error
mu_mci_sort <- mu_mci[order(-mu_mci$residual_chla), ]
mu_mci_sort <- mu_mci[order(-mu_mci$pct_error_chla), ]
mu_mci_sort[1:20, c(185, 191:194)] # this no longer works right - what's it supposed to be??


## solar angle

# error ~ angle
plot(mu_mci$MEAN_SOLAR_ZENITH_ANGLE, mu_mci$residual_chla)
summary(mu_mci$MEAN_SOLAR_ZENITH_ANGLE)

box_min <- 21
box_max <- 45
box_step <- 4
mu_mci$solar_angle_interval <- cut(mu_mci$MEAN_SOLAR_ZENITH_ANGLE, seq(box_min, box_max, box_step))

par(mfrow = c(2,1))
barplot(table(mu_mci$solar_angle_interval), xlab = NULL, ylab = "frequency", xaxt = 'n') # ylab = NULL, yaxt = 'n'
boxplot(residual_chla ~ solar_angle_interval, data = mu_mci,
        las = 3,
        xaxt = 'n',
        xlab = "Solar Zenith Angle",
        ylab = "chl a error (ug/l)")
axis(side = 1, las = 3,
     at = seq(from = 0.5, to = length(seq(box_min, box_max, box_step)) - 0.5, by = 1), 
     seq(box_min, box_max, box_step))
par(mfrow = c(1,1))

# angle ~ month
plot(mu_mci$month, mu_mci$MEAN_SOLAR_ZENITH_ANGLE)
table(mu_mci$month)

box_min <- min(mu_mci$month)
box_max <- max(mu_mci$month)
box_step <- 1

par(mfrow = c(2,1))
barplot(table(mu_mci$month), xlab = NULL, ylab = "frequency", xaxt = 'n') # ylab = NULL, yaxt = 'n'
boxplot(MEAN_SOLAR_ZENITH_ANGLE ~ month, data = mu_mci,
        las = 3,
        xaxt = 'n',
        xlab = "Month",
        ylab = "Solar Zenith Angle")
axis(side = 1, las = 3,
     at = seq(from = 1, to = length(seq(box_min, box_max, box_step)), by = 1), 
     seq(box_min, box_max, box_step))
par(mfrow = c(1,1))

## sensor angle
plot(mu_mci$MEAN_INCIDENCE_ZENITH_ANGLE_B4, mu_mci$residual_chla)

summary(mu_mci$MEAN_INCIDENCE_ZENITH_ANGLE_B4)
box_min <- 2
box_max <- 11
box_step <- 1
mu_mci$sensor_angle_interval <- cut(mu_mci$MEAN_INCIDENCE_ZENITH_ANGLE_B4, seq(box_min, box_max, box_step))

par(mfrow = c(2,1))
barplot(table(mu_mci$sensor_angle_interval), xlab = NULL, ylab = "frequency", xaxt = 'n') # ylab = NULL, yaxt = 'n'
boxplot(residual_chla ~ sensor_angle_interval, data = mu_mci,
        las = 3,
        xaxt = 'n',
        xlab = "Sensor Zenith Angle",
        ylab = "chl a error (ug/l)")
axis(side = 1, las = 3,
     at = seq(from = 0.5, to = length(seq(box_min, box_max, box_step)) - 0.5, by = 1), 
     seq(box_min, box_max, box_step))
par(mfrow = c(1,1))


## sediment -------------------------------
table(mu_mci$sediment)
boxplot(residual_chla ~ sediment, data = mu_mci,
        xlab = "baseline slope (low indicates sediment)",
        ylab = "chl a residual")
text(1, 80, sprintf("n = %s", table(mu_mci$sediment)[1]))
text(2, 80, sprintf("n = %s", table(mu_mci$sediment)[2]))

## month ----------------------------------
# boxplot
par(mfrow = c(2,1))
barplot(table(mu_mci$month), xlab = NULL, ylab = "frequency")
boxplot(residual_chla ~ month, data = mu_mci,
        las = 3,
        xaxt = 'n',
        xlab = "month",
        ylab = "chl a error (ug/l)")
axis(side = 1,
     at = seq(from = 1, to = length(unique(mu_mci$month)), by = 1), 
     labels = sort(unique(mu_mci$month)))
par(mfrow = c(1,1))

## shore dist ----------------------------------
plot(mu_mci$dist_shore_m, mu_mci$residual_chla, 
     xlim = c(0, 2000), # try removing this too
     #ylim = c(0, 200),
     xlab = "distance from shore (m)",
     ylab = "chl a error (ug/L)",
     pch = 20,
     col = alpha("black", alpha = 0.4))
abline(v = 30, lty = 2)
#rect(0, 0, 30, 350, border = NULL, col = alpha("orange", alpha = 0.5))

# boxplot
mu_mci$dist_shore_m_interval <- cut(mu_mci$dist_shore_m, seq(0, 2000, 50))

par(mfrow = c(2,1))
barplot(table(mu_mci$dist_shore_m_interval), xlab = "distance from shore (m)", ylab = "frequency")
#barplot(table(mu_mci$dist_shore_m_interval), xlab = NULL, ylab = NULL, xaxt = 'n', yaxt = 'n')
boxplot(residual_chla ~ dist_shore_m_interval, data = mu_mci,
        las = 3,
        xaxt = 'n',
        xlab = "distance from shore (m)",
        ylab = "chl a error (ug/l)")
axis(side = 1, las = 3,
     at = seq(from = 0.5, to = 40.5, by = 1), 
     labels = c(rbind(seq(from = 0, to = 2000, by = 100), ""))[1:41])

## offset days ---------------
plot(mu_mci$offset_days, mu_mci$residual_chla)
plot(mu_mci$offset_hrs, mu_mci$residual_chla)

plot(abs(mu_mci$offset_hrs), mu_mci$residual_chla, 
     #ylim = c(0, 200),
     xlab = "time offset (hours)",
     ylab = "chl a residual (ug/L)",
     pch = 20,
     col = alpha("black", alpha = 0.4))

par(mfrow = c(2,1))
barplot(table(mu_mci$offset_days), xlab = "offset days", ylab = "frequency")
boxplot(residual_chla ~ offset_days, data = mu_mci,
        xlab = "offset days",
        ylab = "chl a residual")

## hour of day ---------------
mu_mci$offset_hrs_day <- (mu_mci$offset_hrs + 12) %% 24 - 12
mu_mci_hrs <- mu_mci[which(abs(mu_mci$offset_hrs_day) < 10), ]
#mu_mci_hrs <- mu_mci

# boxplot 1500 x 900
par(mfrow = c(2,1))
mu_mci$offset_hrs_day_interval <- cut(mu_mci$offset_hrs_day, seq(-12, 12, 0.5))
barplot(table(mu_mci$offset_hrs_day_interval), xlab = "offset hour of day", ylab = "frequency")
boxplot(residual_chla ~ offset_hrs_day_interval, data = mu_mci,
        xlab = "offset hour of day",
        ylab = "chl a residual")

# quadratic model
hrs <- mu_mci_hrs$offset_hrs_day
hrs2 <- mu_mci_hrs$offset_hrs_day ^ 2
qmod <- lm(mu_mci_hrs$residual_chla ~ hrs + hrs2)
summary(qmod)

timevalues <- seq(-12, 12, 0.1)
predictedcounts <- predict(qmod, list(hrs=timevalues, hrs2=timevalues^2))

plot(mu_mci_hrs$offset_hrs_day, mu_mci_hrs$residual_chla, xlim = c(-12, 12))
lines(timevalues, predictedcounts, col = "darkgreen", lwd = 3)


## method ---------------
boxplot(residual_chla ~ ResultAnalyticalMethod.MethodIdentifierContext, data = mu_mci,
        ylab = "chl a residual",
        xlab = "Method Identifier Context")
text(0.7, 150, paste0("n = ", table(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext)[1]))
text(1.7, 150, paste0("n = ", table(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext)[2]))
text(2.7, 150, paste0("n = ", table(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext)[3]))

# shore dist vs method
boxplot(dist_shore_m ~ ResultAnalyticalMethod.MethodIdentifierContext, data = mu_mci,
        ylab = "distance from shore (m)",
        xlab = "Method Identifier Context")
text(0.7, 14000, paste0("n = ", table(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext)[1]))
text(1.7, 14000, paste0("n = ", table(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext)[2]))
text(2.7, 14000, paste0("n = ", table(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext)[3]))

par(mfrow = c(2,1))
barplot(table(mu_mci[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == "APHA"), ]$dist_shore_m_interval), xlab = "distance from shore (m)", ylab = "frequency", main = "APHA")
boxplot(residual_chla ~ dist_shore_m_interval, data = mu_mci[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == "APHA"), ],
        xlab = "distance from shore (m)",
        ylab = "chl a residual")

barplot(table(mu_mci[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == "USEPA"), ]$dist_shore_m_interval), xlab = "distance from shore (m)", ylab = "frequency", main = "USEPA")
boxplot(residual_chla ~ dist_shore_m_interval, data = mu_mci[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == "USEPA"), ],
        xlab = "distance from shore (m)",
        ylab = "chl a residual")

barplot(table(mu_mci[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == "USGS"), ]$dist_shore_m_interval), xlab = "distance from shore (m)", ylab = "frequency", main = "USGS")
boxplot(residual_chla ~ dist_shore_m_interval, data = mu_mci[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == "USGS"), ],
        xlab = "distance from shore (m)",
        ylab = "chl a residual")

mu_method <- mu_mci[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == "APHA"), ]
plot(mu_method$dist_shore_m, mu_method$residual_chla)

m_usgs <- lm(residual_chla ~ dist_shore_m, mu_usgs)

## satellite ---------------
boxplot(residual_chla ~ SPACECRAFT_NAME, data = mu_mci,
        ylab = "chl a residual",
        xlab = "Satellite")
text(0.7, 150, paste0("n = ", table(mu_mci$SPACECRAFT_NAME)[1]))
text(1.7, 150, paste0("n = ", table(mu_mci$SPACECRAFT_NAME)[2]))


## plot error by factor variables ---------------

library(vioplot)

plot_error_byfactor <- function(data, var_name, plotvalue_name) {
  var_values <- unique(data[, which(colnames(data) == var_name)])
  
  expr_vio <- vioplot(data[which(data[col])])
  
  for (v in 1:length(values)) {
    
  }
  
  
  vioplot()  
}

eval(parse(text = "plot(1, 2)"))

factor_tab <- table(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext)
factor_vals <- names(factor_tab)
sum(is.na((mu_mci$ResultAnalyticalMethod.MethodIdentifierContext)))

vioplot(mu_mci$residual_chla[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == factor_vals[1])],
        mu_mci$residual_chla[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == factor_vals[2])],
        mu_mci$residual_chla[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == factor_vals[3])],
        mu_mci$residual_chla[which(is.na(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext))],
        names = c(
          paste0(factor_vals[1], "\n", factor_tab[1]),
          paste0(factor_vals[2], "\n", factor_tab[2]),
          paste0(factor_vals[3], "\n", factor_tab[3]),
          "NA\n10"),
        col = "lightblue")


#### scraps




## same MCI values
mci_freq <- as.data.frame(table(mu_mci$MCI_L1C))
mci_freq <- mci_freq[order(-mci_freq$Freq)[1:30], ]

# these sets are from the same location around the same time (therefore same images) but different in situ chl-a
# looks like intensive sampling events - keep?
mci_0117 <- mu_mci[round(mu_mci$MCI_L1C, 4) == 0.0117, ]
mci_0100 <- mu_mci[round(mu_mci$MCI_L1C, 5) == 0.01005, ]

for (i in 1:nrow(mci_freq)) {
  mu_mci_value <- mu_mci[round(mu_mci$MCI_L1C, 6) == round(Var1), ]
  nimgs <- length(unique(mu_mci_value$PRODUCT_ID))
}


## different days
mu_mci <- mu_mci[mu_mci$offset_days == "same day", ]
#...


## high values
'sum(mu_mci$chla_corr >= 200)
mu_mci$chla_corr[mu_mci$chla_corr > 1000]
mu_mci_hi <- mu_mci[mu_mci$chla_corr >= 150 & mu_mci$chla_corr <= 600, ]
sort(mu_mci$chla_corr[mu_mci$chla_corr >= 150])
plot(sort(mu_mci$chla_corr[mu_mci$chla_corr < 1000]), ylab = "in situ chlorophyll-a (ug/l)")
plot(sort(mu_mci_hi$chla_corr), ylab = "in situ chlorophyll-a (ug/l)")'

## outliers
'
library(ggpubr)

hist(log(mu_mci$chla_corr))
summary(log(mu_mci$chla_corr))

## SD * 3 - cant use since not normal (linear nor log)
# log
min_cut <- mean(log(mu_mci$chla_corr)) - 3 * sd(log(mu_mci$chla_corr))
max_cut <- mean(log(mu_mci$chla_corr)) + 3 * sd(log(mu_mci$chla_corr))
exp(max_cut)

# linear
min_cut <- mean((mu_mci$chla_corr)) - 3 * sd((mu_mci$chla_corr))
max_cut <- mean((mu_mci$chla_corr)) + 3 * sd((mu_mci$chla_corr))
max_cut

# determine if normal
library(ggpubr)
ggqqplot(mu_mci$chla_corr)
ggqqplot(log(mu_mci$chla_corr))

shapiro.test(mu_mci$chla_corr) # non-normal
shapiro.test(log(mu_mci$chla_corr)) # even log transformed is non-normal

## IQR +- [IQR * 1.5]
q1 <- summary(mu_mci$chla_corr)[2]
q3 <- summary(mu_mci$chla_corr)[5]

c(q1 - (1.5 * (q3 - q1)), q3 + (1.5 * (q3 - q1)))

##
#mu_mci <- mu_mci[mu_mci$chla_corr <= 200, ]
'

## depth duplicates in n = 45 (same day, filters)
x5 <- c(1563,1564,1926,1927,5421,5422,6076,6077,8807,8929)

mu_mci_dups <- mu_mci[mu_mci$X.5 %in% x5, ]

mu_mci_dups[, 125:131]

index <- c(7:8)
mu_mci_dups$chla_corr[index]
mu_mci_dups$chla_s2[index]
#

## depth - doesn't improve validation
hist(mu_mci$depth_corr)
'sum(is.na(mu_mci$depth_corr) &
is.na(mu_mci$topdepth_corr) & 
is.na(mu_mci$botdepth_corr) & 
is.na(mu_mci$ActivityRelativeDepthName))'

mu_mci$surface <- "no"
mu_mci$surface[which(mu_mci$depth_corr <= 0.5 | mu_mci$ActivityRelativeDepthName == "Surface" | 
                       mu_mci$botdepth_corr <= 0.5)] <- "<= 0.5 m"
'mu_mci$surface[which((mu_mci$depth_corr > 0.5 & mu_mci$depth_corr <= 1) | 
(mu_mci$botdepth_corr > 0.5 & mu_mci$depth_corr <= 1))] <- "0.5 - 1 m"
mu_mci$surface[which((mu_mci$depth_corr > 1 & mu_mci$depth_corr <= 1.5) | 
(mu_mci$botdepth_corr > 1 & mu_mci$depth_corr <= 1.5))] <- "1 - 1.5 m"'
table(mu_mci$surface)
#mu_mci <- mu_mci[mu_mci$surface == "<= 0.5 m", ]



## To Do:

# average duplicates (including 2s)
# (investigate different days from current data)

# fix axis labels
# choose coefficients
# bootstrap?
# https://stackoverflow.com/questions/39646173/fitting-a-regression-line-to-graph-with-log-axes-in-r/40432168

# expand out to 10 days
# include estuaries (probably just same day, but no more than +/- 3 day)

# options(scipen = 1)
