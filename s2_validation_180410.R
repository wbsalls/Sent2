

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
#source("/Users/wilsonsalls/Desktop/Git/Sent2/error_metrics_1800611.R")

setwd("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/681_imgs")
#setwd("/Users/wilsonsalls/Desktop/EPA/Sentinel2/Validation/681_imgs")

opar <- par() # grab original par settings for plotting later

# MCI L1C

mu_mci_l1c <- read.csv("validation_S2_682imgs_MCI_L1C_2018-11-21.csv", stringsAsFactors = FALSE)

# rename MCI columns
colnames(mu_mci_l1c)[which(colnames(mu_mci_l1c) == "MCI_val_1")] <- "MCI_L1C_1"
colnames(mu_mci_l1c)[which(colnames(mu_mci_l1c) == "MCI_val_2")] <- "MCI_L1C_2"
colnames(mu_mci_l1c)[which(colnames(mu_mci_l1c) == "MCI_val_3")] <- "MCI_L1C_3"
colnames(mu_mci_l1c)[which(colnames(mu_mci_l1c) == "MCI_val_4")] <- "MCI_L1C_4"
colnames(mu_mci_l1c)[which(colnames(mu_mci_l1c) == "MCI_val_5")] <- "MCI_L1C_5"
colnames(mu_mci_l1c)[which(colnames(mu_mci_l1c) == "MCI_val_6")] <- "MCI_L1C_6"
colnames(mu_mci_l1c)[which(colnames(mu_mci_l1c) == "MCI_val_7")] <- "MCI_L1C_7"
colnames(mu_mci_l1c)[which(colnames(mu_mci_l1c) == "MCI_val_8")] <- "MCI_L1C_8"
colnames(mu_mci_l1c)[which(colnames(mu_mci_l1c) == "MCI_val_9")] <- "MCI_L1C_9"

# MCI BRR
mu_mci_brr <- read.csv("validation_S2_682imgs_MCI_BRR_2019-08-09.csv", stringsAsFactors = FALSE)
nrow(mu_mci_l1c) == nrow(mu_mci_brr) # confirm they're the same
mu_mci_brrONLY <- mu_mci_brr[which(!(mu_mci_brr$X.3 %in% mu_mci_l1c$X.3)), ]

# rename MCI columns
colnames(mu_mci_brr)[which(colnames(mu_mci_brr) == "MCI_val_1")] <- "MCI_BRR_1"
colnames(mu_mci_brr)[which(colnames(mu_mci_brr) == "MCI_val_2")] <- "MCI_BRR_2"
colnames(mu_mci_brr)[which(colnames(mu_mci_brr) == "MCI_val_3")] <- "MCI_BRR_3"
colnames(mu_mci_brr)[which(colnames(mu_mci_brr) == "MCI_val_4")] <- "MCI_BRR_4"
colnames(mu_mci_brr)[which(colnames(mu_mci_brr) == "MCI_val_5")] <- "MCI_BRR_5"
colnames(mu_mci_brr)[which(colnames(mu_mci_brr) == "MCI_val_6")] <- "MCI_BRR_6"
colnames(mu_mci_brr)[which(colnames(mu_mci_brr) == "MCI_val_7")] <- "MCI_BRR_7"
colnames(mu_mci_brr)[which(colnames(mu_mci_brr) == "MCI_val_8")] <- "MCI_BRR_8"
colnames(mu_mci_brr)[which(colnames(mu_mci_brr) == "MCI_val_9")] <- "MCI_BRR_9"

# check unique IDs
check_unique <- function(input_df) {
  for (c in 1:ncol(input_df)) {
    unique_status <- "ERROR"
    if (length(unique(input_df[, c])) == nrow(input_df)) {
      unique_status <- "*** unique*** "
      print(sprintf("col %s: %s - *** unique***", c, colnames(input_df)[c]))
    }
  }
}

check_unique(mu_mci_l1c)
check_unique(mu_mci_brr)
# X.3, X.4, X.5 are unique, but it seems X.5 has been mixed up a bit. 
# X.3 should be secure since it was the output of matchups (and what went into extraction)

# merge BRR with L1C, selecting only necessary columns from BRR
l1c <- mu_mci_l1c[, which(colnames(mu_mci_l1c) %in% c("X.3", "MCI_L1C_1", "MCI_L1C_2", "MCI_L1C_3", "MCI_L1C_4", "MCI_L1C_5",
                                                      "MCI_L1C_6", "MCI_L1C_7", "MCI_L1C_8", "MCI_L1C_9"))]
mu_mci <- merge(mu_mci_brr, l1c, by = "X.3", all.x = TRUE)

# initial plot
#plot(mu_mci$MCI_L1C_1, mu_mci$MCI_BRR_1, xlim = c(-0.05, 0.05), ylim = c(-0.05, 0.05))
#plot(mu_mci$chla_corr, mu_mci$MCI_BRR_1, xlim = c(0, 300), ylim = c(-0.05, 0.05))
#plot(mu_mci$chla_corr, mu_mci$MCI_L1C_1, xlim = c(0, 300), ylim = c(-0.05, 0.05))

# average 9-pixel window (L1C and BRR)
mci_val_colindex <- which(colnames(mu_mci) == "MCI_L1C_1"):which(colnames(mu_mci) == "MCI_L1C_9")
mu_mci$MCI_L1C_mean <- apply(mu_mci[, mci_val_colindex], 1, mean)

mci_val_colindex <- which(colnames(mu_mci) == "MCI_BRR_1"):which(colnames(mu_mci) == "MCI_BRR_9")
mu_mci$MCI_BRR_mean <- apply(mu_mci[, mci_val_colindex], 1, mean)


# choose which MCI to use *******
mci_type <- "MCI_BRR_1" # <<<** MCI_L1C_1 (single), MCI_L1C_mean, MCI_BRR_1 (single), MCI_BRR_mean
mu_mci$MCI <- mu_mci[, which(colnames(mu_mci) == mci_type)]
mu_mci$mci_type <- mci_type

#

# fix chron
mu_mci$samp_localTime <- chron(dates. = substr(mu_mci$samp_localTime, 2, 9), 
                               times. = substr(mu_mci$samp_localTime, 11, 18))
mu_mci$img_localTime <- chron(dates. = substr(mu_mci$img_localTime, 2, 9), 
                              times. = substr(mu_mci$img_localTime, 11, 18))
mu_mci$samp_localDate <- substr(mu_mci$samp_localTime, 2, 9)

mu_mci$offset_hrs <- as.numeric(mu_mci$samp_localTime - mu_mci$img_localTime) * 24


# remove duplicates: identify based on duplicated chlorophyll-a and MCI (L1C)
# most duplicates have 0 MCI_L1C
val_df <- data.frame(mu_mci$chla_corr, mu_mci$MCI, mu_mci$LatitudeMeasure, mu_mci$LongitudeMeasure, mu_mci$samp_localDate)
sum(duplicated(val_df))
val_df_dups <- val_df[duplicated(val_df), ]

mu_mci <- mu_mci[!duplicated(val_df), ]

# depth - remove NAs that were left
mu_mci <- mu_mci[-which(is.na(mu_mci$depth_corr) &
                          is.na(mu_mci$topdepth_corr) & 
                          is.na(mu_mci$botdepth_corr) & 
                          is.na(mu_mci$ActivityRelativeDepthName)), ]

# export points
'
lon <- mu_mci$LongitudeMeasure
lat <- mu_mci$LatitudeMeasure
mu_mci_pts_all <- SpatialPointsDataFrame(coords = matrix(c(lon, lat), ncol = 2), 
                                         mu_mci, proj4string = CRS("+init=epsg:4326"))
writeOGR(obj = mu_mci_pts_all[, -which(colnames(mu_mci) %in% c("samp_localTime", "img_localTime"))],
         dsn = "O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/681_imgs/geospatial", 
         layer = "xxxx",  driver = "ESRI Shapefile")

mu_mci_pts_0 <- mu_mci_pts_all[!is.na(mu_mci_pts_all$MCI) & mu_mci_pts_all$MCI == 0, ]
writeOGR(obj = mu_mci_pts_0, dsn = "O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/681_imgs/geospatial", 
         layer = "mu_mci_0",  driver = "ESRI Shapefile")
mu_mci_pts_499 <- mu_mci_pts_all[!is.na(mu_mci_pts_all$MCI) & mu_mci_pts_all$MCI == max(mu_mci$MCI, na.rm = TRUE), ]
writeOGR(obj = mu_mci_pts_499, dsn = "O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/681_imgs/geospatial", 
         layer = "mu_mci_499",  driver = "ESRI Shapefile")
'


# make copy
mu_mci_prefilter <- mu_mci


## cleaning ---------------------------------------------------------------------------------------
mu_mci <- mu_mci_prefilter # reset

## in situ chla
summary(mu_mci$chla_corr)
sum(mu_mci$chla_corr > 1000)
mu_mci <- mu_mci[mu_mci$chla_corr <= 1000, ]
'sum(mu_mci$chla_corr > 300)
mu_mci <- mu_mci[mu_mci$chla_corr <= 300, ]'


## remove NA MCI
sum(is.na(mu_mci$MCI))
mu_mci <- mu_mci[!is.na(mu_mci$MCI), ]

'
mu_mci$imgtype <- substr(mu_mci$PRODUCT_ID, 1, 10)
mu_mci_na <- mu_mci[is.na(mu_mci$MCI), ]
table(mu_mci_na$imgtype)
'

## remove MCI = 4.999496
max(mu_mci$MCI)
sum(mu_mci$MCI == max(mu_mci$MCI))
#mu_mci <- mu_mci[-which(mu_mci$MCI == max(mu_mci$MCI)), ] # why doesn't this work right???
mu_mci <- mu_mci[which(mu_mci$MCI < 4), ]

'
mu_mci_499 <- mu_mci[mu_mci$MCI == max(mu_mci$MCI), ]
table(mu_mci_499$imgtype)
plot(sort(mu_mci_499$chla_corr), ylab = "in situ chlorophyll-a (ug/l)")
plot(sort(mu_mci$chla_corr))
hist(mu_mci_499$chla_corr)
'

# plot raw MCI
#plot(mu_mci$chla_corr, mu_mci$MCI, xlab = "in situ chlorophyll-a (ug/l)", ylab = "MCI (Level 1C)")

## remove MCI = 0
sum(mu_mci$MCI == 0)
mu_mci <- mu_mci[mu_mci$MCI != 0, ]

'
mu_mci_0 <- mu_mci[mu_mci$MCI == 0, ]
table(mu_mci_0$imgtype)
plot(mu_mci_0$chla_corr, mu_mci_0$chla_s2)
summary(mu_mci_0$chla_corr)
summary(mu_mci$chla_corr)
'

## method
table(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext)
method_sub <- "APHA" # APHA USEPA USGS
mu_mci <- mu_mci[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == method_sub), ]


## offset time
offset_min <- 0
offset_max <- 0
offset_threshold <- offset_min:offset_max
mu_mci <- mu_mci[mu_mci$offset_days %in% offset_threshold, ]


if (offset_min == offset_max) {
  plot_title <- sprintf("+/- %s day", offset_min)
} else {
  plot_title <- sprintf("+/- %s-%s days", offset_min, offset_max)
}

## for reset
mu_mci_cleaned <- mu_mci


## calc chl a from MCI  ---------------------------------------------------------------------------------------------------

mu_mci <- mu_mci_cleaned

# calculate chla from various conversions

# 1a) binding ontario -------
slope.mci <- 0.0002 # from Binding et al. 2013 - ontario
intercept.mci <- -0.0012 # from Binding et al. 2013 - ontario
mu_mci$s2_chl_ontario <- (mu_mci$MCI - intercept.mci) / slope.mci

# 1b) binding erie -------
slope.mci <- 0.0004 # from Binding et al. 2013 - Erie
intercept.mci <- -0.0021 # from Binding et al. 2013 - Erie
mu_mci$s2_chl_erie <- (mu_mci$MCI - intercept.mci) / slope.mci

# 1c) binding lake of the woods -------
mu_mci$s2_chl_lotw <- exp((mu_mci$MCI + 0.017) / 0.0077)

# 2) Mollaee thesis (p 77) -------
'slope.mci <- 0.0001790292 # 1/2158 = 0.00046
intercept.mci <- -0.0018 # -3.9/2158 = -0.0018
mu_mci$s2_chl_mollaee <- (mu_mci$MCI - intercept.mci) / slope.mci'

# 3) custom -------
'slope.mci <- 0.0004 * 0.5 # from Binding et al. 2013 - Erie, times correction factor from slope
intercept.mci <- -0.0021 # from Binding et al. 2013 - Erie
mu_mci$s2_chl_custom <- (mu_mci$MCI - intercept.mci) / slope.mci'

# 4) cal-val split -------
'set.seed(1)
index.selected <- sample(1:nrow(mu_mci), size = floor(nrow(mu_mci) * 0.8), replace = FALSE)
mu_mci_calc <- mu_mci[index.selected, ] # select 80% for cal
mu_mci <- mu_mci[-index.selected, ] # retain remaining 20% for val
  
# fit linear model; set coefficients
model_s2chla <- lm(mu_mci_calc$MCI ~ mu_mci_calc$chla_corr)
  
slope.mci <- model_s2chla$coefficients[2]
intercept.mci <- model_s2chla$coefficients[1]
mu_mci$s2_chl_split <- (mu_mci$MCI - intercept.mci) / slope.mci'


# choose which MCI-chla conversion to use *******
# using erie for now since ontario removes 28 additional negatives
chla_conv <- "s2_chl_ontario" # <<<** s2_chl_ontario, s2_chl_erie, s2_chl_lotw, s2_chl_mollaee, s2_chl_custom, s2_chl_split
mu_mci$chla_s2 <- mu_mci[, which(colnames(mu_mci) == chla_conv)] # select column for s2 chla
mu_mci$chla_conv <- chla_conv # specify which conversion used
mu_mci <- mu_mci[mu_mci$chla_s2 >= 0, ] # remove negatives


sum(mu_mci$chla_s2 < 0)
sum(mu_mci$s2_chl_erie < 0)
sum(mu_mci$s2_chl_ontario < 0)
sum(mu_mci$s2_chl_erie[which(mu_mci$s2_chl_ontario < 0)] > 0)
sum(mu_mci$s2_chl_ontario[which(mu_mci$s2_chl_erie < 0)] > 0)

# remove negative S2 Chla
mu_mci <- mu_mci[mu_mci$chla_s2 >= 0, ] # remove negatives
#mu_mci[mu_mci$chla_s2 < 0, ] <- 0 # or set negatives to 0


# for reset
mu_mci_s2chla <- mu_mci

# ------------------------------------------------------------------


## integrate depth duplicates
# creates a new table with integrated records, and comments where integration occurred
# retains the old table with comments

# reset
mu_mci <- mu_mci_s2chla

# initiate
mu_mci$integration_comment <- ""
mu_mci_integrated <- data.frame()

#
for (r in 1:nrow(mu_mci)) {
  
  # skip if it's a duplicate that has already been handled
  if (grepl("REMOVED", mu_mci$integration_comment[r])) {
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
  
  # check if depth is the same; if not, skip
  if (length(unique(concurrent$depth_corr)) == 1 &
      length(unique(concurrent$topdepth_corr)) == 1 & 
      length(unique(concurrent$botdepth_corr)) == 1 & 
      length(unique(concurrent$ActivityRelativeDepthName)) == 1) {
    same_depth <- TRUE
  } else {
    mu_mci$integration_comment[r] <- paste0("*** duplicate with different depths - X.3: ", toString(concurrent$X.3))
    print(sprintf("*** SKIPPING... duplicate with different depths: mu_mci row #%s, X.3 %s", r, toString(concurrent$X.3)))
    mu_mci_integrated <- rbind(mu_mci_integrated, mu_mci[r, ])
    next
  }
  
  mu_mci_newrow <- mu_mci[r, ]
  
  ## integrating
  
  # diff in situ sample, same img
  if (length(unique(concurrent$bio_uniqueID)) > 1) {
    
    # as long as depth is the same, average in situ chl
    if (isTRUE(same_depth)) {
      
      mu_mci_newrow$chla_corr <- mean(concurrent$chla_corr) # set average chl
      
      mu_mci_newrow$integration_comment <- 
        sprintf("avged in situ chl; original values: %s; original X.3: %s. ", 
                toString(concurrent$chla_corr), toString(concurrent$X.3)) # add comment
      
      mu_mci$integration_comment[r] <- 
        sprintf("avged in situ chl; original values: %s; original X.3: %s. ", 
                toString(concurrent$chla_corr), toString(concurrent$X.3)) # add comment
      
      mu_mci$integration_comment[concurrent_index[2:length(concurrent_index)]] <- 
        sprintf("REMOVED; in situ duplicate with %s. ", concurrent$X.3[1]) # flag future obsevations to be skipped
    }
  }
  
  # diff imgs, same in situ sample
  if (length(unique(concurrent$GRANULE_ID)) > 1) {
    
    # as long as depth is the same, average S2 chl; otherwise, add to list to check
    if (isTRUE(same_depth)) {
      
      mu_mci_newrow$chla_s2 <- mean(concurrent$chla_s2) # set average chl
      
      mu_mci_newrow$integration_comment <- paste0(mu_mci_newrow$integration_comment,
                                                  sprintf("avged S2 chl; original values: %s; original X.3: %s.", 
                                                          toString(round(concurrent$chla_s2, 1)), toString(round(concurrent$X.3, 1)))) # add comment
      
      
      mu_mci$integration_comment[r] <- paste0(mu_mci$integration_comment[r],
                                              sprintf("avged S2 chl; original values: %s; original X.3: %s.", 
                                                      toString(round(concurrent$chla_s2, 1)), toString(round(concurrent$X.3, 1)))) # add comment
      
      mu_mci$integration_comment[concurrent_index[2:length(concurrent_index)]] <- 
        paste0(mu_mci$integration_comment[concurrent_index[2:length(concurrent_index)]], 
               sprintf("REMOVED; img duplicate with %s.", concurrent$X.3[1])) # flag future obsevations to be skipped
      
      # print alert if S2 chla values are different - may indicate cloud in one image
      if (sd(concurrent$chla_s2) > 1) {
        print(sprintf("*** S2 chla SD > 1!! CHECK %s ***", toString(concurrent$X.3)))
      }
    }
  }
  # add integrated row to new df
  mu_mci_integrated <- rbind(mu_mci_integrated, mu_mci_newrow) # add this row to output
}

# rename old and new tables
mu_mci_preintegrated <- mu_mci
mu_mci <- mu_mci_integrated
sprintf("%s duplicates removed", nrow(mu_mci_preintegrated) - nrow(mu_mci)) # show number of duplicates removed

# write out to csv for checking
#write.csv(mu_mci_preintegrated, sprintf("mu_mci_preintegrated_%s.csv", Sys.Date()))
#write.csv(mu_mci, sprintf("mu_mci_integrated_%s.csv", Sys.Date()))

# >>>> revise mu_mci_integrated.csv if needed; rename file: add "MANUAL" add end <<<<
# re-load revised data
#mu_mci <- read.csv("mu_mci_integrated_2019-xx-xx_MANUAL.csv", stringsAsFactors = FALSE)

# -----------------------------------------------------

# calculate error
mu_mci$error_chla <- (mu_mci$chla_s2 - mu_mci$chla_corr) # error
mu_mci$error_chla_abs <- abs(mu_mci$error_chla) # abs error
mu_mci$pct_error_chla <- ((mu_mci$chla_s2 - mu_mci$chla_corr) / mu_mci$chla_corr) * 100 # % error
mu_mci$pct_error_chla_abs <- abs(mu_mci$pct_error_chla) # abs error


chl_eutrFn <- function(x) {
  if (x < 2) {
    return("oligotrophic")
  } else if (x <= 7) {
    return("mesotrophic")
  } else if (x <= 30) {
    return("eutrophic")
  } else {
    return("hypereutrophic")
  }
}
mu_mci$chl_eutr <- sapply(mu_mci$chla_corr, chl_eutrFn)

# save mu_mci
mu_mci_prefilter <- mu_mci



## additional criteria ------------------------------------------------------------------------------------

# reset mu_mci
mu_mci <- mu_mci_prefilter

## remove bad points identified in imagery
length(mu_mci$X.3) == length(unique(mu_mci$X.3)) # checking if unique: yes

img_comments <- read.csv("ImageCheck_0day_comments_X3.csv", stringsAsFactors = FALSE)

sum(img_comments$X.3 %in% mu_mci$X.3)

mu_mci <- merge(mu_mci, img_comments[, which(colnames(img_comments) %in% c("X.3", "tier", "glint"))],
                by = "X.3", all.x = TRUE, all.y = FALSE)

sum(is.na(mu_mci$tier)) # should be 0
#boxplot(error_chla_abs ~ tier, data = mu_mci)

# investigate file to see make sure img comments weren't lost in duplicates
#write.csv(mu_mci, "mu_mci_imgComments.csv")

# apply removal
print(sprintf("removing %s bad imagery points", sum((mu_mci$tier %in% c("3", "x")))))
mu_mci <- mu_mci[-which(mu_mci$tier %in% c("3", "x")), ]
#


## shore dist
sum(mu_mci$dist_shore_m < 30) # how many?
mu_mci$pct_error_chla[mu_mci$dist_shore_m <= 30] # display error
mu_mci <- mu_mci[mu_mci$dist_shore_m >= 30, ] # remove


## sediment -------------
# merge raw band values table
raw_bands <- read.csv("mu_rawbands_3day_X3.csv", stringsAsFactors = FALSE)
sum(raw_bands$X.3 %in% mu_mci$X.3)
mu_mci <- merge(mu_mci, raw_bands, by = "X.3", all.x = TRUE)

# calculate slope
mu_mci$mci_baseline_slope <- (mu_mci$b6_1 - mu_mci$b4_1) / (740 - 655)
sum(is.na(mu_mci$mci_baseline_slope))

#plot(mu_mci$mci_baseline_slope, rep(1, nrow(mu_mci)))
#plot(mu_mci$mci_baseline_slope, mu_mci$error_chla)

# assign cutoff
sed_cutoff <- -2 # Binding recommendation: retain only points that are > -0.15

mu_mci$sediment <- ""
mu_mci$sediment[mu_mci$mci_baseline_slope < sed_cutoff] <- "sediment"
mu_mci$sediment[mu_mci$mci_baseline_slope >= sed_cutoff] <- "no sediment flag"
table(mu_mci$sediment)
mu_mci$sediment <- factor(mu_mci$sediment, levels(factor(mu_mci$sediment))[c(2, 1)])

sprintf("%s/%s points retained (removing %s)", 
        sum(mu_mci$mci_baseline_slope > -4), 
        nrow(mu_mci), nrow(mu_mci) - sum(mu_mci$mci_baseline_slope > -4))

# apply cutoff
mu_mci <- mu_mci[mu_mci$mci_baseline_slope > sed_cutoff, ]

# ------

## clouds
#mu_mci <- mu_mci[mu_mci$CLOUDY_PIXEL_PERCENTAGE == 0, ]

## season
mu_mci$month <- as.numeric(substr(mu_mci$samp_localTime, 2, 3))
#table(mu_mci$month)
#mu_mci <- mu_mci[mu_mci$month %in% 6:8, ]


#mu_mci <- mu_mci[which(!is.na(mu_mci$chla_corr)), ]

# write csv of final validation set
#write.csv(mu_mci, sprintf("mu_mci_finalset_%s_%s.csv", Sys.Date(), chla_conv))

# save
mu_mci_final <- mu_mci


### ---------------------------------------------------------------------------------------------------

# choose chla conversion
chl_file <- "ontario" # ontario, erie
mu_mci <- read.csv(sprintf("mu_mci_finalset_2019-08-22_s2_chl_%s.csv", chl_file), stringsAsFactors = FALSE)
mu_mci$month <- as.numeric(substr(mu_mci$samp_localTime, 2, 3))

# cut below 10?
mu_mci <- mu_mci[mu_mci$chla_corr >= 10, ]

### validation plot  -----------------------------------------------------------------------------------

# reset
mu_mci <- mu_mci_final
mu_mci$pid <- 1:nrow(mu_mci) # for viewing point IDs

#jpeg(sprintf("val_%s_%s.png", offset_min, offset_max), width = 800, height = 860)
plot_error_metrics(x = mu_mci$chla_corr, y = mu_mci$chla_s2, # export 800 x 860; 600 x 645 for paper
                   xname = "in situ chlorophyll-a (ug/l)", 
                   yname = "S2-derived chlorophyll-a (ug/l)", 
                   #yname = "S2-derived chlorophyll-a (ug/l, from MCI using L1C reflectance)", 
                   #title = plot_title, 
                   #title = paste0(method_sub, ", ", plot_title), # if subsetting by method
                   equal_axes = TRUE, 
                   log_axes = "", # xy, x, y, ""
                   log_space = T, # T, F
                   plot_abline = FALSE,
                   text_x = 0.04, # 0.04; min(mu_mci$chla_corr, mu_mci$chla_s2)
                   #text_y = ,
                   mape = FALSE,
                   rand_error = FALSE,
                   regr_stats = FALSE,
                   states = mu_mci$state,
                   lakes = mu_mci$comid,
                   xlim = c(0.04, max(mu_mci$chla_corr, mu_mci$chla_s2)), # 0.04 (min 0.05)
                   ylim = c(0.04, max(mu_mci$chla_corr, mu_mci$chla_s2)),
                   show_metrics = TRUE, 
                   #xaxt="n",
                   #yaxt="n",
                   col = alpha("black", 0.4), 
                   #col = mu_mci$sedimentf,
                   #col = mu_mci$state_col,
                   pch = 20)
cat(sprintf("S2 -> chl relationship: *** %s *** \nS2 regression slope = %s; intercept = %s \n%s images\n", 
            chla_conv,
            signif(slope.mci, digits = 2), signif(intercept.mci, digits = 2),
            length(unique(mu_mci$GRANULE_ID))))
#legend("bottomright", legend = unique(mu_mci$sedimentf), col = c("black", "red"), border = NULL)
#dev.off()
#mu_mci$pid <- 1:nrow(mu_mci)
#text(x = mu_mci$chla_corr, y = mu_mci$chla_s2, labels = mu_mci$pid)

####

# plot trophic categories
threshold_lty <- 2
abline(h = 2, lty = threshold_lty, col = "blue")
abline(v = 2, lty = threshold_lty, col = "blue")
abline(h = 7, lty = threshold_lty, col = "green")
abline(v = 7, lty = threshold_lty, col = "green")
abline(h = 30, lty = threshold_lty, col = "red")
abline(v = 30, lty = threshold_lty, col = "red")


# categorical by trophic state; confusion matrix
chl_eutrFn <- function(x) {
  if (x < 2) {
    return("oligotrophic")
  } else if (x <= 7) {
    return("mesotrophic")
  } else if (x <= 30) {
    return("eutrophic")
  } else {
    return("hypereutrophic")
  }
}

chl_eutrFn <- function(x) {
  if (x <= 7) {
    return("mesotrophic")
  } else if (x <= 30) {
    return("eutrophic")
  } else {
    return("hypereutrophic")
  }
}

mu_mci$chl_eutr <- sapply(mu_mci$chla_corr, chl_eutrFn)
mu_mci$s2_eutr <- sapply(mu_mci$chla_s2, chl_eutrFn)
sum(mu_mci$chl_eutr == mu_mci$s2_eutr)

mu_mci$chl_eutr <- factor(mu_mci$chl_eutr, 
                          levels = c("oligotrophic", "mesotrophic", "eutrophic", "hypereutrophic"))
mu_mci$s2_eutr <- factor(mu_mci$s2_eutr, 
                         levels = c("oligotrophic", "mesotrophic", "eutrophic", "hypereutrophic"))
mu_mci$chl_eutr <- factor(mu_mci$chl_eutr, 
                          levels = c("mesotrophic", "eutrophic", "hypereutrophic"))
mu_mci$s2_eutr <- factor(mu_mci$s2_eutr, 
                         levels = c("mesotrophic", "eutrophic", "hypereutrophic"))

library(caret)
confusionMatrix(data = mu_mci$s2_eutr, reference = mu_mci$chl_eutr)

# with adjustment based on regression slope
mu_mci$chla_s2_adj <- mu_mci$chla_s2 / 0.49
#mu_mci$chla_s2_adj <- mu_mci$chla_s2 / 1.2
#mu_mci$chla_s2 <- mu_mci$chla_s2_adj
mu_mci$chla_s2_adj_eutr <- sapply(mu_mci$chla_s2_adj, chl_eutrFn)
sum(mu_mci$chl_eutr == mu_mci$chla_s2_adj_eutr)


##

# color by state
mu_mci$state_col <- "black"
mu_mci$state_col[which(mu_mci$state == "CO")] <- "green"
mu_mci$state_col[which(mu_mci$state == "OK")] <- "red"
mu_mci$state_col[which(mu_mci$state == "WI")] <- "cyan"

legend(0.05, 10, legend=c("MN", "WI", "OK", "CO"),
       col=c("black", "cyan", "red", "green"), pch = 20)


#plot(mu_mci$chla_corr, mu_mci$chla_s2, xlim = c(0, 415), ylim = c(0, 415), xlab = "in situ chlorophyll-a (ug/l)", ylab = "S2-derived chlorophyll-a (from MCI L1C)")
#plot(mu_mci$chla_corr, mu_mci$error_chla)
#plot(mu_mci$chla_corr, mu_mci$pct_error_chla_abs)

# write image list
write.csv(unique(mu_mci[, c("PRODUCT_ID", "GRANULE_ID")]), "O:/PRIV/NERL_ORD_CYAN/Sentinel2/Images/composited/0day/img_list.csv")

# subset by CV  (uses L1C; can sub in BRR) -------------------------------------------
mci_val_colindex <- which(colnames(mu_mci) == "MCI_L1C_1"):which(colnames(mu_mci) == "MCI_L1C_9")

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



# investigate patterns ---------------------------------------------------------------------------------------------------------
par()$mfrow
par(mfrow = c(2,1))
par(mfrow = c(1,1))

## residual vs. in situ value - what's happening with points above 1:1 line?
plot(mu_mci$chla_corr, mu_mci$error_chla, xlab = "in situ chlorophyll-a (ug/l)", ylab = "chl a error (ug/L)")

## check high error
mu_mci_sort <- mu_mci[order(-mu_mci$error_chla), ]
mu_mci_sort <- mu_mci[order(-mu_mci$pct_error_chla_abs), ]
mu_mci_sort[1:20, c(185, 191:194)] # this no longer works right - what's it supposed to be??


## depth --------------------------
mu_mci$depth_m_stdized <- NA

sum(mu_mci$ActivityRelativeDepthName == "Surface", na.rm = TRUE)
mu_mci$depth_m_stdized[which(mu_mci$ActivityRelativeDepthName == "Surface")] <- 0

sum(!is.na(mu_mci$depth_corr))
mu_mci$depth_m_stdized[which(!is.na(mu_mci$depth_corr))] <- 
  mu_mci$depth_corr[which(!is.na(mu_mci$depth_corr))]

sum(!is.na(mu_mci$topdepth_corr))
mu_mci$depth_m_stdized[which(!is.na(mu_mci$topdepth_corr))] <- 
  apply(data.frame(topd = mu_mci$topdepth_corr[which(!is.na(mu_mci$topdepth_corr))],
                   botd = mu_mci$botdepth_corr[which(!is.na(mu_mci$topdepth_corr))]), 1, mean)

plot(mu_mci$depth_m_stdized, mu_mci$chla_corr)

# depth columns
#ActivityRelativeDepthName
#depth_corr
#topdepth_corr
#botdepth_corr

"ActivityRelativeDepthName"
"ActivityDepthHeightMeasure.MeasureValue"
"ActivityDepthHeightMeasure.MeasureUnitCode"
"ActivityTopDepthHeightMeasure.MeasureValue"
"ActivityTopDepthHeightMeasure.MeasureUnitCode"
"ActivityBottomDepthHeightMeasure.MeasureValue"
"ActivityBottomDepthHeightMeasure.MeasureUnitCode"


## sediment -------------------------------
table(mu_mci$sediment)

plot(mu_mci$mci_baseline_slope, mu_mci$pct_error_chla, 
     pch = 20, xlab = "MCI baseline slope", ylab = "S2 chl-a % error")
abline(v = -4, lty = 3)
abline(h=0)
#abline(v=-0.15)
# 800 x 600 plot

boxplot(pct_error_chla ~ sediment, data = mu_mci,
        xlab = "baseline slope (low indicates sediment)",
        ylab = "chl a residual")
text(1, 80, sprintf("n = %s", table(mu_mci$sediment)[1]))
text(2, 80, sprintf("n = %s", table(mu_mci$sediment)[2]))


## shore dist ----------------------------------
plot(mu_mci$dist_shore_m, mu_mci$error_chla, 
     xlim = c(0, 2000), # try removing this too
     #ylim = c(0, 200),
     xlab = "distance from shore (m)",
     ylab = "chl-a error (ug/L)",
     pch = 20,
     col = alpha("black", alpha = 0.4))
abline(v = 30, lty = 2)
#rect(0, 0, 30, 350, border = NULL, col = alpha("orange", alpha = 0.5))

## boxplots
max_depth_boxplot <- 1400
slice_boxplot <- 50
mu_mci$dist_shore_m_interval <- cut(mu_mci$dist_shore_m, seq(0, max_depth_boxplot, slice_boxplot))

# error vs dist
par(mfrow = c(2,1))
#layout(matrix(c(1,2,2), nrow = 4, ncol = 1, byrow = TRUE))
par(mar = c(0.5, 4.1, 10, 2.1)) # par(mar = c(bottom, left, top, right))
barplot(table(mu_mci$dist_shore_m_interval), ylab = "freq.", names.arg = FALSE)
#barplot(table(mu_mci$dist_shore_m_interval), xlab = NULL, ylab = NULL, xaxt = 'n', yaxt = 'n')
par(mar = c(5.1, 4.1, 0.5, 2.1))
boxplot(mu_mci$error_chla_abs ~ dist_shore_m_interval, data = mu_mci,
        las = 3,
        xaxt = 'n',
        xlab = "distance from shore (m)",
        ylab = "S2 chl-a % error")
axis(side = 1, las = 3,
     at = seq(from = 0.5, to = max_depth_boxplot / slice_boxplot + 0.5, by = 1), 
     labels = c(rbind(seq(from = 0, to = max_depth_boxplot, by = slice_boxplot * 2), ""))[1:(max_depth_boxplot / slice_boxplot + 1)])
par(opar)

# chla vs dist
par(mfrow = c(2,1))
#layout(matrix(c(1,2,2), nrow = 4, ncol = 1, byrow = TRUE))
par(mar = c(0.5, 4.1, 10, 2.1)) # par(mar = c(bottom, left, top, right))
barplot(table(mu_mci$dist_shore_m_interval), ylab = "freq", names.arg = FALSE)
#barplot(table(mu_mci$dist_shore_m_interval), xlab = NULL, ylab = NULL, xaxt = 'n', yaxt = 'n')
par(mar = c(5.1, 4.1, 0.5, 2.1))
boxplot(mu_mci$chla_corr ~ dist_shore_m_interval, data = mu_mci,
        las = 3,
        xaxt = 'n',
        xlab = "distance from shore (m)",
        ylab = "in situ chl-a (ug/l)")
axis(side = 1, las = 3,
     at = seq(from = 0.5, to = max_depth_boxplot / slice_boxplot + 0.5, by = 1), 
     labels = c(rbind(seq(from = 0, to = max_depth_boxplot, by = slice_boxplot * 2), ""))[1:(max_depth_boxplot / slice_boxplot + 1)])
par(opar)


## image characteristics -------------------------

## solar angle ------------------

# error vs angle - YES
plot(mu_mci$MEAN_SOLAR_ZENITH_ANGLE, mu_mci$error_chla_abs)
plot(mu_mci$MEAN_SOLAR_ZENITH_ANGLE, mu_mci$pct_error_chla_abs)
summary(mu_mci$MEAN_SOLAR_ZENITH_ANGLE)

box_min <- 21
box_max <- 45
box_step <- 4
mu_mci$solar_angle_interval <- cut(mu_mci$MEAN_SOLAR_ZENITH_ANGLE, seq(box_min, box_max, box_step))

par(mfrow = c(2,1), cex = 1, mgp = c(2, 0.6, 0)) #
par(mar = c(2, 4.1, 5, 2.1)) # par(mar = c(bottom, left, top, right))
barplot(table(mu_mci$solar_angle_interval), xlab = NULL, ylab = "freq.", xaxt = 'n') # ylab = NULL, yaxt = 'n'
par(mar = c(3, 4.1, 0, 2.1)) # par(mar = c(bottom, left, top, right))
boxplot(pct_error_chla_abs ~ solar_angle_interval, data = mu_mci,
        las = 3,
        xaxt = 'n',
        xlab = "Solar Angle",
        ylab = "chl-a % error")
axis(side = 1, las = 1,
     at = seq(from = 0.5, to = length(seq(box_min, box_max, box_step)) - 0.5, by = 1), 
     seq(box_min, box_max, box_step))
par(opar)



# combo plot: chla, solar angle ~ month - YES
par(mfrow = c(4,1), cex = 1, mgp = c(2, 0.6, 0)) #
par(mar = c(0, 4.1, 2, 2.1)) # par(mar = c(bottom, left, top, right))
barplot(table(mu_mci$month), xlab = NULL, ylab = "freq.", xaxt = 'n') # ylab = NULL, yaxt = 'n'
par(mar = c(1, 4.1, 2, 2.1)) # par(mar = c(bottom, left, top, right))
boxplot(pct_error_chla_abs ~ month, data = mu_mci,
        las = 3,
        xaxt = 'n',
        xlab = NULL,
        ylab = "chl-a % error")
boxplot(chla_corr ~ month, data = mu_mci,
        las = 3,
        xaxt = 'n',
        xlab = NULL,
        ylab = "chl-a (ug/l)")
par(mar = c(3, 4.1, 0, 2.1)) # par(mar = c(bottom, left, top, right))
boxplot(MEAN_SOLAR_ZENITH_ANGLE ~ month, data = mu_mci,
        las = 3,
        xaxt = 'n',
        xlab = "Month",
        ylab = "Solar Angle")
axis(side = 1, las = 1,
     at = seq(from = 1, to = length(seq(min(mu_mci$month), max(mu_mci$month), 1)), by = 1), 
     seq(min(mu_mci$month), max(mu_mci$month), 1))
par(opar) # 5.1 4.1 4.1 2.1



# angle vs month - (YES)
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

## sensor angle ---------
plot(mu_mci$MEAN_INCIDENCE_ZENITH_ANGLE_B4, mu_mci$error_chla)

# boxplot  - error vs angle - NO
summary(mu_mci$MEAN_INCIDENCE_ZENITH_ANGLE_B4)
box_min <- 2
box_max <- 11
box_step <- 1
mu_mci$sensor_angle_interval <- cut(mu_mci$MEAN_INCIDENCE_ZENITH_ANGLE_B4, seq(box_min, box_max, box_step))

par(mfrow = c(2,1))
barplot(table(mu_mci$sensor_angle_interval), xlab = NULL, ylab = "frequency", xaxt = 'n') # ylab = NULL, yaxt = 'n'
boxplot(error_chla_abs ~ sensor_angle_interval, data = mu_mci,
        las = 3,
        xaxt = 'n',
        xlab = "Sensor Zenith Angle",
        ylab = "chl a error (ug/l)")
axis(side = 1, las = 3,
     at = seq(from = 0.5, to = length(seq(box_min, box_max, box_step)) - 0.5, by = 1), 
     seq(box_min, box_max, box_step))
par(mfrow = c(1,1))


## month ----------------------------------
plot(mu_mci$month, mu_mci$pct_error_chla_abs)
plot(mu_mci$month, mu_mci$error_chla)

# boxplot - error vs month - (YES)
par(mfrow = c(2,1))
barplot(table(mu_mci$month), xlab = NULL, ylab = "frequency", xaxt = 'n')
boxplot(error_chla_abs ~ month, data = mu_mci,
        las = 3,
        xaxt = 'n',
        xlab = "month",
        ylab = "chl a error (ug/l)")
axis(side = 1,
     at = seq(from = 1, to = length(unique(mu_mci$month)), by = 1), 
     labels = sort(unique(mu_mci$month)))
par(mfrow = c(1,1))

# boxplot - chla value vs month - NO
par(mfrow = c(2,1))
barplot(table(mu_mci$month), xlab = NULL, ylab = "frequency", xaxt = 'n')
boxplot(chla_corr ~ month, data = mu_mci,
        las = 3,
        xaxt = 'n',
        xlab = "month",
        ylab = "in situ chl a (ug/l)")
axis(side = 1,
     at = seq(from = 1, to = length(unique(mu_mci$month)), by = 1), 
     labels = sort(unique(mu_mci$month)))
par(mfrow = c(1,1))


## satellite ---------------
boxplot(error_chla_abs ~ SPACECRAFT_NAME, data = mu_mci,
        ylab = "chl a residual",
        xlab = "Satellite")
text(0.7, 150, paste0("n = ", table(mu_mci$SPACECRAFT_NAME)[1]))
text(1.7, 150, paste0("n = ", table(mu_mci$SPACECRAFT_NAME)[2]))
abline(v=0)


## glint ---------
boxplot(error_chla ~ glint, data = mu_mci,
        ylab = "chl-a residual",
        xlab = "Satellite")



## offset days ---------------
plot(mu_mci$offset_days, mu_mci$error_chla)
plot(mu_mci$offset_hrs, mu_mci$error_chla)

plot(abs(mu_mci$offset_hrs), mu_mci$error_chla, 
     #ylim = c(0, 200),
     xlab = "time offset (hours)",
     ylab = "chl a residual (ug/L)",
     pch = 20,
     col = alpha("black", alpha = 0.4))

par(mfrow = c(2,1))
barplot(table(mu_mci$offset_days), xlab = "offset days", ylab = "frequency")
boxplot(error_chla ~ offset_days, data = mu_mci,
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
boxplot(error_chla ~ offset_hrs_day_interval, data = mu_mci,
        xlab = "offset hour of day",
        ylab = "chl a residual")

# quadratic model
hrs <- mu_mci_hrs$offset_hrs_day
hrs2 <- mu_mci_hrs$offset_hrs_day ^ 2
qmod <- lm(mu_mci_hrs$error_chla ~ hrs + hrs2)
summary(qmod)

timevalues <- seq(-12, 12, 0.1)
predictedcounts <- predict(qmod, list(hrs=timevalues, hrs2=timevalues^2))

plot(mu_mci_hrs$offset_hrs_day, mu_mci_hrs$error_chla, xlim = c(-12, 12))
lines(timevalues, predictedcounts, col = "darkgreen", lwd = 3)


## method ---------------
boxplot(error_chla ~ ResultAnalyticalMethod.MethodIdentifierContext, data = mu_mci,
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
boxplot(error_chla ~ dist_shore_m_interval, data = mu_mci[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == "APHA"), ],
        xlab = "distance from shore (m)",
        ylab = "chl a residual")

barplot(table(mu_mci[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == "USEPA"), ]$dist_shore_m_interval), xlab = "distance from shore (m)", ylab = "frequency", main = "USEPA")
boxplot(error_chla ~ dist_shore_m_interval, data = mu_mci[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == "USEPA"), ],
        xlab = "distance from shore (m)",
        ylab = "chl a residual")

barplot(table(mu_mci[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == "USGS"), ]$dist_shore_m_interval), xlab = "distance from shore (m)", ylab = "frequency", main = "USGS")
boxplot(error_chla ~ dist_shore_m_interval, data = mu_mci[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == "USGS"), ],
        xlab = "distance from shore (m)",
        ylab = "chl a residual")

mu_method <- mu_mci[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == "APHA"), ]
plot(mu_method$dist_shore_m, mu_method$error_chla)

m_usgs <- lm(error_chla ~ dist_shore_m, mu_usgs)


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

vioplot(mu_mci$error_chla[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == factor_vals[1])],
        mu_mci$error_chla[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == factor_vals[2])],
        mu_mci$error_chla[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == factor_vals[3])],
        mu_mci$error_chla[which(is.na(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext))],
        names = c(
          paste0(factor_vals[1], "\n", factor_tab[1]),
          paste0(factor_vals[2], "\n", factor_tab[2]),
          paste0(factor_vals[3], "\n", factor_tab[3]),
          "NA\n10"),
        col = "lightblue")



## other validation plotting ---------------------------------------------------------------------------------------

# mci vs chl-a
'
l1c <- calc_error_metrics(mu_mci$chla_corr, mu_mci$MCI)
l1c
#plot_error_metrics(x = mu_mci$chla_corr, y = mu_mci$MCI, 
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
#plot(mu_mci$chla_corr, mu_mci$MCI, col = mu_mci$offset_days_factor)
#plot(mu_mci$chla_corr, mu_mci$MCI, col = rainbow(mu_mci$offset_days_factor))
#legend(10, 0.045, levels(mu_mci$offset_days_factor), col = rainbow(1:length(mu_mci$offset_days_factor)), pch=1)

#
library(ggplot2)
qplot(mu_mci$chla_corr, mu_mci$MCI, col = mu_mci$offset_days_factor)

#
plot(mu_mci$chla_corr, mu_mci$MCI, col = topo.colors(n = 11, alpha = 0.5), pch = 16, xlim = c(0, 210))
legend(195, 0.045, levels(mu_mci$offset_days_factor), col = topo.colors(11, alpha = 0.5), pch = 16)
plot(mu_mci$chla_corr[mu_mci$offset_days %in% 0:3], mu_mci$MCI[mu_mci$offset_days %in% 0:3], 
     col = topo.colors(n = 11, alpha = 0.5), pch = 16, xlim = c(0, 210)) # doesn't work


#### scraps




## same MCI values
mci_freq <- as.data.frame(table(mu_mci$MCI))
mci_freq <- mci_freq[order(-mci_freq$Freq)[1:30], ]

# these sets are from the same location around the same time (therefore same images) but different in situ chl-a
# looks like intensive sampling events - keep?
mci_0117 <- mu_mci[round(mu_mci$MCI, 4) == 0.0117, ]
mci_0100 <- mu_mci[round(mu_mci$MCI, 5) == 0.01005, ]

for (i in 1:nrow(mci_freq)) {
  mu_mci_value <- mu_mci[round(mu_mci$MCI, 6) == round(Var1), ]
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
