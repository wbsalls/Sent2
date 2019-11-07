

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

setwd("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/681_imgs")

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


## remove MCI = 4.999496
max(mu_mci$MCI)
sum(mu_mci$MCI == max(mu_mci$MCI))
#mu_mci <- mu_mci[-which(mu_mci$MCI == max(mu_mci$MCI)), ] # why doesn't this work right???
mu_mci <- mu_mci[which(mu_mci$MCI < 4), ]


## remove MCI = 0
sum(mu_mci$MCI == 0)
mu_mci <- mu_mci[mu_mci$MCI != 0, ]


## method
table(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext)
method_sub <- "APHA" # APHA USEPA USGS
mu_mci <- mu_mci[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == method_sub), ]


## offset time
offset_min <- 0
offset_max <- 0
offset_threshold <- offset_min:offset_max
mu_mci <- mu_mci[mu_mci$offset_days %in% offset_threshold, ]


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


# choose which MCI-chla conversion to use *******
# using erie for now since ontario removes 28 additional negatives
chla_conv <- "s2_chl_ontario" # <<<** s2_chl_ontario, s2_chl_erie, s2_chl_lotw, s2_chl_mollaee, s2_chl_custom, s2_chl_split
mu_mci$chla_s2 <- mu_mci[, which(colnames(mu_mci) == chla_conv)] # select column for s2 chla
mu_mci$chla_conv <- chla_conv # specify which conversion used
mu_mci <- mu_mci[mu_mci$chla_s2 >= 0, ] # remove negatives


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

# assign cutoff
sed_cutoff <- -4 # Binding recommendation: retain only points that are > -0.15

mu_mci$sediment <- ""
mu_mci$sediment[mu_mci$mci_baseline_slope < sed_cutoff] <- "sediment"
mu_mci$sediment[mu_mci$mci_baseline_slope >= sed_cutoff] <- "no sediment flag"
table(mu_mci$sediment)
mu_mci$sediment <- factor(mu_mci$sediment, levels(factor(mu_mci$sediment))[c(2, 1)])

sprintf("%s/%s points retained (removing %s)", 
        sum(mu_mci$mci_baseline_slope > sed_cutoff), 
        nrow(mu_mci), nrow(mu_mci) - sum(mu_mci$mci_baseline_slope > sed_cutoff))

# apply cutoff
mu_mci <- mu_mci[mu_mci$mci_baseline_slope > sed_cutoff, ]

# ------

## season
mu_mci$month <- as.numeric(substr(mu_mci$samp_localTime, 2, 3))


# save
mu_mci_final <- mu_mci


### ---------------------------------------------------------------------------------------------------

# choose chla conversion
chl_file <- "ontario" # ontario, erie
mu_mci <- read.csv(sprintf("mu_mci_finalset_2019-08-22_s2_chl_%s.csv", chl_file), stringsAsFactors = FALSE)
mu_mci$month <- as.numeric(substr(mu_mci$samp_localTime, 2, 3))

# cut below 10?
#mu_mci <- mu_mci[mu_mci$chla_corr >= 10, ]

### validation plot  -----------------------------------------------------------------------------------

# reset
mu_mci <- mu_mci_final
mu_mci$pid <- 1:nrow(mu_mci) # for viewing point IDs

plot_error_metrics(x = mu_mci$chla_corr, y = mu_mci$chla_s2, # export 800 x 860; 600 x 645 for paper
                   xname = "in situ chlorophyll-a (ug/l)", 
                   yname = "S2-derived chlorophyll-a (ug/l)", 
                   #yname = "S2-derived chlorophyll-a (ug/l, from MCI using L1C reflectance)", 
                   #title = plot_title, 
                   #title = paste0(method_sub, ", ", plot_title), # if subsetting by method
                   equal_axes = TRUE, 
                   log_axes = "xy", # xy, x, y, ""
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
                   col = alpha("black", 0.4), 
                   pch = 20)

####


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

## single vs multi pixel
plot(mu_mci$chla_corr, mu_mci$MCI_BRR_1, 
     ylim = c(min(mu_mci$MCI_BRR_1, mu_mci$MCI_BRR_mean), max(mu_mci$MCI_BRR_1, mu_mci$MCI_BRR_meanr)))
text(x = mu_mci$chla_corr, y = mu_mci$MCI_BRR_1, labels = rownames(mu_mci))

plot(mu_mci$chla_corr, mu_mci$MCI_BRR_mean)
text(x = mu_mci$chla_corr, y = mu_mci$MCI_BRR_mean, labels = rownames(mu_mci),
     ylim = c(min(mu_mci$MCI_BRR_1, mu_mci$MCI_BRR_mean), max(mu_mci$MCI_BRR_1, mu_mci$MCI_BRR_meanr)))



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


## satellite ---------------
boxplot(error_chla_abs ~ SPACECRAFT_NAME, data = mu_mci,
        ylab = "chl a residual",
        xlab = "Satellite")
text(0.7, 150, paste0("n = ", table(mu_mci$SPACECRAFT_NAME)[1]))
text(1.7, 150, paste0("n = ", table(mu_mci$SPACECRAFT_NAME)[2]))
abline(v=0)






