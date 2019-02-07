

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

source("C:/Users/WSalls/Desktop/Git/Sent2/error_metrics_1800611.R")
#source("/Users/wilsonsalls/Desktop/Git/Sent2/error_metrics_1800611.R")

setwd("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/681_imgs")
#setwd("/Users/wilsonsalls/Desktop/EPA/Sentinel2/Validation/681_imgs")

#mu_mci_raw <- mu_mci
mu_mci_raw <- read.csv("validation_S2_682imgs_MCI_L1C_2018-11-21.csv", stringsAsFactors = FALSE)


# rename MCI column
colnames(mu_mci_raw)[which(colnames(mu_mci_raw) == "MCI_val_1")] <- "MCI_L1C"

# remove duplicates: identify based on duplicated chlorophyll-a and MCI (L1C)
val_df <- data.frame(mu_mci_raw$chla_corr, mu_mci_raw$MCI_L1C, mu_mci_raw$LatitudeMeasure, mu_mci_raw$LongitudeMeasure)
sum(duplicated(val_df))
val_df_dups <- val_df[duplicated(val_df), ]

mu_mci <- mu_mci_raw[!duplicated(val_df), ]

# most duplicates have 0 MCI_L1C

#* fix chron
mu_mci$samp_localTime <- chron(dates. = substr(mu_mci$samp_localTime, 2, 9), 
                               times. = substr(mu_mci$samp_localTime, 11, 18))
mu_mci$img_localTime <- chron(dates. = substr(mu_mci$img_localTime, 2, 9), 
                              times. = substr(mu_mci$img_localTime, 11, 18))

mu_mci$offset_hrs <- as.numeric(mu_mci$samp_localTime - mu_mci$img_localTime) * 24

#

## calc chl a from MCI  ---------------

slope.mci <- 0.0004 # from Binding et al. 2013 - Erie
intercept.mci <- -0.0021 # from Binding et al. 2013 - Erie

mu_mci$chla_s2 <- (mu_mci$MCI_L1C - intercept.mci) / slope.mci

mu_mci$residual_chla <- abs(mu_mci$chla_s2 - mu_mci$chla_corr) # residual
mu_mci$pct_error_chla <- (abs(mu_mci$chla_s2 - mu_mci$chla_corr) / mu_mci$chla_corr) * 100 # % error

mu_mci_presubset <- mu_mci


## subset ---------------------------------------------------------------------------------------
mu_mci <- mu_mci_presubset

# remove land-adjacent
#mu_mci <- mu_mci[mu_mci$dist_shore_m >= 30, ]

# remove NAs
sum(is.na(mu_mci$MCI_L1C))
mu_mci <- mu_mci[!is.na(mu_mci$MCI_L1C), ]

# remove MCI = 4.999
max(mu_mci$MCI_L1C)
sum(mu_mci$MCI_L1C == max(mu_mci$MCI_L1C))
'mu_mci_499 <- mu_mci[mu_mci$MCI_L1C == max(mu_mci$MCI_L1C), ]
plot(sort(mu_mci_499$chla_corr), ylab = "in situ chlorophyll-a (ug/l)")
plot(sort(mu_mci$chla_corr))
hist(mu_mci_499$chla_corr)'

mu_mci <- mu_mci[mu_mci$MCI_L1C != max(mu_mci$MCI_L1C), ]

# plot raw MCI
#plot(mu_mci$chla_corr, mu_mci$MCI_L1C, xlab = "in situ chlorophyll-a (ug/l)", ylab = "MCI (Level 1C)")
#

# remove 0s (NA MCI_L1C)
sum(mu_mci$MCI_L1C == 0)

'sum(mu_mci$MCI_L1C == 0)
mu_mci_0 <- mu_mci[mu_mci$MCI_L1C == 0, ]
mu_mci_0 <- mu_mci_0[mu_mci_0$chla_corr < 200, ]
plot(mu_mci_0$chla_corr, mu_mci_0$chla_s2)
hist(mu_mci_0$chla_corr) # slightly more right-skewed than hist with all data, so 0 values tend to have lower in situ chl (good)
hist(mu_mci$chla_corr)'


mu_mci <- mu_mci[mu_mci$MCI_L1C != 0, ]

## remove negatives
sum(mu_mci$chla_s2 < 0)
mu_mci <- mu_mci[mu_mci$chla_s2 > 0, ] # remove calculated negatives -617 (depends on coefficients used)

min(mu_mci$MCI_L1C)
sum(mu_mci$MCI_L1C < -0.01)
mu_mci <- mu_mci[mu_mci$MCI_L1C > -0.01, ] # not relevant if removing negative S2 chl since intercept is -0.0021

# plot raw S2 chla
#plot(mu_mci$chla_corr, mu_mci$chla_s2, ylim = c(0, 200), xlab = "in situ chlorophyll-a (ug/l)", ylab = "S2-derived chlorophyll-a (from MCI L1C)")

# export filtered validation data set
#write.csv(mu_mci, sprintf("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/682_imgs/validation_S2_682imgs_MCI_Chla_filtered_%s.csv", Sys.Date()))
mu_mci_filtered <- mu_mci # for resetting data

#-------------------------------------

# subset by offset time ------------
mu_mci <- mu_mci_filtered # reset
offset_min <- 0
offset_max <- 3
offset_threshold <- offset_min:offset_max
mu_mci <- mu_mci[mu_mci$offset_days %in% offset_threshold, ]

# subset by CV -------------------
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

# subset by method  -------------------
#mu_mci <- mu_mci_filtered
method_sub <- "APHA" # APHA USEPA USGS
mu_mci <- mu_mci[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == method_sub), ]

# shore dist -------------------
mu_mci <- mu_mci[mu_mci$dist_shore_m > 30, ]

# clouds -------------------
#mu_mci <- mu_mci[mu_mci$CLOUDY_PIXEL_PERCENTAGE == 0, ]

# depth - remove 
mu_mci <- mu_mci[-which(is.na(mu_mci$depth_corr) &
                    is.na(mu_mci$topdepth_corr) & 
                    is.na(mu_mci$botdepth_corr) & 
                    is.na(mu_mci$ActivityRelativeDepthName)), ]

### validation plot  ----------------------------------

col_plot <- alpha("black", 0.3)
pch_plot <- 20
#jpeg(sprintf("val_%s_%s.png", offset_min, offset_max), width = 800, height = 860)
if (offset_min == offset_max) {
  plot_title <- sprintf("+/- %s day", offset_min)
} else {
  plot_title <- sprintf("+/- %s-%s days", offset_min, offset_max)
}
plot_error_metrics(x = mu_mci$chla_corr, y = mu_mci$chla_s2, # export 800 x 860
                   xname = "in situ chlorophyll-a (ug/l)", 
                   yname = "S2-derived chlorophyll-a (from MCI L1C)", 
                   title = plot_title, 
                   #title = paste0(method_sub, ", ", plot_title), # if subsetting by method
                   equal_axes = TRUE, 
                   log_axes = "xy", 
                   log_space = FALSE,
                   plot_abline = FALSE,
                   rsq = TRUE,
                   states = mu_mci$state,
                   lakes = mu_mci$comid,
                   xlim = c(0.05, 200),
                   ylim = c(0.05, 200),
                   xaxt="n",
                   yaxt="n",
                   col = col_plot, pch = pch_plot) # col = alpha("black", 0.3), pch = 20
axis(1, at = c(10^(-1:3)), labels = c(10^(-1:3)))
axis(2, at = c(10^(-1:3)), labels = c(10^(-1:3)))
#dev.off()

threshold_lty <- 2
abline(h = 2, lty = threshold_lty, col = "blue")
abline(h = 7, lty = threshold_lty, col = "green")
abline(h = 30, lty = threshold_lty, col = "red")
abline(v = 2, lty = threshold_lty, col = "blue")
abline(v = 7, lty = threshold_lty, col = "green")
abline(v = 30, lty = threshold_lty, col = "red")

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

mu_mci <- mu_mci_filtered # reset

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
plot(mu_mci$chla_corr[mu_mci$offset_days %in% 0:3], mu_mci$MCI_L1C[mu_mci$offset_days %in% 0:3], col = topo.colors(n = 11, alpha = 0.5), pch = 16, xlim = c(0, 210)) # doesn't work




# investigate patterns ---------------------------------------------------------------------------
par()$mfrow
par(mfrow = c(2,1))
par(mfrow = c(1,1))

## residual vs. in situ value *************   interesting!   *******
plot(mu_mci$chla_corr, mu_mci$residual_chla, xlab = "in situ chlorophyll-a (ug/l)", ylab = "chl a error (ug/L)")

## check high error
mu_mci_sort <- mu_mci[order(-mu_mci$residual_chla), ]
mu_mci_sort <- mu_mci[order(-mu_mci$pct_error_chla), ]
mu_mci_sort[1:20, c(185, 191:194)] # this no longer works right - what's it supposed to be??

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
plot(sort(mu_mci$chla_corr[mu_mci$chla_corr < 1000]), ylab = "in situ chlorophyll-a (ug/l)")
plot(sort(mu_mci_hi$chla_corr), ylab = "in situ chlorophyll-a (ug/l)")'

## outliers
'
library(ggpubr)

hist(log(mu_mci$chla_corr))
summary(log(mu_mci$chla_corr))

# SD * 3 - cant use since not normal (linear nor log)
min_cut <- mean(log(mu_mci$chla_corr)) - 3 * sd(log(mu_mci$chla_corr))
max_cut <- mean(log(mu_mci$chla_corr)) + 3 * sd(log(mu_mci$chla_corr))
exp(max_cut)

min_cut <- mean((mu_mci$chla_corr)) - 3 * sd((mu_mci$chla_corr))
max_cut <- mean((mu_mci$chla_corr)) + 3 * sd((mu_mci$chla_corr))
max_cut

library(ggpubr)
ggqqplot(mu_mci$chla_corr)
ggqqplot(log(mu_mci$chla_corr))

shapiro.test(mu_mci$chla_corr) # non-normal
shapiro.test(log(mu_mci$chla_corr)) # even log transformed is non-normal

# IQR * 1.5
(summary(mu_mci$chla_corr)[5] - summary(mu_mci$chla_corr)[2]) * 1.5

#mu_mci <- mu_mci[mu_mci$chla_corr <= 200, ]
'

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
