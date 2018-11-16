library(raster)
library(rgdal)
library(sp)
library(testit) # has_error


# query MCI values at in situ points ------------------------------------------------------------------------------------------

setwd("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation")

mu <- read.csv("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Matchups/out/Matchups_S2_chla_10day_filtered_2018-07-23.csv", stringsAsFactors = FALSE)
#mu <- read.csv("/Users/wilsonsalls/Desktop/EPA/S2/Validation/xxx", stringsAsFactors = FALSE)

# subset to same day (if desired)
#mu_sameday <- mu[mu$offset_days == "same day", ]

# set single-pixel extraction (FALSE) or 3x3 window extraction (TRUE)
window_extraction <- TRUE

# make spdf of matchups
lon <- mu$LongitudeMeasure # **
lat <- mu$LatitudeMeasure # **
mu_pts <- SpatialPointsDataFrame(coords = matrix(c(lon, lat), ncol = 2), 
                                 mu, proj4string = CRS("+init=epsg:4326"))

# append state name to each point
us <- readOGR("O:/PRIV/NERL_ORD_CYAN/Salls_working/geospatial_general/US", "cb_2015_us_state_20m")
#us <- readOGR("/Users/wilsonsalls/Desktop/EPA/geosp_general/cb_2016_us_state_20m", "cb_2016_us_state_20m")
mu_pts_states <- spTransform(mu_pts, crs(us))
mu_pts_states$state <- over(mu_pts_states, us)$STUSPS

# append COMID to each point
lakes <- readOGR("O:/PRIV/NERL_ORD_CYAN/Salls_working/geospatial_general/resolvableLakes/geospatial", "resolvableLakes90m")
#lakes <- readOGR("/Users/wilsonsalls/Desktop/EPA/geosp_general/resolvableLakes/intermediate", "resolvableLakes90m")
mu_pts_lakes <- spTransform(mu_pts_states, crs(lakes))
mu_pts_lakes$comid <- over(mu_pts_lakes, lakes)$COMID

mu_pts <- mu_pts_lakes

# write to shapefile, csv
'
mu_pts$modeled_MCI_L1C <- (mu_pts$MCI_L1C + 0.00357) / 0.000222 # chla = 4504.5 * MCI + 16.08 {L1C, extreme negs removed}
#mu_pts$modeled_MCI_L1C <- (mu_pts$MCI_L1C + 0.0054) / 0.000232 # chla = 4310.3 * MCI + 23.27 {L1C, all}
mu_pts$residual_MCI_L1C <- abs(mu_pts$modeled_MCI_L1C - mu_pts$chla_corr)
mu_pts_brief <- mu_pts[, c(9, 60, 6, 194, 195, 188, 196, 197)] # take only relevant columns
mu_pts_brief <- mu_pts_brief[!is.na(mu_pts_brief$MCI_L1C), ] # remove NA
mu_pts_brief <- mu_pts_brief[mu_pts_brief$MCI_L1C == 0, ] # remove bad L1C (0)
mu_pts_brief$uniqueID <- 1:nrow(mu_pts_brief@data)
mu_pts_brief@data <- mu_pts_brief@data[, c(9, 1:8)]
writeOGR(mu_pts_brief, "O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/geospatial", "matchups_S2_chla", driver = "ESRI Shapefile")
write.csv(mu_pts_brief@data, "O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/validation_S2_50imgs_brief.csv")
'

# location of images
rfolder <- "D:/s2/mci_resample20"
#rfolder <- "/Users/wilsonsalls/Desktop/EPA/S2/Images"
safe_folder <- "D:/s2/raw"

# for naming MCI output column
process_name <- "MCI_L1C"

# check image name matching
mci_imgs <- list.files(rfolder, pattern = "\\.data")
#mci_img_names <- substr(mci_imgs, 16, 75)
mci_img_names <- gsub("mci_resample20_", "", mci_imgs)
mci_img_names <- gsub(".data", "", mci_img_names)
length(unique(mu_pts$PRODUCT_ID))
length(mci_img_names)
sum(mci_img_names %in% mu_pts$PRODUCT_ID)
sum(unique(mu_pts$PRODUCT_ID) %in% mci_img_names)


## ----------------------------------------------------


# run extraction
start_date <- Sys.Date()
print(Sys.time())
for (i in 1:length(mci_imgs)) {
  
  # initialize variable for use below
  notes <- ""
  n_cloud_layers <- 0
  
  # subset points to those in this image
  mu_pts_img <- mu_pts[mu_pts$PRODUCT_ID == sub(".data", "", sub("mci_resample20_", "", mci_imgs[i])), ]
  
  # progress
  print(sprintf("image #%s of %s at %s - %s pts", i, length(mci_imgs), Sys.time(), nrow(mu_pts_img)))
  
  # load raster
  
  # if mci raster doesn't exist, note in summary file and skip to next
  if (!("MCI.img" %in% list.files(file.path(rfolder, mci_imgs[i])))) {
    print("NO MCI IMAGE!!!")
    img_summary_i <- data.frame(img = mci_imgs[i], 
                                n_cloud_layers = NA, 
                                notes = "NO MCI RASTER; ", 
                                n_pts_tot = nrow(mu_pts_img@data), 
                                n_pts_cloudy = 0, 
                                n_pts_noncloudy = 0)
    
    if (has_error(read.csv(sprintf("validation_S2_682imgs_%s_img_summary_%s.csv", process_name, Sys.Date())))) {
      write.table(img_summary_i, sprintf("validation_S2_682imgs_%s_img_summary_%s.csv", process_name, start_date),
                  sep = ",", append = FALSE, row.names = FALSE, col.names = TRUE)
    } else {
      write.table(img_summary_i, sprintf("validation_S2_682imgs_%s_img_summary_%s.csv", process_name, start_date),
                  sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
    }
    
    next
  }
  
  # load raster
  mci_i <- raster(file.path(rfolder, mci_imgs[i], "MCI.img"))
  
  # if no points, update img_summary and skip to next image
  if (length(mu_pts_img) == 0) {
    
    # update img_summary dataframe
    img_summary_i <- data.frame(img = mci_imgs[i], 
                                n_cloud_layers = NA, 
                                notes = "no points in image; ", 
                                n_pts_tot = nrow(mu_pts_img@data), 
                                n_pts_cloudy = 0, 
                                n_pts_noncloudy = 0)
    
    if (has_error(read.csv(sprintf("validation_S2_682imgs_%s_img_summary_%s.csv", process_name, Sys.Date())))) {
      write.table(img_summary_i, sprintf("validation_S2_682imgs_%s_img_summary_%s.csv", process_name, start_date),
                  sep = ",", append = FALSE, row.names = FALSE, col.names = TRUE)
    } else {
      write.table(img_summary_i, sprintf("validation_S2_682imgs_%s_img_summary_%s.csv", process_name, start_date),
                  sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
    }
    
    next
  }
  
  # reproject points
  mu_pts_img_proj <- spTransform(mu_pts_img, crs(mci_i))
  
  ## remove cloudy points
  tiles_img <- unique(mu_pts_img_proj$tileID)
  
  # check layers in cloud mask - should be 1, but at least one file has 0
  granule_dir <- file.path(safe_folder, paste0(mci_img_names[i], ".SAFE"), "GRANULE")
  granule_folders <- list.files(granule_dir)
  
  for (t in 1:length(tiles_img)) {
    
    granule_folder <- granule_folders[which((grepl(tiles_img[t], granule_folders)))]
    
    qi_data <- list.files(file.path(granule_dir, granule_folder, "QI_DATA"), pattern = ".gml")
    cloud_file <- qi_data[grep("MSK_CLOUDS", qi_data)]
    layers <- ogrListLayers(file.path(granule_dir, granule_folder, "QI_DATA", cloud_file))
    n_cloud_layers <- n_cloud_layers + length(layers)
    
    # if the layer exists: load cloud mask; get point indices falling on cloud; remove these points
    if (length(layers) == 1) {
      gml <- readOGR(file.path(granule_dir, granule_folder, "QI_DATA", cloud_file), "MaskFeature",
                     disambiguateFIDs = TRUE, verbose = FALSE)
      
      # assign crs to cloud mask if missing (which it always seems to be)
      if (is.na(crs(gml))) {
        crs(gml) <- crs(mu_pts_img_proj)
      }
      
      # query cloud mask value at locations of points
      cloud_pts_index <- over(mu_pts_img_proj, gml)
      
      # only include points with extracted value of NA (meaning they don't fall over a cloud - whether in this mask or a different one)
      mu_pts_img_proj <- mu_pts_img_proj[is.na(cloud_pts_index$gml_id), ]
      
      # subset to cloudy points (if any)
      cloudy_pts_i <- mu_pts_img_proj@data[!is.na(cloud_pts_index$gml_id), ]
      
      # append cloudy points to cloudypts table
      if (nrow(cloudy_pts_i) > 0) {
        if (has_error(read.csv(sprintf("validation_S2_682imgs_%s_cloudypts_%s.csv", process_name, Sys.Date())))) {
          write.table(cloudy_pts_i , sprintf("validation_S2_682imgs_%s_cloudypts_%s.csv", process_name, Sys.Date()),
                      sep = ",", append = FALSE, row.names = FALSE, col.names = TRUE)
        } else {
          write.table(cloudy_pts_i , sprintf("validation_S2_682imgs_%s_cloudypts_%s.csv", process_name, Sys.Date()),
                      sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
        }
        print("cloud(s)")
      }
    }
  }
  
  # initialize dataframe for this image
  mu_mci_i <- data.frame()
  
  # extract
  if (window_extraction == TRUE) {
    for (p in 1:length(mu_pts_img_proj)) {
      
      # print progress inline
      cat(paste(p, " "))
      
      # get cellNum under this pt
      cellNum <- cellFromXY(mci_i, mu_pts_img_proj@coords[p, ])
      
      # get cell indices for 3x3 window
      window_indices <- adjacent(mci_i, cells = cellNum, directions = 8, include = TRUE)
      
      # get values from those indices
      window_vals <- mci_i[window_indices[, 2]]
      
      # make NAs if cellNum is NA
      if (is.na(cellNum)) {
        window_vals <- rep(NA, 9)
      }
      
      # extract value summaries
      '
      window_df <- data.frame(
        nPts_CIwin = length(window_vals),
        nNA_CIwin = sum(is.na(window_vals)),
        mean_CIwin = mean(window_vals, na.rm = TRUE),
        median_CIwin = median(window_vals, na.rm = TRUE),
        min_CIwin = min(window_vals, na.rm = TRUE),
        max_CIwin = max(window_vals, na.rm = TRUE),
        var_CIwin = var(window_vals, na.rm = TRUE)
      )'
      
      # extract actual cell values
      window_df <- data.frame(t(rep(NA, 9)))
      window_df[1:length(window_vals)] <- data.frame(t(window_vals))
      colnames(window_df) <- paste0("CI_val_", 1:9)
      
      # bind to mu_mci
      mu_mci_i <- rbind(mu_mci_i, cbind(mu_pts_img_proj@data[p, ], window_df))
    }
    
  } else {
    # extract single-pixel mci value(s) and add as new column
    mci_vals <- extract(mci_i, mu_pts_img_proj)
    mu_pts_img_proj$mci <- mci_vals
    
    # append these points to mu_mci df
    mu_mci_i <- mu_pts_img_proj@data
  }
  
  # append points to validation table
  if (has_error(read.csv(sprintf("validation_S2_682imgs_%s_%s.csv", process_name, Sys.Date())))) {
    write.table(mu_mci_i, sprintf("validation_S2_682imgs_%s_%s.csv", process_name, Sys.Date()),
                sep = ",", append = FALSE, row.names = FALSE, col.names = TRUE)
  } else {
    write.table(mu_mci_i, sprintf("validation_S2_682imgs_%s_%s.csv", process_name, Sys.Date()),
                sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
  }
  
  # update img_summary dataframe
  img_summary_i <- data.frame(img = mci_imgs[i], 
                              n_cloud_layers = n_cloud_layers, 
                              n_pts_tot = nrow(mu_pts_img@data), 
                              n_pts_cloudy = nrow(mu_pts_img@data) - nrow(mu_pts_img_proj@data), 
                              n_pts_noncloudy = nrow(mu_pts_img_proj@data),
                              notes = notes)
  
  if (has_error(read.csv(sprintf("validation_S2_682imgs_%s_img_summary_%s.csv", process_name, Sys.Date())))) {
    write.table(img_summary_i, sprintf("validation_S2_682imgs_%s_img_summary_%s.csv", process_name, start_date),
                sep = ",", append = FALSE, row.names = FALSE, col.names = TRUE)
  } else {
    write.table(img_summary_i, sprintf("validation_S2_682imgs_%s_img_summary_%s.csv", process_name, start_date),
                sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
  }
}

print(Sys.time())


# --------------------------------------------------------------------------------------------

# validation ----------------------------------------------------------------------------------

library(chron)
library(colorRamps)
library(grDevices)
library(RColorBrewer)

source("C:/Users/WSalls/Desktop/Git/Sent2/error_metrics_1800611.R")
#source("/Users/wilsonsalls/Desktop/Git/Sent2/error_metrics_1800611.R")

#mu_mci_raw <- mu_mci
mu_mci_raw <- read.csv("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/681_imgs/validation_S2_682imgs_MCI_L1C_2018-10-11.csv", stringsAsFactors = FALSE)
#mu_mci_raw <- read.csv("/Users/wilsonsalls/Desktop/EPA/Sentinel2/Validation/682_imgs/validation_S2_682imgs_MCI_L1C_2018-10-10.csv", stringsAsFactors = FALSE)


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

# for plotting color
mu_mci$offset_days_factor <- as.factor(mu_mci$offset_days)
length(levels(mu_mci$offset_days_factor))

jcolors <- data.frame(day = levels(mu_mci$offset_days_factor),
                      color = I(topo.colors(11, alpha = 0.5)))

#

## calc chl a from MCI  ---------------

slope.mci <- 0.0004 # from Binding et al. 2013 - Erie
intercept.mci <- -0.0021 # from Binding et al. 2013 - Erie

mu_mci$chla_s2 <- (mu_mci$MCI_L1C - intercept.mci) / slope.mci

mu_mci <- mu_mci[mu_mci$chla_s2 > 0, ] # remove calculated negatives -617 (depends on coefficients used)

mu_mci$residual_chla <- abs(mu_mci$chla_s2 - mu_mci$chla_corr) # residual
mu_mci$pct_error_chla <- (abs(mu_mci$chla_s2 - mu_mci$chla_corr) / mu_mci$chla_corr) * 100 # % error



## subset ---------------
# remove land-adjacent
#mu_mci <- mu_mci[mu_mci$dist_shore_m >= 30, ]

# remove NAs
sum(is.na(mu_mci$MCI_L1C))
mu_mci <- mu_mci[!is.na(mu_mci$MCI_L1C), ]

# remove 0s (NA MCI_L1C)
sum(mu_mci$MCI_L1C == 0)
mu_mci <- mu_mci[mu_mci$MCI_L1C != 0, ]

# remove outliers
max(mu_mci$MCI_L1C)
sum(mu_mci$MCI_L1C == max(mu_mci$MCI_L1C))
mu_mci <- mu_mci[mu_mci$MCI_L1C != max(mu_mci$MCI_L1C), ]

min(mu_mci$MCI_L1C)
sum(mu_mci$MCI_L1C < -0.01)
mu_mci <- mu_mci[mu_mci$MCI_L1C > -0.01, ]

sum(mu_mci$chla_corr > 200)
mu_mci <- mu_mci[mu_mci$chla_corr < 200, ] # **** discuss

# export filtered validation data set
#write.csv(mu_mci, sprintf("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/682_imgs/validation_S2_682imgs_MCI_Chla_filtered_%s.csv", Sys.Date()))
mu_mci_filtered <- mu_mci # for resetting data


# subset by offset time
mu_mci <- mu_mci_filtered # reset
offset_min <- 0
offset_max <- 10
offset_threshold <- offset_min:offset_max
mu_mci <- mu_mci[mu_mci$offset_days %in% offset_threshold, ]

# subset by method
#mu_mci <- mu_mci_filtered
#method_sub <- "APHA" # APHA USEPA USGS
#mu_mci <- mu_mci[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == method_sub), ]

### plot  ---------------
setwd("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/681_imgs")

# b & w
col_plot <- alpha("black", 0.3)
jpeg(sprintf("val_%s_%s.png", offset_min, offset_max), width = 800, height = 860)
pch_plot <- 20
if (offset_min == offset_max) {
  plot_title <- sprintf("+/- %s day", offset_min)
} else {
  plot_title <- sprintf("+/- %s-%s days", offset_min, offset_max)
}
plot_error_metrics(x = mu_mci$chla_corr, y = mu_mci$chla_s2, # export 800 x 860
                   xname = "in situ chlorophyll-a (ug/l)", 
                   yname = "S2-derived chlorophyll-a (from MCI L1C)", 
                   title = plot_title, 
                   #title = paste0(method_sub, plot_title), # if subsetting by method
                   equal_axes = TRUE, 
                   log_axes = "xy", 
                   log_space = FALSE,
                   plot_abline = FALSE,
                   rsq = FALSE,
                   states = mu_mci$state,
                   lakes = mu_mci$comid,
                   col = col_plot, pch = pch_plot,
                   xlim = c(0.05, 200),
                   ylim = c(0.05, 200),
                   xaxt="n",
                   yaxt="n") # col = alpha("black", 0.3), pch = 20
axis(1, at = c(10^(-1:3)), labels = c(10^(-1:3)))
axis(2, at = c(10^(-1:3)), labels = c(10^(-1:3)))
dev.off()


# each day
mu_mci <- mu_mci_preoffset
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

#



#

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


#plot(mu_mci$chla_corr, mu_mci$MCI_L1C, col = rainbow(mu_mci$offset_days_factor))
#legend(10, 0.03, levels(mu_mci$offset_days_factor), col = rainbow(1:length(mu_mci$offset_days_factor)), pch=1)

plot(mu_mci$chla_corr, mu_mci$MCI_L1C, col = topo.colors(n = 11, alpha = 0.5), pch = 16, xlim = c(0, 210))
legend(195, 0.04, levels(mu_mci$offset_days_factor), col = topo.colors(11, alpha = 0.5), pch = 16)


qplot(mu_mci$chla_corr, mu_mci$MCI_L1C, col = mu_mci$offset_days_factor)



# investigate patterns ---------------------------------------------------------------------------
par()$mfrow
par(mfrow = c(2,1))
par(mfrow = c(1,1))

## residual vs. in situ value ************************
plot(mu_mci$chla_corr, mu_mci$residual_chla)


## check high error
mu_mci_sort <- mu_mci[order(-mu_mci$residual_chla), ]
mu_mci_sort <- mu_mci[order(-mu_mci$pct_error_chla), ]
mu_mci_sort[1:20, c(185, 191:194)]


## shore dist ---------------
plot(mu_mci$dist_shore_m, mu_mci$residual_chla, 
     xlim = c(0, 1000), # try removing this too
     #ylim = c(0, 200),
     xlab = "distance from shore (m)",
     ylab = "chl a error (ug/L)",
     pch = 20,
     col = alpha("black", alpha = 0.4))
abline(v = 30, lty = 2)
#rect(0, 0, 30, 350, border = NULL, col = alpha("orange", alpha = 0.5))

# boxplot
mu_mci$dist_shore_m_interval <- cut(mu_mci$dist_shore_m, seq(0, 2000, 50))
barplot(table(mu_mci$dist_shore_m_interval), xlab = "distance from shore (m)", ylab = "frequency")
boxplot(residual_chla ~ dist_shore_m_interval, data = mu_mci,
        xlab = "distance from shore (m)",
        ylab = "chl a residual")


## offset days ---------------
plot(mu_mci$offset_days, mu_mci$residual_chla)
plot(mu_mci$offset_hrs, mu_mci$residual_chla)

plot(abs(mu_mci$offset_hrs), mu_mci$residual_chla, 
     #ylim = c(0, 200),
     xlab = "time offset (hours)",
     ylab = "chl a residual (ug/L)",
     pch = 20,
     col = alpha("black", alpha = 0.4))

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

mu_method <- mu_mci[which(mu_mci$ResultAnalyticalMethod.MethodIdentifierContext == "USEPA"), ]
plot(mu_method$dist_shore_m, mu_usgs$residual_chla)

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


####




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



# make map of in situ locations ------------------------------------------------------------------

library(sp)
library(rgdal)
library(raster)

# make spdf of matchups
lon <- mu_mci$LongitudeMeasure # **
lat <- mu_mci$LatitudeMeasure # **
mu_mci_pts <- SpatialPointsDataFrame(coords = matrix(c(lon, lat), ncol = 2), 
                                     mu_mci, proj4string = CRS("+init=epsg:4326"))

# append state name to each point
us <- readOGR("O:/PRIV/NERL_ORD_CYAN/Salls_working/geospatial_general/US", "cb_2015_us_state_20m")
mu_mci_pts_proj <- spTransform(mu_mci_pts, crs(us))

conus <- us[-which(us$STUSPS %in% c("AK", "HI", "PR")), ]

library(scales) # for alpha transparency
plot(conus, col = "cornsilk", border = "grey", main = sprintf("+/- %s-day matchups", offset_max))
#plot(conus, col = "cornsilk", border = "grey", main = sprintf("+/- %s-%s-day matchups", offset_min, offset_max))
plot(mu_mci_pts_proj, pch = 20, col = alpha("black", 0.2), add=TRUE)
