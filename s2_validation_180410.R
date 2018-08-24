library(raster)
library(rgdal)
library(sp)


# query MCI values at in situ points ------------------------------------------------------------------------------------------

mu <- read.csv("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Matchups/out/Matchups_S2_chla_10day_filtered_2018-07-23.csv", stringsAsFactors = FALSE)
#mu <- read.csv("/Users/wilsonsalls/Desktop/EPA/S2/Validation/xxx", stringsAsFactors = FALSE)

# subset to same day (if desired)
#mu_sameday <- mu[mu$offset_days == "same day", ]

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

# run extraction
mu_mci <- data.frame()
img_summary <- data.frame()
cloudy_pts <- data.frame()

print(Sys.time())
for (i in 1:length(mci_imgs)) {
  
  # progress
  print(sprintf("image #%s at %s", i, Sys.time()))
  
  # initialize variable for use below
  missing_crs <- ""
  
  # load raster
  mci_i <- raster(file.path(rfolder, mci_imgs[i], "MCI.img"))
  
  # subset points to those in this image
  mu_pts_img <- mu_pts[mu_pts$PRODUCT_ID == substr(mci_imgs[i], 16, 75), ]
  
  # if no points, update img_summary and skip to next image
  if (length(mu_pts_img) == 0) {
    img_summary <- rbind(img_summary, data.frame(img = mci_imgs[i], 
                                                 n_layers = 0, 
                                                 missing_crs = missing_crs, 
                                                 n_pts_tot = nrow(mu_pts_img@data), 
                                                 n_pts_cloudy = 0, 
                                                 n_pts_noncloudy = 0))
    next
  }
  
  # reproject points
  mu_pts_img_proj <- spTransform(mu_pts_img, crs(mci_i))
  
  ## remove cloudy points
  # check layers in cloud mask - should be 1, but at least one file has 0
  granule_dir <- file.path(safe_folder, paste0(mci_img_names[i], ".SAFE"), "GRANULE")
  granule_folder <- list.files(granule_dir)
  layers <- ogrListLayers(file.path(granule_dir, granule_folder, "QI_DATA/MSK_CLOUDS_B00.gml"))
  n_layers <- length(layers)
  
  # if the layer exists: load cloud mask; get point indices falling on cloud; remove these points
  if (length(layers) == 1) {
    gml <- readOGR(file.path(granule_dir, granule_folder, "QI_DATA/MSK_CLOUDS_B00.gml"), "MaskFeature",
                   disambiguateFIDs = TRUE, verbose = FALSE)
    
    # assign crs to cloud mask if missing (which it always seems to be)
    if (is.na(crs(gml))) {
      missing_crs <- "missing crs"
      crs(gml) <- crs(mu_pts_img_proj)
    }
    
    # query cloud mask value at locations of points
    cloud_pts_index <- over(mu_pts_img_proj, gml)
    
    # only include points with extracted value of NA (meaning they don't fall over a cloud)
    mu_pts_img_proj <- mu_pts_img_proj[is.na(cloud_pts_index$gml_id), ]
    cloudy_pts <- rbind(cloudy_pts, mu_pts_img_proj@data[!is.na(cloud_pts_index$gml_id), ])
    if (nrow(mu_pts_img_proj@data[!is.na(cloud_pts_index$gml_id), ]) > 0) {
      print("cloud")
    }
  }
  
  # extract mci values and add as new column
  mci_vals <- extract(mci_i, mu_pts_img_proj)
  mu_pts_img_proj$mci <- mci_vals
  
  # append these points to mu_mci df
  mu_mci <- rbind(mu_mci, mu_pts_img_proj@data)
  
  # update img_summary dataframe
  img_summary <- rbind(img_summary, data.frame(img = mci_imgs[i], 
                                               n_layers = n_layers, 
                                               missing_crs = missing_crs, 
                                               n_pts_tot = nrow(mu_pts_img@data), 
                                               n_pts_cloudy = nrow(mu_pts_img@data) - nrow(mu_pts_img_proj@data), 
                                               n_pts_noncloudy = nrow(mu_pts_img_proj@data)))
}
print(Sys.time())

# rename mci column appropriately
colnames(mu_mci)[which(colnames(mu_mci) == "mci")] <- process_name

# write out
write.csv(mu_mci, sprintf("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/validation_S2_682imgs_%s_%s.csv", process_name, Sys.Date()))
write.csv(img_summary, sprintf("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/validation_S2_682imgs_%s_img_summary_%s.csv", process_name, Sys.Date()))
write.csv(cloudy_pts, sprintf("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/validation_S2_682imgs_%s_cloudypts_%s.csv", process_name, Sys.Date()))



# validation ----------------------------------------------------------------------------------

source("C:/Users/WSalls/Desktop/Git/Sent2/error_metrics_1800611.R")
#source("/Users/wilsonsalls/Desktop/EPA/Sentinel2/Validation/error_metrics_1800403.R")

mu_mci_raw <- mu_mci
mu_mci_raw <- read.csv("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/237_imgs/validation_S2_237imgs_MCI_L1C_2018-04-10.csv", stringsAsFactors = FALSE)
#mu_mci_raw <- read.csv("/Users/wilsonsalls/Desktop/EPA/Sentinel2/Validation/validation_S2_117imgs_MCI_L1C_2018-04-05.csv", stringsAsFactors = FALSE)


# remove duplicates: identify based on duplicated chlorophyll-a and MCI (L1C)
val_df <- data.frame(mu_mci_raw$chla_corr, mu_mci_raw$MCI_L1C)
sum(duplicated(val_df))
val_df_dups <- val_df[duplicated(val_df), ]

mu_mci_orig <- mu_mci_raw[!duplicated(val_df), ]

# it appears all duplicates have 0 MCI_L1C
# *** some points have same chla and MCI_L1C values, but diff MCI_Brr. maybe these came from different images that had (mostly?) the same values prior to Rayleigh correction?

#

# subset
mu_mci <- mu_mci_orig[mu_mci_orig$resolvable_nonEdge == ">= 60 m from shore", ] # remove (possible) land -87
mu_mci <- mu_mci[mu_mci$MCI_L1C != 0, ] # remove 0s (NA MCI_L1C) -216
mu_mci <- mu_mci[!is.na(mu_mci$MCI_L1C), ] # remove NAs -3
mu_mci <- mu_mci[mu_mci$MCI_L1C > -0.01, ] # remove extreme negative values -59
mu_mci <- mu_mci[mu_mci$chla_corr < 200, ] # remove outliers -3 ***?

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

slope.mci <- 0.0004 # from Binding et al. 2013 - Erie
intercept.mci <- -0.0021 # from Binding et al. 2013 - Erie

mu_mci$chla_s2 <- (mu_mci$MCI_L1C - intercept.mci) / slope.mci

# chl-a vs chl-a
mu_mci <- mu_mci[mu_mci$chla_s2 > 0, ] # remove calculated negatives -617 (depends on coefficients used)
l1c_chla <- calc_error_metrics(mu_mci$chla_corr, mu_mci$chla_s2)
l1c_chla
plot_error_metrics(x = mu_mci$chla_corr, y = mu_mci$chla_s2, 
                   xname = "in situ chlorophyll-a (ug/l)", 
                   yname = "S2-derived chlorophyll-a (from MCI L1C)", 
                   title = sprintf("+/- 3-day validation of Sentinel-2-derived chlorophyll-a\n(coefficients from Binding et al. [2013], Lake Erie)"), 
                   equal_axes = TRUE, 
                   log_axes = "xy", 
                   states = mu_mci$state,
                   lakes = mu_mci$comid)




# investigate patterns -------------------------------------------

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

# make spdf of matchups
lon <- mu_mci$LongitudeMeasure # **
lat <- mu_mci$LatitudeMeasure # **
mu_mci_pts <- SpatialPointsDataFrame(coords = matrix(c(lon, lat), ncol = 2), 
                                     mu_mci, proj4string = CRS("+init=epsg:4326"))

# append state name to each point
us <- readOGR("O:/PRIV/NERL_ORD_CYAN/Salls_working/geospatial_general/US", "cb_2015_us_state_20m")
mu_mci_pts_proj <- spTransform(mu_mci_pts, crs(us))

conus <- us[-which(us$STUSPS %in% c("AK", "HI", "PR")), ]

library(scales)
plot(conus, col = "cornsilk", border = "grey")
plot(mu_mci_pts_proj, pch = 20, col = alpha("black", 0.2), add=TRUE)
