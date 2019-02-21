### using mu_mci from s2_validation_180410.R, before filtering and subsetting, 0-3 days::


## load points, pre-filter (from s2_validation_180410.R) ----------------------------
setwd("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/681_imgs")
setwd("/Users/wilsonsalls/Desktop/EPA/Sentinel2/Validation/681_imgs")

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

# depth - remove NAs that were left
mu_mci <- mu_mci[-which(is.na(mu_mci$depth_corr) &
                          is.na(mu_mci$topdepth_corr) & 
                          is.na(mu_mci$botdepth_corr) & 
                          is.na(mu_mci$ActivityRelativeDepthName)), ]


# subset by offset time
offset_min <- 0
offset_max <- 0
offset_threshold <- offset_min:offset_max
mu_mci <- mu_mci[mu_mci$offset_days %in% offset_threshold, ]


## get image list
imgs <- unique(data.frame(mu_mci$PRODUCT_ID, mu_mci$GRANULE_ID))

## load points
lon <- mu_mci$LongitudeMeasure # **
lat <- mu_mci$LatitudeMeasure # **
mu_pts <- SpatialPointsDataFrame(coords = matrix(c(lon, lat), ncol = 2), 
                                 mu_mci, proj4string = CRS("+init=epsg:4326"))

## run extraction
mu_rawbands <- data.frame()

for (i in 1:nrow(imgs)) {
  
  # load points
  mu_pts_img <- mu_pts[mu_pts$PRODUCT_ID == sub(".data", "", sub("mci_resample20_", "", imgs$mu_mci.PRODUCT_ID[i])), ]
  
  # print progress
  cat(sprintf("\nimage #%s of %s (%s) - %s pts... ", i, nrow(imgs), Sys.time(), nrow(mu_pts_img)))
  
  # assign get granule dir and set workspace there
  folder <- imgs$mu_mci.PRODUCT_ID[i]
  granule_id <- imgs$mu_mci.GRANULE_ID[i]
  granule_dir <- file.path("D:/s2/raw", paste0(folder, ".SAFE/GRANULE"), granule_id, "IMG_DATA")
  
  # load rasters
  b4 = raster(file.path(granule_dir, list.files(granule_dir, "*B04.jp2")))
  b5 = raster(file.path(granule_dir, list.files(granule_dir, "*B05.jp2")))
  b6 = raster(file.path(granule_dir, list.files(granule_dir, "*B06.jp2")))
  
  # reproject points
  mu_pts_img_proj <- spTransform(mu_pts_img, crs(b4))
  
  # extract values from window
  for (p in 1:length(mu_pts_img_proj)) {
    
    # print progress inline
    cat(paste(p, " "))
    
    # get cellNum under this pt
    cellNum4 <- cellFromXY(b4, mu_pts_img_proj@coords[p, ])
    cellNum5 <- cellFromXY(b5, mu_pts_img_proj@coords[p, ])
    cellNum6 <- cellFromXY(b6, mu_pts_img_proj@coords[p, ])
    
    # get cell indices for 3x3 window
    window_indices4 <- adjacent(b4, cells = cellNum4, directions = 8, include = TRUE)
    window_indices5 <- adjacent(b5, cells = cellNum5, directions = 8, include = TRUE)
    window_indices6 <- adjacent(b6, cells = cellNum6, directions = 8, include = TRUE)
    
    # get values from those indices
    window_vals4 <- b4[window_indices4[, 2]]
    window_vals5 <- b5[window_indices5[, 2]]
    window_vals6 <- b6[window_indices6[, 2]]
    
    # make NAs if cellNum is NA
    if (is.na(cellNum4)) {
      window_vals4 <- rep(NA, 9)
    }
    if (is.na(cellNum5)) {
      window_vals5 <- rep(NA, 9)
    }
    if (is.na(cellNum6)) {
      window_vals6 <- rep(NA, 9)
    }
    
    # make df of cell values
    window_df <- data.frame(matrix(c(window_vals4, window_vals5, window_vals6), nrow = 1))
    colnames(window_df)[1:9] <- paste0("b4_", 1:9)
    colnames(window_df)[10:18] <- paste0("b5_", 1:9)
    colnames(window_df)[19:27] <- paste0("b6_", 1:9)
    
    # bind to mu_mci, with X.5 as ID field
    mu_rawbands <- rbind(mu_rawbands, data.frame(cbind(X.5 = mu_pts_img$X.5[p], window_df)))
  }
}


write.csv(mu_rawbands, "mu_rawbands_3day.csv")

  