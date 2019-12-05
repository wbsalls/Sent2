### using mu_mci from s2_validation_180410.R, before filtering and subsetting, 0-3 days::


## load points, pre-filter (from s2_validation_180410.R) ----------------------------
setwd("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/681_imgs")

# read in output from 05_s2_point_extraction_MCI.R
mu_mci <- read.csv("validation_S2_682imgs_MCI_L1C_2018-11-21.csv", stringsAsFactors = FALSE)


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
  mu_pts_img <- mu_pts[mu_pts$PRODUCT_ID == imgs$mu_mci.PRODUCT_ID[i], ]
  
  # print progress
  cat(sprintf("\nimage #%s of %s (%s) - %s pts... ", i, nrow(imgs), Sys.time(), nrow(mu_pts_img)))
  
  # get granule dir and set workspace there
  folder <- imgs$mu_mci.PRODUCT_ID[i]
  granule_id <- imgs$mu_mci.GRANULE_ID[i]
  granule_dir <- file.path("D:/s2/BRR_resample20", paste0("brr_resample20_", folder, ".data"))
  
  # load rasters
  b4 = raster(file.path(granule_dir, "rBRR_B4.img"))
  b5 = raster(file.path(granule_dir, "rBRR_B5.img"))
  b6 = raster(file.path(granule_dir, "rBRR_B6.img"))
  
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
    colnames(window_df)[1:9] <- paste0("b4_", 1:9, "_BRR")
    colnames(window_df)[10:18] <- paste0("b5_", 1:9, "_BRR")
    colnames(window_df)[19:27] <- paste0("b6_", 1:9, "_BRR")
    
    # bind to mu_mci, with X.3 as ID field
    mu_rawbands <- rbind(mu_rawbands, data.frame(cbind(X.3 = mu_pts_img$X.3[p], window_df)))
  }
}

write.csv(mu_rawbands, sprintf("mu_BRRbands_%s.csv", Sys.Date()))
