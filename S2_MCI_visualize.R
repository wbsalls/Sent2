## !! uses L1C bands to calculate slope, but BRR for MCI !! ##

library(raster)
library(sf)
library(rgdal)
library(viridis)
library(testit) # has_error


### settings

# *** <<<<< select folder >>>>> ***
#setwd("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/LowerGraniteLake/")
setwd("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/PriestRapids/")

# *** <<<<< select region >>>>> ***
#nhd <- st_read("O:/PRIV/NERL_ORD_CYAN/Salls_working/geospatial_general/resolvableLakes/NHD_NLA_shoredist/nhd_nla_subset_shore_dist.shp")
#region <- st_read("shp/LGL_extent.shp")
region <- st_read("shp/PriestRapids_extent.shp")

# *** <<<<< output tifs? >>>>> ***
tifs_out <- TRUE

# *** <<<<< name region for file output, if desired >>>>> ***
region_name <- ""


### prepare MCI rasters

mcis <- list.files("imgs/mci_20m_rayleigh", include.dirs = TRUE, pattern = ".data")
mcis <- mcis[order(substr(mcis, 29, 36))]
as.numeric(substr(mcis, 29, 36)) # print dates

#
mcis_df <- data.frame()
print(paste0("Start: ", Sys.time()))
tstart <- Sys.time()
for (m in 1:length(mcis)) {
  idate <- substr(mcis[m], 29, 36)
  print(paste0("Preparing MCI ", idate))
  
  # load image
  safe <- list.files("imgs/raw", pattern = idate)
  band_path <- file.path("imgs/raw", safe, "GRANULE", list.files(file.path("imgs/raw", safe, "GRANULE"))[1], "IMG_DATA/")
  mci <- crop(raster(file.path("imgs/mci_20m_rayleigh", mcis[m], "MCI.img")), region)
  b4 <- crop(raster(file.path(band_path, list.files(band_path, pattern = "_B04.jp2"))), region)
  b6 <- crop(raster(file.path(band_path, list.files(band_path, pattern = "_B06.jp2"))), region)
  b12 <- crop(raster(file.path(band_path, list.files(band_path, pattern = "_B12.jp2"))), region)
  b2 <- crop(raster(file.path(band_path, list.files(band_path, pattern = "_B02.jp2"))), region)
  
  # resample 10 m bands to 20
  b4r <- resample(b4, mci)
  b2r <- resample(b2, mci)
  
  # brick; clip to region
  mbrick <- brick(mci, b4r, b6, b12, b2r)
  
  brick_region <- mask(mbrick, mask = region)
  nTot_pixels <- sum(!is.na(values(brick_region[[1]])))
  
  # remove clouds
  gml_path <- file.path("imgs/raw", safe, "GRANULE", list.files(file.path("imgs/raw", safe, "GRANULE"))[1], "QI_DATA/MSK_CLOUDS_B00.gml")
  
  if (has_error(readOGR(gml_path, "MaskFeature", disambiguateFIDs = TRUE, verbose = FALSE))) {
    print(sprintf("Error opening cloud mask for %s! Moving on...", idate))
  } else {
    gml <- readOGR(gml_path, "MaskFeature", disambiguateFIDs = TRUE, verbose = FALSE)
    
    gml_rast <- mask(brick_region[[1]], gml, inverse = TRUE)
    brick_region[[1]][is.na(gml_rast)] <- NA
    nCloudfree_pixels <- sum(!is.na(values(brick_region[[1]])))
  }
  
  # test land mask
  land_mask <- brick_region[[4]] / brick_region[[5]]
  lmask_val <- 0.3
  land_mask[which(values(land_mask > lmask_val))] <- NA
  plot(land_mask, main = paste0("land mask: ", idate))
  
  # apply land mask
  brick_region[which(values(brick_region[[4]]) / values(brick_region[[5]]) > lmask_val)] <- NA
  nWater_pixels <- sum(!is.na(values(brick_region[[1]])))
  
  # alternative land mask
  #brick_region[which(values(brick_region[[4]] > 250))] <- NA
  
  # apply sediment flag
  brick_region[[6]] <- (brick_region[[3]] - brick_region[[2]]) / (740 - 655) # slope expressed as * 10^-4 nm-1
  nSedFlag_pixels <- sum(values(brick_region[[6]] < -1.5), na.rm = TRUE)
  
  brick_region[[1]][brick_region[[6]] < -1.5] <- NA
  
  # view MCI
  plot(brick_region[[1]], main = paste0("MCI: ", idate))
  
  # add metrics to table
  mcis_df <- rbind(mcis_df, data.frame(img = idate,
                                       min = min(values(brick_region[[1]]), na.rm = T),
                                       max = max(values(brick_region[[1]]), na.rm = T),
                                       pctCloudfree_Tot = round(nCloudfree_pixels / nTot_pixels, 3),
                                       pctWater_Cloudfree = round(nWater_pixels / nCloudfree_pixels, 3),
                                       pctSedFlag_Water = round(nSedFlag_pixels / nWater_pixels, 3)))
  
  # brick MCI for plotting later
  if (m == 1) {
    mcis_brick <- brick(brick_region[[1]])
  } else {
    mcis_brick[[m]] <- brick_region[[1]]
  }
  
  # write raster
  if (isTRUE(tifs_out)) {
    writeRaster(brick_region[[1]], sprintf("imgs/MCI_rasts/MCI_remCloudSed_%s_%s.tif", region_name, idate))
  }
}
print(paste0("End: ", Sys.time(), "; elapsed: ", (Sys.time() - tstart)))


mcis_df <- rbind(mcis_df, c(nrow(mcis_df), min(mcis_df$min), max(mcis_df$max), NA, NA, NA))
mcis_df

write.csv(mcis_df, "MCI_pixelcounts.csv")

minc <- min(mcis_df$min)
maxc <- max(mcis_df$max)



### plot MCI

# color stretch
min.color.val <- -0.0021 # -0.0021
max.color.val <- 0.01 # 0.01
plot(c(minc, min.color.val, max.color.val, maxc), y = rep(0, 4), 
     col = c("orange", "blue", "green", "red"))

# check conversion to chlorophyll
(min.color.val - (-0.0012)) / 0.0002 # ontario
(min.color.val - (-0.0021)) / 0.0004 # erie

(max.color.val - (-0.0012)) / 0.0002 # ontario
(max.color.val - (-0.0021)) / 0.0004 # erie

# plot size
size_factor <- 10
pwidth <- (extent(mcis_brick)[2] - extent(mcis_brick)[1]) / size_factor
pheight <- (extent(mcis_brick)[4] - extent(mcis_brick)[3]) / size_factor

# plot
for (m in 1:length(mcis)) {
  idate <- substr(mcis[m], 29, 36)
  print(paste0("Plotting MCI ", idate))
  
  # prep cloud flag
  safe <- list.files("imgs/raw", pattern = idate)
  gml_path <- file.path("imgs/raw", safe, "GRANULE", list.files(file.path("imgs/raw", safe, "GRANULE"))[1], "QI_DATA/MSK_CLOUDS_B00.gml")
  
  # plot MCI
  mbreaks <- c(minc, seq(min.color.val, max.color.val, by = 0.001), maxc)
  jpeg(sprintf("MCI_S2_%s.jpg", idate), width = pwidth, height = pheight, res = 300)
  par(mar = c(0, 0, 1, 0))
  plot(mcis_brick[[m]],
       main = idate,
       #cex.main = 4,
       xaxt = "n", yaxt = "n", box = FALSE, bty = "n",
       #col = colorRampPalette(c("blue", "green", "yellow"))(255),
       col = viridis(length(mbreaks)),
       breaks = mbreaks,
       legend = FALSE,
       colNA = NA)
  if (has_error(readOGR(gml_path, "MaskFeature", disambiguateFIDs = TRUE, verbose = FALSE))) {
    print(sprintf("Error opening cloud mask for %s! Moving on...", idate))
  } else {
    gml <- readOGR(gml_path, "MaskFeature", disambiguateFIDs = TRUE, verbose = FALSE)
    plot(gml, col = "gray", border = NA, add = TRUE)
  }
  scalebar(8000, type = "bar", label = c(0, 4, "8 km"))
  dev.off()
  
  # save a blank image to include for GIF (set to TRUE)
  if (FALSE) {
    if (m == length(mcis)) {
      jpeg(sprintf("MCI_S2_BLANK.jpg"), width = pwidth, height = pheight, res = 300)
      dev.off()
    }
  }
}


### plot RGB

for (m in 1:length(mcis)) {
  idate <- substr(mcis[m], 29, 36)
  print(paste0("Plotting RGB ", idate))
  
  # load image
  safe <- list.files("imgs/raw", pattern = idate)
  band_path <- file.path("imgs/raw", safe, "GRANULE", list.files(file.path("imgs/raw", safe, "GRANULE"))[1], "IMG_DATA/")
  
  b2 <- raster(file.path(band_path, list.files(band_path, pattern = "_B02.jp2")))
  b3 <- raster(file.path(band_path, list.files(band_path, pattern = "_B03.jp2")))
  b4 <- raster(file.path(band_path, list.files(band_path, pattern = "_B04.jp2")))
  
  b2_ext <- crop(b2, region)
  b3_ext <- crop(b3, region)
  b4_ext <- crop(b4, region)
  
  rgb_brick <- brick(b2_ext, b3_ext, b4_ext)
  
  # plot RGB
  jpeg(sprintf("RGB_S2_%s.jpg", idate), width = pwidth, height = pheight, res = 300) #, width = 600, height = 1200, res = 300
  par(mar = c(0, 0, 1, 0))
  plotRGB(rgb_brick, r = 3, g = 2, b = 1, 
          scale = max(values(rgb_brick), na.rm = TRUE),
          main = idate, 
          margins = TRUE,
          stretch = "lin")
  dev.off()
}

