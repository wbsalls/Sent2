# gifmaker.org

# location of images
rfolder <- "D:/s2/mci_resample20"
safe_folder <- "D:/s2/raw"

mci_files <- list.files(rfolder, pattern = ".data")

mci_files_jlake <- unique(grep("17SPV", mci_files, value = TRUE)) # 9
mci_files_utah <- unique(grep("12TVK", mci_files, value = TRUE)) # none


jlake_dir <- "O:/PRIV/NERL_ORD_CYAN/Salls_working/Presentations/AGU2018/data/jordanlake"

#file.copy(from = file.path(rfolder, mci_files_jord), to = jlake_dir, recursive = TRUE) # too slow!!


library(raster)

imglist <- list()

for (i in seq_along(mci_files_jlake)) {
  rast <- raster(file.path(rfolder, mci_files_jlake[i], "MCI.img"))
  
  imglist <- c(imglist, rast)
  plot(rast)
}



# ---------------------------------------------
library(rgdal)
library(raster)

# set image folder paths
#img_folder <- "C:/Users/WSalls/Desktop/s2_imgs_agu/mci/jordan" #jordan or utah
#img_folder <- "/Users/wilsonsalls/Desktop/EPA/Presentations/AGU2018/data/mci/jordan"
img_folder <- "/Users/wilsonsalls/Desktop/EPA/Sentinel2/Images/mci_demo_paper"
imgs <- list.files(img_folder, pattern = ".data")

# load lake shp
#lakes <- readOGR("O:/PRIV/NERL_ORD_CYAN/Salls_working/geospatial_general/resolvableLakes/NHD_NLA_shoredist", "nhd_nla_subset_shore_dist")
#lakes <- readOGR("/Users/wilsonsalls/Desktop/EPA/geosp_general/resolvableLakes/NHD_NLA_shoredist", "nhd_nla_subset_shore_dist")

#lakename <- "utah" # jordan OR utah
lakename <- "example"

# select lake; reproject to UTM for use with rasters, loading a raster first to get CRS
#lake_poly_raw <- lakes[which(lakes$COMID == 166755060), ] #166755060 for jordan; xx for utah
#lake_poly_raw <- readOGR("/Users/wilsonsalls/Desktop/EPA/Presentations/AGU2018/data", "JordanLake")
lake_poly_raw <- readOGR("/Users/wilsonsalls/Desktop/EPA/Sentinel2/Validation/681_imgs/geospatial",
                         "lakes_example")
rast <- raster(file.path(img_folder, imgs[1], "MCI.img"))
lake_poly <- spTransform (lake_poly_raw, crs(rast))

##

#rast_out_dir <- file.path("/Users/wilsonsalls/Desktop/EPA/Presentations/AGU2018/data/mci_cropped/", lakename)
rast_out_dir <- file.path("/Users/wilsonsalls/Desktop/EPA/Sentinel2/Images/mci_demo_paper")

## clip, remove edges, convert to chlorophyll, save new rasters ---------------

for (i in seq_along(imgs)) {
  idate <- substr(imgs[i], 27, 34)
  print(sprintf("image %s of %s: %s", i, length(imgs), idate))
  
  # load raster
  rast <- raster(file.path(img_folder, imgs[i], "MCI.img"))
  
  # clip to lake
  rast_mask <- mask(rast, lake_poly) # raster values
  rast_crop <- crop(rast_mask, lake_poly) # raster extent
  
  # remove edges
  rast_crop <- focal(rast_crop, matrix(c(0,0,0,0,1,0,0,0,0), nrow = 3))
  
  # convert to chlorophyll
  rast_crop <- (rast_crop - (-0.0021)) / 0.0004
  
  # remove negative chl
  values(rast_crop)[which(values(rast_crop) < 0)] <- NA
  
  # write raster
  writeRaster(rast_crop, file.path(rast_out_dir, 
                                   sprintf("chlorophyll_%s_%s.png", lakename, idate)), "GTiff")
}


## --------------------------------------------------------


### plot

# set location to save images
#setwd("/Users/wilsonsalls/Desktop/EPA/Presentations/AGU2018/imgs")
setwd("/Users/wilsonsalls/Desktop/EPA/Sentinel2/Images/mci_demo_paper")

# set location to read rasters from
chl_rasts <- list.files(rast_out_dir, pattern = ".tif")


## plot chlorophyll, save ---------------

# preset min and max
#min <- NA
#max <- NA

library(viridis)

# plot
for (i in seq_along(chl_rasts)) {
  idate <- substr(chl_rasts[i], nchar(chl_rasts[i]) - 11, nchar(chl_rasts[i]) - 4)
  print(sprintf("image %s of %s: %s", i, length(chl_rasts), idate))
  
  # read raster
  rast_crop <- raster(file.path(rast_out_dir, chl_rasts[i]))
  
  # plot
  jpeg(sprintf("chl_%s_%s.png", lakename, idate), width = 1200, height = 850)
  max.color.val <- 50
  plot(rast_crop,
       #main = paste0(substr(idate, 1, 4), "-", substr(idate, 5, 6), "-", substr(idate, 7, 8)),
       cex.main = 4,
       xaxt = "n", yaxt = "n", box = FALSE, bty = "n",
       #col = colorRampPalette(c("blue", "green", "yellow"))(255),
       col = viridis(max.color.val + 1),
       breaks = c(seq(1, max.color.val, length.out = max.color.val), 120),
       legend = FALSE,
       colNA = NA)
  plot(lake_poly, add = TRUE)
  dev.off()
  
  # get min and mox to improve plotting
  #min <- min(min, minValue(rast_crop), na.rm = T)
  #max <- max(max, maxValue(rast_crop), na.rm = T)
}

min # ~0
max # 117.3

# for legend
rast_crop <- raster(file.path(rast_out_dir, chl_rasts[i]))
values(rast_crop)[values(rast_crop) > 50.1] <- NA
max.color.val <- 50
plot(rast_crop,
     #main = paste0(substr(idate, 1, 4), "-", substr(idate, 5, 6), "-", substr(idate, 7, 8)),
     cex.main = 4,
     xaxt = "n", yaxt = "n", box = FALSE, bty = "n",
     #col = colorRampPalette(c("blue", "green", "yellow"))(255),
     col = viridis(50),
     #breaks = c(seq(1, max.color.val, length.out = max.color.val), 120),
     colNA = NA)

#

## plot trophic state, save ---------------

# plot
for (i in seq_along(chl_rasts)) {
  idate <- substr(chl_rasts[i], 20, 27)
  print(sprintf("image %s of %s: %s", i, length(chl_rasts), idate))
  
  # read raster
  rast_crop <- raster(file.path(rast_out_dir, chl_rasts[i]))
  
  # plot
  jpeg(sprintf("trophic_%s_%s.png", lakename, idate), width = 600, height = 1200)
  max.color.val <- 120
  plot(rast_crop, 
       main = paste0(substr(idate, 1, 4), "-", substr(idate, 5, 6), "-", substr(idate, 7, 8)),
       cex.main = 4,
       xaxt = "n", yaxt = "n", box = FALSE, bty = "n",
       col = plasma(4),
       breaks = c(0, 2, 7, 30, max.color.val),
       colNA = NA)
  dev.off()
  
}

# colors used
plasma(4)

#

# blank plot
rast_blank <- rast_crop
values(rast_blank) <- NA

jpeg("xblank_black.png", width = 600, height = 1200)
plot(rast_blank, colNA = "black", xaxt = "n", yaxt = "n", box = FALSE, bty = "n")
dev.off()

jpeg("xblank_white.png", width = 600, height = 1200)
plot(rast_blank, colNA = NA, xaxt = "n", yaxt = "n", box = FALSE, bty = "n")
dev.off()


## pie charts----------------------------
setwd("/Users/wilsonsalls/Desktop/EPA/Presentations/AGU2018/imgs/pie")

clrs <- plasma(4)

#trophic_counts <- data.frame()

for (i in seq_along(chl_rasts)) {
  idate <- substr(chl_rasts[i], 20, 27)
  print(sprintf("image %s of %s: %s", i, length(chl_rasts), idate))
  
  # read raster
  rast_crop <- raster(file.path(rast_out_dir, chl_rasts[i]))
  
  vals <- values(rast_crop)[!is.na(values(rast_crop))]
  
  oligotrophic <- sum(vals < 2)
  mesotrophic <- sum(vals >= 2 & vals < 7)
  eutrophic <- sum(vals >= 7 & vals < 30)
  hypereutrophic <- sum(vals >= 30)
  #total <- length(vals)
  
  #trophic_counts <- rbind(trophic_counts, data.frame(oligotrophic, mesotrophic, eutrophic, hypereutrophic, total))
  
  jpeg(sprintf("pie_%s_%s.png", lakename, idate), width = 600, height = 600)
  pie(c(oligotrophic, mesotrophic, eutrophic, hypereutrophic), labels=NA, col=clrs, main = sprintf("%s: %s", lakename, idate))
  dev.off()
}


# where to change raster options
showMethods("plot")
getMethod("plot", c("Raster", "ANY"))
getAnywhere(".plotraster2")
getAnywhere(".rasterImagePlot")
args(raster:::.rasterImagePlot)
