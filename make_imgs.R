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


# set image folder paths
img_folder <- "C:/Users/WSalls/Desktop/s2_imgs_agu/mci/jordan" #jordan or utah
imgs <- list.files(img_folder, pattern = ".data")

# load lake shp
library(rgdal)
lakes <- readOGR("O:/PRIV/NERL_ORD_CYAN/Salls_working/geospatial_general/resolvableLakes/NHD_NLA_shoredist", 
                 "nhd_nla_subset_shore_dist")

# select jordan lake; reproject to UTM for use with rasters, loading a raster first to get CRS
jordan <- lakes[which(lakes$COMID == 166755060), ]
rast <- raster(file.path(img_folder, imgs[1], "MCI.img"))
jordan_utm <- spTransform (jordan, crs(rast))


# remove edges
for (i in seq_along(imgs)) {
  print(sprintf("image %s of %s", i, length(imgs)))
  
  # load raster
  rast <- raster(file.path(img_folder, imgs[i], "MCI.img"))
  
  # clip to lake
  rast_clip <- mask(rast, jordan_utm)
  rast_crop <- crop(rast_clip, jordan_utm)
  
  # remove edges
  rast_crop <- focal(rast_crop, matrix(c(0,0,0,0,1,0,0,0,0), nrow = 3))
  
  # convert to chlorophyll
  rast_crop <- (rast_crop - (-0.0021)) / 0.0004
  
  # remove negative chl
  values(rast_crop)[which(values(rast_crop) < 0)] <- NA
  
  # plot
  #jpeg(sprintf("jordan_%s.png", substr(imgs[i], 27, 34)), width = 800, height = 860)
  plot(rast_crop, main = substr(imgs[i], 27, 34), xaxt = "n", yaxt = "n", bty = "n")
  #plot(jordan_utm, add = TRUE)
  #dev.off()
}
