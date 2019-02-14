#library(raster)
#library(sp)
library(maptools)

setwd("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Images/composited/0day_may_jul")

# load US
#us <- readOGR("O:/PRIV/NERL_ORD_CYAN/Salls_working/geospatial_general/US", "cb_2015_us_state_20m")

# create list of necessary images from mu_mci; export as csv
imgs <- unique(data.frame(mu_mci$PRODUCT_ID, mu_mci$GRANULE_ID)) #[which(mu_mci$CLOUD_COVERAGE_ASSESSMENT == 0)]
#write.csv(imgs, sprintf("imgs_mumci_0day_may_jul.csv"))

# assign image # to mu_mci
mu_mci$img_number <- NA
for (p in 1:nrow(mu_mci)) {
  mu_mci$img_number[p] <- which(imgs$mu_mci.GRANULE_ID == mu_mci$GRANULE_ID [p])
}

# set up points
mu_mci$pointID <- 1:nrow(mu_mci)

lon <- mu_mci$LongitudeMeasure # **
lat <- mu_mci$LatitudeMeasure # **
mu_pts <- SpatialPointsDataFrame(coords = matrix(c(lon, lat), ncol = 2), 
                                 mu_mci, proj4string = CRS("+init=epsg:4326"))
writeOGR(mu_pts[, -which(colnames(mu_pts@data) %in% c("samp_localTime", "img_localTime"))], 
         "O:/PRIV/NERL_ORD_CYAN/Sentinel2/Images/composited/0day_may_jul/geospatial", "mu_pts", driver = "ESRI Shapefile")


# write list of points
pt_table <- data.frame(PRODUCT_ID = mu_mci$PRODUCT_ID, GRANULE_ID = mu_mci$GRANULE_ID, COMID = mu_mci$comid, 
                       shore_dist = round(mu_mci$dist_shore_m, digits = 2), state = mu_mci$state, 
                       chla_corr = round(mu_mci$chla_corr, digits = 0), chla_s2 = round(mu_mci$chla_s2, digits = 0), 
                       chl_error = round(mu_mci$residual_chla, digits = 0), point_ID = mu_mci$pointID, 
                       point_IDX5 = mu_mci$X.5, img_number = mu_mci$img_number)

write.csv(pt_table, "ImageCheck_0day_may_jul.csv")

# raw imgs
raw_dir <- "D:/s2/raw"
raw_img_names <- gsub(".SAFE", "", list.files(raw_dir))
length(imgs)
sum(imgs %in% raw_img_names)

for (i in 5:nrow(imgs)) {
  print(sprintf("img %s of %s at %s", i, nrow(imgs), Sys.time()))
  
  # find raster folder
  igranule_dir <- file.path(raw_dir, paste0(imgs$mu_mci.PRODUCT_ID[i], ".SAFE"), "GRANULE")
  igranules <- list.files(igranule_dir)
  ifolder <- file.path(igranule_dir, igranules[which(igranules == imgs$mu_mci.GRANULE_ID[i])], "IMG_DATA")
  iimgs <- list.files(ifolder)
  
  # load each raster
  red <- raster(file.path(ifolder, iimgs[grep("_B04", iimgs)]))
  green <- raster(file.path(ifolder, iimgs[grep("_B03", iimgs)]))
  blue <- raster(file.path(ifolder, iimgs[grep("_B02", iimgs)]))
  
  # create RasterBrick (red, green, blue)
  ibrick <- brick(red, green, blue)
  
  # select points and reproject
  mu_i <- mu_pts[which(mu_pts$GRANULE_ID == as.character(imgs$mu_mci.GRANULE_ID[i])), ]
  mu_i_proj <- spTransform(mu_i, crs(ibrick))
  
  # composite, plot overlay points & labels, save as jpeg (with image name label)
  jpeg(sprintf("%s_comp_%s.jpeg", imgs$mu_mci.GRANULE_ID[i], i), width = 3000, height = 2091)
  plotRGB(ibrick, r = 1, g = 2, b = 3, stretch = "lin") # lin, hist, or NULL
  plot(mu_i_proj, col = "red", add = TRUE)
  pointLabel(coordinates(mu_i_proj), labels = as.character(mu_i_proj$pointID), col = "red")
  dev.off()
}
