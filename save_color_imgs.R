library(raster)

setwd("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Images/composited")

# create list of necessary images from mu_mci; export as csv
imgs <- unique(mu_mci$PRODUCT_ID[which(mu_mci$CLOUD_COVERAGE_ASSESSMENT == 0)])
write.csv(imgs, "O:/PRIV/NERL_ORD_CYAN/Sentinel2/Images/imgs_mumci_0cloud.csv")

# raw imgs
raw_dir <- "D:/s2/raw"
raw_img_names <- gsub(".SAFE", "", list.files(raw_dir))
length(imgs)
sum(imgs %in% raw)

for (i in 1:length(imgs)) {
  print(sprintf("img %s of %s at %s", i, length(imgs), Sys.time()))
  
  # find raster folder
  igranule <- file.path(raw_dir, paste0(imgs[i], ".SAFE"), "GRANULE")
  ifolder <- file.path(igranule, list.files(igranule)[1], "IMG_DATA")
  iimgs <- list.files(ifolder)
  
  # load each raster
  red <- raster(file.path(ifolder, iimgs[4]))
  green <- raster(file.path(ifolder, iimgs[3]))
  blue <- raster(file.path(ifolder, iimgs[2]))
  
  # create RasterBrick (red, green, blue)
  ibrick <- brick(red, green, blue)
  
  # composite, save as jpeg (with image name label)
  jpeg(sprintf("%s_comp_%s.jpeg", imgs[i], i), width = 3000, height = 2091)
  plotRGB(ibrick, r=1, g=2, b=3, stretch = "lin") # lin, hist, or NULL
  dev.off()
}