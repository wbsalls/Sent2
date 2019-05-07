
# read us shp for state names
us <- readOGR("O:/PRIV/NERL_ORD_CYAN/Salls_working/geospatial_general/US", "cb_2015_us_state_20m")
us <- readOGR("/Users/wilsonsalls/Desktop/EPA/geosp_general/US", "cb_2015_us_state_20m")

# subset to CONUS
conus <- us[-which(us$STUSPS %in% c("AK", "HI", "PR")), ]


## all points
bio <- read.csv("/Users/wilsonsalls/Desktop/EPA/Sentinel2/Matchups/WQP/prepared/input.csv")

# make spdf of matchups
lon <- bio$LongitudeMeasure # **
lat <- bio$LatitudeMeasure # **
bio_pts <- SpatialPointsDataFrame(coords = matrix(c(lon, lat), ncol = 2), 
                                     bio, proj4string = CRS("+init=epsg:4326"))

# transform
bio_pts_proj <- spTransform(bio_pts, crs(us))


## used points
# make spdf of matchups
lon <- mu_mci$LongitudeMeasure # **
lat <- mu_mci$LatitudeMeasure # **
mu_mci_pts <- SpatialPointsDataFrame(coords = matrix(c(lon, lat), ncol = 2), 
                                     mu_mci, proj4string = CRS("+init=epsg:4326"))
#writeOGR(mu_mci_pts, "./geospatial", "mu_mci_filtered_pts", "ESRI Shapefile")

# transform
mu_mci_pts_proj <- spTransform(mu_mci_pts, crs(us))

# plot
plot(conus, col = "cornsilk2", border = "grey")

#plot(bio_pts_proj, pch = 20, col = alpha("black", 0.2), add=TRUE)
plot(bio_pts_proj, pch = 20, col = "black", add=TRUE)
#plot(mu_mci_pts_proj, pch = 20, col = alpha("red", 0.2), add=TRUE)
plot(mu_mci_pts_proj, pch = 20, col = "red", add=TRUE)
