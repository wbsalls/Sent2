
# read us shp for state names
us <- readOGR("O:/PRIV/NERL_ORD_CYAN/Salls_working/geospatial_general/US", "cb_2015_us_state_20m")
us <- readOGR("/Users/wilsonsalls/Desktop/EPA/geosp_general/US", "cb_2015_us_state_20m")

# subset to CONUS
conus <- us[-which(us$STUSPS %in% c("AK", "HI", "PR")), ]


## all points
wqp <- read.csv("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Matchups/WQP/20180710/input.csv")

# make spdf of matchups
lon <- wqp$LongitudeMeasure # **
lat <- wqp$LatitudeMeasure # **
wqp_pts <- SpatialPointsDataFrame(coords = matrix(c(lon, lat), ncol = 2), 
                                     wqp, proj4string = CRS("+init=epsg:4326"))

# transform
wqp_pts_proj <- spTransform(wqp_pts, crs(us))


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
plot(conus, col = "cornsilk2", border = "grey") # 900 x 625

pts_pch = 20
#plot(wqp_pts_proj, pch = pts_pch, col = alpha("black", 0.2), add=TRUE)
plot(wqp_pts_proj, pch = pts_pch, col = "black", add=TRUE)
#plot(mu_mci_pts_proj, pch = pts_pch, col = alpha("red", 0.2), add=TRUE)
plot(mu_mci_pts_proj, pch = pts_pch, col = "red", add=TRUE)

legend("bottomleft", legend=c("WQP point", "WQP point used for validation"),
       col=c("black", "red"), pch = pts_pch)
