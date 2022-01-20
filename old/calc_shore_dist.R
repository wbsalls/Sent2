# This program calculates the distance (in m) between each sampling point in a user-specified file and the nearest shoreline.

# Required inputs are indicated by "<<<<<<"

# Contact: Wilson Salls - salls.wilson@epa.gov
# US EPA, Office of Research and Development
# Finalized 15 September 2020


###

library(rgdal)
library(sp)
library(rgeos)


### lakes ###

# load lakes shapefile (takes forever)
lakes <- readOGR(dsn = file.path("xxx/NHD_NLA_shoredist"), # <<<<<<
                 layer = "nhd_nla_subset_shore_dist")


# set resolvable lake minimum threshold (m) and select lakes. Or, if preferred, retain all lakes by uncommenting last line.
# e.g. 300 for OLCI single pixel; 900 for OLCI 3x3 pixels
min_size <- xxx # <<<<<< 
lakes_resolvable <- lakes[which(lakes$max_window >= min_size), ]
#lakes_resolvable <- lakes # <<<<<< (run this if retaining all lakes)


### in situ locations - make a pointfile using lat/long ###

# load in situ file
insitu <- read.csv("xxx.csv", stringsAsFactors=FALSE) # <<<<<<

# specify longitude and latitude variable names in in situ file
long_name <- "xxx" # <<<<<<
lat_name <- "xxx" # <<<<<<

# get coordinates and use to make SpatialPointsDataFrame object
coords <- coordinates(data.frame(long = insitu[, long_name], lat = insitu[, lat_name])) 
samp_pts <- SpatialPointsDataFrame(coords, 
                                   proj4string = CRS("+proj=longlat +datum=WGS84"), # <<<<<< (may need to adjust depending on CRS of in situ coordinates)
                                   data = insitu)

# reproject SpatialPointsDataFrame so it can be used with the lakes shapefiles
samp_pts_albers <- spTransform(samp_pts, proj4string(lakes_resolvable))



### perform distance calculation ###

# convert lake polygons to lines for calculating distance from shore
lakes_lines <- as(lakes_resolvable, "SpatialLines")

# make new column for shore distance
samp_pts_albers$dist_shore_m <- NA

# calculate distance from shore for each sample
for (r in seq_along(samp_pts_albers)) {
  samp_pts_albers$dist_shore_m[r] <- gDistance(samp_pts_albers[r, ], lakes_lines)
}


# output as csv - output is input table with distance column appended
write.csv(samp_pts_albers@data, "insitu_data_dist_shore.csv") # <<<<<<


### END ###
