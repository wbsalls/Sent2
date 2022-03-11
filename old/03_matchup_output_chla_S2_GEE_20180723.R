library(chron)
library(rgdal)

setwd("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Matchups/out")
setwd("/Users/wilsonsalls/Desktop/EPA/Sentinel2/Matchups")

mus <- read.csv("Matchups_S2_chla_10day.csv", stringsAsFactors=FALSE)

musub <- mus

nrow(musub)
length(unique(musub$system.index))


##

### subset criteria; formatting ###
# remove columns with mostly NA (90% or more)
rm_cols <- c()
for (c in 1:ncol(musub)) {
  na_pct <- sum(is.na(musub[, c])) / nrow(musub)
  if (na_pct >= 0.9) {
    print(paste0(colnames(musub)[c], ": ", round(na_pct, 2)))
    rm_cols <- c(rm_cols, c)
  }
}


musub <- musub[, -rm_cols]

## in situ criteria ----------------------------------------------------------------------

##depth <= 2 m

#1 convert all to m; if units not given, -> NA
depth_conv <- function(x) {
  
  if (is.na(x)) {
    return(NA)
    
  } else if (x=="feet" | x=="ft") {
    return(1 / 3.28084)
    
  } else if (x=="m" | x=="meters") {
    return(1)
    
  } else {
    return(NA)
  }
}

#depth
table(musub$ActivityDepthHeightMeasure.MeasureUnitCode)
sum(is.na(musub$ActivityDepthHeightMeasure.MeasureUnitCode))
#unique(musub$ActivityDepthHeightMeasure.MeasureValue)
musub$depth_mult <- sapply(musub$ActivityDepthHeightMeasure.MeasureUnitCode, depth_conv)
musub$depth_corr <- as.numeric(musub$ActivityDepthHeightMeasure.MeasureValue) * musub$depth_mult

#top depth
table(musub$ActivityTopDepthHeightMeasure.MeasureUnitCode)
#table(musub$ActivityTopDepthHeightMeasure.MeasureValue) # 0-2 needs to be addressed (removed since bottom = 2.1 m)
musub$topdepth_mult <- sapply(musub$ActivityTopDepthHeightMeasure.MeasureUnitCode, depth_conv)
musub$topdepth_corr <- as.numeric(musub$ActivityTopDepthHeightMeasure.MeasureValue) * musub$topdepth_mult

#bottom depth
table(musub$ActivityBottomDepthHeightMeasure.MeasureUnitCode)
#unique(musub$ActivityBottomDepthHeightMeasure.MeasureValue)
musub$botdepth_mult <- sapply(musub$ActivityBottomDepthHeightMeasure.MeasureUnitCode, depth_conv)
musub$botdepth_corr <- as.numeric(musub$ActivityBottomDepthHeightMeasure.MeasureValue) * musub$botdepth_mult


# delete those > 2 m in any of the depths (keep NAs; dealt with next)
musub <- subset(musub, (musub$depth_corr <= 2 | is.na(musub$depth_corr)) &
                  (musub$topdepth_corr <= 2 | is.na(musub$topdepth_corr)) &
                  (musub$botdepth_corr <= 2 | is.na(musub$botdepth_corr)))

# if all 3 depths are NA and ActivityRelativeDepthName isn't Surface, delete
musub <- musub[-which(is.na(musub$depth_corr) & 
                        is.na(musub$topdepth_corr) & 
                        is.na(musub$botdepth_corr) &
                        (musub$ActivityRelativeDepthName != "Surface")), ]

# also remove NA ActivityRelativeDepthName (included subsequently)
musub <- musub[-which(is.na(musub$depth_corr) &
                          is.na(musub$topdepth_corr) & 
                          is.na(musub$botdepth_corr) & 
                          is.na(musub$ActivityRelativeDepthName)), ]


##

#
table(musub$ResultValueTypeName)
sum(is.na(musub$ResultValueTypeName))
musub <- subset(musub, musub$ResultValueTypeName!="Estimated" & musub$ResultValueTypeName!="Calculated")
# | is.na(musub$ResultValueTypeName))

#
table(musub$ResultStatusIdentifier)
sum(is.na(musub$ResultStatusIdentifier))
musub <- subset(musub, musub$ResultStatusIdentifier!="Rejected" & musub$ResultStatusIdentifier!="Preliminary")
# | is.na(musub$ResultStatusIdentifier))

#probe, analysis methods, sampling groups
table(musub$SampleCollectionEquipmentName) #remove Probe/Sensor, benthic grab
sum(is.na(musub$SampleCollectionEquipmentName))
musub <- subset(musub, musub$SampleCollectionEquipmentName!="Probe/Sensor" & 
                  musub$SampleCollectionEquipmentName!="Benthic Grab (Other)")
# | is.na(musub$SampleCollectionEquipmentName))

#
table(musub$ResultAnalyticalMethod.MethodName)
sum(is.na(musub$ResultAnalyticalMethod.MethodName))
bad_anmeth <- c("a-b-c Determination by fluorometer", "In-Vitro Determination of Chlorophyll", 
                "Chlorophyll a-b-c Determination by fluorometer", "Chlorophyll a,phyto,fluorometric",
                "Fluorometric Analysis of Chlorophyll a (originally 2334.22A)", "In Vitro Determination of Chlorophyll a")
musub <- subset(musub, !(musub$ResultAnalyticalMethod.MethodName %in% bad_anmeth)) #*************

# (look for outliers down the road)
table(musub$SampleCollectionMethod.MethodIdentifier)

#
table(musub$ActivityMediaSubdivisionName)
sum(is.na(musub$ActivityMediaSubdivisionName))
musub <- subset(musub, musub$ActivityMediaSubdivisionName %in% c("Surface Water", "Ocean Water"))
# | is.na(musub$ActivityMediaSubdivisionName))



## chl-a formatting
sum(is.na(musub$ResultMeasureValue))
musub <- musub[-which(is.na(musub$ResultMeasureValue)),] # remove NAs

sum(is.na(as.numeric(musub$ResultMeasureValue))) # check for values that won't convert to numeric
#badmeas <- c("< 0.4", "*Non-detect", "PENDING", NA)
#musub <- subset(musub, !(musub$ResultMeasureValue %in% badmeas))
musub$chla_raw <- as.numeric(as.character(musub$ResultMeasureValue))

table(musub$ResultMeasure.MeasureUnitCode)
sum(is.na(musub$ResultMeasure.MeasureUnitCode))

chla_conv <- function(x) {
  if (x=="mg/l") {
    return(1000)
  } else if (x %in% c("ug/l", "mg/m3")) { # mg/m3 = ug/l
    return(1)
  } else {
    return(NA)
  }
}
musub$chla_mult <- sapply(musub$ResultMeasure.MeasureUnitCode, chla_conv)
musub$chla_corr <- musub$chla_raw*musub$chla_mult

#musub <- subset(musub, musub$chla_corr!=20700)
hist(musub$chla_corr, plot=FALSE)
hist(musub$chla_corr)
hist(musub$chla_corr[musub$chla_corr <= 300])

chl_rangeFn <- function(x) {
  if (x <= 15) {
    return("0-15")
  } else if (x <= 50) {
    return("15-50")
  } else if (x <= 200) {
    return("50-200")
  } else {
    return("> 200")
  }
}
musub$chl_range <- sapply(musub$chla_corr, chl_rangeFn)
table(musub$chl_range)

chl_eutrFn <- function(x) {
  if (x < 2) {
    return("oligotrophic")
  } else if (x <= 7) {
    return("mesotrophic")
  } else if (x <= 30) {
    return("eutrophic")
  } else {
    return("hypereutrophic")
  }
}
musub$chl_eutr <- sapply(musub$chla_corr, chl_eutrFn)
table(musub$chl_eutr)



##

musub_preimg <- musub
musub <- musub_preimg ## ** use to reset **



## img criteria -----------------------------------------------------------------

#solar noon
sum(is.na(musub$MEAN_SOLAR_ZENITH_ANGLE))
hist(musub$MEAN_SOLAR_ZENITH_ANGLE)
musub <- subset(musub, musub$MEAN_SOLAR_ZENITH_ANGLE <= 45)

#clouds
sum(is.na(musub$CLOUDY_PIXEL_PERCENTAGE))
hist(musub$CLOUDY_PIXEL_PERCENTAGE, main="All (remaining) records", xlab="Cloud %")
musub <- subset(musub, musub$CLOUDY_PIXEL_PERCENTAGE <= 10) # max in GEE was set to 10
hist(musub$CLOUDY_PIXEL_PERCENTAGE, main="Clouds <= 10%", xlab="Cloud %")

#
nrow(musub)
length(unique(musub$system.index))


## add state -----------------------------------------------------------------
us <- readOGR("/Users/wilsonsalls/Desktop/EPA/geosp_general/US", "cb_2015_us_state_20m")

coords <- coordinates(data.frame(long = musub$LongitudeMeasure, lat = musub$LatitudeMeasure))
samp_pts <- SpatialPoints(coords, proj4string=CRS(proj4string(us)))


musub$state <- over(samp_pts, us)$STUSPS


# write formatted matchups table ------------------------------------------------
## output

#whole table
write.csv(musub, sprintf("Matchups_S2_chla_10day_filtered_%s.csv", Sys.Date()))

# same day
#musub_sameday <- subset(musub, musub$offset_days=="same day")
#write.csv(musub_sameday, "Matchups_sameday.csv")


# --------------------------------------------------------------------------------








## time window selection ---------------------------------------------------------


### metrics ###

#matchup count per offset window
offset_counts <- data.frame(table(musub$offset_days))
offset_counts <- rbind(offset_counts[4,], offset_counts[1:3,])
rownames(offset_counts) <- NULL

for (row in 1:nrow(offset_counts)) {
  offset_counts$cumFreq[row] <- sum(offset_counts$Freq[1:row])
}
offset_counts

barplot(offset_counts$Freq, names.arg=offset_counts$Var1, ylab="# Matchups (non-cumulative)")
barplot(offset_counts$cumFreq, names.arg=offset_counts$Var1, ylab="Cumulative # Matchups")

d0 <- subset(musub, musub$offset_days=="same day")
d01 <- subset(musub, musub$offset_days=="same day" | musub$offset_days=="+/- 1 day")
d012 <- subset(musub, musub$offset_days=="same day" | musub$offset_days=="+/- 1 day" | musub$offset_days=="+/- 2 days")
d0123 <- subset(musub, musub$offset_days=="same day" | musub$offset_days=="+/- 1 day" | musub$offset_days=="+/- 2 days" | musub$offset_days=="+/- 3 days")

# number of images at each time window
length(unique(d0$system.index))
length(unique(d01$system.index))
length(unique(d012$system.index))
length(unique(d0123$system.index))

#make copy
musub2 <- musub

#use copy
#musub <- musub2

#subset by time window
musub <- subset(musub, musub$offset_days=="same day")

#make image list
imgs <- data.frame(sort(table(musub$system.index), decreasing=TRUE))
#imgs$scene <- rownames(imgs)
#imgs <- imgs[,c(2,1)]
colnames(imgs) <- c("sceneID", "matchups")
#ownames(imgs) <- NULL

musub$system.index <- factor(musub$system.index) #this may or may not help


'
#test
nrow(musub[musub$system.index=="LC80270292014198LGN00",])
musub$chla_corr[which(musub$system.index==levels(musub$system.index)[9])]
musub$chla_corr[which(musub$system.index=="LC80140322014171LGN00")]
levels(m1$system.index)[14]
[1] "LC80160352015188LGN00"
nrow(subset(musub, musub$system.index=="LC80160352015188LGN00"))
[1] 42
musub$chla_corr[which(musub$system.index=="LC80140292014203LGN00")]
#
'

# image table ----------------------------------------------------------------

#img table
#read in Landsat archive list
l8 <- read.csv("O:\\PRIV\\NERL_ORD_CYAN\\Landsat\\xMatchupData\\LANDSAT_8_2017_07_10.csv", stringsAsFactors=FALSE)
# <- read.csv("/Users/wilsonsalls/Desktop/EPA/Chlorophyll_a_Landsat/matchup_output/feb13/LANDSAT_8.csv", stringsAsFactors=FALSE)

#add information to image list
for (r in 1:nrow(imgs)) {
  imgs$path[r] <- l8$path[which(l8$sceneID==imgs$sceneID[r])]
  imgs$row[r] <- l8$row[which(l8$sceneID==imgs$sceneID[r])]
  imgs$date[r] <- l8$acquisitionDate[which(l8$sceneID==imgs$sceneID[r])]
  imgs$clouds[r] <- l8$cloudCoverFull[which(l8$sceneID==imgs$sceneID[r])]
}
imgs$pathrow <- paste(imgs$path, imgs$row, sep="-")

#make scene list from image list
scenes <- data.frame(sort(table(imgs$pathrow), decreasing=TRUE))
#scenes$pathrow <- rownames(scenes)
#scenes <- scenes[,c(2,1)]
colnames(scenes) <- c("pathrow", "imgs")
#rownames(scenes) <- NULL

#add #matchups per scene
for (r in 1:nrow(scenes)) {
  scenes$matchups[r] <- sum(subset(imgs, imgs$pathrow==scenes$pathrow[r])$matchups)
}


write.csv(imgs, "L8matchup_image_list.csv")
write.csv(scenes, "L8matchup_scenes_list.csv")




# visualizations ------------------------------------------------------------------------------------------


#3 day
pr <- readOGR("O:\\PRIV\\NERL_ORD_CYAN\\Landsat\\xMatchupData\\LandsatScenes", "wrs2_descending")
us <- readOGR("O:\\PRIV\\NERL_ORD_CYAN\\Salls_working\\geospatial_general\\US", "cb_2015_us_state_20m")

pr$pathrow <- paste(pr$PATH, pr$ROW, sep="-")

#select path-rows from shapefile that are in the scenelist
prmu <- subset(pr, pr$pathrow %in% scenes$pathrow)

#conus
rm_us <- c("Hawaii", "Alaska", "Puerto Rico")
us <- subset(us, !(us$NAME %in% rm_us))

#map scenes
library(viridis)
library(plotrix)

plot(us, border="gray")
plot(prmu, main="Scenes Containing Same-day Matchups", add=TRUE)


for (r in 1:nrow(prmu)) {
  prmu$imgs[r] <- scenes$imgs[which(scenes$pathrow==prmu$pathrow[r])]
  prmu$matchups[r] <- scenes$matchups[which(scenes$pathrow==prmu$pathrow[r])]
}
prmu$imgs1 <- 12 - prmu$imgs
prmu$matchups1 <- max(prmu$matchups)+1 - prmu$matchups

#map matchups per scene
plot(us, border="gray", main="Matchups per Landsat Tile,\n+/- 3 days", cex.main = 1.5)
plot(prmu, add=TRUE)
plot(prmu, col=viridis(max(prmu$matchups1), direction = -1)[prmu$matchups1], add=TRUE)
#plot(prmu, col=heat.colors(max(prmu$matchups1))[prmu$matchups1], add=TRUE) # rainbow() gray.colors()
color.legend(-122, 23, -119, 29, legend = c(1, max(prmu$matchups1)), rect.col = viridis(max(prmu$matchups1)), 
             gradient = "y", align = "rb", cex =1.2)



#map images per scene
plot(us, border="gray", main="Image Dates per Landsat Tile,\n+/- 3 days", cex.main = 1.5)
plot(prmu, add=TRUE)
plot(prmu, col=viridis(max(prmu$imgs1), direction = -1)[prmu$imgs1], add=TRUE)
color.legend(-122, 23, -119, 29, legend = c(1, max(prmu$imgs1)), rect.col = viridis(max(prmu$imgs1)), 
             gradient = "y", align = "rb", cex =1.2)



### plot coordinates
coords <- coordinates(data.frame(long=musub$LongitudeMeasure, lat=musub$LatitudeMeasure))
library(raster)
matchup_pts <- SpatialPoints(coords, proj4string=CRS(projection(pr)))

plot(us)
plot(matchup_pts, add = TRUE, pch = 20)

##########################


#same day


#1 day
#change imgs object to reflect each subset (d0, d01, d012, d0123)

#2 day


#boxplots **********
m1 <- subset(musub, musub$system.index %in% imgs[1:50,1])
boxplot(m1$chla_corr ~ m1$system.index, names=NULL, ylab="chl-a (ug/L)")#, xlab="image index", main="Images 1-50")

m2 <- subset(musub, musub$system.index %in% imgs[51:100,1])
boxplot(m2$chla_corr ~ m2$system.index, names=NULL, ylab="chl-a (ug/L)")#, xlab="image index", main="Images 51-100")

m3 <- subset(musub, musub$system.index %in% imgs[101:150,1])
boxplot(m3$chla_corr ~ m3$system.index, names=NULL, ylab="chl-a (ug/L)")#, xlab="image index", main="Images 101-150")

m4 <- subset(musub, musub$system.index %in% imgs[151:200,1])
boxplot(m4$chla_corr ~ m4$system.index, names=NULL, ylab="chl-a (ug/L)")#, xlab="image index", main="Images 151-200")

m5 <- subset(musub, musub$system.index %in% imgs[201:nrow(imgs),1])
boxplot(m5$chla_corr ~ m5$system.index, names=NULL, ylab="chl-a (ug/L)")#, xlab="image index", main="Images 201-232")


