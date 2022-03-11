# This script creates a table containing saptiotemporal matches between an input file of in situ data and an image metafile.

# set threshold for maximum number of days between Landsat overpass and sample collection
# set variable names

# update image metafile
# add to daylight savings time dates, if necessary

library(chron)
library(rgdal)
library(raster)
library(sp)
library(rgeos)


### inputs ### ----------------------------------------------------

# input in situ data path
setwd("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Matchups")

# read in WQ input data, created from InputFormatter_CONUS_03_all_BIO_STA_cols.R
wq <- read.csv("WQP/20180710/input.csv", stringsAsFactors=FALSE)

# if subsetting to only some rows in in situ file (e.g. if multithreading)
n_start <- 1 # 1
n_end <- 13301 #nrow(wq)

# provide suffix for output file name
file_suffix <- sprintf("_S2_chla_%s_%s", n_start, n_end)

# set max size of time window desired on either side of sample (can be subset to smaller window later, so be generous)
include_days <- 10

# specify variable names in in situ file
wq_lon <- "LongitudeMeasure" # "LongitudeMeasure" for WQP
wq_lat <- "LatitudeMeasure" # "LatitudeMeasure" for WQP
wq_date <- "ActivityStartDate" # "ActivityStartDate" for WQP
wq_time <- "ActivityStartTime.Time" # set NULL if no time. # "ActivityStartTime.Time" for WQP

# specify date and time format in in situ file
wq_date_format <- "y-m-d" # "y-m-d" for WQP
wq_time_format <- "h:m:s" # "h:m:s" for WQP


## reference files

# read in metafile
img_file <- read.csv(file.path("S2_metalist", "S2_metalist_GEE_2018-07-10.csv"), stringsAsFactors=FALSE)

# read in tiling grid shapefile
pr_shp <- readOGR(dsn = file.path("tile_grid/tile_grid_S2_current"), layer = "tile_grid_S2")

# read in time zone shapefile
tz_shp <- readOGR(dsn = file.path("O:/PRIV/NERL_ORD_CYAN/Landsat/xMatchupFiles/timezones"),  
                  layer = "timezones_CONUS_final")

# read in MERIS resolvable lakes shapefile (revised version from Erin and Blake) - for shore dist.
lakes <- readOGR(dsn = file.path("O:/PRIV/NERL_ORD_CYAN/Salls_working/geospatial_general/resolvableLakes/geospatial"), 
                 layer = "resolvableLakes60m")

# convert lake polygons to lines for calculating distance from shore
lakes_lines <- as(lakes, "SpatialLines")


### end inputs ### ----------------------------------------------------


### set up ### 

# if no time given, make all times noon (using h:m:s)
if (is.null(wq_time)) {
  wq_time <- "wq_time_default"
  wq$wq_time_default <- "12:00:00"
}

# time zone converter
tzonetab <- data.frame(rbind(c("EST",5),
                             c("EDT",4),
                             c("CST",6),
                             c("CDT",5),
                             c("MST",7),
                             c("MDT",6),
                             c("PST",8),
                             c("PDT",7),
                             c("AST",7),
                             c("ADT",7)), stringsAsFactors=FALSE)
colnames(tzonetab) <- c("TIMEZONE", "forUTC")


# make a pointfile using the lat/long from the sample locations
coords <- coordinates(data.frame(long=wq[, wq_lon], lat=wq[, wq_lat]))
samp_pts <- SpatialPoints(coords, proj4string=CRS(projection(pr_shp)))

# reproject this pointfile so it can be used with the lakes shapefiles
samp_pts_albers <- spTransform(samp_pts, proj4string(lakes))


# remove columns from image file
rm_img_cols <- c("system.footprint", ".geo")
img_file <- img_file[, -which(colnames(img_file) %in% rm_img_cols)]


#### perform matchups #### ----------------------------------------------------------------------------

# make empty matchup table to be populated below
mutab <- data.frame()

# for each water quality sample:
start <- Sys.time()
for (wrow in n_start:n_end) {
  
  # track progress
  wstart <- Sys.time()
  print(sprintf("Matching images for sample %s of %s (#%s of %s in this session) at %s", wrow, nrow(wq), 
                (wrow - n_start + 1), (n_end - n_start + 1), wstart))
  
  # for error commenting later
  wq_comment <- ""
  
  # for preventing unnecessary duplicate spatial querying - resetting
  sample_location_status <- "first time"
  
  
  # select wrowth sampling point for identifying time zone and Landsat scene
  wpt <- samp_pts[wrow]
  
  # get date-time, setting to 1 am if no time given
  if (is.na(wq[wrow, wq_time])) {
    wdt_local <- chron(dates.=wq[wrow, wq_date], 
                      times.= "0:01:00", 
                      format=c(dates=wq_date_format, times=wq_time_format), 
                      out.format=c(dates="m/d/y", times="h:m:s"))
    wq_comment <- paste(wq_comment, "no in situ time; ")
  } else {
    wdt_local <- chron(dates.=wq[wrow, wq_date], 
                      times.=wq[wrow, wq_time], 
                      format=c(dates=wq_date_format, times=wq_time_format), 
                      out.format=c(dates="m/d/y", times="h:m:s"))
  }
  
  # convert date-time to UTC
  ## get time zone
  w_tz <- over(samp_pts[wrow], tz_shp)$TimeZone
  
  if (is.na(w_tz)) {
    w_tz <- "Central" # just a proxy to get time; can correct in table later if nec.
    wq_comment <- paste(wq_comment, "point falls outside any timezone; ")
  }
  
  ## determine whether daylight savings time (S = standard/no, D = daylight/yes)
  ## based on http://www.timetemperature.com/tzus/daylight_saving_time.shtml
  dst <- "not set"
  if (
    (wdt_local >= chron(dates.="11/1/09", times.="02:00:00") & 
     wdt_local < chron(dates.="3/14/10", times.="02:00:00")) |
    (wdt_local >= chron(dates.="11/7/10", times.="02:00:00") & 
     wdt_local < chron(dates.="3/13/11", times.="02:00:00")) |
    (wdt_local >= chron(dates.="11/6/11", times.="02:00:00") & 
     wdt_local < chron(dates.="3/11/12", times.="02:00:00")) |
    (wdt_local >= chron(dates.="11/4/12", times.="02:00:00") & 
     wdt_local < chron(dates.="3/10/13", times.="02:00:00")) |
    (wdt_local >= chron(dates.="11/3/13", times.="02:00:00") & 
     wdt_local < chron(dates.="3/9/14", times.="02:00:00")) | 
    (wdt_local >= chron(dates.="11/2/14", times.="02:00:00") & 
     wdt_local < chron(dates.="3/8/15", times.="02:00:00")) |
    (wdt_local >= chron(dates.="11/1/15", times.="02:00:00") & 
     wdt_local < chron(dates.="3/13/16", times.="02:00:00")) |
    (wdt_local >= chron(dates.="11/6/16", times.="02:00:00") & 
     wdt_local < chron(dates.="3/12/17", times.="02:00:00")) |
    (wdt_local >= chron(dates.="11/5/17", times.="02:00:00") & 
     wdt_local < chron(dates.="3/11/18", times.="02:00:00")) |
    (wdt_local >= chron(dates.="11/4/18", times.="02:00:00") & 
     wdt_local < chron(dates.="3/10/19", times.="02:00:00"))
  ) 
  {
    dst <- "S" # Standard Time
  } else if (
    (wdt_local >= chron(dates.="3/8/09", times.="02:00:00") & 
     wdt_local < chron(dates.="11/1/09", times.="02:00:00")) |
    (wdt_local >= chron(dates.="3/14/10", times.="02:00:00") & 
     wdt_local < chron(dates.="11/7/10", times.="02:00:00")) |
    (wdt_local >= chron(dates.="3/13/11", times.="02:00:00") & 
     wdt_local < chron(dates.="11/6/11", times.="02:00:00")) |
    (wdt_local >= chron(dates.="3/11/12", times.="02:00:00") & 
     wdt_local < chron(dates.="11/4/12", times.="02:00:00")) |
    (wdt_local >= chron(dates.="3/10/13", times.="02:00:00") & 
     wdt_local < chron(dates.="11/3/13", times.="02:00:00")) |
    (wdt_local >= chron(dates.="3/9/14", times.="02:00:00") & 
     wdt_local < chron(dates.="11/2/14", times.="02:00:00")) | 
    (wdt_local >= chron(dates.="3/8/15", times.="02:00:00") & 
     wdt_local < chron(dates.="11/1/15", times.="02:00:00")) |
    (wdt_local >= chron(dates.="3/13/16", times.="02:00:00") & 
     wdt_local < chron(dates.="11/6/16", times.="02:00:00")) |
    (wdt_local >= chron(dates.="3/12/17", times.="02:00:00") & 
     wdt_local < chron(dates.="11/5/17", times.="02:00:00")) |
    (wdt_local >= chron(dates.="3/11/18", times.="02:00:00") & 
     wdt_local < chron(dates.="11/4/18", times.="02:00:00")) |
    (wdt_local >= chron(dates.="3/10/19", times.="02:00:00") & 
     wdt_local < chron(dates.="11/3/19", times.="02:00:00"))
  ) 
  {
    dst <- "D" # Daylight Savings Time
  } else {
    dst <- "S"
    wq_comment <- paste(wq_comment, "DST error; ")
  }
  
  ## select proper time zone-dst - uses first letter of spatially queried time zone, dst variable, and "T" to construct time zone code
  assignedTimeZoneCode <- sprintf("%s%sT", substr(w_tz, 1, 1), dst)
  
  ## assign UTC time
  wdt_utc <- wdt_local + as.numeric(tzonetab$forUTC[which(tzonetab$TIMEZONE==assignedTimeZoneCode)])/24
  
  # overlay point onto path/row shp to extract tile ID
  w_pr <- over(samp_pts[wrow], pr_shp, returnList = TRUE)[[1]]
  
  # for each path/row returned:
  for (pr in 1:nrow(w_pr)) {
    tile <- as.character(w_pr$Name[pr])
    
    # subset metafile to tile ID
    img_file_pr <- subset(img_file, (img_file$tileID==tile))
    
    if (nrow(img_file_pr) == 0) {
      print("No images for this tile")
      next
    }
    
    # for each img in this path/row:
    for (irow in 1:nrow(img_file_pr)) {
      # get image date-time
      idt_utc <- chron(dates.=img_file_pr$date_img[irow], 
                   times.=img_file_pr$time_img[irow], 
                   format=c(dates="y-m-d", times="h:m:s"), 
                   out.format=c(dates="m/d/y", times="h:m:s"))
      
      # convert image date-time to local time
      idt_local <- idt_utc - as.numeric(tzonetab$forUTC[which(tzonetab$TIMEZONE == assignedTimeZoneCode)]) / 24
      
      # if img date is in time window...
      offset_days <- as.numeric(abs(dates(wdt_local) - dates(idt_local)))
      if (offset_days <= include_days) {
        
        print(sprintf("    **Adding matchup (#%s)**", nrow(mutab) + 1))
        
        if (sample_location_status == "first time") {
        # get COMID of lake; can filter NAs later to remove non-resolvable - perform subsequently
        comid <- over(samp_pts_albers[wrow], lakes)$COMID
        
        # calculate distance to shore - perform subsequently
        dist_shore_m <- gDistance(samp_pts_albers[wrow], lakes_lines)
        }
        
        sample_location_status <- "already spatially queried"
        
        #add row to matchup table
        addrow <- data.frame(COMID=comid,
                             samp_UTC=wdt_utc,
                             samp_localTime= wdt_local,
                             img_UTC=idt_utc,
                             img_localTime= idt_local,
                             offset_days,
                             offset_hrs=abs(wdt_local - idt_local)/24,
                             #imgName=img_file_pr$system.index[irow],
                             #imgID=img_file_pr$Id[irow],
                             #tileID=tile,
                             #clouds=img_file_pr$cloudCoverFull[irow],
                             #sun_elev=img_file_pr$sunElevation[irow],
                             #nadir=img_file_pr$NADIR_OFFNADIR[irow],
                             img_file_pr[irow, ],
                             wq[wrow, ],
                             dist_shore_m,
                             wq_comment = wq_comment)
        mutab <- rbind (mutab, addrow)
      }
    }
  }
  
  #progress
  wend <- Sys.time()
  welapsed <-round(wend-wstart, 0)
  elapsed <- round(wend-start, 1)
  remains <- (wend-start)/(wrow-(n_start)+1)*(n_end-wrow)
  print(sprintf("  Row done after %s %s; %s %s elapsed; est. completion at %s (%s %s more)", 
                welapsed, units(welapsed), 
                elapsed, units(elapsed), 
                wend+remains, 
                round(remains, 1), units(remains)))
}


###

write.csv(mutab, sprintf("Matchups_%sday_%s%s.csv", include_days, Sys.Date(), file_suffix))

print("Finished!!!")

#########
