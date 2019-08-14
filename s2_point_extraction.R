library(raster)
library(rgdal)
library(sp)
library(testit) # has_error


# query MCI values at in situ points ------------------------------------------------------------------------------------------

setwd("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation")

mu <- read.csv("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Matchups/out/Matchups_S2_chla_10day_filtered_2018-07-23.csv", stringsAsFactors = FALSE)
#mu <- read.csv("/Users/wilsonsalls/Desktop/EPA/S2/Validation/xxx", stringsAsFactors = FALSE)

# subset to same day (if desired)
#mu_sameday <- mu[mu$offset_days == "same day", ]

# set single-pixel extraction (FALSE) or 3x3 window extraction (TRUE)
window_extraction <- TRUE

# make spdf of matchups
lon <- mu$LongitudeMeasure # **
lat <- mu$LatitudeMeasure # **
mu_pts <- SpatialPointsDataFrame(coords = matrix(c(lon, lat), ncol = 2), 
                                 mu, proj4string = CRS("+init=epsg:4326"))

# append state name to each point
us <- readOGR("O:/PRIV/NERL_ORD_CYAN/Salls_working/geospatial_general/US", "cb_2015_us_state_20m")
#us <- readOGR("/Users/wilsonsalls/Desktop/EPA/geosp_general/cb_2016_us_state_20m", "cb_2016_us_state_20m")
mu_pts_states <- spTransform(mu_pts, crs(us))
mu_pts_states$state <- over(mu_pts_states, us)$STUSPS

# append COMID to each point
lakes <- readOGR("O:/PRIV/NERL_ORD_CYAN/Salls_working/geospatial_general/resolvableLakes/geospatial", "resolvableLakes90m")
#lakes <- readOGR("/Users/wilsonsalls/Desktop/EPA/geosp_general/resolvableLakes/intermediate", "resolvableLakes90m")
mu_pts_lakes <- spTransform(mu_pts_states, crs(lakes))
mu_pts_lakes$comid <- over(mu_pts_lakes, lakes)$COMID

mu_pts <- mu_pts_lakes

# write to shapefile, csv
'
mu_pts$modeled_MCI_L1C <- (mu_pts$MCI_L1C + 0.00357) / 0.000222 # chla = 4504.5 * MCI + 16.08 {L1C, extreme negs removed}
#mu_pts$modeled_MCI_L1C <- (mu_pts$MCI_L1C + 0.0054) / 0.000232 # chla = 4310.3 * MCI + 23.27 {L1C, all}
mu_pts$residual_MCI_L1C <- abs(mu_pts$modeled_MCI_L1C - mu_pts$chla_corr)
mu_pts_brief <- mu_pts[, c(9, 60, 6, 194, 195, 188, 196, 197)] # take only relevant columns
mu_pts_brief <- mu_pts_brief[!is.na(mu_pts_brief$MCI_L1C), ] # remove NA
mu_pts_brief <- mu_pts_brief[mu_pts_brief$MCI_L1C == 0, ] # remove bad L1C (0)
mu_pts_brief$uniqueID <- 1:nrow(mu_pts_brief@data)
mu_pts_brief@data <- mu_pts_brief@data[, c(9, 1:8)]
writeOGR(mu_pts_brief, "O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/geospatial", "matchups_S2_chla", driver = "ESRI Shapefile")
write.csv(mu_pts_brief@data, "O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/validation_S2_50imgs_brief.csv")
'

# location of images
rfolder <- "D:/s2/mci_resample20"
#rfolder <- "/Users/wilsonsalls/Desktop/EPA/S2/Images"
rfolder <- "O:/PRIV/NERL_ORD_CYAN/Sentinel2/Images/MCI_BRR"

safe_folder <- "D:/s2/raw"

# for reading files
process_prefix <- "mci_brr_resample20_"

# for naming MCI output files
process_name <- "MCI_BRR"

# check image name matching
# ***************** consider using GRANULE_ID instead if possible since PRODUCT_ID is not unique and results in duplicates *****
# these duplicates are probably not a problem since they should have same MCI value, and thus get filtered out
# (can make unique image list with PRODUCT_ID, GRANULE_ID in matchups)
mci_imgs <- list.files(rfolder, pattern = "\\.data")
#mci_img_names <- substr(mci_imgs, 16, 75)
mci_img_names <- gsub(process_prefix, "", mci_imgs)
mci_img_names <- gsub(".data", "", mci_img_names)
length(unique(mu_pts$PRODUCT_ID))
length(mci_img_names)
sum(mci_img_names %in% mu_pts$PRODUCT_ID)
sum(unique(mu_pts$PRODUCT_ID) %in% mci_img_names)


## ----------------------------------------------------



# run extraction
start_date <- Sys.Date()
start_time <- Sys.time()
for (i in 1:length(mci_imgs)) {
  
  # initialize variable for use below
  notes <- ""
  n_cloud_layers <- 0
  
  # subset points to those in this image
  mu_pts_img <- mu_pts[mu_pts$PRODUCT_ID == sub(".data", "", sub(process_prefix, "", mci_imgs[i])), ]
  
  # progress
  cat(sprintf("\nimage #%s of %s (%s) - %s pts\n", i, length(mci_imgs), Sys.time(), nrow(mu_pts_img)))
  
  # load raster
  
  # if mci raster doesn't exist, note in summary file and skip to next
  if (!("MCI.img" %in% list.files(file.path(rfolder, mci_imgs[i])))) {
    print("NO MCI IMAGE!!!")
    img_summary_i <- data.frame(img = mci_imgs[i], 
                                n_cloud_layers = NA, 
                                n_pts_tot = nrow(mu_pts_img@data), 
                                n_pts_cloudy = 0, 
                                n_pts_noncloudy = 0,
                                notes = "NO MCI RASTER; ")
    
    if (has_error(read.csv(sprintf("validation_S2_682imgs_%s_img_summary_%s.csv", process_name, Sys.Date())))) {
      write.table(img_summary_i, sprintf("validation_S2_682imgs_%s_img_summary_%s.csv", process_name, start_date),
                  sep = "|", append = FALSE, row.names = FALSE, col.names = TRUE)
    } else {
      write.table(img_summary_i, sprintf("validation_S2_682imgs_%s_img_summary_%s.csv", process_name, start_date),
                  sep = "|", append = TRUE, row.names = FALSE, col.names = FALSE)
    }
    
    next
  }
  
  # load raster
  mci_i <- raster(file.path(rfolder, mci_imgs[i], "MCI.img"))
  
  # if no points, update img_summary and skip to next image
  if (length(mu_pts_img) == 0) {
    
    # update img_summary dataframe
    img_summary_i <- data.frame(img = mci_imgs[i], 
                                n_cloud_layers = NA, 
                                n_pts_tot = nrow(mu_pts_img@data), 
                                n_pts_cloudy = 0, 
                                n_pts_noncloudy = 0,
                                notes = "no points in image; ")
    
    if (has_error(read.csv(sprintf("validation_S2_682imgs_%s_img_summary_%s.csv", process_name, Sys.Date())))) {
      write.table(img_summary_i, sprintf("validation_S2_682imgs_%s_img_summary_%s.csv", process_name, start_date),
                  sep = "|", append = FALSE, row.names = FALSE, col.names = TRUE)
    } else {
      write.table(img_summary_i, sprintf("validation_S2_682imgs_%s_img_summary_%s.csv", process_name, start_date),
                  sep = "|", append = TRUE, row.names = FALSE, col.names = FALSE)
    }
    
    next
  }
  
  # reproject points
  mu_pts_img_proj <- spTransform(mu_pts_img, crs(mci_i))
  
  ## remove cloudy points
  tiles_img <- unique(mu_pts_img_proj$tileID)
  
  # check layers in cloud mask - should be 1, but at least one file has 0
  granule_dir <- file.path(safe_folder, paste0(mci_img_names[i], ".SAFE"), "GRANULE")
  granule_folders <- list.files(granule_dir)
  
  for (t in 1:length(tiles_img)) {
    
    granule_folder <- granule_folders[which((grepl(tiles_img[t], granule_folders)))]
    
    qi_data <- list.files(file.path(granule_dir, granule_folder, "QI_DATA"), pattern = ".gml")
    cloud_file <- qi_data[grep("MSK_CLOUDS", qi_data)]
    layers <- ogrListLayers(file.path(granule_dir, granule_folder, "QI_DATA", cloud_file))
    n_cloud_layers <- n_cloud_layers + length(layers)
    
    # if the layer exists: load cloud mask; get point indices falling on cloud; remove these points
    if (length(layers) == 1) {
      gml <- readOGR(file.path(granule_dir, granule_folder, "QI_DATA", cloud_file), "MaskFeature",
                     disambiguateFIDs = TRUE, verbose = FALSE)
      
      # assign crs to cloud mask if missing (which it always seems to be)
      if (is.na(crs(gml))) {
        crs(gml) <- crs(mu_pts_img_proj)
      }
      
      # query cloud mask value at locations of points
      cloud_pts_index <- over(mu_pts_img_proj, gml)
      
      # only include points with extracted value of NA (meaning they don't fall over a cloud - whether in this mask or a different one)
      mu_pts_img_proj <- mu_pts_img_proj[is.na(cloud_pts_index$gml_id), ]
      
      # subset to cloudy points (if any)
      cloudy_pts_i <- mu_pts_img_proj@data[!is.na(cloud_pts_index$gml_id), ]
      
      # append cloudy points to cloudypts table
      if (nrow(cloudy_pts_i) > 0) {
        if (has_error(read.csv(sprintf("validation_S2_682imgs_%s_cloudypts_%s.csv", process_name, Sys.Date())))) {
          write.table(cloudy_pts_i , sprintf("validation_S2_682imgs_%s_cloudypts_%s.csv", process_name, Sys.Date()),
                      sep = "|", append = FALSE, row.names = FALSE, col.names = TRUE)
        } else {
          write.table(cloudy_pts_i , sprintf("validation_S2_682imgs_%s_cloudypts_%s.csv", process_name, Sys.Date()),
                      sep = "|", append = TRUE, row.names = FALSE, col.names = FALSE)
        }
        print("cloud(s)")
      }
    }
  }
  
  # only proceed if there are still points remaining after cloud removal
  if (nrow(mu_pts_img_proj) > 0) {
    
    # initialize dataframe for this image
    mu_mci_i <- data.frame()
    
    # extract
    if (window_extraction == TRUE) {
      for (p in 1:length(mu_pts_img_proj)) {
        
        # print progress inline
        cat(paste(p, " "))
        
        # get cellNum under this pt
        cellNum <- cellFromXY(mci_i, mu_pts_img_proj@coords[p, ])
        
        # get cell indices for 3x3 window
        window_indices <- adjacent(mci_i, cells = cellNum, directions = 8, include = TRUE)
        
        # get values from those indices
        window_vals <- mci_i[window_indices[, 2]]
        
        # make NAs if cellNum is NA
        if (is.na(cellNum)) {
          window_vals <- rep(NA, 9)
        }
        
        # extract value summaries
        '
        window_df <- data.frame(
        nPts_CIwin = length(window_vals),
        nNA_CIwin = sum(is.na(window_vals)),
        mean_CIwin = mean(window_vals, na.rm = TRUE),
        median_CIwin = median(window_vals, na.rm = TRUE),
        min_CIwin = min(window_vals, na.rm = TRUE),
        max_CIwin = max(window_vals, na.rm = TRUE),
        var_CIwin = var(window_vals, na.rm = TRUE)
        )'
        
        # extract actual cell values
        window_df <- data.frame(t(rep(NA, 9)))
        window_df[1:length(window_vals)] <- data.frame(t(window_vals))
        colnames(window_df) <- paste0("MCI_val_", 1:9)
        
        # bind to mu_mci
        mu_mci_i <- rbind(mu_mci_i, cbind(mu_pts_img_proj@data[p, ], window_df))
      }
      
    } else {
      # extract single-pixel mci value(s) and add as new column
      mci_vals <- extract(mci_i, mu_pts_img_proj)
      mu_pts_img_proj$mci <- mci_vals
      
      # append these points to mu_mci df
      mu_mci_i <- mu_pts_img_proj@data
    }
    
    # append points to validation table
    if (has_error(read.csv(sprintf("validation_S2_682imgs_%s_%s.csv", process_name, Sys.Date())))) {
      # this occurs on the first iteration and will print an error, but it's ok
      write.table(mu_mci_i, sprintf("validation_S2_682imgs_%s_%s.csv", process_name, Sys.Date()),
                  sep = "|", append = FALSE, row.names = FALSE, col.names = TRUE)
    } else {
      write.table(mu_mci_i, sprintf("validation_S2_682imgs_%s_%s.csv", process_name, Sys.Date()),
                  sep = "|", append = TRUE, row.names = FALSE, col.names = FALSE)
    }
  }
  
  # update img_summary dataframe
  img_summary_i <- data.frame(img = mci_imgs[i], 
                              n_cloud_layers = n_cloud_layers, 
                              n_pts_tot = nrow(mu_pts_img@data), 
                              n_pts_cloudy = nrow(mu_pts_img@data) - nrow(mu_pts_img_proj@data), 
                              n_pts_noncloudy = nrow(mu_pts_img_proj@data),
                              notes = notes)
  
  if (has_error(read.csv(sprintf("validation_S2_682imgs_%s_img_summary_%s.csv", process_name, Sys.Date())))) {
    # this occurs on the first iteration and will print an error, but it's ok
    write.table(img_summary_i, sprintf("validation_S2_682imgs_%s_img_summary_%s.csv", process_name, start_date),
                sep = "|", append = FALSE, row.names = FALSE, col.names = TRUE)
  } else {
    write.table(img_summary_i, sprintf("validation_S2_682imgs_%s_img_summary_%s.csv", process_name, start_date),
                sep = "|", append = TRUE, row.names = FALSE, col.names = FALSE)
  }
}

print(sprintf("started: %s | finished: %s", start_time, Sys.time()))


# get | csvs back to comma csvs
#output_date <- Sys.Date()
output_date <- "2019-08-09"

valpipes <- read.table(sprintf("681_imgs/old/pipes/validation_S2_682imgs_%s_%s.csv", process_name, output_date), 
                       stringsAsFactors = FALSE, sep = "|", header = TRUE, quote = "\"")
cloudpipes <- read.table(sprintf("681_imgs/old/pipes/validation_S2_682imgs_%s_cloudypts_%s.csv", process_name, output_date), 
                         stringsAsFactors = FALSE, sep = "|", header = TRUE, quote = "\"")
summarypipes <- read.table(sprintf("681_imgs/old/pipes/validation_S2_682imgs_%s_img_summary_%s.csv", process_name, output_date), 
                           stringsAsFactors = FALSE, sep = "|", header = TRUE, quote = "\"")

write.csv(valpipes, sprintf("681_imgs/validation_S2_682imgs_%s_%s.csv", process_name, output_date))
write.csv(cloudpipes, sprintf("681_imgs/validation_S2_682imgs_%s_cloudypts_%s.csv", process_name, output_date))
write.csv(summarypipes, sprintf("681_imgs/validation_S2_682imgs_%s_img_summary_%s.csv", process_name, output_date))
