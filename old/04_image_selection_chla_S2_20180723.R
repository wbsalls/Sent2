library(rgdal)

setwd("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Matchups")
#setwd("/Users/wilsonsalls/Desktop/EPA/Sentinel2/Matchups")

mu_filt_all <- read.csv("out/Matchups_S2_chla_10day_filtered_2018-07-23.csv", stringsAsFactors = FALSE)


# subset to sameday (or don't)
#mu_filt <- mu_filt_all[mu_filt_all$offset_days == "same day", ]
mu_filt <- mu_filt_all


## offset_days freq table ----------------------------------------------------------------
offset_counts <- data.frame(table(mu_filt$offset_days)) # make freq table
colnames(offset_counts) <- c("offset_days", "n_matchups") # change colnames
offset_counts$offset_days <- as.numeric(as.character(offset_counts$offset_days)) # make offset_days numeric

for (row in 1:nrow(offset_counts)) {
  # cumulative matchups
  offset_counts$cum_matchups[row] <- sum(offset_counts$n_matchups[1:row])
  
  # images per day
  offset_counts$nimgs[row] <- length(unique(mu_filt$system.index[which(mu_filt$offset_days == offset_counts$offset_days[row])]))
  
  # cumulative images per day
  offset_counts$cum_imgs[row] <- length(unique(mu_filt$system.index[which(mu_filt$offset_days <= offset_counts$offset_days[row])]))
  
  # additional cumulative images per day
  if (row == 1) {
    offset_counts$cum_imgs_addl[row] <- offset_counts$cum_imgs[row]
  } else {
    offset_counts$cum_imgs_addl[row] <- offset_counts$cum_imgs[row] - offset_counts$cum_imgs[row - 1]
  }
}


barplot(offset_counts$n_matchups, names.arg=offset_counts$offset_days, ylab="# Matchups (non-cumulative)")
barplot(offset_counts$cum_matchups, names.arg=offset_counts$offset_days, ylab="Cumulative # Matchups")



## build img frequency table ----------------------------------------------------------------

img_freq <- data.frame(table(mu_filt$system.index)) # make freq table
colnames(img_freq) <- c("system.index", "n_matchups") # change colnames

ncols_img_freq <- ncol(img_freq)

for (d in 0:10) {
  print(d)
  
  mus_d <- c()
  
  for (i in 1:nrow(img_freq)) {
    mus_d <- c(mus_d, 
               nrow(mu_filt[which(mu_filt$offset_days == d & 
                                    mu_filt$system.index == img_freq$system.index[i]), ]))
  }
  img_freq <- cbind(img_freq, mus_d)
  colnames(img_freq)[ncols_img_freq + d + 1] <- sprintf("day%s_matchups", d)
}

img_freq_backup <- img_freq


### add information to frequency table ---------------------------------------------------

# add PRODUCT_ID
mu_filt_ID <- mu_filt[, c("system.index", "PRODUCT_ID")]
mu_filt_ID <- unique(mu_filt_ID[, c("system.index", "PRODUCT_ID")])
img_freq <- merge(img_freq, mu_filt_ID, by = "system.index", all.x = TRUE)


## get UUID from Copernicus metafile
metalist_cop <- read.csv("S2_metalist/S2_metalist_Cop_2018-07-10.csv", stringsAsFactors = FALSE)
cop_uuid <- metalist_cop[, c("Id", "Name")] # make data frame with just ID and Name to remove unnecessary fields

# change system.index to character (from factor) in order for merge to work properly
img_freq$system.index <- as.character(img_freq$system.index)

# merge
img_freq <- merge(img_freq, cop_uuid, all.x = TRUE, all.y = FALSE, 
                  by.x = "PRODUCT_ID", by.y = "Name")

sum(is.na(img_freq$Id))
# many images from matchup file (originally from GEE) can't be matched in Cop file.
# this is a problem since the Cop file has UUID necessary for bulk download.


## try to get Id for images not matched, using date-time from filenames - hasn't worked yet
# select imgs that did not have a match in the Copernicus file
img_freq_na <- img_freq[is.na(img_freq$Id), ]

# substring Cop names to get datetime
cop_uuid$partCopNames <- substr(cop_uuid$Name, 12, 26)

# for each image with no match in Cop file, substring name to get date; attempt to match with a partCopName; bind to salvaged_uuids
salvaged_uuids <- data.frame()
salvaged_count <- c()

img_freq_na$salvaged <- NA

for(r in 1:nrow(img_freq_na)) {
  if (r %% 50 == 0) {print(sprintf("%s of %s", r, nrow(img_freq_na)))}
  
  partGeePROD_ID <- substr(img_freq_na$PRODUCT_ID[r], 48, 62)
  
  matched_uuid <- cop_uuid[which(cop_uuid$partCopNames == partGeePROD_ID), ]
  
  salvaged_uuids <- rbind(salvaged_uuids, matched_uuid)
  
  salvaged_count <- c(salvaged_count, nrow(matched_uuid))
  
  #img_freq_na$salvaged[r] <- xxx
}




# remove NA Ids
img_freq <- img_freq[!is.na(img_freq$Id), ]

# investigate duplicates
length(unique(img_freq$PRODUCT_ID))
length(unique(img_freq$Id))

# create concatenated PRODUCT_ID and Id to see if these two fields are co-duplicated (yes, they are)
#img_freq$PRODID__Id <- paste0(img_freq$PRODUCT_ID, "__", img_freq$Id)
#length(unique(img_freq$PRODID__Id)) #196 - all PRODUCT_ID dups area also Id dups

# see duplicates
img_freq_dups <- img_freq[duplicated(data.frame(img_freq$PRODUCT_ID, img_freq$Id)), ]

# remove duplicates
img_freq <- img_freq[!duplicated(data.frame(img_freq$PRODUCT_ID, img_freq$Id)), ]


# remove images already downloaded
have_imgs <- substr(list.files("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Images/PhaseI/zips"), 1, 36)
have_imgs <- c(have_imgs, substr(list.files("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Images/PhaseII/zips"), 1, 36))
have_imgs <- c(have_imgs, substr(list.files("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Images/PhaseIII/zips"), 1, 36))

sum(img_freq$Id %in% have_imgs)
img_freq <- img_freq[-which((img_freq$Id %in% have_imgs) == TRUE), ]


## format and write image list

# reset rownames
rownames(img_freq) <- 1:nrow(img_freq)

# reorder columns
img_freq1 <- img_freq[, c(1:2, 15, 3:14)]

# write image download csv
write.csv(img_freq1, sprintf("out/images4download_S2_chla_10day_%s.csv", Sys.Date()))

# write image download txt
img_freq1 <- read.csv("out/images4download_S2_chla_10day_2018-07-25.csv", stringsAsFactors = FALSE)

write(img_freq1$Id, sprintf("out/images4download_S2_chla_10day_%s.txt", Sys.Date()))


#


### add auxilary information to image list ---------------------------------------------------

'
## add chl-a range in image

img_freq$chla_min <- NA
img_freq$chla_max <- NA

for (i in 1:nrow(img_freq)) {
  chla <- mu_filt$chla_corr[mu_filt$system.index == img_freq$system.index[i]]
  img_freq$chla_min[i] <- min(chla, na.rm = TRUE)
  img_freq$chla_max[i] <- max(chla, na.rm = TRUE)
}


## query states by tile

# read in tiling grid shapefile
tiles <- readOGR(dsn = "O:/PRIV/NERL_ORD_CYAN/Sentinel2/Matchups/tile_grid/tile_grid_S2_current", layer = "tile_grid_S2")

# read in and transform US shapefile
us83 <- readOGR(dsn = "O:/PRIV/NERL_ORD_CYAN/Salls_working/geospatial_general/US", layer = "cb_2015_us_state_20m")
us <- spTransform(us83, proj4string(tiles))

# query state data of all tiles (just takes one state in the case of multi-state overlap; ok for this purpose)
tiles_states <- over(tiles, us)
tiles$state <- tiles_states$STUSPS

# get tile id for merge
img_freq$tile <- substr(img_freq$system.index, 34, 38)

# merge state to freq_table
img_freq_states <- merge(img_freq, tiles@data[, c(2, 13)], by.x = "tile", by.y = "Name")


## write image table
write.csv(img_freq_states, sprintf("out/image_frequency_S2_chla_%s.csv", Sys.Date()))
'


### how many lakes do we have?
imgs <- read.csv("out/images4download_S2_chla_10day_OLDIMGSTOO_2018-07-26.csv")

mu_filt_imgs <- mu_filt_all[which(mu_filt_all$PRODUCT_ID %in% imgs$PRODUCT_ID), ]
mu_filt_imgs <- mu_filt_imgs[which(mu_filt_imgs$dist_shore_m >= 30), ]

length(unique(mu_filt_imgs$COMID))




#



#### scraps ------------------------------------------------------------------------------------------------------------

## add image name ------------------------------------------------------------------------------

# load img_freq_states (if not already loaded)
img_freq_states <- read.csv("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Matchups/out/image_frequency_S2_chla.csv", 
                            stringsAsFactors = FALSE)

# load GEE metalist
metalist_gee <- read.csv("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Matchups/S2_metalist/S2_metalist_GEE.csv", 
                         stringsAsFactors = FALSE)

img_freq_Name <- merge(img_freq_states, metalist_gee[, c("system.index", "PRODUCT_ID")], by = "system.index", 
                       all.x = TRUE, all.y = FALSE)

write.csv(img_freq_Name, "O:/PRIV/NERL_ORD_CYAN/Sentinel2/Matchups/out/image_frequency_S2_chla.csv")



###


# add # matchups per image for each matchup --------------------------------------------------------
img_freq <- data.frame(table(mu_filt$system.index))

mu_filt$img_freq <- NA
for (r in 1:nrow(mu_filt)) {
  mu_filt$img_freq[r] <- img_freq[which(img_freq$Var1 == mu_filt$system.index[r]), 2] 
}

#write.csv(mu_filt, "Matchups_S2_chla_3day_GEE_filtered_counts.csv")

data.frame(sort(table(mu_filt$system.index), decreasing = TRUE))
imgs_top <- names(sort(table(mu_filt$system.index), decreasing = TRUE)[1:5])

mu_topimgs <- mu_filt[mu_filt$system.index %in% imgs_top, ]



