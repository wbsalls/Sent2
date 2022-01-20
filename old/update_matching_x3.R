### switch to matching with X.3 instead of X.5 since the latter isn't consistent

setwd("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Validation/681_imgs")

mu_mci_raw <- read.csv("validation_S2_682imgs_MCI_L1C_2018-11-21.csv", stringsAsFactors = FALSE)

mu_x <- mu_mci_raw[, which(colnames(mu_mci_raw) %in% c("X.5", "X.3"))]


## raw bands for sediment
raw_bands <- read.csv("mu_rawbands_3day.csv", stringsAsFactors = FALSE)

raw_bands_x3 <- merge(raw_bands, mu_x, by = "X.5")

write.csv(raw_bands_x3, "mu_rawbands_3day_X3.csv")


## bad imagery
img_comments_orig <- read.csv("ImageCheck_0day_comments.csv", stringsAsFactors = FALSE)

img_comments_x3 <- merge(img_comments_orig, mu_x, by.x = "point_IDX5", by.y = "X.5")

write.csv(img_comments_x3, "ImageCheck_0day_comments_X3.csv")

#

mu_mci_missing <- mu_mci[which(mu_mci$X.3 %in% missing_x3), 
                         which(colnames(mu_mci) %in% c("X.3", "PRODUCT_ID", "GRANULE_ID", "COMID", "shore_dist", "state", 
                                                       "chla_corr", "chla_s2", "chl_error", "dist_shore_m"))]
write.csv(mu_mci_missing, "missing_BRR.csv")

#missing_x3 <- mu_mci$X.3[which(!(mu_mci$X.3 %in% img_comments$X.3))]
