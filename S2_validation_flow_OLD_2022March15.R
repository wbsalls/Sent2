
opar <- par()

nima_path <- "C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/S2/Nima/"

l2gen <- read.csv(file.path(nima_path, "Pahlevan_l2gen.csv"), stringsAsFactors = FALSE)
acolite <- read.csv(file.path(nima_path, "Pahlevan_acolite.csv"), stringsAsFactors = FALSE)

## pick data source
chldata <- l2gen

# remove NA in situ chl
sum(is.na(chldata$In.Situ.chl))
chldata <- chldata[!is.na(chldata$In.Situ.chl), ]

# MCI validation

chldata$MCI_rhot <- calc_mci(R1 = chldata$rhot.665., R2 = chldata$rhot.705., R3 = chldata$rhot.740.)
chldata$DCI_rhot <- calc_dci(R1 = chldata$rhot.665., R2 = chldata$rhot.705., R3 = chldata$rhot.740.)
chldata$NDCI_rhot <- calc_ndci(R1 = chldata$rhot.665., R2 = chldata$rhot.705.)

chldata$MCI_rhos <- calc_mci(R1 = chldata$rhos.665., R2 = chldata$rhos.705., R3 = chldata$rhos.740.)
chldata$DCI_rhos <- calc_dci(R1 = chldata$rhos.665., R2 = chldata$rhos.705., R3 = chldata$rhos.740.)
chldata$NDCI_rhos <- calc_ndci(R1 = chldata$rhos.665., R2 = chldata$rhos.705.)

chldata$MCI_Rrs <- calc_mci(R1 = chldata$Rrs.665., R2 = chldata$Rrs.705., R3 = chldata$Rrs.740.)
chldata$DCI_Rrs <- calc_dci(R1 = chldata$Rrs.665., R2 = chldata$Rrs.705., R3 = chldata$Rrs.740.)
chldata$NDCI_Rrs <- calc_ndci(R1 = chldata$Rrs.665., R2 = chldata$Rrs.705.)

# for testing fn
obs <- chldata$In.Situ.chl
p1 <- chldata$MCI_rhos
portion_cal <- 0.8



#chl_inds <- colnames(chldata)[which(grepl("CI_", colnames(chldata)))]
chl_inds <- colnames(chldata)[which(grepl("MCI_", colnames(chldata)) | grepl("NDCI_", colnames(chldata)))]
chl_inds <- colnames(chldata)[which(grepl("MCI_", colnames(chldata)))]
chl_inds <- colnames(chldata)[which(grepl("NDCI_", colnames(chldata)))]
chl_inds <- chl_inds[-(which(chl_inds == "NDCI_Rrs"))]

source("C:/Users/WSALLS/Git/Sent2/cal_val.R")


#layout.matrix <- matrix(c(1:6), nrow = 2, ncol = 3)
#layout(mat = layout.matrix)

# n plots required
length(chl_inds) * 2 * 2 # chl_inds * cal+val * sed+no

#par(mfrow = c(2, 2))
par(mfrow = c(3, 4))

filter_sed <- TRUE #TRUE FALSE

error_df <- data.frame()

for (c in seq_along(chl_inds)) {
  print(sprintf("%s: %s", c, chl_inds[c]))
  proc_level <- strsplit(chl_inds[c], "_")[[1]][2]
  ra_name <- paste0(proc_level, ".665.")
  rc_name <- paste0(proc_level, ".740.")
  this_data <- chldata
  
  valc <- cal_val(obs = this_data$In.Situ.chl, p1 = this_data[[chl_inds[c]]], 
                  portion_cal = 0.8, main = chl_inds[c], 
                  neg.rm = TRUE,
                  negs2zero = FALSE,
                  log_axes_val = "xy",
                  xlim_cal = range(chldata$In.Situ.chl, na.rm = TRUE), 
                  ylim_cal = c(-1, 1), #range(chldata[, which(colnames(chldata) %in% chl_inds)], na.rm = TRUE), #c(-1, 1), #
                  #xylim_val = c(0.01, max(chldata$In.Situ.chl, this_data[[chl_inds[c]]], na.rm = TRUE)),
                  pos_text_x_val = min(chldata$In.Situ.chl, na.rm = TRUE))
  
  error_df <- rbind(error_df, cbind(data.frame(run = chl_inds[c]), valc))
  
  if(filter_sed) {
    
    # filter sediment
    this_data$baseline_slope <- calc_baseline_slope(this_data[[ra_name]], this_data[[rc_name]])
    this_data <- this_data[this_data$baseline_slope > -0.00015, ]
    
    
    valc <- cal_val(obs = this_data$In.Situ.chl, p1 = this_data[[chl_inds[c]]], 
                    portion_cal = 0.8, main = paste0(chl_inds[c], "_noSed"),
                    neg.rm = TRUE,
                    negs2zero = FALSE,
                    log_axes_val = "xy",
                    xlim_cal = range(chldata$In.Situ.chl, na.rm = TRUE), 
                    ylim_cal = c(-1, 1), #range(chldata[, which(colnames(chldata) %in% chl_inds)], na.rm = TRUE), #c(-1, 1), #
                    pos_text_x_val = min(chldata$In.Situ.chl, na.rm = TRUE))
    
    error_df <- rbind(error_df, cbind(data.frame(run = paste0(chl_inds[c], "_noSed")), valc))
  }
}


rownames(error_df) <- NULL
error_df



