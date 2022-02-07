
opar <- par()

source("C:/Users/WSALLS/Git/Sent2/cal_val.R")

nima_path <- "C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/S2/Nima/"

l2gen <- read.csv(file.path(nima_path, "Pahlevan_l2gen.csv"), stringsAsFactors = FALSE)
acolite <- read.csv(file.path(nima_path, "Pahlevan_acolite.csv"), stringsAsFactors = FALSE)

sum(is.na(l2gen$In.Situ.chl))

l2gen <- l2gen[!is.na(l2gen$In.Situ.chl), ]

# MCI validation

l2gen$MCI_rhot <- calc_mci(R1 = l2gen$rhot.665., R2 = l2gen$rhot.705., R3 = l2gen$rhot.740.)
l2gen$DCI_rhot <- calc_dci(R1 = l2gen$rhot.665., R2 = l2gen$rhot.705., R3 = l2gen$rhot.740.)

l2gen$MCI_rhos <- calc_mci(R1 = l2gen$rhos.665., R2 = l2gen$rhos.705., R3 = l2gen$rhos.740.)
l2gen$DCI_rhos <- calc_dci(R1 = l2gen$rhos.665., R2 = l2gen$rhos.705., R3 = l2gen$rhos.740.)

l2gen$MCI_Rrs <- calc_mci(R1 = l2gen$Rrs.665., R2 = l2gen$Rrs.705., R3 = l2gen$Rrs.740.)
l2gen$DCI_Rrs <- calc_dci(R1 = l2gen$Rrs.665., R2 = l2gen$Rrs.705., R3 = l2gen$Rrs.740.)

# for testing fn
obs <- l2gen$In.Situ.chl
p1 <- l2gen$MCI_rhos
portion_cal <- 0.8



chl_inds <- colnames(l2gen)[which(grepl("MCI_", colnames(l2gen)))]

source("C:/Users/WSALLS/Git/Sent2/cal_val.R")


#layout.matrix <- matrix(c(1:6), nrow = 2, ncol = 3)
#layout(mat = layout.matrix)
par(mfrow = c(3, 4))

error_df <- data.frame()

for (c in seq_along(chl_inds)) {
  print(sprintf("%s: %s", c, chl_inds[c]))
  proc_level <- substr(chl_inds[c], 5, 8)
  ra_name <- paste0(proc_level, ".665.")
  rc_name <- paste0(proc_level, ".740.")
  this_data <- l2gen
  
  valc <- cal_val(obs = this_data$In.Situ.chl, p1 = this_data[[chl_inds[c]]], 
          portion_cal = 0.8, main = chl_inds[c], 
          neg.rm = TRUE,
          neg_0 = FALSE,
          log_axes_val = "xy",
          xlim_cal = range(l2gen$In.Situ.chl, na.rm = TRUE), 
          ylim_cal = range(l2gen[, which(colnames(l2gen) %in% chl_inds)], na.rm = TRUE),
          xylim_val = range(l2gen$In.Situ.chl, na.rm = TRUE),
          pos_text_x_val = min(l2gen$In.Situ.chl, na.rm = TRUE))
  
  error_df <- rbind(error_df, cbind(data.frame(run = chl_inds[c]), valc))
  
  # filter sediment
  this_data$baseline_slope <- calc_baseline_slope(this_data[[ra_name]], this_data[[rc_name]])
  this_data <- this_data[this_data$baseline_slope > -0.00015, ]
  

  valc <- cal_val(obs = this_data$In.Situ.chl, p1 = this_data[[chl_inds[c]]], 
                  portion_cal = 0.8, main = paste0(chl_inds[c], "_noSed"),
                  neg.rm = TRUE,
                  neg_0 = FALSE,
                  log_axes_val = "xy",
                  xlim_cal = range(l2gen$In.Situ.chl, na.rm = TRUE), 
                  ylim_cal = range(l2gen[, which(colnames(l2gen) %in% chl_inds)], na.rm = TRUE),
                  pos_text_x_val = min(l2gen$In.Situ.chl, na.rm = TRUE))
  
  error_df <- rbind(error_df, cbind(data.frame(run = paste0(chl_inds[c], "_noSed")), valc))

}


rownames(error_df) <- NULL
error_df



