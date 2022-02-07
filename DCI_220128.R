
# note that ra, rb, rc, la, lb, lc were changed to R1, R2, R3, w1, w2, w3
# relics of that format remain

source("C:/Users/WSALLS/Git/Sent2/error_metrics_220120.R")

setwd("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/DCI")

chl_file <- "ontario" # ontario, erie
mu_mci <- read.csv(sprintf("mu_mci_finalset_2019-11-27_s2_chl_%s.csv", chl_file), stringsAsFactors = FALSE)


# dynamic chlorophyll index

calc_mci <- function(R1, R2, R3, w1 = 665, w2 = 705, w3 = 740) {
  mci <- R2 - R1 - (((w2 - w1) / (w3 - w1)) * (R3 - R1))
  return(mci)
}

calc_dci <- function(R1, R2, R3, w1 = 665, w2 = 705, w3 = 740) {
  if (NA %in% c(R1, R2, R3)) {
    return(NA)
  }
  
  mab <- (R2 - R1) / (w2 - w1)
  mbc <- (R3 - R2) / (w3 - w2)
  
  if (abs(mbc) > mab) {
    # left peak
    mbc <- (R3 - R2) / (w3 - w2)
    rd <- R1
    ld <- ((R3 - rd) / mbc) + w3
    le <- (w1 + ld) / 2
    re <- R3 - mbc * (w3 - le)
    dci <- calc_mci(R1 = R1, R2 = re, R3 = R3, w2 = le)
  } else {
    # right peak
    # calc here
    dci <- calc_mci(R1 = R1, R2 = R2, R3 = R3)
  }
  return(dci)
}

calc_baseline_slope <- function(R1, R3, w1 = 665, w3 = 740) {
  bslope <- (R3 - R1) / (w3 - w1)
  return(bslope)
}



# checking MCI calc
mu_mci$mci_calc <- calc_mci(R1 = mu_mci$b4_1, R2 = mu_mci$b5_1, R3 = mu_mci$b6_1)
mu_mci$mci_brr_calc <- calc_mci(R1 = mu_mci$b4_1_BRR, R2 = mu_mci$b5_1_BRR, R3 = mu_mci$b6_1_BRR)
plot(mu_mci$MCI_L1C_1, mu_mci$mci_calc/10000)
plot(mu_mci$MCI_BRR_1, mu_mci$mci_brr_calc)
abline(0,1)

# calc DCI
mu_mci$dci <- mapply(calc_dci, R1 = mu_mci$b4_1, R2 = mu_mci$b5_1, R3 = mu_mci$b6_1)
mu_mci$dci_brr <- mapply(calc_dci, R1 = mu_mci$b4_1_BRR, R2 = mu_mci$b5_1_BRR, R3 = mu_mci$b6_1_BRR)
plot(mu_mci$mci_calc, mu_mci$dci)
plot(mu_mci$mci_brr_calc, mu_mci$dci_brr)
abline(0,1)

# calc chl
slope.mci <- 0.0002 # from Binding et al. 2013 - ontario # 5000
intercept.mci <- 0.0012 # from Binding et al. 2013 - ontario # 6
mu_mci$s2_chl_ontario_DCI <- (mu_mci$dci/10000 + intercept.mci) / slope.mci
mu_mci$s2_chl_ontario_DCI_BRR <- (mu_mci$dci_brr + intercept.mci) / slope.mci
mu_mci$s2_chl_ontario_mci_calc <- (mu_mci$mci_calc/10000 + intercept.mci) / slope.mci
mu_mci$s2_chl_ontario_mci_brr_calc <- (mu_mci$mci_brr_calc + intercept.mci) / slope.mci
plot(mu_mci$s2_chl_ontario_DCI, mu_mci$s2_chl_ontario_DCI_BRR)

plot_error_metrics(x = mu_mci$chla_corr, y = mu_mci$s2_chl_ontario_mci_brr_calc)

# try averaging MCI and DCI
mu_mci$dcimci <- apply(data.frame(mu_mci$s2_chl_ontario_DCI, mu_mci$s2_chl_ontario_mci_calc), 1, mean)


# compare L1C and BRR
plot(mu_mci$s2_chl_ontario_mci_calc, mu_mci$s2_chl_ontario_mci_brr_calc)
abline(0,1)
mu_mci$s2_chl_ontario_mci_brr_calc - mu_mci$s2_chl_ontario_mci_calc
summary(mu_mci$s2_chl_ontario_mci_brr_calc - mu_mci$s2_chl_ontario_mci_calc)
plot(mu_mci$s2_chl_ontario_mci_calc, mu_mci$s2_chl_ontario_mci_brr_calc - mu_mci$s2_chl_ontario_mci_calc)

# checking original slopes
mu_mci$m45 <- (mu_mci$b5_1 - mu_mci$b4_1) / (705 - 665)
mu_mci$m56 <- (mu_mci$b6_1 - mu_mci$b5_1) / (740 - 705)

plot(abs(mu_mci$m45 - mu_mci$m56), abs(mu_mci$error_chla))

# slopes that are more different appear to have more error, 
# confirming that DCI may be of use


# plotting


satvar <- "s2_chl_ontario_mci_calc" # chla_s2, s2_chl_ontario_DCI, dcimci
val_metrics <- plot_error_metrics(x = mu_mci$chla_corr, 
                                  y = mu_mci[, which(colnames(mu_mci) == satvar)],
                                  xname = expression(italic("in situ") * " chl " * italic(a) * " (" * mu * "g " * L^-1 * ")"), 
                                  yname = satvar,
                                  #yname = expression("S2-derived chl " * italic(a) * " (" * mu * "g " * L^-1 * ")"), 
                                  #yname = "S2-derived chlorophyll a (ug/L)", 
                                  #yname = "S2-derived chlorophyll a (ug/L, from MCI using L1C reflectance)", 
                                  #title = plot_title, 
                                  equal_axes = TRUE, 
                                  log_axes = "", # xy, x, y, ""
                                  log_space = F, # T, F
                                  plot_abline = FALSE,
                                  #text_x = min(mu_mci$chla_corr, mu_mci$chla_s2),
                                  #text_y = ,
                                  mape = FALSE,
                                  rand_error = FALSE,
                                  regr_stats = FALSE,
                                  #states = mu_mci$state,
                                  #lakes = mu_mci$comid,
                                  #xlim = c(min(mu_mci$chla_corr, mu_mci$chla_s2, na.rm = T), max(mu_mci$chla_corr, mu_mci$chla_s2, na.rm = T)),
                                  #ylim = c(min(mu_mci$chla_corr, mu_mci$chla_s2, na.rm = T), max(mu_mci$chla_corr, mu_mci$chla_s2, na.rm = T)),
                                  xlim = c(0.01706488, 175.78009722), 
                                  ylim = c(0.01706488, 175.78009722), 
                                  show_metrics = TRUE, 
                                  #xaxt="n",
                                  #yaxt="n",
                                  col = alpha("black", 0.4), 
                                  #col = mu_mci$sedimentf,
                                  #col = mu_mci$state_col,
                                  pch = 20)
text(0.02, 
     175.78009722,
     adj = c(0, 1),
     bquote(MAE[mult] * " = " * .(signif(val_metrics$MAE[2], digits = 3))))
text(0.02, 
     105,
     adj = c(0, 1),
     bquote(bias[mult] * " = " * .(signif(val_metrics$bias[2], digits = 3))))
text(0.02, 
     60,
     adj = c(0, 1),
     paste0("n = ", val_metrics$n[2]))



# scraps
multall <- function(a, b, c) {
  return(a*b*c)
}

mapply(multall, a = dfx[, 1], b = dfx[, 2], c = dfx[, 3])

