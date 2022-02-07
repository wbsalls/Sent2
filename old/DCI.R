setwd("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/DCI")

chl_file <- "ontario" # ontario, erie
mu_mci <- read.csv(sprintf("mu_mci_finalset_2019-11-27_s2_chl_%s.csv", chl_file), stringsAsFactors = FALSE)


# dynamic chlorophyll index

calc_mci <- function(ra, rb, rc, la = 665, lb = 705, lc = 740) {
  mci <- rb - ra - (((lb - la) / (lc - la)) * (rc - ra))
  return(mci)
}

calc_dci <- function(ra, rb, rc, la = 665, lb = 705, lc = 740) {
  if (NA %in% c(ra, rb, rc)) {
    return(NA)
  }
  
  mab <- (rb - ra) / (lb - la)
  mbc <- (rc - rb) / (lc - lb)
  
  if (abs(mbc) > mab) {
    # left peak
    mbc <- (rc - rb) / (lc - lb)
    rd <- ra
    ld <- ((rc - rd) / mbc) + lc
    le <- (la + ld) / 2
    re <- rc - mbc * (lc - le)
    dci <- calc_mci(ra = ra, rb = re, rc = rc, lb = le)
  } else {
    # right peak
    # calc here
    dci <- calc_mci(ra = ra, rb = rb, rc = rc)
  }
  return(dci)
}


# checking MCI calc
mu_mci$mci_calc <- calc_mci(ra = mu_mci$b4_1, rb = mu_mci$b5_1, rc = mu_mci$b6_1)
mu_mci$mci_brr_calc <- calc_mci(ra = mu_mci$b4_1_BRR, rb = mu_mci$b5_1_BRR, rc = mu_mci$b6_1_BRR)
plot(mu_mci$MCI_L1C_1, mu_mci$mci_calc/10000)
plot(mu_mci$MCI_BRR_1, mu_mci$mci_brr_calc)
abline(0,1)

# calc DCI
mu_mci$dci <- mapply(calc_dci, ra = mu_mci$b4_1, rb = mu_mci$b5_1, rc = mu_mci$b6_1)
mu_mci$dci_brr <- mapply(calc_dci, ra = mu_mci$b4_1_BRR, rb = mu_mci$b5_1_BRR, rc = mu_mci$b6_1_BRR)
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


# scraps
multall <- function(a, b, c) {
  return(a*b*c)
}

mapply(multall, a = dfx[, 1], b = dfx[, 2], c = dfx[, 3])

