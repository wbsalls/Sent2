### make spectra lines

# S2 Level 1C raw values are TOA reflectance * 10,000; need to divide by 10,000 first

# make df
spectra <- mu_mci[which(mu_mci$mci_baseline_slope < sed_cutoff), c(1, 213, 222, 231, 240)] # sed w/info

spectra_all <- mu_mci[, c(213, 222, 231)] # all
spectra_clear <- mu_mci[which(mu_mci$mci_baseline_slope > sed_cutoff), c(213, 222, 231)] # non sed
spectra_sed <- mu_mci[which(mu_mci$mci_baseline_slope < sed_cutoff), c(213, 222, 231)] # sed

# plot all
plot(4:6, 4:6, type = "n", ylim = c(0, max(spectra_all)), 
     xlab = "band", ylab = "reflectance", xaxt="n")
axis(1, xaxp=c(4, 6, 2), las=2)
for (r in 1:nrow(spectra_clear)) {
  lines(c(4, 5, 6), spectra_clear[r, ], col = "blue")
}

for (r in 1:nrow(spectra_sed)) {
  lines(c(4, 5, 6), spectra_sed[r, ], col = "red")
}

# plot examples
plot(4:6, 4:6, type = "n", ylim = c(0, max(spectra_all) / 10000), 
     xlab = "band", ylab = "reflectance", xaxt="n")
axis(1, xaxp=c(4, 6, 2), las=2)
lines(c(4, 5, 6), spectra_all[which(mu_mci$MCI_L1C == min(mu_mci$MCI_L1C)), ] / 10000, col = "black")
lines(c(4, 5, 6), spectra_all[which(mu_mci$MCI_L1C == max(mu_mci$MCI_L1C)), ] / 10000, col = "black", lty = 2)
lines(c(4, 5, 6), spectra_all[which(mu_mci$mci_baseline_slope == min(mu_mci$mci_baseline_slope)), ] / 10000, 
      col = "black", lty = 3)

legend(5.3, 1375 / 10000, legend=c("Clear", "High-chlorophyll", "Sediment-influenced"),
       col=c("black", "black", "black"), lty=c(1, 2, 3), cex=0.8)

# baselines & MCI lines
lines(c(4, 6), spectra_all[which(mu_mci$MCI_L1C == max(mu_mci$MCI_L1C)), c(1, 3)] / 10000, col = "green", lty = 3)
lines(c(4, 6), spectra_all[which(mu_mci$mci_baseline_slope == min(mu_mci$mci_baseline_slope)), c(1, 3)] / 10000, 
      col = "red", lty = 3)
lines(c(5, 5), c((sum(spectra_all[which(mu_mci$MCI_L1C == max(mu_mci$MCI_L1C)), c(1, 3)]) / 2), 
                 spectra_all[which(mu_mci$MCI_L1C == max(mu_mci$MCI_L1C)), 2]) / 10000, col = "green", lty = 3)
lines(c(5, 5), c((sum(spectra_all[which(mu_mci$mci_baseline_slope == min(mu_mci$mci_baseline_slope)), c(1, 3)]) / 2), 
                 spectra_all[which(mu_mci$mci_baseline_slope == min(mu_mci$mci_baseline_slope)), 2]) / 10000, col = "red", lty = 3)


## MCI calculation
plot(c(4, 5, 6), spectra_all[which(mu_mci$MCI_L1C == max(mu_mci$MCI_L1C)), ] / 10000, 
     ylim = c(0, max(spectra_all) / 10000), pch = 20, 
     xlab = "band", ylab = "reflectance", xaxt="n", yaxt = "n")
axis(1, xaxp=c(4, 6, 2), las=2)
lines(c(4, 5, 6), spectra_all[which(mu_mci$MCI_L1C == max(mu_mci$MCI_L1C)), ] / 10000, col = "black", lty = 1)
lines(c(4, 6), spectra_all[which(mu_mci$MCI_L1C == max(mu_mci$MCI_L1C)), c(1, 3)] / 10000, col = "gray", lty = 2)
#lines(c(5, 5), c((sum(spectra_all[which(mu_mci$MCI_L1C == max(mu_mci$MCI_L1C)), c(1, 3)]) / 2), 
                 #spectra_all[which(mu_mci$MCI_L1C == max(mu_mci$MCI_L1C)), 2]) / 10000, col = "red", lty = 3, lwd = 3)
arrows(x0 = 5, y0 = (sum(spectra_all[which(mu_mci$MCI_L1C == max(mu_mci$MCI_L1C)), c(1, 3)]) / 2) / 10000, 
       x1 = 5, y1 = spectra_all[which(mu_mci$MCI_L1C == max(mu_mci$MCI_L1C)), 2] / 10000, 
       code = 2, col = "black", lty = 1, lwd = 2, length = 0.2)
text(4.5, 0.067, "MCI baseline", col = "black")
text(5.15, 0.085, "MCI peak", col = "black", font = 2)
#

### band ranges ----------------------------------------

bands <- read.csv("O:/PRIV/NERL_ORD_CYAN/Sentinel2/bands.csv")

plot(c(600, 800), c(0, 3), type = "n", xlab = "wavelength (nm)", ylab = "", yaxt = "n")
# MERIS
for (r in 1:nrow(bands)) {
  lines(c(bands[r, 2] - bands[r, 3] / 2, bands[r, 2] + bands[r, 3] / 2), c(1, 1),
        lwd = 4, col = "black")
}
# S2A
for (r in 1:nrow(bands)) {
  lines(c(bands[r, 5] - bands[r, 6] / 2, bands[r, 5] + bands[r, 6] / 2), c(2.1, 2.1),
        lwd = 4, col = "black")
}
# S2B
for (r in 1:nrow(bands)) {
  lines(c(bands[r, 7] - bands[r, 8] / 2, bands[r, 7] + bands[r, 8] / 2), c(1.9, 1.9),
        lwd = 4, col = "black")
}
text(610, 1, "MERIS")
text(610, 2.2, "S-2A")
text(610, 1.8, "S-2B")

