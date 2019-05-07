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
lines(c(4, 5, 6), spectra_all[which(mu_mci$MCI_L1C == min(mu_mci$MCI_L1C)), ] / 10000, col = "blue")
lines(c(4, 5, 6), spectra_all[which(mu_mci$MCI_L1C == max(mu_mci$MCI_L1C)), ] / 10000, col = "green3", lty = 2)
lines(c(4, 5, 6), spectra_all[which(mu_mci$mci_baseline_slope == max(mu_mci$mci_baseline_slope)), ] / 10000, 
      col = "red", lty = 4)

legend(5.3, 1375 / 10000, legend=c("Clear", "High-chlorophyll", "Sediment-influenced"),
       col=c("blue", "green3", "red"), lty=c(1, 2, 4), cex=0.8)

# baselines & MCI lines
lines(c(4, 6), spectra_all[which(mu_mci$MCI_L1C == max(mu_mci$MCI_L1C)), c(1, 3)] / 10000, col = "green", lty = 3)
lines(c(4, 6), spectra_all[which(mu_mci$mci_baseline_slope == max(mu_mci$mci_baseline_slope)), c(1, 3)] / 10000, 
      col = "red", lty = 3)
lines(c(5, 5), c((sum(spectra_all[which(mu_mci$MCI_L1C == max(mu_mci$MCI_L1C)), c(1, 3)]) / 2), 
                 spectra_all[which(mu_mci$MCI_L1C == max(mu_mci$MCI_L1C)), 2]) / 10000, col = "green", lty = 3)
lines(c(5, 5), c((sum(spectra_all[which(mu_mci$mci_baseline_slope == max(mu_mci$mci_baseline_slope)), c(1, 3)]) / 2), 
                 spectra_all[which(mu_mci$mci_baseline_slope == max(mu_mci$mci_baseline_slope)), 2]) / 10000, col = "red", lty = 3)


