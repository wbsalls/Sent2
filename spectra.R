### make spectra lines

# S2 Level 1C raw values are TOA reflectance * 10,000; need to divide by 10,000 first

# make df
#spectra <- mu_mci[which(mu_mci$mci_baseline_slope < sed_cutoff), c(1, 213, 222, 231, 240)] # sed w/info

spectra_all <- mu_mci[, which(colnames(mu_mci) %in% c("b4_1", "b5_1", "b6_1", "mci_baseline_slope"))] # all
spectra_clear <- spectra_all[which(spectra_all$mci_baseline_slope > sed_cutoff), 1:3] # non sed
spectra_sed <- spectra_all[which(spectra_all$mci_baseline_slope < sed_cutoff), 1:3] # sed

# plot all
plot(4:6, 4:6, type = "n", ylim = c(0, max(spectra_all)), 
     xlab = "S2 band #", ylab = "reflectance", xaxt="n")
axis(1, xaxp=c(4, 6, 2), las=2)
for (r in 1:nrow(spectra_clear)) {
  lines(c(4, 5, 6), spectra_clear[r, ], col = "blue")
}

for (r in 1:nrow(spectra_sed)) {
  lines(c(4, 5, 6), spectra_sed[r, ], col = "red")
}

# plot examples
jpeg("5_spectra.jpg", width = 600*6, height = 450*6, res = 600)
plot(4:6, 4:6, type = "n", ylim = c(0, max(spectra_all) / 10000), 
     xlab = "S2 band #", ylab = "reflectance", xaxt="n")
axis(1, xaxp=c(4, 6, 2))
lines(c(4, 5, 6), spectra_all[which(mu_mci$MCI_BRR_1 == min(mu_mci$MCI_BRR_1)), 1:3] / 10000, col = "black")
#lines(c(4, 5, 6), spectra_all[which(mu_mci$MCI_BRR_1 == max(mu_mci$MCI_BRR_1)), 1:3] / 10000, col = "black", lty = 2)
lines(c(4, 5, 6), spectra_all[which(mu_mci$MCI_BRR_1 == sort(mu_mci$MCI_BRR_1, decreasing = TRUE)[3]), 1:3] / 10000, col = "black", lty = 2)
#lines(c(4, 5, 6), spectra_all[which(mu_mci$mci_baseline_slope == min(mu_mci$mci_baseline_slope)), 1:3] / 10000, col = "black", lty = 3)
lines(c(4, 5, 6), spectra_all[which(mu_mci$mci_baseline_slope == sort(mu_mci$mci_baseline_slope, decreasing = FALSE)[2]), 1:3] / 10000, col = "black", lty = 3)
legend(5.24, 1080 / 10000, legend=c("Clear", "High-chlorophyll", "Sediment-influenced"),
       col=c("black", "black", "black"), lty=c(1, 2, 3), cex=0.8)
dev.off()

# baselines & MCI lines
lines(c(4, 6), spectra_all[which(mu_mci$MCI_BRR_1 == max(mu_mci$MCI_BRR_1)), c(1, 3)] / 10000, col = "green", lty = 3)
lines(c(4, 6), spectra_all[which(mu_mci$mci_baseline_slope == min(mu_mci$mci_baseline_slope)), c(1, 3)] / 10000, 
      col = "red", lty = 3)
lines(c(5, 5), c((sum(spectra_all[which(mu_mci$MCI_BRR_1 == max(mu_mci$MCI_BRR_1)), c(1, 3)]) / 2), 
                 spectra_all[which(mu_mci$MCI_BRR_1 == max(mu_mci$MCI_BRR_1)), 2]) / 10000, col = "green", lty = 3)
lines(c(5, 5), c((sum(spectra_all[which(mu_mci$mci_baseline_slope == min(mu_mci$mci_baseline_slope)), c(1, 3)]) / 2), 
                 spectra_all[which(mu_mci$mci_baseline_slope == min(mu_mci$mci_baseline_slope)), 2]) / 10000, col = "red", lty = 3)

# Fig 2 ---------------------------------------------------------

jpeg("2_MCI_bands.jpg", width = 650*6, height = 600*6, res = 600)

par(mfrow = c(2,1))
par(mar = c(4, 4, 2, 2))

## MCI calculation

mci_examp <- c(700, 1100, 800) / 10000
plot(c(4, 5, 6), mci_examp, 
     ylim = c(0, max(mci_examp) * 1.2), pch = 20, 
     xlab = "band", ylab = "reflectance", xaxt = "n", yaxt = "n")
#axis(1, xaxp=c(4, 6, 2))
axis(1, at = c(4, 5, 6), labels = c("a", "b", "c"))
lines(c(4, 5, 6), mci_examp, col = "black", lty = 1)
lines(c(4, 6), mci_examp[c(1, 3)], col = "gray", lty = 2)
#lines(c(5, 5), c((sum(spectra_all[which(mu_mci$MCI_BRR_1 == max(mu_mci$MCI_BRR_1)), c(1, 3)]) / 2), 
                 #spectra_all[which(mu_mci$MCI_BRR_1 == max(mu_mci$MCI_BRR_1)), 2]) / 10000, col = "red", lty = 3, lwd = 3)
arrows(x0 = 5, y0 = (sum(mci_examp[c(1, 3)]) / 2),
       x1 = 5, y1 = mci_examp[2], 
       code = 2, col = "black", lty = 1, lwd = 2, length = 0.2)
text(4.5, 0.065, "MCI baseline", col = "black")
text(5.17, 0.085, "MCI peak", col = "black", font = 2)

text(4, 0.12, expression(bold("a.")))
#

### band ranges ----------------------------------------
options(stringsAsFactors = FALSE)
bands <- read.csv("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Images/bands.csv")
bands <- read.csv("/Users/wilsonsalls/Desktop/EPA/Sentinel2/Images/bands.csv")

plot(c(650, 755), c(0, 3), type = "n", xlab = "wavelength (nm)", ylab = "", yaxt = "n")
# MERIS
for (r in 1:nrow(bands)) {
  lines(c(as.numeric(bands[r, 2]) - as.numeric(bands[r, 3]) / 2, 
          as.numeric(bands[r, 2]) + as.numeric(bands[r, 3]) / 2), c(1, 1),
        lwd = 4, col = "gray55")
}
# S2A
for (r in 1:nrow(bands)) {
  lines(c(as.numeric(bands[r, 5]) - as.numeric(bands[r, 6]) / 2, 
          as.numeric(bands[r, 5]) + as.numeric(bands[r, 6]) / 2), c(2.1, 2.1),
        lwd = 4, col = "black")
}
# S2B
for (r in 1:nrow(bands)) {
  lines(c(as.numeric(bands[r, 7]) - as.numeric(bands[r, 8]) / 2, 
          as.numeric(bands[r, 7]) + as.numeric(bands[r, 8]) / 2), c(1.9, 1.9),
        lwd = 4, col = "black")
}
text(665, 1, "MERIS", col = "gray55")
text(755, 2.15, "S-2A")
text(755, 1.85, "S-2B")

text(650, 2.85, expression(bold("b.")))

dev.off()

par(opar)
