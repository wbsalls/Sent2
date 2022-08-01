
setwd("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/S2")

l2gen <- read.csv("Nima/Pahlevan_l2gen.csv", stringsAsFactors = FALSE)
acolite <- read.csv("Nima/Pahlevan_acolite.csv", stringsAsFactors = FALSE)

l2gen$uniqueIDer <- paste(l2gen$Scene.ID, l2gen$In.Situ.datetime, l2gen$In.Situ.chl, sep = "; ")
acolite$uniqueIDer <- paste(acolite$Scene.ID, acolite$In.Situ.datetime, acolite$In.Situ.chl, sep = "; ")

merged_mus <- merge(l2gen, acolite, by = "uniqueIDer")

colnames(merged_mus)

plot(merged_mus$rhot.705..x, merged_mus$rhot.705..y)
points(merged_mus$rhos.705..x, merged_mus$rhos.705..y, pch = 20, col = "blue")


plot(merged_mus$rhos.705..x, merged_mus$rhos.705..y)


plot(merged_mus$Rrs.705..x, merged_mus$Rrs.705..y)
abline(0, 1, col = "red")


## compare ACs

#merging acolite posthoc to existing l2gen df
mu_conus$uniqueIDer <- paste(mu_conus$Scene.ID, mu_conus$In.Situ.datetime, mu_conus$In.Situ.chl, sep = "; ")

mu_conus_addAcolite <- merge(mu_conus, acolite, by = "uniqueIDer", all.x = TRUE, all.y = FALSE)

# calculate indices
mu_conus_addAcolite$MCI_rhot_aco <- calc_mci(R1 = mu_conus_addAcolite$rhot.665..y, R2 = mu_conus_addAcolite$rhot.705..y, R3 = mu_conus_addAcolite$rhot.740..y)
mu_conus_addAcolite$NDCI_rhot_aco <- calc_ndci(R1 = mu_conus_addAcolite$rhot.665..y, R2 = mu_conus_addAcolite$rhot.705..y)

mu_conus_addAcolite$MCI_rhos_aco <- calc_mci(R1 = mu_conus_addAcolite$rhos.665..y, R2 = mu_conus_addAcolite$rhos.705..y, R3 = mu_conus_addAcolite$rhos.740..y)
mu_conus_addAcolite$NDCI_rhos_aco <- calc_ndci(R1 = mu_conus_addAcolite$rhos.665..y, R2 = mu_conus_addAcolite$rhos.705..y)

mu_conus_addAcolite$MCI_Rrs_aco <- calc_mci(R1 = mu_conus_addAcolite$Rrs.665..y, R2 = mu_conus_addAcolite$Rrs.705..y, R3 = mu_conus_addAcolite$Rrs.740..y)
mu_conus_addAcolite$NDCI_Rrs_aco <- calc_ndci(R1 = mu_conus_addAcolite$Rrs.665..y, R2 = mu_conus_addAcolite$Rrs.705..y)

# compile all vars to iterate chl calibration
chl_algos_vars <- c("MCI_rhot", "MCI_rhos", "MCI_Rrs",
                    "MCI_rhot_aco", "MCI_rhos_aco", "MCI_Rrs_aco",
                    "NDCI_rhot", "NDCI_rhos", "NDCI_Rrs",
                    "NDCI_rhot_aco", "NDCI_rhos_aco", "NDCI_Rrs_aco")
chl_algos_names <- c("rhot, l2gen", "rhos, l2gen", "Rrs, l2gen",
                    "rhot, Acolite", "rhos, Acolite", "Rrs, Acolite",
                    "rhot, l2gen", "rhos, l2gen", "Rrs, l2gen",
                    "rhot, Acolite", "rhos, Acolite", "Rrs, Acolite")

chl_algos <- mu_conus_addAcolite[, chl_algos_vars]


# plot all

source("C:/Users/WSALLS/Git/Sent2/cal_val.R")

#par(mfrow = c(3, 4))

layout.matrix <- matrix(c(1, 3, 5, 
                          2, 4, 6,
                          7, 9, 11,
                          8, 10, 12), 
                        nrow = 3, ncol = 4)
layout(layout.matrix)
layout.show(12)

algo_df <- data.frame()

regr_model <- 4
if (regr_model == 1) {m12 <- "M1"} else {m12 <- "M2"}

switch_y <- TRUE

for (c in seq_along(colnames(chl_algos))) {
  
  algc <- strsplit(chl_algos_vars[c], split = "_")[[1]][1]
  
  cvm2 <- cal_val(obs_dat = mu_conus_addAcolite$In.Situ.chl.x, 
                  p1_dat = chl_algos[, c], 
                  portion_cal = 0.8, 
                  set_seed = TRUE, 
                  neg.rm = TRUE, 
                  negs2zero = FALSE, 
                  nboots = 10,
                  switch_y_cal = switch_y, 
                  regr_model_cal = regr_model, 
                  main = paste0(chl_algos_names[c], " - ", m12),
                  alg_name = algc,
                  log_axes_val = "")
  
  auxdf <- data.frame(algo = algc,
                proclevel = strsplit(chl_algos_vars[c], split = "_")[[1]][2],
                ACproc = strsplit(chl_algos_vars[c], split = "_")[[1]][3],
                regr_model = regr_model, 
                switch_y = switch_y)
  
  rowc <- cbind(auxdf, cvm2)
  
  algo_df <- rbind(algo_df, rowc)
} # export as 1200x950

algo_df

write.csv(algo_df, "C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/S2/out/algo_df.csv")


## comparing processing levels (l2gen)
plot(mu_conus$MCI_rhot, mu_conus$MCI_rhos)
abline(0, 1)

mu_conus$chl_m2bs_rhot <- (mean(boot_m2$t[, 2]) * mu_conus$MCI_rhot) + mean(boot_m2$t[, 1])
mu_conus$chl_m2bs_rhos <- (mean(boot_m2$t[, 2]) * mu_conus$MCI_rhos) + mean(boot_m2$t[, 1])
mu_conus$chl_m2bs_Rrs <- (mean(boot_m2$t[, 2]) * mu_conus$MCI_Rrs) + mean(boot_m2$t[, 1])

plot(mu_conus$chl_m2bs_rhot, mu_conus$chl_m2bs_rhos)

summary(abs(mu_conus$chl_m2bs_rhot - mu_conus$chl_m2bs_rhos))
sort(abs(mu_conus$chl_m2bs_rhot - mu_conus$chl_m2bs_rhos))

plot(mu_conus$chl_m2bs_rhos, mu_conus$chl_m2bs_Rrs * pi)
abline(0, 1)
