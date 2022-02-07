

opar <- par()

nima_path <- "C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/S2/Nima/"

l2gen <- read.csv(file.path(nima_path, "Pahlevan_l2gen.csv"), stringsAsFactors = FALSE)
acolite <- read.csv(file.path(nima_path, "Pahlevan_acolite.csv"), stringsAsFactors = FALSE)


## cross-check

colnames(acolite)
colnames(l2gen)

sort(colnames(acolite)[!(colnames(acolite) %in% colnames(l2gen))])
sort(colnames(l2gen)[!(colnames(l2gen) %in% colnames(acolite))])
sort(colnames(l2gen)[!(colnames(l2gen) %in% colnames(acolite))])[18:34]


## duplicates

l2gen$overpass.date <- (substr(l2gen$Overpass.datetime, 1, 10))
l2gen$insitu.date <- (substr(l2gen$In.Situ.datetime, 1, 10))

# with these four columns - none!
l2gen_dupfields <- l2gen[, which(colnames(l2gen) %in% c(
  "In.Situ.lat", "In.Situ.lon", "Scene.ID", "insitu.date"
))]
l2gen_duprm <- l2gen[!duplicated(l2gen_dupfields), ]

# no duplicates with "In.Situ.lat", "In.Situ.lon", "insitu.date"
l2gen_dupfields <- l2gen[, which(colnames(l2gen) %in% c(
  "In.Situ.lat", "In.Situ.lon", "insitu.date"
))]
l2gen_duprm <- l2gen[!duplicated(l2gen_dupfields), ]

# 3 duplicates with "In.Situ.lat", "In.Situ.lon", "Scene.ID"
l2gen_dupfields <- l2gen[, which(colnames(l2gen) %in% c(
  "In.Situ.lat", "In.Situ.lon", "Scene.ID"
))]
l2gen_duprm <- l2gen[!duplicated(l2gen_dupfields), ]
l2gen_dups <- l2gen[duplicated(l2gen_dupfields), ]

## **check what's up with these dups. probably being pulled from an adjacent day
#   is that ok? I'd say no... check entire dataset for those

for (r in 1:nrow(l2gen_dups)) {
  these_dups <- l2gen[l2gen[, which(colnames(l2gen) %in% c(
    "In.Situ.lat", "In.Situ.lon", "Scene.ID"
  ))], ]
  
  which()
  
}


## time density

summary(l2gen$Overpass.time.difference..minutes.)
hist(l2gen$Overpass.time.difference..minutes.)


# parse date/time
l2gen$sat_year <- as.numeric(substr(l2gen$Overpass.datetime, 1, 4))
l2gen$sat_month <- as.numeric(substr(l2gen$Overpass.datetime, 6, 7))
l2gen$sat_day <- as.numeric(substr(l2gen$Overpass.datetime, 9, 10))
l2gen$sat_hour <- as.numeric(substr(l2gen$Overpass.datetime, 12, 13)) +
  round((as.numeric(substr(l2gen$Overpass.datetime, 15, 16)) / 60), 2)
l2gen$ins_hour <- as.numeric(substr(l2gen$In.Situ.datetime, 12, 13)) +
  round((as.numeric(substr(l2gen$In.Situ.datetime, 15, 16)) / 60), 2)

# all
hist(l2gen$sat_hour)
plot(l2gen$In.Situ.lon, l2gen$sat_hour) # NOTE: Overpass.datetime is in UTC

hist(l2gen$ins_hour)
plot(l2gen$In.Situ.lon, l2gen$ins_hour) # NOTE: In.Situ.datetime appears to be in local time
# *** This may mean a discrepancy in time difference

hist(l2gen$Overpass.time.difference..minutes. / 60)


# US
l2gen_us <- l2gen[l2gen$In.Situ.lon < -50 & l2gen$In.Situ.lat > 25 & l2gen$In.Situ.lat < 49, ]

hist(l2gen_us$sat_hour)
plot(l2gen_us$In.Situ.lon, l2gen_us$sat_hour)

hist(l2gen_us$ins_hour)
plot(l2gen_us$In.Situ.lon, l2gen_us$ins_hour)

hist(l2gen_us$Overpass.time.difference..minutes. / 60)



mdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
l2gen$tseriesday <- (l2gen$year - 2015)*365.25 + (l2gen$month * mean(mdays)) + l2gen$day
hist(l2gen$tseriesday)
l2gen$tseriesmonth <- substr(l2gen$Overpass.datetime, 1, 7)

plot(table(l2gen$tseriesmonth))


## space

par(mfrow = c(1, 2))

plot(l2gen$In.Situ.lon, l2gen$In.Situ.lat)
abline(h = 25)
abline(h = 49)
abline(v = -50)

nrow(l2gen[l2gen$In.Situ.lon < -50 & l2gen$In.Situ.lat > 25, ]) # US/Canada
nrow(l2gen[l2gen$In.Situ.lon < -50 & l2gen$In.Situ.lat > 25 & l2gen$In.Situ.lat < 49, ]) # CONUS
# l2gen: 1721/2808 in US/Canada; 1636 in CONUS
# acolite: 2202/3053 in US/Canada; 2050 in CONUS

l2gen_conus <- l2gen[l2gen$In.Situ.lon < -50 & l2gen$In.Situ.lat > 25 & l2gen$In.Situ.lat < 49, ]
plot(l2gen_conus$In.Situ.lon, l2gen_conus$In.Situ.lat)
points(c(-92, -87.3, -86.9, -88.3, -122.3), c(46.8, 41.6, 46.2, 46, 38), col = "red")

par(opar)

## MCI validation

l2gen$MCI_rhot <- calc_mci(R1 = l2gen$rhot.665., R2 = l2gen$rhot.705., R3 = l2gen$rhot.740.)
l2gen$DCI_rhot <- calc_dci(R1 = l2gen$rhot.665., R2 = l2gen$rhot.705., R3 = l2gen$rhot.740.)

l2gen$MCI_rhos <- calc_mci(R1 = l2gen$rhos.665., R2 = l2gen$rhos.705., R3 = l2gen$rhos.740.)
l2gen$DCI_rhos <- calc_dci(R1 = l2gen$rhos.665., R2 = l2gen$rhos.705., R3 = l2gen$rhos.740.)

l2gen$MCI_Rrs <- calc_mci(R1 = l2gen$Rrs.665., R2 = l2gen$Rrs.705., R3 = l2gen$Rrs.740.)
l2gen$DCI_Rrs <- calc_dci(R1 = l2gen$Rrs.665., R2 = l2gen$Rrs.705., R3 = l2gen$Rrs.740.)


chl_inds <- colnames(l2gen)[which(grepl("MCI_", colnames(l2gen)))]


par(mfrow = c(3, 2))

pxlim <- c(0, 140)
pylim <- c(-0.03, 0.03)

error_df <- data.frame()

for (c in seq_along(chl_inds)) {
  print(sprintf("%s: %s", c, chl_inds[c]))
  proc_level <- substr(chl_inds[c], 5, 8)
  ra_name <- paste0(proc_level, ".665.")
  rc_name <- paste0(proc_level, ".740.")
  this_data <- l2gen
  
  # error metrics
  mc <- lm(this_data[[chl_inds[c]]] ~ this_data$In.Situ.chl)
  n_mc <- nrow(this_data[(!is.na(this_data$In.Situ.chl) & !is.na(this_data[[chl_inds[c]]])), ])
  rsq_mc <- round(summary(mc)$r.squared, 3)
  error_df <- rbind(error_df, cbind(run = paste0(chl_inds[c]),
                                    n = n_mc,
                                    minCI = min(this_data[[chl_inds[c]]], na.rm = TRUE),
                                    maxCI = max(this_data[[chl_inds[c]]], na.rm = TRUE),
                                    meanCI = min(this_data[[chl_inds[c]]], na.rm = TRUE),
                                    medianCI = min(this_data[[chl_inds[c]]], na.rm = TRUE),
                                    r_sq = rsq_mc))
  
  # plot
  plot(this_data$In.Situ.chl, this_data[[chl_inds[c]]], 
       main = paste0(chl_inds[c]), xlim = pxlim, ylim = pylim)
  abline(mc) # , untf=TRUE # if in log space
  abline(h=0)
  text(100, -0.02, sprintf("n = %s \nr-sq = %s", n_mc, rsq_mc), adj = c(0, 1))
  
  
  # filter sediment
  this_data$baseline_slope <- calc_baseline_slope(this_data[[ra_name]], this_data[[rc_name]])
  this_data <- this_data[this_data$baseline_slope > -0.00015, ]
  
  # error metrics
  mc <- lm(this_data[[chl_inds[c]]] ~ this_data$In.Situ.chl)
  n_mc <- nrow(this_data[(!is.na(this_data$In.Situ.chl) & !is.na(this_data[[chl_inds[c]]])), ])
  rsq_mc <- round(summary(mc)$r.squared, 3)
  error_df <- rbind(error_df, cbind(run = paste0(chl_inds[c], "_noSed"),
                                    n = n_mc,
                                    minCI = min(this_data[[chl_inds[c]]], na.rm = TRUE),
                                    maxCI = max(this_data[[chl_inds[c]]], na.rm = TRUE),
                                    meanCI = min(this_data[[chl_inds[c]]], na.rm = TRUE),
                                    medianCI = min(this_data[[chl_inds[c]]], na.rm = TRUE),
                                    r_sq = rsq_mc))
  
  # plot
  plot(this_data$In.Situ.chl, this_data[[chl_inds[c]]], 
       main = paste0(chl_inds[c], " no sed"), xlim = pxlim, ylim = pylim)
  abline(mc) # , untf=TRUE # if in log space
  abline(h=0)
  text(100, -0.02, sprintf("n = %s \nr-sq = %s", n_mc, rsq_mc), adj = c(0, 1))
}

par(opar)

plot(l2gen$In.Situ.chl, l2gen$mci_rrs, log = "", main = "MCI",
     ylim = c(-0.02, 0.02))
#ylim = c(min(l2gen$mci_rrs, l2gen$dci_rrs, na.rm = T), max(l2gen$mci_rrs, l2gen$dci_rrs, na.rm = T)))
plot(l2gen$In.Situ.chl, l2gen$dci_rrs, log = "", main = "DCI",
     ylim = c(-0.02, 0.02))
#ylim = c(min(l2gen$mci_rrs, l2gen$dci_rrs, na.rm = T), max(l2gen$mci_rrs, l2gen$dci_rrs, na.rm = T)))

abline(0.0012, 0.0002) #ontario
abline(0.0021, 0.0004) #erie


slope.mci <- 0.0004 # from Binding et al. 2013 - Erie # 2500
intercept.mci <- 0.0021 # from Binding et al. 2013 - Erie # 10.5


mlm <- lm(l2gen$mci_rrs ~ l2gen$In.Situ.chl)
summary(mlm)

dlm <- lm(l2gen$dci_rrs ~ l2gen$In.Situ.chl)
summary(dlm)


val_metrics <- plot_error_metrics(x = l2gen$In.Situ.chl, y = l2gen$dci_rrs, # export 800 x 860; 600 x 645 for paper
                                  #xname = expression(italic("in situ") * " chl " * italic(a) * " (" * mu * "g " * L^-1 * ")"), 
                                  #yname = expression("S2-derived chl " * italic(a) * " (" * mu * "g " * L^-1 * ")"), 
                                  #yname = "S2-derived chlorophyll a (ug/L)", 
                                  #yname = "S2-derived chlorophyll a (ug/L, from MCI using L1C reflectance)", 
                                  #title = plot_title, 
                                  equal_axes = F, 
                                  log_axes = "xy", # xy, x, y, ""
                                  log_space = TRUE, # T, F
                                  plot_abline = TRUE,
                                  #text_x = min(mu_mci$chla_corr, mu_mci$chla_s2),
                                  #text_y = ,
                                  mape = FALSE,
                                  rand_error = FALSE,
                                  regr_stats = FALSE,
                                  #states = mu_mci$state,
                                  #lakes = mu_mci$comid,
                                  #xlim = c(min(mu_mci$chla_corr, mu_mci$chla_s2, na.rm = T), max(mu_mci$chla_corr, mu_mci$chla_s2, na.rm = T)),
                                  #ylim = c(min(mu_mci$chla_corr, mu_mci$chla_s2, na.rm = T), max(mu_mci$chla_corr, mu_mci$chla_s2, na.rm = T)),
                                  
                                  show_metrics = TRUE, 
                                  #xaxt="n",
                                  #yaxt="n",
                                  col = alpha("black", 0.4), 
                                  #col = mu_mci$sedimentf,
                                  #col = mu_mci$state_col,
                                  pch = 20)
