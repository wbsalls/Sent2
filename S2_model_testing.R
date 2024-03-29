

### MCI calibration testing ---------------------------------------------------------------

library(lmodel2)
library(boot)
library(scales)
library(ggpubr)

mu_conus <- read.csv("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/S2/calvalready.csv")

# preliminary linear model fits; plotting

#
fitmci <- lm(formula = MCI_rhos ~ In.Situ.chl, data = mu_conus)

plot(mu_conus$In.Situ.chl, mu_conus$MCI_rhos, col = alpha("black", 0.3), pch = 20)
abline(fitmci$coefficients[1], fitmci$coefficients[2])

plot((mu_conus$In.Situ.chl), (mu_conus$MCI_rhos), log = "xy", col = alpha("black", 0.3), pch = 20)
abline((fitmci$coefficients[1]), (fitmci$coefficients[2]), untf = TRUE) #??

#
fitis <- lm(formula = In.Situ.chl ~ MCI_rhos, data = mu_conus)

plot(mu_conus$MCI_rhos, mu_conus$In.Situ.chl, col = alpha("black", 0.3), pch = 20)
abline(fitis$coefficients[1], fitis$coefficients[2])

plot(mu_conus$MCI_rhos, mu_conus$In.Situ.chl, log = "xy", col = alpha("black", 0.3), pch = 20)
abline(fitis$coefficients[1], fitis$coefficients[2], untf = TRUE)


### these regressions are not reversible with lm! type 2 are, though:

fitmci2 <- lmodel2((mu_conus$MCI_rhos) ~ (mu_conus$In.Situ.chl),
                   range.y = "interval", range.x = "relative")

# ** this is the one
fitis2 <- lmodel2((mu_conus$In.Situ.chl) ~ (mu_conus$MCI_rhos),
                  range.y = "relative", range.x = "interval")


## test symmetry of regressions

# model 1
fitmci$coefficients[2] #slope
1 / fitis$coefficients[2] #slope

fitmci$coefficients[1] #int
-(fitis$coefficients[1] / fitis$coefficients[2]) #int


fitis$coefficients[2] #slope
1 / fitmci$coefficients[2] #slope

fitis$coefficients[1] #int
-(fitmci$coefficients[1] / fitmci$coefficients[2]) #int

# model 2
fitmci2$regression.results$Slope #slope
1 / fitis2$regression.results$Slope #slope

fitmci2$regression.results$Intercept #int
-(fitis2$regression.results$Intercept / fitis2$regression.results$Slope) #int


fitis2$regression.results$Slope #slope
1 / fitmci2$regression.results$Slope #slope

fitis2$regression.results$Intercept #int
-(fitmci2$regression.results$Intercept / fitmci2$regression.results$Slope) #int


## test log transforming data

# problem: MCI has negatives, problematic for log transformation.
# one solution: add a constant and then transform
# transform MCI(linearly) so no negatives
min(mu_conus$MCI_rhos)
#mci_trans <- -1 * (floor(min(mu_conus$MCI_rhos) * 10000) / 10000)
mci_trans <- 0.006
mu_conus$MCI_rhos_trans <- mu_conus$MCI_rhos + mci_trans


## test normality - not normal (nor with log transform)
# had tried roots (e.g. cubic) but doesn't work with negatives either

hist(mu_conus$In.Situ.chl)
hist(log(mu_conus$In.Situ.chl))
hist(mu_conus$In.Situ.chl[mu_conus$In.Situ.chl < 20])
hist((mu_conus$In.Situ.chl) ^ (1/2))
hist((mu_conus$In.Situ.chl) ^ (1/3))
hist((mu_conus$In.Situ.chl) ^ (1/4))

hist(mu_conus$MCI_rhos)
hist(log(mu_conus$MCI_rhos_trans))
hist((mu_conus$MCI_rhos_trans) ^ (1/3))

shapiro.test((mu_conus$In.Situ.chl))
shapiro.test(log(mu_conus$In.Situ.chl))
shapiro.test((mu_conus$In.Situ.chl) ^ (1/3))
shapiro.test((mu_conus$In.Situ.chl) ^ (1/7))

shapiro.test(mu_conus$MCI_rhos)
shapiro.test(log(mu_conus$MCI_rhos_trans))
shapiro.test((mu_conus$MCI_rhos_trans) ^ (1/3))


for (n in 1:20) {
  print(paste0(n, ": ", shapiro.test((mu_conus$In.Situ.chl) ^ (1/n))))
}

ggqqplot((mu_conus$In.Situ.chl))
ggqqplot((mu_conus$MCI_rhos))
ggqqplot(log(mu_conus$In.Situ.chl))
ggqqplot((mu_conus$mci_logt))
ggqqplot((mu_conus$MCI_rhos) ^ (1/3))
ggqqplot((mu_conus$In.Situ.chl) ^ (1/10))


## ** check bivariate normal test

plot((mu_conus$In.Situ.chl), (mu_conus$MCI_rhos), pch = "'")
plot(log(mu_conus$In.Situ.chl), log(mu_conus$MCI_rhos_trans), pch = "'")
plot(log(mu_conus$In.Situ.chl), log(mu_conus$MCI_rhos), pch = "'")
plot(log(mu_conus$MCI_rhos), log(mu_conus$In.Situ.chl), pch = "'")
plot(log(mu_conus$MCI_rhos), (mu_conus$In.Situ.chl), pch = "'")
plot((mu_conus$MCI_rhos), log(mu_conus$In.Situ.chl), pch = "'")

plot(log(mu_conus$In.Situ.chl), mu_conus$MCI_rhos, pch = "'")

plot(log(mu_conus$MCI_rhos), log(mu_conus$In.Situ.chl), pch = "'")

plot((mu_conus$MCI_rhos)^(1/3), (mu_conus$In.Situ.chl)^(1/3), pch = "'")


#
fitis_logxy <- lm(formula = log(In.Situ.chl) ~ log(mu_conus$MCI_rhos_trans), data = mu_conus)
plot(log(mu_conus$MCI_rhos_trans), log(mu_conus$In.Situ.chl), pch = "'")
abline(fitis_logxy$coefficients[1], fitis_logxy$coefficients[2])

fitis_logxy$coefficients[1]

fitmci_logxy <- lm(formula = log(MCI_rhos_trans) ~ log(mu_conus$In.Situ.chl), data = mu_conus)
plot(log(mu_conus$In.Situ.chl), log(mu_conus$MCI_rhos_trans), pch = "'")
abline(fitmci_logxy$coefficients[1], fitmci_logxy$coefficients[2])



### cal -------------------------------------
# split
set.seed(1)
cal_ind <- sample(1:nrow(mu_conus), ceiling(nrow(mu_conus) * 0.8), replace = FALSE)
mu_conus_cal <- mu_conus[cal_ind, ]
mu_conus_val <- mu_conus[-cal_ind, ]

# cal
cal_is <- lmodel2((mu_conus_cal$In.Situ.chl) ~ (mu_conus_cal$MCI_rhos),
                  range.y = "relative", range.x = "interval")

'cal_mci <- lmodel2((mu_conus_cal$MCI_rhos) ~ (mu_conus_cal$In.Situ.chl),
                  range.y = "interval", range.x = "relative")

# cal log
cal_islog <- lmodel2(log(mu_conus_cal$In.Situ.chl) ~ log(mu_conus_cal$MCI_rhos_trans),
                  range.y = "interval", range.x = "interval")
'

# set MCI chla
b0 <- cal_is$regression.results$Intercept[4]
b1 <- cal_is$regression.results$Slope[4]
mu_conus_val$chla_mci <- b1 * mu_conus_val$MCI_rhos + b0

b0rlog <- cal_islog$regression.results$Intercept[4]
b1rlog <- cal_islog$regression.results$Slope[4]
mu_conus_val$chla_mci_rlog <- exp(b1rlog * log(mu_conus_val$MCI_rhos_trans) + b0rlog)
mu_conus_val$chla_mci_rlog1 <- mu_conus_val$MCI_rhos_trans ^ b1rlog * exp(b0rlog)

plot(mu_conus_val$In.Situ.chl, mu_conus_val$chla_mci)
plot(mu_conus_val$In.Situ.chl, mu_conus_val$chla_mci, log = "xy")
plot(log(mu_conus_val$In.Situ.chl), log(mu_conus_val$chla_mci))
abline(0, 1)
#abline(-b0 / b1, 1 / b1)

plot(mu_conus_val$In.Situ.chl, mu_conus_val$chla_mci_rlog)
plot(mu_conus_val$In.Situ.chl, mu_conus_val$chla_mci_rlog, log = "xy")

calc_error_metrics(mu_conus_val$In.Situ.chl, mu_conus_val$chla_mci)
calc_error_metrics(mu_conus_val$In.Situ.chl, mu_conus_val$chla_mci_rlog)

plot(mu_conus_val$chla_mci, mu_conus_val$chla_mci_rlog)
abline(0, 1)

plot(mu_conus_val$MCI_rhos, mu_conus_val$chla_mci_rlog)

# compare chl preds for Model I in each direction
b0i <- cal_is$regression.results$Intercept[1]
b1i <- cal_is$regression.results$Slope[1]
mu_conus_val$chla_mci_olsi <- b1i * mu_conus_val$MCI_rhos + b0i

b0m <- cal_mci$regression.results$Intercept[1] / cal_mci$regression.results$Slope[1]
b1m <- 1 /cal_mci$regression.results$Slope[1]
mu_conus_val$chla_mci_olsm <- b1m * mu_conus_val$MCI_rhos + b0m

data.frame(mu_conus_val$chla_mci_olsi, mu_conus_val$chla_mci_olsm)
plot(mu_conus_val$chla_mci_olsi, mu_conus_val$chla_mci_olsm)
points(mu_conus_val$chla_mci_olsi, mu_conus_val$chla_mci, pch = "'")
df_ols <- data.frame(mu_conus_val$chla_mci_olsi, mu_conus_val$chla_mci_olsm, mu_conus_val$chla_mci)
colnames(df_ols) <- c("insitu", "mci", "RMA")

# test regression assumptions
calislm <- lm((mu_conus_cal$In.Situ.chl) ~ (mu_conus_cal$MCI_rhos_trans))
calisloglm <- lm(log(mu_conus_cal$In.Situ.chl) ~ log(mu_conus_cal$MCI_rhos_trans))

plot(calislm)
plot(calisloglm)


# Binding et al. 2013 coefficients
mu_conus_val$chla_ontario <- 
  (1 / 0.0002) * mu_conus_val$MCI_rhos - (-0.0012 / 0.0002)
plot(mu_conus_val$In.Situ.chl, mu_conus_val$chla_ontario, main = "ontario")
abline(0, 1)

mu_conus_val$chla_erie <- 
  (1 / 0.0004) * mu_conus_val$MCI_rhos - (-0.0021 / 0.0004)
plot(mu_conus_val$In.Situ.chl, mu_conus_val$chla_erie, main = "erie")
abline(0, 1)



## test sensitivity of random cal/val split ----------
set.seed(1)
coeff_jiggle <- data.frame()
for (i in 1:100) {
  cal_ind <- sample(1:nrow(mu_conus), ceiling(nrow(mu_conus) * 0.8), replace = FALSE)
  mu_conus_cal <- mu_conus[cal_ind, ]
  mu_conus_val <- mu_conus[-cal_ind, ]
  
  cal_is_i <- lmodel2((mu_conus_cal$In.Situ.chl) ~ (mu_conus_cal$MCI_rhos),
                      range.y = "relative", range.x = "interval")
  b0i <- cal_is_i$regression.results$Intercept[3]
  b1i <- cal_is_i$regression.results$Slope[3]
  
  mu_conus_val$chla_mci <- b1i * mu_conus_val$MCI_rhos + b0i
  mu_conus_val <- mu_conus_val[mu_conus_val$chla_mci >= 0, ]
  #mu_conus_val$chla_mci[mu_conus_val$chla_mci < 0] <- 0.0001
  
  metrics_i <- calc_error_metrics(mu_conus_val$In.Situ.chl, mu_conus_val$chla_mci)
  
  coeff_jiggle <- rbind(coeff_jiggle, cbind(data.frame(n = nrow(mu_conus_val),
                                                       b0 = b0i,
                                                       b1 = b1i),
                                            metrics_i[, 1:5]))
}
coeff_jiggle
coeff_jiggle_ols <- coeff_jiggle
coeff_jiggle_rma <- coeff_jiggle
coeff_jiggle_sma <- coeff_jiggle



## test bootstrapping *** switch to type 2 regression ----------
# function to obtain regression coefficients
bs <- function(data, indices, formula) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit))
}


# bootstrapping with 1000 replications
boots <- boot(data = mu_conus, statistic = bs,
              R = 228, formula = In.Situ.chl ~ MCI_rhos) # conf ints only work R >= n observations
boots
plot(boots, index=1)
plot(boots, index=2)
as.numeric(boots$t0[1])

# comparing confidence intervals... SE*1.96?
boot.ci(boots, type="bca", index=1) # intercept
boots$t0[1] - 0.5905116 * 1.96
boots$t0[1] + 0.5905116 * 1.96

boot.ci(boots, type="bca", index=2) # slope
boots$t0[2] - 290.9722251 * 1.96
boots$t0[2] + 290.9722251 * 1.96



## automated all -------

chl_inds <- colnames(chldata)[which(grepl("MCI_", colnames(chldata)))]


par(mfrow = c(3, 2))

pxlim <- c(0, 140)
pylim <- c(-0.03, 0.03)

error_df <- data.frame()

for (c in seq_along(chl_inds)) {
  print(sprintf("%s: %s", c, chl_inds[c]))
  proc_level <- substr(chl_inds[c], 5, 8)
  ra_name <- paste0(proc_level, ".665.")
  rc_name <- paste0(proc_level, ".740.")
  this_data <- chldata
  
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

plot(chldata$In.Situ.chl, chldata$mci_rrs, log = "", main = "MCI",
     ylim = c(-0.02, 0.02))
#ylim = c(min(chldata$mci_rrs, chldata$dci_rrs, na.rm = T), max(chldata$mci_rrs, chldata$dci_rrs, na.rm = T)))
plot(chldata$In.Situ.chl, chldata$dci_rrs, log = "", main = "DCI",
     ylim = c(-0.02, 0.02))
#ylim = c(min(chldata$mci_rrs, chldata$dci_rrs, na.rm = T), max(chldata$mci_rrs, chldata$dci_rrs, na.rm = T)))

abline(0.0012, 0.0002) #ontario
abline(0.0021, 0.0004) #erie


slope.mci <- 0.0004 # from Binding et al. 2013 - Erie # 2500
intercept.mci <- 0.0021 # from Binding et al. 2013 - Erie # 10.5


mlm <- lm(chldata$mci_rrs ~ chldata$In.Situ.chl)
summary(mlm)

dlm <- lm(chldata$dci_rrs ~ chldata$In.Situ.chl)
summary(dlm)


val_metrics <- plot_error_metrics(x = chldata$In.Situ.chl, y = chldata$chla_rhos, # export 800 x 860; 600 x 645 for paper
                                  #xname = expression(italic("in situ") * " chl " * italic(a) * " (" * mu * "g " * L^-1 * ")"), 
                                  #yname = expression("S2-derived chl " * italic(a) * " (" * mu * "g " * L^-1 * ")"), 
                                  #yname = "S2-derived chlorophyll a (ug/L)", 
                                  #yname = "S2-derived chlorophyll a (ug/L, from MCI using L1C reflectance)", 
                                  #title = plot_title, 
                                  equal_axes = TRUE, 
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

### plot Seegers data

seegers <- read.csv("S2/CyANChlBS_Matchups_2021June23.csv", stringsAsFactors = FALSE)

seegers$MERIS_chl <- 6620 * seegers$MERIS_ci_cyano - 3.1

# add to existing plot
plot(seegers$chl, seegers$MERIS_chl, add = TRUE, pch = 20, col = "red")
points(seegers$chl, seegers$MERIS_chl, add = TRUE, pch = 20, col = "red")

# separate plotting
plot(seegers$chl, seegers$MERIS_chl, 
     xlim = c(0, 150), ylim = c(0, 150))
plot(chldata$In.Situ.chl, chldata$chla_rhos, 
     xlim = c(0, 150), ylim = c(0, 150))



### ------------------------------------------------------------------------ ###



# investigate potential timezone issue ------------------------------------------------
par(mfrow = c(2, 1))

hist(chldata$sat_hour)
plot(chldata$In.Situ.lon, chldata$sat_hour,
     xlab = "longitude", ylab = "satellite hour") 
# NOTE: Overpass.datetime appears to be in UTC

hist(chldata$ins_hour)
plot(chldata$In.Situ.lon, chldata$ins_hour,
     xlab = "longitude", ylab = "in situ hour") 
# NOTE: In.Situ.datetime appears to be in local time
hist(chldata$ins_hour[chldata$In.Situ.lon < -50], xlab = "in situ hour", main = "lat < -50 (Western Hemi)")
hist(chldata$ins_hour[chldata$In.Situ.lon > -50], xlab = "in situ hour", main = "lat > -50 (Eastern Hemi)")
# *** This likely means Overpass.time.difference..minutes. is incorrect, and that matchups don't fully reflect same-day

plot(chldata$In.Situ.lon, chldata$In.Situ.lat)

hist(chldata$Overpass.time.difference..minutes. / 60)

summary(chldata$In.Situ.datetime - chldata$Overpass.datetime)

sum((chldata$In.Situ.datetime - chldata$Overpass.datetime) == chldata$Overpass.time.difference..minutes.)


chldata_us <- chldata[chldata$In.Situ.lon < -50 & chldata$In.Situ.lat > 25 & chldata$In.Situ.lat < 49, ]

hist(chldata_us$sat_hour)
plot(chldata_us$In.Situ.lon, chldata_us$sat_hour)

hist(chldata_us$ins_hour)
plot(chldata_us$In.Situ.lon, chldata_us$ins_hour)

hist(chldata_us$Overpass.time.difference..minutes. / 60)



mdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
chldata$tseriesday <- (chldata$year - 2015)*365.25 + (chldata$month * mean(mdays)) + chldata$day
hist(chldata$tseriesday)
chldata$tseriesmonth <- substr(chldata$Overpass.datetime, 1, 7)

plot(table(chldata$tseriesmonth))

# diff data sources
table(chldata$Database)
length(unique(chldata$Database))

table(chldata_us$Database)
length(unique(chldata_us$Database))

summary(chldata$In.Situ.lon)

par(mfrow = c(4, 7), mar = c(2, 2, 2, 1))
for (d in unique(chldata$Database)) {
  chldata_d <- chldata[chldata$Database == d, ]
  plot(chldata_d$In.Situ.lon, chldata_d$ins_hour,
       xlim = c(-180, 180),
       ylim = c(0, 24),
       xlab = "longitude", ylab = "in situ hour", main = d)
  #abline(h = c(7, 19)) # rough daylight hours in local time
  #abline(h = c(11, 3), lty = 3) # rough daylight hours in the US, UTC, most liberal window (EST to WDT)
  rect(xleft = -180, ybottom = 7, xright = 180, ytop = 19, 
       col = alpha("orange", 0.3))
  rect(xleft = -180, ybottom = 11, xright = -50, ytop = 24, 
       col = alpha("blue", 0.3))
  rect(xleft = -180, ybottom = 0, xright = -50, ytop = 3, 
       col = alpha("blue", 0.3))
}
#plot.new() # to fill empty plot lattice slots

par(opar)



### ----------------------------------------------------------------------


### scraps - time

write.csv(chldata, "chldata_posix.csv")


chldataET <- chldata[chldata$Database %in% ETsources, ]

chldataET$In.Situ.datetime_UTC[chldataET$Database %in% ETsources] <- 
  as.POSIXct(chldataET$In.Situ.datetime[chldataET$Database %in% ETsources], tz = "America/New_York")

chldataET$In.Situ.datetime_UTC[1:10]
as.POSIXct(chldataET$In.Situ.datetime[1:10], tz = "America/New_York")

as_datetime(1438692120)

head(chldata$In.Situ.datetime_UTC)
head(data.frame(
  chldata$In.Situ.datetime, 
  chldata$In.Situ.datetime_UTC,
  chldata$Database))

testtime <- as.POSIXct("2016-02-05 15:38:00", tz = "America/New_York")
as.POSIXct(format(testtime, tz="UTC", usetz=TRUE), tz = "UTC")


class(chldata$Overpass.datetime)
as.POSIXct("2015-07-26 16:02:34", tz = "GMT")

##
summary(chldata$Overpass.time.difference..minutes.)
hist(chldata$Overpass.time.difference..minutes. / 60)



# parse date/time
chldata$sat_year <- as.numeric(substr(chldata$Overpass.datetime, 1, 4))
chldata$sat_month <- as.numeric(substr(chldata$Overpass.datetime, 6, 7))
chldata$sat_day <- as.numeric(substr(chldata$Overpass.datetime, 9, 10))
chldata$sat_hour <- as.numeric(substr(chldata$Overpass.datetime, 12, 13)) +
  round((as.numeric(substr(chldata$Overpass.datetime, 15, 16)) / 60), 2)
chldata$ins_hour <- as.numeric(substr(chldata$In.Situ.datetime, 12, 13)) +
  round((as.numeric(substr(chldata$In.Situ.datetime, 15, 16)) / 60), 2)


# if performing offset time analysis, remove those with noon or midnight times
chldata$ins_time <- substr(chldata$In.Situ.datetime, 12, 19)
sum(chldata$ins_time == "00:00:00")
sum(chldata$ins_time == "12:00:00")
chldata <- chldata[!(chldata$Database %in% c("Mackenzie_River", "Uruguay")), ]

# regarding duplicates - solved after timezone correction:
## **being pulled from an adjacent day
#   is that ok? I'd say no... check entire dataset for those. need to get timezones down first.

###




### rough space

par(mfrow = c(1, 2))

plot(chldata$In.Situ.lon, chldata$In.Situ.lat)
abline(h = 25)
abline(h = 49)
abline(v = -50)

nrow(chldata[chldata$In.Situ.lon < -50 & chldata$In.Situ.lat > 25, ]) # US/Canada
nrow(chldata[chldata$In.Situ.lon < -50 & chldata$In.Situ.lat > 25 & chldata$In.Situ.lat < 49, ]) # CONUS
# chldata: 1721/2808 in US/Canada; 1636 in CONUS
# acolite: 2202/3053 in US/Canada; 2050 in CONUS

chldata_conus <- chldata[chldata$In.Situ.lon < -50 & chldata$In.Situ.lat > 25 & chldata$In.Situ.lat < 49, ]
plot(chldata_conus$In.Situ.lon, chldata_conus$In.Situ.lat)
points(c(-92, -87.3, -86.9, -88.3, -122.3), c(46.8, 41.6, 46.2, 46, 38), col = "red")

par(opar)

# US
#us_raw <- getData("GADM",country="USA",level=1)


###



# plotting all points by error (initial)
plot(chlpts, col = color.scale(chlpts$chla_err_add, c(0, 1, 1), c(1, 1, 0), 0), pch = 20, add = TRUE)
plot(chlpts, col = color.scale(log(abs(chlpts$chla_err_add)), c(0, 1, 1), c(1, 1, 0), 0), pch = 20, add = TRUE)
plot(chlpts, col = color.scale(chlpts$chla_err_mult, c(0, 1, 1), c(1, 1, 0), 0), pch = 20, add = TRUE)
plot(chlpts, col = color.scale(log(abs(chlpts$chla_err_mult)), c(0, 1, 1) ,c(1, 1, 0), 0), pch = 20, add = TRUE)


# plotting points in NHD lakes by error (initial)
plot(chlpts_nhd, col = color.scale(chlpts_nhd$chla_err_add, c(0, 1, 1), c(1, 1, 0), 0), pch = 20)
plot(chlpts_nhd, col = color.scale(log(abs(chlpts_nhd$chla_err_add)), c(0, 1, 1), c(1, 1, 0), 0), pch = 20, add = TRUE)
plot(chlpts_nhd, col = color.scale(chlpts_nhd$chla_err_mult, c(0, 1, 1), c(1, 1, 0), 0), pch = 20, add = TRUE)
plot(chlpts_nhd, col = color.scale(log(abs(chlpts_nhd$chla_err_mult)), c(0, 1, 1) ,c(1, 1, 0), 0), pch = 20, add = TRUE)

