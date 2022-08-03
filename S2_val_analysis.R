source("C:/Users/WSALLS/Git/Sent2/cal_val.R")

par(mfrow = c(1, 1))

cvm2 <- cal_val(data = mu_conus_addAcolite,
                obs_name = "In.Situ.chl.x", 
                p1_name = "MCI_rhot", 
                portion_cal = 0.8, 
                set_seed = TRUE, 
                neg.rm = TRUE, 
                negs2zero = FALSE, 
                nboots = 300,
                switch_y_cal = TRUE, 
                regr_model_cal = 4, 
                main = paste0("rhot, l2gen - "),
                alg_name = "MCI",
                log_axes_val = "", # xy
                returnObjects = TRUE
                #, pos_text_x = 100,
                #pos_text_y = 0.07
)

str(cvm2)

cvm2$metrics
caldat <- cvm2$cal_data
valdat <- cvm2$val_data
cal_boot <- cvm2$cal_boot

mu_conus_addAcolite$index_assigned <- as.numeric(rownames(mu_conus_addAcolite))
mu_conus_addAcolite_pred <- merge(mu_conus_addAcolite, valdat,
                                  by = "index_assigned",
                                  all.x = TRUE)
nrow(caldat)
nrow(valdat)

mu_conus_addAcolite$set <- NA
mu_conus_addAcolite$set[caldat$index_assigned] <- "cal"
mu_conus_addAcolite$set[valdat$index_assigned] <- "val"

plot(mu_conus_addAcolite_pred[valdat$index_assigned, c("In.Situ.chl.x", "pred")])
abline(0, 1)

# location
plot(mu_conus_addAcolite_pred[mu_conus_addAcolite_pred$set=="cal", ]$lon.x, 
     mu_conus_addAcolite_pred[mu_conus_addAcolite_pred$set=="cal", ]$lat.x)
points(mu_conus_addAcolite_pred[mu_conus_addAcolite_pred$set=="val", ]$lon.x, 
       mu_conus_addAcolite_pred[mu_conus_addAcolite_pred$set=="val", ]$lat.x,
       pch = 20, col = "red")


# set data with which to perform further analysis
andata <- mu_conus_addAcolite_pred[valdat$index_assigned, ]

# calc error
andata$error_chla <- (andata$pred - andata$In.Situ.chl.x) # error
andata$error_chla_abs <- abs(andata$error_chla) # abs error
andata$pct_error_chla <- ((andata$pred - andata$In.Situ.chl.x) / andata$In.Situ.chl.x) * 100 # % error
andata$pct_error_chla_abs <- abs(andata$pct_error_chla) # abs error



# sediment
plot(andata$baseline_slope, andata$error_chla)

# shoredist
plot(andata$dist_shore_m, andata$error_chla)

# lat/lon
data.frame(mu_conus$In.Situ.lat, mu_conus$lat)
sum(mu_conus$In.Situ.lat == mu_conus$lat)
summary(mu_conus$In.Situ.lat - mu_conus$lat)
plot(andata$lat.x, andata$error_chla)
plot(andata$lon.x, andata$error_chla)

# time diff
plot(abs(andata$Overpass.time.difference..minutes..x), andata$error_chla)


#
plot(andata$In.Situ.tss.x, andata$error_chla)
plot(andata$In.Situ.cdom.x, andata$error_chla)



## plot all factors

colnames(andata)[1:140]
factor_list <- c(9:13, 74:124, 137, 139, 140)
factor_list <- c(9:13, 14:73, 74:124, 128:133, 137, 139, 140)
length(factor_list)

err_type <- "error_chla_abs" # error_chla error_chla_abspct_error_chla pct_error_chla_abs

ploti <- TRUE
par(mfrow = c(3, 4))

factor_df <- data.frame()

for (i in factor_list) {
  err <- andata[, err_type]
  xi <- as.numeric(andata[, colnames(andata)[i]])
  
  if (length(unique(xi[!is.na(xi)])) < 2) {
    next
  }
  
  mi <- lm(err ~ xi)
  slope_std <- round(coef(mi)[2] * (sd(xi) / sd(err)), 2)
  
  factor_df <- rbind(factor_df, data.frame(factor = colnames(andata)[i],
                                           rsq = round(summary(mi)$r.squared, 3),
                                           slope_std = slope_std,
                                           n = sum(!is.na(xi))))
  
  if (ploti) {
  plot(xi, err,
       xlab = colnames(andata)[i],
       ylab = err_type,
       pch = 20)
  abline(h = 0, lty = 2)
  abline(coef(mi)[1], coef(mi)[2])
  text(par('usr')[1], par('usr')[4], adj = c(0, 1),
       paste0("R-sq = ", round(summary(mi)$r.squared, 3),
              "\nn = ", sum(!is.na(xi))))
  }
}

factor_df[order(factor_df$rsq, decreasing = TRUE), ]
