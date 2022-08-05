source("C:/Users/WSALLS/Git/Sent2/cal_val.R")


### main validation on l2gen rhot

par(mfrow = c(1, 2))

cvm2 <- cal_val(data = mu_conus_addAcolite,
                obs_name = "In.Situ.chl", 
                p1_name = "MCI_rhos", 
                portion_cal = 0.8, 
                set_seed = TRUE, 
                neg.rm = TRUE, 
                negs2zero = FALSE, 
                nboots = 1000,
                switch_y_cal = TRUE, 
                regr_model_cal = 4, 
                main = paste0("rhos, l2gen - "),
                alg_name = "MCI",
                log_axes_val = "", # xy
                returnObjects = TRUE
                #, pos_text_x = 100,
                #pos_text_y = 0.07
)


### prepare data for factor analysis

# pull each object from output
cvm2$metrics
caldat <- cvm2$cal_data
valdat <- cvm2$val_data
cal_boot <- cvm2$cal_boot

# specify cal or val for each row
mu_conus_addAcolite$set <- NA
mu_conus_addAcolite$set[caldat$index_assigned] <- "cal"
mu_conus_addAcolite$set[valdat$index_assigned] <- "val"

# merge validation predictions to full table, creating index based on row numbers to match index in output
mu_conus_addAcolite$index_assigned <- as.numeric(rownames(mu_conus_addAcolite))
mu_conus_addAcolite_pred <- merge(mu_conus_addAcolite, valdat,
                                  by = "index_assigned",
                                  all.x = TRUE)

valdat_all <- mu_conus_addAcolite_pred[mu_conus_addAcolite_pred$set == "val", ]

plot(mu_conus_addAcolite_pred[valdat$index_assigned, c("In.Situ.chl.x", "pred")])
abline(0, 1)

# calculate pred chl for all (including calibration; consider use for analysis of factors)
mu_conus_addAcolite_pred$chla_rhos <- cvm2$metrics$b1_cal * mu_conus_addAcolite_pred$MCI_rhos + cvm2$metrics$b0_cal
mu_conus_addAcolite_pred$chla_rhos[mu_conus_addAcolite_pred$chla_rhos < 0] <- NA

# specify which dataframe to use for analysis
andata <- mu_conus_addAcolite_pred[valdat$index_assigned, ]
andata <-mu_conus_addAcolite_pred

# calc error
andata$error_chla <- (andata$pred - andata$In.Situ.chl.x) # error
andata$error_chla_abs <- abs(andata$error_chla) # abs error
andata$pct_error_chla <- ((andata$pred - andata$In.Situ.chl.x) / andata$In.Situ.chl.x) * 100 # % error
andata$pct_error_chla_abs <- abs(andata$pct_error_chla) # abs error



### analyze factors

# location
plot(mu_conus_addAcolite_pred[mu_conus_addAcolite_pred$set=="cal", ]$lon.x, 
     mu_conus_addAcolite_pred[mu_conus_addAcolite_pred$set=="cal", ]$lat.x)
points(mu_conus_addAcolite_pred[mu_conus_addAcolite_pred$set=="val", ]$lon.x, 
       mu_conus_addAcolite_pred[mu_conus_addAcolite_pred$set=="val", ]$lat.x,
       pch = 20, col = "red")

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


### satellite comparison
head(valdat_all$Scene.ID.x)


### analyze all factors

colnames(andata)[1:140]
factor_list <- c(9:13, 74:124, 137, 139, 140)
factor_list <- c(9:13, 14:73, 74:124, 128:133, 137, 139, 140)
length(factor_list)

err_type <- "error_chla_abs" # error_chla error_chla_abs pct_error_chla pct_error_chla_abs

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
  slope_std <- round(coef(mi)[2] * (sd(xi, na.rm = TRUE) / sd(err)), 2)
  
  factor_df <- rbind(factor_df, data.frame(factor = colnames(andata)[i],
                                           factor_index = i,
                                           rsq = round(summary(mi)$r.squared, 2),
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
         paste0("R-sq = ", round(summary(mi)$r.squared, 2),
                "\nstd. slope = ", slope_std,
                "\nn = ", sum(!is.na(xi))))
  }
}

factor_df[order(factor_df$rsq, decreasing = TRUE), ] # TSS, NO2


### map




### confusion matrix
# categorical by trophic state; confusion matrix
library(caret)

# all four classes
chl_trophicState <- function(x) {
  if (is.na(x)) {
    return(NA)
  } else if (x < 2) {
    return("oligotrophic")
  } else if (x <= 7) {
    return("mesotrophic")
  } else if (x <= 30) {
    return("eutrophic")
  } else {
    return("hypereutrophic")
  }
}

trophic_levels <- c("oligotrophic", "mesotrophic", "eutrophic", "hypereutrophic")

valdat_all$troph_is <- sapply(valdat_all$In.Situ.chl.x, chl_trophicState)
valdat_all$troph_s2 <- sapply(valdat_all$pred, chl_trophicState)
sum(valdat_all$troph_is == valdat_all$troph_s2, na.rm = TRUE)
sum(!is.na(valdat_all$troph_s2))
valdat_all$troph_is <- factor(valdat_all$troph_is, 
                              levels = trophic_levels)
valdat_all$troph_s2 <- factor(valdat_all$troph_s2, 
                              levels = trophic_levels)

# run matrix
confusionMatrix(data = valdat_all$troph_s2, reference = valdat_all$troph_is)


### error in each class

class_metrics <- data.frame()

for (l in trophic_levels) {
  valdat_l <- valdat_all[which(valdat_all$troph_is == l), ]
  mae_l <- calc_mae(valdat_l$In.Situ.chl.x, valdat_l$pred)
  bias_l <- calc_bias(valdat_l$In.Situ.chl.x, valdat_l$pred)
  
  class_metrics <- rbind(class_metrics, data.frame(trophic_class = l,
                                                   MAE_mult = round(mae_l, 2),
                                                   bias_mult = round(bias_l, 2)))
}
class_metrics


### iterative tests


iterboot_df <- data.frame()
set.seed(1)

for (b in 1:100) {
  
  print(sprintf("--------  %s    at    %s  --------", b, Sys.time()))
  
  cvm2_iter <- cal_val(data = mu_conus_addAcolite,
                       obs_name = "In.Situ.chl", 
                       p1_name = "MCI_rhos", 
                       portion_cal = 0.8, 
                       set_seed = FALSE, 
                       neg.rm = TRUE, 
                       negs2zero = FALSE, 
                       nboots = 1000,
                       switch_y_cal = TRUE, 
                       regr_model_cal = 4, 
                       main = paste0(b, " - rhot, l2gen - "),
                       alg_name = "MCI",
                       log_axes_val = "", # xy
                       returnObjects = TRUE
                       #, pos_text_x = 100,
                       #pos_text_y = 0.07
  )
  
  iterboot_df <- rbind(iterboot_df, cvm2_iter$metrics)
  
}

par(mfrow = c(2, 3))

hist(iterboot_df$b1_cal, main = "b1", xlab = "b1")
abline(v = cvm2_iter$metrics$b1_cal, lty = 2)
hist(iterboot_df$b0_cal, main = "b0", xlab = "b0")
abline(v = cvm2_iter$metrics$b0_cal, lty = 2)
hist(iterboot_df$Rsq_cal, main = "R-sq", xlab = "R-sq")
abline(v = cvm2_iter$metrics$Rsq_cal, lty = 2)
hist(iterboot_df$MAE_mult, main = "MAE_mult", xlab = "MAE_mult")
abline(v = cvm2_iter$metrics$MAE_mult, lty = 2)
hist(iterboot_df$bias_mult, main = "bias_mult", xlab = "bias_mult")
abline(v = cvm2_iter$metrics$bias_mult, lty = 2)

summary(iterboot_df$b1_cal)
summary(iterboot_df$b0_cal)
summary(iterboot_df$Rsq_cal)
summary(iterboot_df$MAE_mult)
summary(iterboot_df$bias_mult)

iterboot_vars <- c("b1_cal", "b0_cal", "Rsq_cal", "MAE_mult", "bias_mult")

isum_df <- data.frame()
for (i in iterboot_vars) {
  isummary <- signif(unclass(summary(iterboot_df[, i])), 4)
  isum_df <- rbind(isum_df, isummary)
  
}
rownames(isum_df) <- iterboot_vars
colnames(isum_df) <- names(summary(iterboot_df$b1_cal))
isum_df

write.csv(iterboot_df, "out/boot_iterations.csv")



