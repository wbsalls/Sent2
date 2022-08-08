setwd("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/S2/out")
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
mu_conus_addAcolite$set[caldat$row_inputdata] <- "cal"
mu_conus_addAcolite$set[valdat$row_inputdata] <- "val"
table(mu_conus_addAcolite$set)

# merge validation predictions to full table, creating index based on row numbers to match index in output
mu_conus_addAcolite$row_inputdata <- as.numeric(rownames(mu_conus_addAcolite))
mu_conus_addAcolite_pred <- merge(mu_conus_addAcolite, valdat,
                                  by = "row_inputdata",
                                  all.x = TRUE)

valdat_all <- mu_conus_addAcolite_pred[which(mu_conus_addAcolite_pred$set == "val"), ]


# calculate pred chl for all (including calibration; consider use for analysis of factors)
mu_conus_addAcolite_pred$chla_rhos <- cvm2$metrics$b1_cal * mu_conus_addAcolite_pred$MCI_rhos + cvm2$metrics$b0_cal

# investigate and remove negative pred chl values
plot(sort(mu_conus_addAcolite_pred$In.Situ.chl[(mu_conus_addAcolite_pred$chla_rhos < 0)]))
plot(mu_conus_addAcolite_pred$In.Situ.chl[(mu_conus_addAcolite_pred$chla_rhos < 0)], 
     mu_conus_addAcolite_pred$chla_rhos[(mu_conus_addAcolite_pred$chla_rhos < 0)],
     xlab = "in situ chl", ylab = "pred chl")
mu_conus_addAcolite_pred$chla_rhos[mu_conus_addAcolite_pred$chla_rhos < 0] <- NA



# specify which dataframe to use for analysis
#andata <- mu_conus_addAcolite_pred[valdat$row_inputdata, ]
andata <- mu_conus_addAcolite_pred

# calc error
andata$error_chla <- (andata$chla_rhos - andata$In.Situ.chl) # error
andata$error_chla_abs <- abs(andata$error_chla) # abs error
andata$pct_error_chla <- ((andata$chla_rhos - andata$In.Situ.chl) / andata$In.Situ.chl) * 100 # % error
andata$pct_error_chla_abs <- abs(andata$pct_error_chla) # abs error



### analyze factors

# sediment
plot(andata$baseline_slope, andata$error_chla)

# shoredist
plot(andata$dist_shore_m, andata$error_chla)

# location
plot(mu_conus_addAcolite_pred[mu_conus_addAcolite_pred$set=="cal", ]$lon, 
     mu_conus_addAcolite_pred[mu_conus_addAcolite_pred$set=="cal", ]$lat)
points(mu_conus_addAcolite_pred[mu_conus_addAcolite_pred$set=="val", ]$lon, 
       mu_conus_addAcolite_pred[mu_conus_addAcolite_pred$set=="val", ]$lat,
       pch = 20, col = "red")

# lat/lon
data.frame(mu_conus$In.Situ.lat, mu_conus$lat)
sum(mu_conus$In.Situ.lat == mu_conus$lat)
summary(mu_conus$In.Situ.lat - mu_conus$lat)
plot(andata$lat, andata$error_chla)
plot(andata$lon, andata$error_chla)

# time diff
plot(abs(andata$Overpass.time.difference..minutes.), andata$error_chla)


#
plot(as.numeric(andata$In.Situ.tss), andata$error_chla)
plot(andata$In.Situ.cdom, andata$error_chla)


# satellite comparison
andata$satellite <- substr(andata$Scene.ID, 1, 3)
table(andata$satellite)
boxplot(error_chla ~ satellite, data = andata)


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
         xlab = colnames(andata)[i], ylab = err_type,
         pch = 20, main = i)
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

library(rgdal)
library(sp)
library(raster)
library(GISTools)
library(plotrix)


# read us shp for state names
us_raw <- readOGR("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/geospatial_general/conus_shp", "cb_2015_us_state_20m")
conus_raw <- us_raw[!(us_raw$NAME %in% c("Alaska", "Hawaii", "Puerto Rico")), ]
conus <- spTransform(conus_raw, CRS("+init=epsg:5070")) # to AEA

#us <- spTransform(us_raw, CRS("+init=epsg:5070")) # to AEA
#conus <- us[!(us$NAME %in% c("Alaska", "Hawaii", "Puerto Rico")), ]

pts_andata <- SpatialPointsDataFrame(coords = andata[, c("lon", "lat")], data = andata,
                                     proj4string = CRS("+init=epsg:4326"))
pts_andata_proj <- spTransform(pts_andata, crs(conus))

par(mar = c(2, 2, 2, 2))

jpeg("3_map.jpg", width = 900*6, height = 625*6, res = 600)
plot(conus, col = "grey94", border = "white") # 900 x 625
plot(pts_andata_proj[which(pts_andata_proj$set == "cal"), ], add = TRUE, pch = 19, col = alpha("black", 0.3))
plot(pts_andata_proj[which(pts_andata_proj$set == "val"), ], add = TRUE, pch = 18, col = alpha("orange", 0.5), bg = NULL)
legend(-2.2e+06, 0.6e+06, legend=c("Calibration points", "Validation points"),
       col=c(alpha("black", 0.3), alpha("orange", 0.5)), pch = c(19, 18))
#north.arrow(-2.1e+06, 0.7e+06, len = 100000, lab = "N")
dev.off()


### geographic analysis
library(ggplot2)
library(ggspatial)
library(sf)

# error distribution
plot(sort((pts_andata_proj$In.Situ.chl)))
plot(sort((pts_andata_proj$chla_rhos)))
abline(h = 0)
plot(sort((pts_andata_proj$error_chla)))
abline(h = 0)
abline(v = 114)

# base plot
plot(conus, col = "grey94", border = "white") # 900 x 625
plot(pts_andata_proj, col = color.scale(pts_andata_proj$error_chla_abs, c(0, 1, 1), c(1, 1, 0), 0), pch = 20, add = TRUE)
plot(pts_andata_proj, col = color.scale(log10(pts_andata_proj$pct_error_chla_abs), c(0, 1, 1), c(1, 1, 0), 0), pch = 20, add = TRUE)

# ggplot 
# testing
ggplot(andata[!is.na(andata$error_chla), ], aes(lon, lat, error_chla)) +
  geom_point(aes(color = error_chla), size = 2) +
  scale_color_gradient2(low = "yellow", mid = "white", high = "blue", midpoint = 0)

ggplot() + geom_polygon(data = conus, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(data = andata, aes(x = lon, y = lat)) +
  theme_void()

# works
sites <- st_as_sf(andata[!is.na(andata$error_chla), ], coords = c("lon", "lat"), 
                   crs = 4326, agr = "constant")

ggplot() +
  geom_polygon(data = conus_raw, aes(x = long, y = lat, group = group), 
               colour = "black", fill = "gray90") +
  geom_sf(data = sites, aes(color = error_chla), size = 2, alpha = 3) +
  geom_sf(data = sites, size = 2, shape = 1, alpha = 0.3) +
  scale_color_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) + 
  theme_void()





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

valdat_all$troph_is <- sapply(valdat_all$In.Situ.chl, chl_trophicState)
valdat_all$troph_s2 <- sapply(valdat_all$chla_rhos, chl_trophicState)
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
  mae_l <- calc_mae(valdat_l$In.Situ.chl, valdat_l$chla_rhos)
  bias_l <- calc_bias(valdat_l$In.Situ.chl, valdat_l$chla_rhos)
  
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



