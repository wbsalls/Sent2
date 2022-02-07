
source("C:/Users/WSALLS/Git/Sent2/algorithms.R")
source("C:/Users/WSALLS/Git/Sent2/error_metrics_220120.R")


cal_val <- function(obs, p1, 
                    portion_cal = 0.8, 
                    set_seed = TRUE, 
                    neg.rm = TRUE, 
                    neg_0 = FALSE,
                    main = NULL, 
                    log_axes_val = "xy",
                    xlim_cal = range(obs, na.rm = TRUE), 
                    ylim_cal = range(p1, na.rm = TRUE), 
                    xylim_val = range(obs, na.rm = TRUE), 
                    pos_text_x_val = min(obs, p1, na.rm = TRUE),
                    ...) {
  
  # set seed to retain same "randomness"
  if(set_seed){
    set.seed(1)
  }
  
  # restrict to complete cases
  full_set <- data.frame(obs, p1)
  full_set <- full_set[complete.cases(full_set), ]
  
  # select cal and val sets
  cal_index <- sample(1:nrow(full_set), size = floor(nrow(full_set) * portion_cal), replace = FALSE)
  cal_set <- full_set[cal_index, ] # select 80% for cal
  val_set <- full_set[-cal_index, ] # retain remaining 20% for val
  
  # fit linear model; set coefficients; plot
  mcal <- lm(cal_set$p1 ~ cal_set$obs)
  
  mcal_b1 <- mcal$coefficients[2] # slope
  mcal_b0 <- mcal$coefficients[1] # intercept
  mcal_Rsq <- summary(mcal)$r.squared # R-squared
  
  # calibration plot
  plot(cal_set$obs, cal_set$p1, 
       xlim = xlim_cal,
       ylim = ylim_cal,
       col = alpha("black", 0.4), 
       pch = 20,
       main = main,
       xlab = expression(italic("in situ") * " chl " * italic(a) * " (" * mu * "g " * L^-1 * ")"),
       ylab = "MCI")
  abline(mcal_b0, mcal_b1)
  text(80, 0, paste0("R-sq = ", round(mcal_Rsq, 3),
                      "\nslope = ", round(mcal_b1, 5), 
                      "\nintercept = ", round(mcal_b0, 5), 
                      "\nn = ", length(cal_set$obs)),
       adj = c(0, 1))
  
  
  
  # predict on val set
  val_set$pred <- (val_set$p1 - mcal_b0) / mcal_b1
  
  n_neg <- sum(val_set$pred < 0)
  
  if (!neg_0) {
    print(sprintf("Removing %s negative S2 chl a values", n_neg))
    val_set <- val_set[val_set$pred >= 0, ]
  } else {
    print(sprintf("Retaining %s negative S2 chl a values as 0", n_neg))
    val_set$pred[val_set$pred < 0] <- 0.00001
  }
  
  # validation metrics & plot
  val_metrics <- plot_error_metrics(x = val_set$obs, y = val_set$pred,
                                    xname = expression(italic("in situ") * " chl " * italic(a) * " (" * mu * "g " * L^-1 * ")"), 
                                    yname = expression("S2-derived chl " * italic(a) * " (" * mu * "g " * L^-1 * ")"), 
                                    log_axes = log_axes_val,
                                    equal_axes = TRUE, 
                                    xlim = xylim_val, 
                                    ylim = xylim_val, 
                                    stack_lows = c(0, 0),
                                    plot_regrline = FALSE,
                                    show_metrics = TRUE,
                                    metrics_mult_space = TRUE,
                                    show_mape = FALSE, 
                                    show_regr_stats = FALSE,
                                    pos_text_x = pos_text_x_val,
                                    #pos_text_y = NULL,
                                    print_metrics = TRUE,
                                    col = alpha("black", 0.4), 
                                    pch = 20,
                                    main = main)
  #abline(v = 10)
  #abline(h = 10)
  
  cal_val_metrics <- cbind(val_metrics, data.frame(val_minx = min(val_set$obs),
                                                   val_maxx = max(val_set$obs),
                                                   val_meanx = mean(val_set$obs),
                                                   val_medianx = median(val_set$obs),
                                                   val_miny = min(val_set$p1),
                                                   val_maxy = max(val_set$p1),
                                                   val_meany = mean(val_set$p1),
                                                   val_mediany = median(val_set$p1),
                                                   n_neg_S2chla = n_neg,
                                                   Rsq_cal = mcal_Rsq,
                                                   slope_cal = mcal_b1,
                                                   intercept_cal = mcal_b0,
                                                   cal_minx = min(cal_set$obs),
                                                   cal_maxx = max(cal_set$obs),
                                                   cal_meanx = mean(cal_set$obs),
                                                   cal_medianx = median(cal_set$obs),
                                                   cal_miny = min(cal_set$p1),
                                                   cal_maxy = max(cal_set$p1),
                                                   cal_meany = mean(cal_set$p1),
                                                   cal_mediany = median(cal_set$p1),
                                                   cal_n = nrow(cal_set)))
  
  return(cal_val_metrics)
}
