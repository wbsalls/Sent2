####!!!! switch boot R back to 1000


source("C:/Users/WSALLS/Git/Sent2/algorithms.R")
source("C:/Users/WSALLS/Git/Sent2/error_metrics_220120.R")


cal_val <- function(obs_dat, p1_dat, 
                    portion_cal = 0.8, 
                    set_seed = TRUE, 
                    main = NULL, 
                    alg_name = names(p1_dat),
                    switch_y_cal = TRUE,
                    regr_model_cal = 4, # corresponding to lmodel2() regression type
                    #xlim_cal = range(obs_dat, na.rm = TRUE), 
                    #ylim_cal = range(p1_dat, na.rm = TRUE), 
                    neg.rm = TRUE, 
                    negs2zero = FALSE,
                    log_axes_val = "xy",
                    xylim_val = range(obs_dat, na.rm = TRUE),
                    ...) {
  
  # set seed to retain same "randomness"
  if(set_seed){
    set.seed(1)
  }
  
  # restrict to complete cases
  full_set <- data.frame(obs_dat, p1_dat)
  colnames(full_set) <- c("obs", "p1") # to ensure no processes are using full data
  full_set <- full_set[complete.cases(full_set), ]
  
  # select cal and val sets
  cal_index <- sample(1:nrow(full_set), size = floor(nrow(full_set) * portion_cal), replace = FALSE)
  cal_set <- full_set[cal_index, ] # select 80% for cal
  val_set <- full_set[-cal_index, ] # retain remaining 20% for val
  
  'if (isTRUE(switch_y_cal)) {
    ycal <- obs
    xcal <- p1
  } else if (isFALSE(switch_y_cal)) {
    ycal <- p1
    xcal <- obs
  }'
  
  ### calibrate
  
  if (isTRUE(switch_y_cal)) {
    
    if (isTRUE(bstrap)) {
      bsm2 <- function(data, indices, formula) {
        d <- data[indices, ] # allows boot to select sample
        fit <- lmodel2(formula, data = d,
                       range.y = "relative", range.x = "interval")
        return(c(fit$regression.results$Intercept[regr_model_cal],
                 fit$regression.results$Slope[regr_model_cal],
                 fit$rsquare))
      }
      
      boot_m2 <- boot(data = cal_set, statistic = bsm2,
                      R = 10, formula = obs ~ p1)
      
      mcal_b1 <- mean(boot_m2$t[, 2])
      mcal_b0 <- mean(boot_m2$t[, 1])
      mcal_Rsq <- mean(boot_m2$t[, 3])
      
    } else if (isFALSE(bstrap)) {
      
      # fit linear model; set coefficients
      mcal <- lmodel2(obs ~ p1, data = cal_set,
                      range.y = "relative", range.x = "interval")
      
      mcal_b1 <- mcal$regression.results$Slope[regr_model_cal] # slope
      mcal_b0 <- mcal$regression.results$Intercept[regr_model_cal] # intercept
      mcal_Rsq <- mcal$rsquare # R-squared
    }
    
    # plot
    plot(cal_set$p1, cal_set$obs, 
         #xlim = xlim_cal,
         #ylim = ylim_cal,
         col = alpha("black", 0.4), 
         pch = 20,
         main = paste0(main, " (fit)"),
         ylab = expression(italic("in situ") * " chl " * italic(a) * " (" * mu * "g " * L^-1 * ")"),
         xlab = alg_name)
    abline(mcal_b0, mcal_b1)
    
    calplot_range_x <- par('usr')[2] - par('usr')[1]
    calpos_text_x <- par('usr')[1] + calplot_range_x * 0.05
    
    calplot_range_y <- par('usr')[4] - par('usr')[3]
    calpos_text_y <- par('usr')[3] + calplot_range_y * 0.95
    
    text(x = calpos_text_x, y = calpos_text_y, 
         paste0("R-sq = ", round(mcal_Rsq, 3),
                "\nslope = ", round(mcal_b1, 0), 
                "\nintercept = ", round(mcal_b0, 2), 
                "\nn = ", length(cal_set$obs)),
         adj = c(0, 1), cex = 1.2)
    
  } else if (isFALSE(switch_y_cal)) {
    if (isTRUE(bstrap)) {
      bsm2 <- function(data, indices, formula) {
        d <- data[indices, ] # allows boot to select sample
        fit <- lmodel2(formula, data = d,
                       range.y = "interval", range.x = "relative")
        return(c(fit$regression.results$Intercept[regr_model_cal],
                 fit$regression.results$Slope[regr_model_cal],
                 fit$rsquare))
      }
      
      boot_m2 <- boot(data = cal_set, statistic = bsm2,
                      R = 10, formula = obs ~ p1)
      
      mcal_b1 <- mean(boot_m2$t[, 2])
      mcal_b0 <- mean(boot_m2$t[, 1])
      mcal_Rsq <- mean(boot_m2$t[, 3])
      
    } else if (isFALSE(bstrap)) {
    
    # fit linear model; set coefficients
    mcal <- lmodel2(p1 ~ obs, data = cal_set,
                    range.y = "interval", range.x = "relative")
    
    mcal_b1 <- mcal$regression.results$Slope[regr_model_cal] # slope
    mcal_b0 <- mcal$regression.results$Intercept[regr_model_cal] # intercept
    mcal_Rsq <- mcal$rsquare # R-squared
    }
    
    # plot
    plot(cal_set$obs, cal_set$p1, 
         #xlim = xlim_cal,
         #ylim = ylim_cal,
         col = alpha("black", 0.4), 
         pch = 20,
         main = main,
         xlab = expression(italic("in situ") * " chl " * italic(a) * " (" * mu * "g " * L^-1 * ")"),
         ylab = alg_name)
    abline(mcal_b0, mcal_b1)
    
    calplot_range_x <- par('usr')[2] - par('usr')[1]
    calpos_text_x <- par('usr')[1] + calplot_range_x * 0.05
    
    calplot_range_y <- par('usr')[4] - par('usr')[3]
    calpos_text_y <- par('usr')[3] + calplot_range_y * 0.95
    
    text(x = calpos_text_x, y = calpos_text_y, 
         paste0("R-sq = ", round(mcal_Rsq, 3),
                "\nslope = ", round(mcal_b1, 5), 
                "\nintercept = ", round(mcal_b0, 5), 
                "\nn = ", length(cal_set$obs)),
         adj = c(0, 1), cex = 1.2)
  }
  
  
  ### predict and assess with val set
  if (isTRUE(switch_y_cal)) {
    val_set$pred <- (mcal_b1 * val_set$p1) + mcal_b0
  } else {
    val_set$pred <- (val_set$p1 - mcal_b0) / mcal_b1
  }
  
  n_neg <- sum(val_set$pred < 0)
  
  if (!negs2zero) {
    print(sprintf("Removing %s negative S2 chl a values", n_neg))
    val_set <- val_set[val_set$pred >= 0, ]
  } else {
    print(sprintf("Retaining %s negative S2 chl a values as 0", n_neg))
    val_set$pred[val_set$pred < 0] <- 0.00001
  }
  
  if (is.null(xylim_val)) {
    xylim_val <- c(0.01, max(c(val_set$obs, val_set$pred), na.rm = TRUE))
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
                                    print_metrics = TRUE,
                                    col = alpha("black", 0.4), 
                                    pch = 20,
                                    main = paste0(main, " (validation)"))
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
