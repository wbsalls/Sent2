

library(boot)

source("C:/Users/WSALLS/Git/Sent2/algorithms.R")
source("C:/Users/WSALLS/Git/Sent2/error_metrics_220120.R")


cal_val <- function(data, obs_name, p1_name,
                    portion_cal = 0.8, 
                    set_seed = TRUE, 
                    main = NULL, 
                    alg_name = names(p1_dat),
                    switch_y_cal = TRUE,
                    nboots = 0, 
                    regr_model_cal = 4, # corresponding to lmodel2() regression type
                    #xlim_cal = range(obs_dat, na.rm = TRUE), 
                    #ylim_cal = range(p1_dat, na.rm = TRUE), 
                    neg.rm = TRUE, 
                    negs2zero = FALSE,
                    conf_ints = TRUE,
                    log_axes_val = "xy",
                    text_cal = TRUE,
                    xylim_val = range(data[, obs_name], na.rm = TRUE),
                    returnObjects = FALSE,
                    ...) {
  
  # set seed to retain same "randomness"
  if (set_seed) {
    set.seed(1)
  }
  
  # select whether bootstrap based on nboots
  if (nboots == 0) {
    bstrap <- FALSE
  } else if (nboots > 0) {
    bstrap <- TRUE
  }
  
  # assign index for subsequent data re-joining
  data$row_inputdata <- 1:nrow(data)
  
  # restrict to complete cases
  full_set <- data[, colnames(data) %in% c(obs_name, p1_name, "row_inputdata")]
  full_set <- full_set[complete.cases(full_set), ]
  
  # rename observed and predictor columns
  colnames(full_set)[which(colnames(full_set) == obs_name)] <- "obs"
  colnames(full_set)[which(colnames(full_set) == p1_name)] <- "p1"
  
  # select cal and val sets
  cal_index <- sample(1:nrow(full_set), size = floor(nrow(full_set) * portion_cal), replace = FALSE)
  cal_set <- full_set[cal_index, ] # select 80% for cal
  val_set <- full_set[-cal_index, ] # retain remaining 20% for val
  
  
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
      
      mboot <- boot(data = cal_set, statistic = bsm2,
                    R = nboots, formula = obs ~ p1)
      
      mcal_b1 <- mean(mboot$t[, 2])
      mcal_b0 <- mean(mboot$t[, 1])
      mcal_Rsq <- mean(mboot$t[, 3])
      
    } else if (isFALSE(bstrap)) {
      
      # fit linear model; set coefficients
      mcal <- lmodel2(obs ~ p1, data = cal_set,
                      range.y = "relative", range.x = "interval")
      
      mcal_b1 <- mcal$regression.results$Slope[regr_model_cal] # slope
      mcal_b0 <- mcal$regression.results$Intercept[regr_model_cal] # intercept
      mcal_Rsq <- mcal$rsquare # r-squared
      
      # set confidence intervals as NA since not bootstrapping
      b0_lci <- NA
      b0_uci <- NA
      b1_lci <- NA
      b1_uci <- NA
    }
    
    # plot
    plot(cal_set$p1, cal_set$obs, 
         #xlim = xlim_cal,
         #ylim = ylim_cal,
         col = alpha("black", 0.4), 
         pch = "", # suppressed so transparency isn't affected with below points()
         main = paste0(main, " fit"),
         ylab = expression(italic("in situ") * " chl " * italic(a) * " (" * mu * "g " * L^-1 * ")"),
         xlab = alg_name)
    abline(mcal_b0, mcal_b1)
    
    # confidence interval on regression line
    if (isTRUE(bstrap & conf_ints)) {
      # create vector of x values from which to calculate confidence interval values
      xincrements <- seq(min(cal_set$p1), 
                         max(cal_set$p1) + 1.2 * max(cal_set$p1), 
                         length.out=120)
      
      b0_lci <- boot.ci(mboot, type="bca", index=1)$bca[4]
      b0_uci <- boot.ci(mboot, type="bca", index=1)$bca[5]
      b1_lci <- boot.ci(mboot, type="bca", index=2)$bca[4]
      b1_uci <- boot.ci(mboot, type="bca", index=2)$bca[5]
      
      # establish upper and lower confidence interval values for each value of x
      y_lci <- c()
      for (x in seq_along(xincrements)) {
        y_lci_l <- b1_lci * xincrements[x] + b0_lci
        y_lci_u <- b1_uci * xincrements[x] + b0_lci
        y_lcix <- min(y_lci_l, y_lci_u)
        y_lci <- c(y_lci, y_lcix)
      }
      
      y_uci <- c()
      for (x in seq_along(xincrements)) {
        y_uci_l <- b1_lci * xincrements[x] + b0_uci
        y_uci_u <- b1_uci * xincrements[x] + b0_uci
        y_ucix <- max(y_uci_l, y_uci_u)
        y_uci <- c(y_uci, y_ucix)
      }
      
      # plot confidence range; plot confidence interval space & lines and regression line
      polygon(c(xincrements, rev(xincrements)), c(y_lci, rev(y_uci)), col = "grey90", border = FALSE)
      lines(xincrements, y_lci, lty = 'dashed', col = 'red')
      lines(xincrements, y_uci, lty = 'dashed', col = 'red')
      abline(mcal_b0, mcal_b1)
      
      # cover lines for negative y-values; redraw covered axis; redraw points
      polygon(c(par('usr')[1], par('usr')[2], par('usr')[2], par('usr')[1]), 
              c(par('usr')[3], par('usr')[3], 0, 0), 
              border = NA, col = "white")
      box(col = "black")
    }
    
    points(cal_set$p1, cal_set$obs, col = alpha("black", 0.4), pch = 20)
    
    # text
    if (isTRUE(text_cal)) {
      calplot_range_x <- par('usr')[2] - par('usr')[1]
      calpos_text_x <- par('usr')[1] + calplot_range_x * 0.05
      
      calplot_range_y <- par('usr')[4] - par('usr')[3]
      calpos_text_y <- par('usr')[3] + calplot_range_y * 0.95
      
      text(x = calpos_text_x, y = calpos_text_y, 
           bquote(atop(atop(y * " = " * .(round(mcal_b1, 0)) * x * " + " * .(round(mcal_b0, 2)),
                            r^2 * " = " * .(round(mcal_Rsq, 3))),
                       atop(n * " = " * .(length(cal_set$obs))))),
           adj = c(0, 1), cex = 1.7)
    }
    
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
      
      mboot <- boot(data = cal_set, statistic = bsm2,
                    R = nboots, formula = obs ~ p1)
      
      mcal_b1 <- mean(mboot$t[, 2])
      mcal_b0 <- mean(mboot$t[, 1])
      mcal_Rsq <- mean(mboot$t[, 3])
      
    } else if (isFALSE(bstrap)) {
      
      # fit linear model; set coefficients
      mcal <- lmodel2(p1 ~ obs, data = cal_set,
                      range.y = "interval", range.x = "relative")
      
      mcal_b1 <- mcal$regression.results$Slope[regr_model_cal] # slope
      mcal_b0 <- mcal$regression.results$Intercept[regr_model_cal] # intercept
      mcal_Rsq <- mcal$rsquare # r-squared
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
         bquote(atop(atop(r^2 * " = " * .(round(mcal_Rsq, 3)),
                          slope * " = " * .(round(mcal_b1, 5))),
                     atop(intercept * " = " * .(round(mcal_b0, 5)),
                          n * " = " * .(length(cal_set$obs))))),
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
                                    pch = "", # suppressed so transparency isn't affected with below points()
                                    main = paste0(main, " validation"),
                                    ...)
  
  ## plot error bars on predictions, if bootstrapped
  if(isTRUE(bstrap) & log_axes_val == "" & isTRUE(conf_ints)) {
    
    # assign predicted chl based on bootstrapped coeffs, and 
    val_set$pred_lci <- b1_lci * val_set$p1 + b0_lci
    val_set$pred_uci <- b1_uci * val_set$p1 + b0_uci
    
    # plot each error bar
    for (r in 1:nrow(val_set)) {
      xc <- c(val_set$obs[r], val_set$obs[r])
      yc <- c(val_set$pred_lci[r], val_set$pred_uci[r])
      xwidth <- (par('usr')[2] - par('usr')[1]) * 0.01
      lines(x = xc, y = yc, col = "gray")
      lines(x = xc + c(-xwidth, xwidth), y = rep(yc[1], 2), col = "gray")
      lines(x = xc + c(-xwidth, xwidth), y = rep(yc[2], 2), col = "gray")
    }
  }
  
  points(x = val_set$obs, y = val_set$pred, col = alpha("black", 0.4), pch = 20)
  
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
                                                   b1_cal = mcal_b1,
                                                   b1_lci = b1_lci,
                                                   b1_uci = b1_uci,
                                                   b0_cal = mcal_b0,
                                                   b0_lci = b0_lci,
                                                   b0_uci = b0_uci,
                                                   cal_minx = min(cal_set$obs),
                                                   cal_maxx = max(cal_set$obs),
                                                   cal_meanx = mean(cal_set$obs),
                                                   cal_medianx = median(cal_set$obs),
                                                   cal_miny = min(cal_set$p1),
                                                   cal_maxy = max(cal_set$p1),
                                                   cal_meany = mean(cal_set$p1),
                                                   cal_mediany = median(cal_set$p1),
                                                   cal_n = nrow(cal_set))) #!!!!!
  if (isFALSE(returnObjects)) {
    return(cal_val_metrics)
  } else {
    if (isTRUE(bstrap)) {
      return(list(metrics = cal_val_metrics, 
                  cal_data = cal_set, 
                  val_data = val_set,
                  cal_boot = mboot))
    } else {
      return(list(metrics = cal_val_metrics, 
                  cal_data = cal_set, 
                  val_data = val_set))
    }
  }
  
}
