# using Bridget Seegers's metrics (not 10-based)
# this version uses:
## model-II regression
## log-log transformation **

library(lmodel2)
library(scales) # alpha

calc_mae <- function(observed, modeled, mult_space = TRUE) {
  if (mult_space == TRUE) {
    mmae <- 10 ^ (sum(
      abs(log10(modeled) - log10(observed))
    ) / length(observed))
  } else {
    mmae <- (sum(
      abs((modeled) - (observed))
    ) / length(observed))
  }
  return(mmae)
}

calc_bias <- function(observed, modeled, mult_space = TRUE) {
  if (mult_space == TRUE) {
    mbias <- 10 ^ (sum(
      log10(modeled) - log10(observed)
    ) / length(observed))
  } else {
    mbias <- (sum(
      (modeled) - (observed)
    ) / length(observed))
  }
  return(mbias)
}

calc_mape <- function(observed, modeled) {
  mmape <- (sum(
    abs((modeled) - (observed)) / abs((observed))
  ) / length(observed))
  return(mmape * 100)
}

calc_rmse <- function(observed, modeled) {
  mrmse <- sqrt(sum(
    ((modeled) - (observed)) ^ 2
  ) / length(observed))
  return(mrmse)
}


# ------------------

calc_error_metrics <- function(x, y) {
  # if lengths are different, return error
  if (length(x) != length(y)) {
    print("Lengths differ! Returning -9999")
    return(-9999)
  }
  
  ## remove NAs
  # remove NAs in x from both
  x1 <- x[!is.na(x)]
  y1 <- y[!is.na(x)]
  # remove NAs in y from both
  y2 <- y1[!is.na(y1)]
  x2 <- x1[!is.na(y1)]
  
  # select regression type: 1 for OLS (usually used for Type I?); 2 for major axis regression [MA] (usually for Type II?)
  # (standard major axis [SMA] and ranged major axis [RMA] also available - both for Type II?)
  rtype <- 2
  # create model
  m1 <- lmodel2(y2 ~ x2)
  m1.slope = m1$regression.results$Slope[rtype]
  m1.int = m1$regression.results$Intercept[rtype]
  
  # compile metrics
  metric_df <- data.frame(MAE_mult = calc_mae(x2, y2, mult_space = TRUE), 
                          bias_mult = calc_bias(x2, y2, mult_space = TRUE), 
                          MAE_std = calc_mae(x2, y2, mult_space = FALSE), 
                          bias_std = calc_bias(x2, y2, mult_space = FALSE), 
                          MAPE = calc_mape(x2, y2), 
                          RMSE = calc_rmse(x2, y2), 
                          slope = m1.slope, 
                          int = m1.int, 
                          Rsq = m1$rsquare, 
                          n = length(x2))
  return(metric_df)
}

# ------------------------

# uses calc_error_metrics() to place metrics on plot
plot_error_metrics <- function(x, y, 
                               xname = deparse(substitute(x)), 
                               yname = deparse(substitute(y)), 
                               log_axes = "xy",
                               equal_axes = TRUE, 
                               xlim = c(min(x, y, na.rm = TRUE), max(x, y, na.rm = TRUE)),
                               ylim =  c(min(x, y, na.rm = TRUE), max(x, y, na.rm = TRUE)),
                               stack_lows = c(0, 0),
                               plot_regrline = FALSE,
                               show_metrics = TRUE,
                               metrics_mult_space = TRUE,
                               show_mape = FALSE, 
                               show_regr_stats = FALSE,
                               pos_text_x = min(x, y, na.rm = TRUE),
                               pos_text_y = NULL,
                               print_metrics = TRUE,
                               #xaxt = xaxt,
                               #yaxt = yaxt,
                               ...) {
  
  ## prep
  
  # make data frame with x and y
  df <- data.frame(x, y)
  
  # remove NAs
  df <- df[!is.na(df$x), ]
  df <- df[!is.na(df$y), ]
  
  # calculate error metrics for plot
  err_metr <- calc_error_metrics(df$x, df$y)
  
  # set low values to something higher for log plotting, if desired
  if (length(stack_lows) == 2) {
    df$x[df$x <= 0] <- stack_lows[1]
    df$y[df$y <= 0] <- stack_lows[2]
  }
  
  # create log model to plot abline properly
  if (log_axes == "xy") {
    rtype_plot <- 2
    logm <- lmodel2(log10(df$y) ~ log10(df$x))
    line.int <- logm$regression.results$Intercept[rtype_plot] # *** change if switching regression type
    line.slope <- logm$regression.results$Slope[rtype_plot] # *** change if switching regression type
  } else {
    line.int <- err_metr$int
    line.slope <- err_metr$slope
  }
  
  # set whether to plot axes (no if log)
  if (grepl("x", log_axes)) {
    pxaxt <- "n"
  } else {
    pxaxt <- "s"
  }
  if (grepl("y", log_axes)) {
    pyaxt <- "n"
  } else {
    pyaxt <- "s"
  }
  
  ## plot
  plot(df$x, df$y, log = log_axes, xlab = xname, ylab = yname, 
       xaxt = pxaxt, yaxt = pyaxt,
       xlim = xlim, 
       ylim = ylim,
       ...) # col = alpha("black", 0.3), pch = 20
  
  abline(0, 1, lty = 3, untf = TRUE) # show y = x line
  
  if (plot_regrline == TRUE) {
    abline(line.int, line.slope, untf = TRUE) # show model line, transformed to log space
    legend("bottomright", y.intersp = 0.5, c("Fit model", "y = x"), lty = c(1, 3), bty = "n")
  } else {
    legend("bottomright", y.intersp = 0.5, "y = x", lty = 3, bty = "n")
  }
  
  # add axes if log transformed
  if (grepl("x", log_axes)) {
    axis(1, at = c(10 ^ (-1:3)), labels = c(10 ^ (-1:3)))
  }
  if (grepl("y", log_axes)) {
    axis(2, at = c(10 ^ (-1:3)), labels = c(10 ^ (-1:3)))
  }
  
  
  ## add metrics text to plot
  
  if(is.null(pos_text_y)) {
    ymin <- min(x, y, na.rm = TRUE)
    ymax <- max(x, y, na.rm = TRUE)
    pos_text_y <- c(ymax, 
                    (ymax - ymin) * 1 / 2, 
                    (ymax - ymin) * 1 / 4)
  }
  
  
  
  if (show_metrics) {
    text(x = pos_text_x, y = pos_text_y[1], 
         adj = c(0, 1),
         bquote(MAE[mult] * " = " * .(signif(err_metr$MAE_mult, digits = 3))))
    text(x = pos_text_x, y = pos_text_y[2],
         adj = c(0, 1),
         bquote(bias[mult] * " = " * .(signif(err_metr$bias_mult, digits = 3))))
    text(x = pos_text_x, y = pos_text_y[3],
         adj = c(0, 1),
         paste0("n = ", err_metr$n))
  }
  
  # print to console
  if (print_metrics == TRUE) {
    print(err_metr)
  }
}