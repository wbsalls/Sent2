# using Bridget Seegers's metrics (not 10-based)
# this version uses:
## model-II regression
## log-log transformation **

library(lmodel2)
library(scales) # alpha

calc_bias <- function(observed, modeled, log_space = TRUE) {
  if (log_space == TRUE) {
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

calc_mae <- function(observed, modeled, log_space = TRUE) {
  if (log_space == TRUE) {
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

calc_mape <- function(observed, modeled) {
    mmape <- (sum(
      abs((modeled) - (observed)) / abs((observed))
    ) / length(observed))
  return(mmape * 100)
}

# ------------------

calc_error_metrics <- function(x, y, log_space = TRUE, rtype = 2) {
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
  rtype <- rtype
  # create model
  m1 <- lmodel2(y2 ~ x2)
  m1.slope = m1$regression.results$Slope[rtype]
  m1.int = m1$regression.results$Intercept[rtype]
  
  # make predictions based on model - depracated
  #preds <- m1.slope * x2 + m1.int
  
  # use those predictions to calculate metrics - Major Axis Regression
  metric_df <- data.frame(slope = m1.slope,
                          int = m1.int,
                          r.sq = m1$rsquare,
                          MAE = calc_mae(x2, y2, log_space = log_space), 
                          MAPE = calc_mape(x2, y2), 
                          bias = calc_bias(x2, y2, log_space = log_space), 
                          rand.err = calc_mae(x2, y2, log_space = log_space) - abs(calc_bias(x2, y2, log_space = log_space)), 
                          n = length(x2))
  return(metric_df)
}

# ------------------------

# uses calc_error_metrics to place metrics on plot
plot_error_metrics <- function(x, y, 
                               xname = deparse(substitute(x)), 
                               yname = deparse(substitute(y)), 
                               rtype_plot = 2,
                               title = NULL, 
                               equal_axes = FALSE, 
                               log_axes = "",
                               plot_abline = TRUE,
                               mape = TRUE,
                               rand_error = TRUE,
                               regr_stats = TRUE,
                               states = NA, 
                               lakes = NA,
                               log_space = TRUE,
                               show_metrics = FALSE,
                               #xaxt = xaxt,
                               #yaxt = yaxt,
                               ...) {
  
  #
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
  
  # make data frame with x, y, and states and lakes if provided
  df <- data.frame(x, y, states, lakes)
  
  ## remove NAs
  df <- df[!is.na(df$x), ]
  df <- df[!is.na(df$y), ]
  
  # calculate error metrics for plot
  err_metr <- calc_error_metrics(df$x, df$y, rtype = rtype_plot, log_space = log_space)
  
  # reset values for log plotting, if necessary
  if ("x" %in% strsplit(log_axes, "")[[1]]) {
    df$x[df$x <= 0] <- 0.1
  }
  if ("y" %in% strsplit(log_axes, "")[[1]]) {
    df$y[df$y <= 0] <- 0.1
  }
  # create log model to plot model abline properly
  if (log_axes == "xy") {
    logm <- lmodel2(log(df$y) ~ log(df$x))
    line.int <- logm$regression.results$Intercept[rtype_plot] # *** change if switching regression type
    line.slope <- logm$regression.results$Slope[rtype_plot] # *** change if switching regression type
  } else {
    line.int <- err_metr$int
    line.slope <- err_metr$slope
  }
  
  if (equal_axes == FALSE) {
    plot(df$x, df$y, log = log_axes, pch = 1, xlab = xname, ylab = yname, main = title,
         xaxt = pxaxt, yaxt = pyaxt, ...)
    abline(line.int, line.slope, untf = TRUE) # show model line
    #abline(0, 0, lty = 2) # show y = 0 line
    
    legend("bottomright", y.intersp = 0.5,
           c("Fit model"), 
           lty=c(1), bty = "n")
  }
  else if (equal_axes == TRUE) {
    plot(df$x, df$y, log = log_axes, xlab = xname, ylab = yname, main = title, 
         xaxt = pxaxt, yaxt = pyaxt,
         #xlim = c(min(df$x, df$y), max(df$x, df$y)), 
         #ylim = c(min(df$x, df$y), max(df$x, df$y)),
         ...) # col = alpha("black", 0.3), pch = 20
    
    if (plot_abline == TRUE) {
      abline(line.int, line.slope, untf = TRUE) # show model line, transformed to log space
    }
    abline(0, 1, lty = 3, untf = TRUE) # show y = x line
    #abline(0, 0, lty = 2) # show y = 0 line
    #abline(v = 0, lty = 2) # show x = 0 line
    
    if (plot_abline == TRUE) {
      legend("bottomright", y.intersp = 0.5, c("Fit model", "y = x"), lty=c(1, 3), bty = "n")
    } else {
      legend("bottomright", y.intersp = 0.5, c("y = x"), lty=c(3), bty = "n")
    }
  }
  
  ## add axes if log transformed and axes withheld
  if (grepl("x", log_axes)) {
  axis(1, at = c(10^(-1:3)), labels = c(10^(-1:3)))
  }
  if (grepl("y", log_axes)) {
  axis(2, at = c(10^(-1:3)), labels = c(10^(-1:3)))
  }
  
  ## add metrics text
  # specify number of states and lakes, if included
  add.text <- ""
  if (sum(is.na(states)) != length(states)) {
    states <- states[!is.na(states)]
    add.text <- paste0(add.text, paste0("\n# states = ", length(unique(states))))
  }
  if (sum(is.na(lakes)) != length(lakes)) {
    lakes <- lakes[!is.na(lakes)]
    add.text <- paste0(add.text, paste0("\n# lakes = ", length(unique(lakes))))
  }
  
  # add text
  if (equal_axes == TRUE) {
    text_x <- 0.05 #min(df$x, df$y)
    text_y <- max(df$x, df$y) #150
  } else if (equal_axes == FALSE) {
    text_x <- min(df$x)
    text_y <- max(df$y)
  }
  
  text(x = text_x, y = text_y, adj = c(0, 1), 
       paste0(#"y = ", signif(err_metr$slope, digits = 3), "x + ", signif(err_metr$int, digits = 3), "\n",
              "MAE = ", signif(err_metr$MAE, digits = 3), "\n", 
              "bias = ", signif(err_metr$bias, digits = 3), "\n", 
              if (mape == TRUE) {paste0("MAPE = ", signif(err_metr$MAPE, digits = 3), "\n")}, 
              if (rand_error == TRUE) {paste0("random error = ", signif(err_metr$rand.err, digits = 3), "\n")}, 
              if (regr_stats == TRUE) {paste0("slope = ", round(err_metr$slope, 2), "\n",
                                              "intercept = ", round(err_metr$int, 2), "\n",
                                              "R-sq. = ", round(err_metr$r.sq, 2), "\n")},
              "n = ", err_metr$n, 
              add.text))
  
  if (show_metrics == TRUE){
    err_metr_print <- rbind(calc_error_metrics(df$x, df$y, rtype = rtype_plot, log_space = FALSE), 
                            calc_error_metrics(df$x, df$y, rtype = rtype_plot, log_space = TRUE))
    rownames(err_metr_print) <- c("linear", "multiplicative")
    print(err_metr_print)
  }
}