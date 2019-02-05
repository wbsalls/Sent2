#vals <- runif(9, 0, 0.03)

mu_mci <- read.csv("C:/Users/WSalls/Desktop/validation_S2_682imgs_MCI_L1C_2018-11-16.csv",
                   quote = "")

vals <- mu_mci[, which(substr(colnames(mu_mci), 1, 6) == "CI_val")]

## population SD
# differs from base R SD function in that uses N instead of N - 1, since we want population SD vs. sample SD
sd_pop <- function(vals) {
  vals <- vals[!is.na(vals)]
  sqrt(sum((vals - mean(vals)) ^ 2) / (length(vals)))
}

## unfiltered
cv <- sd_pop(vals) / mean(vals, na.rm = TRUE)


## filtered
cv_filtered <- function(vals, filtered = TRUE) {
  
  if (isTRUE(filtered)) {
  vals <- vals[which(vals > (1.5 * sd(vals) - mean(vals)) & vals < (1.5 * sd(vals) + mean(vals)))]
  }
  
}


## filtered OLD
filtered_vals <- vals[which(vals > (1.5 * sd(vals) - mean(vals)) & vals < (1.5 * sd(vals) + mean(vals)))]

mean_filt <- sum(filtered_vals) / length(filtered_vals)

sd_filt <- sqrt(sum((filtered_vals - mean_filt) ^ 2) / (length(filtered_vals)))

cv_filt <- sd_filt / mean_filt

