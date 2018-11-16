vals <- runif(9, 0, 0.03)

## unfiltered
# differs from SD function in that uses N instead of N - 1, since we want population SD vs. sample SD
sd <- sqrt(sum((vals - mean(vals, na.rm = TRUE)) ^ 2) / (length(vals)))

cv <- sd(vals, na.rm = TRUE) / mean(vals, na.rm = TRUE)


## filtered
filtered_vals <- vals[which(vals > (1.5 * sd(vals) - mean(vals)) & vals < (1.5 * sd(vals) + mean(vals)))]

mean_filt <- sum(filtered_vals) / length(filtered_vals)

sd_filt <- sqrt(sum((filtered_vals - mean_filt) ^ 2) / (length(filtered_vals)))

cv_filt <- sd_filt / mean_filt

