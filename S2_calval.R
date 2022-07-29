
library(lmodel2)
library(boot)
library(scales)
library(ggpubr)

mu_conus <- read.csv("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/S2/calvalready.csv")


# split
set.seed(1)
cal_ind <- sample(1:nrow(mu_conus), floor(nrow(mu_conus) * 0.8), replace = FALSE)
mu_conus_cal <- mu_conus[cal_ind, ]
mu_conus_val <- mu_conus[-cal_ind, ]

############### calibrate ###############

# raw regressions
chl_m1_is <- lm(In.Situ.chl ~ MCI_rhos, data = mu_conus_cal)
chl_m1_mci <- lm(MCI_rhos ~ In.Situ.chl, data = mu_conus_cal)
chl_m2 <- lmodel2(In.Situ.chl ~ MCI_rhos, data = mu_conus_cal,
                  range.y = "relative", range.x = "interval")


## bootstrapping

# m2
bs2 <- function(data, indices, formula) {
  d <- data[indices, ] # allows boot to select sample
  fit <- lmodel2(formula, data = d,
          range.y = "relative", range.x = "interval")
  return(c(fit$regression.results$Intercept[4],
           fit$regression.results$Slope[4]))
}

boot_m2 <- boot(data = mu_conus_cal, statistic = bs2,
              R = 1000, formula = In.Situ.chl ~ MCI_rhos)

1 / chl_m2$regression.results$Slope[4]
chl_m2$regression.results$Intercept[4] / chl_m2$regression.results$Slope[4]

# m1
bs1 <- function(data, indices, formula) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit))
}

boot_m1 <- boot(data = mu_conus_cal, statistic = bs1,
              R = 1000, formula = In.Situ.chl ~ MCI_rhos)


# calculate chl for all methods
mu_conus_cal$chl_m1_is <- (coef(chl_m1_is)[2] * mu_conus_cal$MCI_rhos) + coef(chl_m1_is)[1]
mu_conus_cal$chl_m1_mci <- -(mu_conus_cal$MCI_rhos - coef(chl_m1_mci)[1]) / coef(chl_m1_mci)[2]
mu_conus_cal$chl_m2 <- (chl_m2$regression.results$Slope[4] * mu_conus_cal$MCI_rhos) + chl_m2$regression.results$Intercept[4]
mu_conus_cal$chl_m1bs <- (mean(boot_m1$t[, 2]) * mu_conus_cal$MCI_rhos) + mean(boot_m1$t[, 1])
mu_conus_cal$chl_m2bs <- (mean(boot_m2$t[, 2]) * mu_conus_cal$MCI_rhos) + mean(boot_m2$t[, 1])

compare_chl <- mu_conus_cal[, which(colnames(mu_conus_cal) %in% 
                                  c("In.Situ.chl", "chl_m1_is", "chl_m1_mci", "chl_m2", "chl_m1bs", "chl_m2bs"))]

# see summary values for each method
apply(X = compare_chl, MARGIN = 2, FUN = summary)

# see error metrics for all methods
apply_error_metrics <- function(ydat, xdat) {
  return(calc_error_metrics(x = xdat, y = ydat))
}

apply(X = compare_chl[, -1], MARGIN = 2, FUN = apply_error_metrics, xdat = mu_conus_cal$In.Situ.chl)

# plot all methods
plotem <- function(ydat, xdat) {
  plot(xdat, ydat, ylim = c(-20, 160), main = sum(ydat < 0))
  abline(0, 1)
}

apply(X = compare_chl[, -1], MARGIN = 2, FUN = plotem, xdat = mu_conus_cal$In.Situ.chl)


# plot all lines on in situ vs MCI
plot(mu_conus_cal$MCI_rhos, mu_conus_cal$In.Situ.chl)
abline(coef(chl_m1_is)[1], coef(chl_m1_is)[2], lty = 3)
abline(-coef(chl_m1_mci)[1] / coef(chl_m1_mci)[2], 1 / (coef(chl_m1_mci)[2]), lty = 5)
abline(chl_m2$regression.results$Intercept[4], chl_m2$regression.results$Slope[4], lty = 1)
abline(mean(boot_m1$t[, 1]), mean(boot_m1$t[, 2]))
abline(mean(boot_m2$t[, 1]), mean(boot_m2$t[, 2]))

# plot lines on MCI vs in situ
plot(mu_conus_cal$In.Situ.chl, mu_conus_cal$MCI_rhos)
abline(coef(chl_m1_mci)[1], coef(chl_m1_mci)[2], lty = 5)
abline(-coef(chl_m1_is)[1] / coef(chl_m1_is)[2], 1 / (coef(chl_m1_is)[2]), lty = 3)
abline(-chl_m2$regression.results$Intercept[4] / chl_m2$regression.results$Slope[4], 1 / chl_m2$regression.results$Slope[4], lty = 1)

mean(mu_conus_cal$MCI_rhos)
mean(mu_conus_cal$In.Situ.chl)

## confidence intervals - query and plot
boot_m2

b0 <- mean(boot_m2$t[, 1])
b1 <- mean(boot_m2$t[, 2])

boot.ci(boot_m2, index=1) # intercept
boot.ci(boot_m2, index=2) # slope

boot.ci(boot_m2, type="bca", index=1)
ci_b0$bca[4]
ci_b0$bca[5]


#https://stackoverflow.com/questions/14069629/how-can-i-plot-data-with-confidence-intervals
mtest <- lm(mu_conus_cal$In.Situ.chl ~ mu_conus_cal$MCI_rhos)
newx <- data.frame(x = seq(min(mu_conus_cal$In.Situ.chl), max(mu_conus_cal$In.Situ.chl), length.out=100))
preds <- predict(mtest, newdata = newx, interval = 'confidence')


###

# pull values and CIs
b0 <- mean(boot_m2$t[, 1])
b1 <- mean(boot_m2$t[, 2])

ci_b0 <- boot.ci(boot_m2, type="bca", index=1)
ci_b1 <- boot.ci(boot_m2, type="bca", index=2)


## confidence interval on regression line

#xincrements <- data.frame(x = seq(min(mu_conus_cal$MCI_rhos), max(mu_conus_cal$MCI_rhos), length.out=100))
xincrements <- seq(min(mu_conus_cal$MCI_rhos), max(mu_conus_cal$MCI_rhos), length.out=100)
y_lci <- ci_b1$bca[4] * xincrements + ci_b0$bca[4]
y_uci <- ci_b1$bca[5] * xincrements + ci_b0$bca[5]



plot(mu_conus_cal$MCI_rhos, mu_conus_cal$In.Situ.chl, pch = 20)
abline(b0, b1)

# intervals ****FIX THIS*****
lines(xincrements, y_lci, lty = 'dashed', col = 'red')
lines(xincrements, y_uci, lty = 'dashed', col = 'red')
abline(h = 0)

points(xincrements, y_lci, col = 'red', pch = ".")
lines(xincrements, y_uci, lty = 'dashed', col = 'red')


## error bars on predictions

# assign predicted chl based on bootstrapped coeffs, and 
mu_conus_cal$chl_mci <- b1 * mu_conus_cal$MCI_rhos + b0
mu_conus_cal$chl_mci_lci <- ci_b1$bca[4] * mu_conus_cal$MCI_rhos + ci_b0$bca[4]
mu_conus_cal$chl_mci_uci <- ci_b1$bca[5] * mu_conus_cal$MCI_rhos + ci_b0$bca[5]

# plot
plot(mu_conus_cal$In.Situ.chl, mu_conus_cal$chl_mci, pch = 20)
abline(0, 1)
abline(h =  0)

# plot each error bar
for (r in 1:nrow(mu_conus_cal)) {
  xc <- c(mu_conus_cal$In.Situ.chl[r], mu_conus_cal$In.Situ.chl[r])
  yc <- c(mu_conus_cal$chl_mci_lci[r], mu_conus_cal$chl_mci_uci[r])
  lines(x = xc, y = yc)
}



############### validate ###############

mu_conus_val$chl_m2bs <- (mean(boot_m2$t[, 2]) * mu_conus_val$MCI_rhos) + mean(boot_m2$t[, 1])

plot(mu_conus_val$In.Situ.chl, mu_conus_val$chl_m2bs)
abline(0, 1)

plot(mu_conus_cal$In.Situ.chl, mu_conus_cal$chl_m2bs)
points(mu_conus_val$In.Situ.chl, mu_conus_val$chl_m2bs, pch = 20)

calc_error_metrics(x = mu_conus_val$In.Situ.chl, y = mu_conus_val$chl_m2bs)

###############

# 
boots
plot(boots, index=1)
plot(boots, index=2)
as.numeric(boots$t0[1])
as.numeric(boots$t0[2])

mean(boots$t[, 1])
mean(boots$t[, 2])

boot.ci(boots, index=1) # intercept
boot.ci(boots, index=2) # slope
#


library(boot)
# function to obtain regression weights
bs <- function(data, indices, formula) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit))
}
# bootstrapping with 1000 replications
results <- boot(data=mtcars, statistic=bs,
                R=32, formula=mpg~wt+disp)

# view results
results
plot(results, index=1) # intercept
plot(results, index=2) # wt
plot(results, index=3) # disp

# get 95% confidence intervals
boot.ci(results, type="bca", index=1) # intercept
boot.ci(results, type="bca", index=2) # wt
boot.ci(results, type="bca", index=3) # disp


##

samplemean <- function(xd, d) {
  return(mean(xd[d]))
}

b = boot(x, samplemean, R=2)
b

samplen <- function(xd, d) {
  return(length(xd[d]))
}

b = boot(x, samplen, R=10)
