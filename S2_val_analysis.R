source("C:/Users/WSALLS/Git/Sent2/cal_val.R")

par(mfrow = c(1, 2))

cvm2 <- cal_val(obs_dat = mu_conus_addAcolite$In.Situ.chl.x, 
                p1_dat = mu_conus_addAcolite$MCI_rhot, 
                portion_cal = 0.8, 
                set_seed = TRUE, 
                neg.rm = TRUE, 
                negs2zero = FALSE, 
                nboots = 300,
                switch_y_cal = switch_y, 
                regr_model_cal = regr_model, 
                main = paste0("rhot, l2gen - "),
                alg_name = algc,
                log_axes_val = "",
                returnObjects = TRUE)
