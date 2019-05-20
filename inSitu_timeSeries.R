setwd("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Jordan_Data")
setwd("/Users/wilsonsalls/Desktop/EPA/Sentinel2/Jordan_Data")

files <- list.files("./HawRiver")

comb <- data.frame()

# for each file
for (f in seq_along(files)) {
  
  # read lines of file
  file_lines <- readLines(con = file(file.path("./HawRiver", files[f]), "r"))
  
  # print length of file
  print(sprintf("#%s/%s : %s : %s observations : %s", f, length(files), files[f], length(file_lines), Sys.time()))
  
  # for each line, if it's a data row, append it to the dataframe
  for (l in seq_along(file_lines)) {
    file_line <- strsplit(file_lines[l], split = " +")[[1]]
    if (length(file_line) == 10 & 
        !is.na(as.numeric(file_line[3]))) {
      comb <- rbind(comb, data.frame(matrix(file_line, ncol = 10, nrow = 1), stringsAsFactors = FALSE))
    } else {
      #print(sprintf("skipping line %s with %s elements", l, length(file_line)))
    }
  }
}

## column names
# date the profile was taken, the time, tempurature (deg C), conductivity (millisiemens/cm), salinity (psu), 
# depth (m), pH, turbitiy (ntu), chlorophyl (micrograms/L), dissolved oxygen (mg/L)
headers <- c("date", "time", "temp_C", "cond_mS_cm", "salinity_psu", 
             "depth_m", "pH", "turbitiy_ntu", "chlorophyl_ug_L", "dissoxygen_mg_L")

colnames(comb) <- headers

### ------------------------------------------------


## show # NAs
for(c in 1:ncol(comb)) {
  print(sprintf("%s: %s NA", colnames(comb)[c], sum(is.na(comb[, c]))))
}

## make date-time
library(chron)
comb$datetime <- chron(dates. = comb$date, times. = comb$time)

## experiment
comb <- comb[comb$date == "05/19/17", ] # just to experiment

plot(comb$datetime, comb$depth_m) # these are depth profiles!!

plot(comb$depth_m, comb$chlorophyl_ug_L) # ~2 m drop-off, ~1 m max
ggplot(comb, aes(as.numeric(depth_m), as.numeric(chlorophyl_ug_L))) +
  stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = FALSE) +       
  scale_fill_viridis_c() +
  coord_cartesian(expand = FALSE) +
  xlab("Depth (m)") + 
  ylab("Chlorophyll (ug/L)") #+
#geom_point(shape = '.', col = 'white') # 


hist(as.numeric(comb$chlorophyl_ug_L)) # bimodal chl distr is explained by Z-shaped chl vs depth curve

comb <- comb[comb$depth_m <= 0.5, ]

plot(comb$datetime, comb$chlorophyl_ug_L)


## aggregate
comb$minute <- chron(comb$datetime, out.format = c("m/d/y", "h:m"))
head(comb$minute)
comb_ag <- aggregate(comb, by = list(comb$minute), FUN = mean)



### scraps ###
