datfile <- file("O:/PRIV/NERL_ORD_CYAN/Sentinel2/Jordan_Data/HawRiver/JLHaw_20170517.dat", "r")

# column names
# date the profile was taken, the time, tempurature (deg C), conductivity (millisiemens/cm), salinity (psu), 
# depth (m), pH, turbitiy (ntu), chlorophyl (micrograms/L), dissolved oxygen (mg/L)
headers <- c("date", "time", "temp_C", "cond_mS_cm", "salinity_psu", 
             "depth_m", "pH", "turbitiy_ntu", "chlorophyl_ug_L", "dissoxygen_mg_L")

# read lines
file_lines <- readLines(con = datfile)

# for each line, if it's a data row, append it to the dataframe
combined_data <- data.frame()

for (l in seq_along(file_lines)) {
  file_line <- strsplit(file_lines[l], split = " +")[[1]]
  if (length(file_line) == 10 & 
      !is.na(as.numeric(file_line[3]))) {
    combined_data <- rbind(combined_data, data.frame(matrix(file_line, ncol = 10, nrow = 1), stringsAsFactors = FALSE))
    } else {
      #print(sprintf("skipping line %s with %s elements", l, length(file_line)))
  }
}

# reset column names
colnames(combined_data) <- headers

# show # NAs
for(c in 1:ncol(combined_data)) {
  print(sprintf("%s: %s NA", colnames(combined_data)[c], sum(is.na(combined_data[, c]))))
}
