

calc_mci <- function(R1, R2, R3, w1 = 665, w2 = 705, w3 = 740) {
  mci <- R2 - R1 - ((R3 - R1) * ((w2 - w1) / (w3 - w1)))
  return(mci)
}


calc_baseline_slope <- function(R1, R3, w1 = 665, w3 = 740) {
  bslope <- (R3 - R1) / (w3 - w1)
  return(bslope)
}


calc_ndci <- function(R1, R2) {
  ndci <- (R2 - R1) / (R2 + R1)
  return(ndci)
}


calc_dci <- function(R1, R2, R3, w1 = 665, w2 = 705, w3 = 740) {
  if (NA %in% c(R1, R2, R3)) {
    return(NA)
  }
  
  mab <- (R2 - R1) / (w2 - w1)
  mbc <- (R3 - R2) / (w3 - w2)
  
  if (abs(mbc) > mab) {
    # left peak
    mbc <- (R3 - R2) / (w3 - w2)
    rd <- R1
    ld <- ((R3 - rd) / mbc) + w3
    le <- (w1 + ld) / 2
    re <- R3 - mbc * (w3 - le)
    dci <- calc_mci(R1 = R1, R2 = re, R3 = R3, w2 = le)
  } else {
    # right peak
    # calc here
    dci <- calc_mci(R1 = R1, R2 = R2, R3 = R3)
  }
  return(dci)
}
