apply_lor <- function(p0, lor) 1 / (1 + exp(-log(p0 / (1 - p0)) - lor))


get_or <- function(mlu, ref_mlu) {
  
  or0 <- (mlu[1] / (1 - mlu[1])) / (ref_mlu[1] / (1 - ref_mlu[1]))
  
  n_run <- 1e5
  while (T) {
    p0 <- runif(1e5, ref_mlu[2], ref_mlu[3])
    lors <- rnorm(1e5, 0, 10)
    
    p1 <- apply_lor(p0, lors)
    sel <- (p1 < mlu[3]) & (p1 > mlu[2])
    
    if (mean(sel) > 0.02 | n_run > 1e7) {
      break
    } else {
      n_run <- round(n_run * 10)  
    }
  }
  return(c(or0, quantile(exp(lors[sel]), c(0.025, 0.975))))
}

## RZV
# Singe-dose / two-doses Izurieta 2021
get_or(c(56.9, 55.0, 58.8) / 100, c(70.1, 68.6, 71.5) / 100)


# Realworld Mbinta 2022 / Trial Strezova 2022
get_or(c(79, 58, 90) / 100, c(89, 85.6, 91.3) / 100)


## ZVL
# UK / Meta
get_or(c(64, 60, 68) / 100, c(67.2, 65.4, 68.8) / 100)




