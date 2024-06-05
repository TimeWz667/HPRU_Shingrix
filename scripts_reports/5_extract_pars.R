library(jsonlite)
library(tidyverse)



load(here::here("data", "processed_epi", "Epi_HZ_IC_CPRD.rdata"))


write_csv(Incidence_HZ, here::here("out", "dd.csv"))




