library(tidyverse)



for(file in dir("R")) source(here::here("R", file))



## Loading parameters 


pars_base <- local({
  load(here::here("pars", "pars_base_15_rw.rdata"));pars
})


yss_naive <- exec_cohort_rzv(pars_base, age0s = c(70, 75, 80))


yss_revac <- exec_cohort_rerzv(pars_base, age0s =  c(70, 75, 80), age1s = 85)


