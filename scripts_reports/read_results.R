library(tidyverse)



tab_ce_re <- read_csv(here::here("docs", "tabs", "rw_35", "stats_re_ce.csv"))
tab_ce_re %>% 
  filter(Index == "Thres")


tab_ce_uv <- read_csv(here::here("docs", "tabs", "rw_35", "stats_uv_ce.csv"))
tab_ce_uv %>% 
  filter(Index == "Thres")


tab_ce_uv %>% 
  filter(Arm == "RZV_2d") %>% 
  filter(Index == "Thres") %>% 
  filter(Age0 >= 70 & Age0 <= 80) %>% 
  pull(M) %>% range


tab_ce_uv %>% 
  filter(Arm == "RZV_2d") %>% 
  filter(Index == "Thres") %>% 
  filter(Age0 >= 80)


tab_ce_uv %>% 
  filter(Arm == "RZV_1d") %>% 
  filter(Index == "Thres") %>% 
  filter(Age0 >= 80)

tab_ce_uv %>% 
  filter(Arm == "RZV_2d") %>% 
  filter(Index == "Thres") %>% 
  filter(Age0 %in% c(80, 85, 90, 95))

tab_ce_uv %>% 
  filter(Arm == "RZV_1d") %>% 
  filter(Index == "Thres") %>% 
  filter(Age0 %in% c(80, 85, 90, 95))

tab_ce_re %>% 
  filter(Arm == "ReRZV_2d") %>% 
  filter(Index == "Thres") %>%
  filter(Age0 == 70) %>% 
  filter(Age1 %in% c(85, 90, 95))

tab_ce_re %>% 
  filter(Arm == "ReRZV_1d") %>% 
  filter(Index == "Thres") %>% 
  filter(Age0 == 70) %>% 
  filter(Age1 %in% c(85, 90, 95))

