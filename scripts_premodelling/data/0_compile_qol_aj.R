library(tidyverse)


folder_raw <- function(x) here::here("data", "raw", x)
folder_data <- function(x) here::here("data", "processed_ce", x)



### QALY AJ's QL
QL_HZ <- read_csv(folder_raw("QL_AJ.csv"), col_names = c("age", "QL_HZ_d")) %>% 
  full_join(tibble(age = 0:100)) %>% 
  arrange(age) %>% 
  fill(QL_HZ_d, .direction = "updown")


QL_HZ_6m <- read_csv(folder_raw("QL_6m_pre_vac_AJ.csv")) %>% 
  full_join(tibble(age = 0:100)) %>% 
  arrange(age) %>% 
  fill(QL_6m_pre_vac, .direction = "updown") %>% 
  mutate(
    QL_6m_post_vac = QL_6m_pre_vac * 0.646914809
  )



additional_QL_HZ_6m <- read_csv(folder_raw("23-07-2019 AJs additional QL over 6m zvl.csv")) %>% 
  rename(additional_QL_HZ_6m = `additional QL 6m`) %>% 
  full_join(tibble(age = 0:100)) %>% 
  arrange(age) %>% 
  fill(additional_QL_HZ_6m, .direction = "updown") 

QOL <- QL_HZ %>% 
  left_join(QL_HZ_6m) %>% 
  left_join(additional_QL_HZ_6m)


save(QOL, file = folder_data("QOL_AJ.rdata"))
