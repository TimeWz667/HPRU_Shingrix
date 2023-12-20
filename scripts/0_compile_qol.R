library(tidyverse)


folder_raw <- function(x) here::here("data", "raw", x)
folder_data <- function(x) here::here("data", x)



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



### QALY loss HZ

QL_y1 <- read_csv(folder_raw("11-07-2018 QL with LE ph simps 1000 bootstrap runs y1.csv"))[-1] %>% 
  mutate(Key = 1:n()) %>% 
  pivot_longer(- Key, values_to = "QL_y1", names_to = "age") %>% 
  mutate(age = as.numeric(gsub("age_", "", age)))


QL_y2 <- read_csv(folder_raw("11-07-2018 QL with LE ph simps 1000 bootstrap runs y2.csv"))[-1] %>% 
  mutate(Key = 1:n()) %>% 
  pivot_longer(- Key, values_to = "QL_y2", names_to = "age") %>% 
  mutate(age = as.numeric(gsub("age_", "", age)))


QL_y1_o3m <- read_csv(folder_raw("13-09-2018 QL with LE ph simps 1000 bootstrap runs y1 over 3m.csv"))[-1] %>% 
  mutate(Key = 1:n()) %>% 
  pivot_longer(- Key, values_to = "QL_y1_o3m", names_to = "age") %>% 
  mutate(age = as.numeric(gsub("age_", "", age)))


QL <- crossing(age = 0:100, Key = 1:1000) %>% 
  left_join(QL_y1) %>%
  left_join(QL_y2) %>% 
  left_join(QL_y1_o3m) %>% 
  arrange(Key, age) %>%
  group_by(Key) %>% 
  fill(QL_y1, QL_y2, QL_y1_o3m, .direction = "updown") %>% 
  ungroup() %>% 
  mutate(
    QL_HZ = QL_y1 + QL_y2
  )


calc_ql_death <- function(pop, dis=0.035) {
  
  # #Szende A, Janssen B, Cabases J. Self-reported population health: an international perspective based on EQ-5D. Dordrecht: Springer; 2014. TTO value set England. p30
  QOL <- tibble(
    age = 18:100,
    QOL=c(rep(0.929, length(18:24)),
          rep(0.919, length(25:34)),
          rep(0.893, length(35:44)),
          rep(0.855, length(45:54)),
          rep(0.810,  length(55:64)),
          rep(0.773, length(65:74)),
          rep(0.703, length(75:100)))
    ) %>% 
    full_join(tibble(age = 0:100)) %>% 
    arrange(age) %>% 
    fill(QOL, .direction = "updown")
  
  pop %>% left_join(crossing(va = 0:100, age = 0:100)) %>% 
    arrange(va) %>% 
    group_by(va) %>% 
    filter(age >= va) %>%
    left_join(QOL %>% select(va = age, QOL)) %>% 
    mutate(
      p_surv = cumprod(1 - Background_mortality),
      p_surv_d = p_surv * (1 + dis) ^ - (age - va),
      ql = p_surv * QOL,
      ql_d = p_surv_d * QOL
    ) %>% 
    summarise(
      LE = sum(p_surv),
      LE_d = sum(p_surv_d),
      QL_death0 = sum(ql),
      QL_death0_d = sum(ql_d)
    ) %>% 
    left_join(QOL)
} 




save(QL, calc_ql_death, file = folder_data("QOL_LE.rdata"))
