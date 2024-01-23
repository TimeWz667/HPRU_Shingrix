library(tidyverse)


folder_raw <- function(x) here::here("data", "raw", x)
folder_data <- function(x) here::here("data", "processed_ce", x)


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
  mutate(
    QOL = ifelse(is.na(QOL), 1, QOL)
  )
  fill(QOL, .direction = "updown")


QL <- crossing(age = 0:100, Key = 1:1000) %>% 
  left_join(QL_y1) %>%
  left_join(QL_y2) %>% 
  left_join(QL_y1_o3m) %>% 
  arrange(Key, age) %>%
  group_by(Key) %>% 
  fill(QL_y1, QL_y2, QL_y1_o3m, .direction = "updown") %>% 
  ungroup() %>% 
  left_join(QOL) %>% 
  mutate(
    QL_HZ = QL_y1 + QL_y2
  ) %>% 
  relocate(Key, age, QOL)


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
  
  pop %>% left_join(crossing(va = 0:100, age = 0:100), by = "age", relationship = "many-to-many") %>% 
    arrange(va) %>% 
    group_by(va) %>% 
    filter(age >= va) %>%
    left_join(QOL %>% select(va = age, QOL), by = "va") %>% 
    mutate(
      p_surv = cumprod(1 - mortality),
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
    rename(age = va) %>% 
    left_join(QOL)
} 


save(QL, calc_ql_death, file = folder_data("QOL_LE.rdata"))
