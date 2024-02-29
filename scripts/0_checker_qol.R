library(tidyverse)


theme_set(theme_bw())

discount_rate_effects <- 0.035

load(here::here("data", "processed_demography", "Population_IC_Ons_2015.rdata"))
load(here::here("data", "processed_ce", "QOL_LE.rdata"))


## Note: QL_death0.csv mis-calculated the discounting rate
QL_death0 <- read_csv(here::here("data", "processed_ce", "QL_death0.csv"))


Pop %>% left_join(crossing(va = 0:100, age = 0:100)) %>% 
  arrange(va) %>% 
  group_by(va) %>% 
  filter(age >= va) %>%
  left_join(QL %>% select(age, QOL) %>% group_by(age) %>% summarise(QOL = mean(QOL))) %>% 
  mutate(
    p_surv = cumprod(1 - Background_mortality),
    p_surv_d = p_surv * (1 + discount_rate_effects) ^ - (age - va),
    ql = p_surv * QOL,
    ql_d = p_surv_d * QOL
  ) %>%
  filter(age >= va) %>% 
  summarise(
    LE = sum(p_surv),
    LE_d = sum(p_surv_d),
    QL = sum(ql),
    QL_d = sum(ql_d)
  ) %>% 
  rename(age = va) %>% 
  ggplot() +
  geom_line(aes(x = age, y = QL_d)) +
  geom_point(data = QL_death0, aes(x = age, y = QL_death0))

