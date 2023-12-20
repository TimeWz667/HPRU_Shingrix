library(tidyverse)


theme_set(theme_bw())


folder_data <- function(x) here::here("data", x)


discount_rate_effects <- 0.035

load(folder_data("Population_IC_Ons_2015.rdata"))

load(folder_data("QOL_LE.rdata"))


Pop %>% left_join(crossing(va = 0:100, age = 0:100)) %>% 
  arrange(va) %>% 
  group_by(va) %>% 
  filter(age >= va) %>%
  left_join(QOL %>% select(va = age, QOL)) %>% 
  mutate(
    p_surv = cumprod(1 - Background_mortality),
    p_surv_d = p_surv * (1 + discount_rate_effects) ^ - (age - va),
    ql = p_surv * QOL,
    ql_d = p_surv_d * QOL
  ) %>% 
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


Pop %>% left_join(crossing(va = 0:100, age = 0:100)) %>% 
  arrange(va) %>% 
  group_by(va) %>% 
  filter(age >= va) %>%
  left_join(QOL %>% filter(Key == 1) %>% select(va = age, QOL)) %>% 
  mutate(
    p_surv = cumprod(1 - Background_mortality),
    p_surv_d = p_surv * (1 + discount_rate_effects) ^ - (age - va),
    ql = p_surv * QOL,
    ql_d = p_surv_d * QOL
  ) %>% 
  filter(va == 30)



