library(tidyverse)
library(betareg)
library(readxl)




dat_burden <- read_xlsx(here::here("data", "raw", "Zoster_Burden_Results_Nov19_Trimmed.xlsx"), sheet = 1) %>% 
  select(Age, Immunocompromised, N = `Person- years`, IncR = `Incidence per 1,000 pyrs`) %>% 
  mutate(IncR = IncR / 1000) %>% 
  full_join(
    read_xlsx(here::here("data", "raw", "Zoster_Burden_Results_Nov19_Trimmed.xlsx"), sheet = 2) %>% 
      select(Age, Immunocompromised, IncR_GP = `Incidence per 1,000 pyrs`) %>% 
      mutate(IncR_GP = IncR_GP / 1000)
  ) %>% 
  full_join(
    read_xlsx(here::here("data", "raw", "Zoster_Burden_Results_Nov19_Trimmed.xlsx"), sheet = 3) %>%
      select(Age, Immunocompromised, p_phn = `Prop events with PHN`)
  ) %>% 
  filter(!is.na(Age) & Age != "101+") %>% 
  mutate(Age = as.numeric(Age))


save(dat_burden, file = here::here("data", "hz_burden_22.rdata"))



dat_burden_17 <- read_csv(here::here("data", "raw", "Incidence_RCGP_2017_Nick.csv"))




dat_burden %>% tail()

dat_burden %>% 
  mutate(
    IncR = ifelse(Immunocompromised, IncR, IncR * 2)
  ) %>% 
  ggplot() +
  geom_point(aes(x = Age, y = IncR, color = Immunocompromised)) +
  geom_line(data = dat_burden_17, aes(x = age_sep, y = zoster_inc_1000py / 1000 * 2.3)) +
  scale_y_log10()



dat_burden %>% 
  group_by(Age) %>% 
  summarise(
    IncR = sum(IncR * N) / sum(N)
  ) %>% 
  ggplot() +
  geom_point(aes(x = Age, y = IncR)) +
  geom_line(data = dat_burden_17, aes(x = age_sep, y = zoster_inc_1000py / 1000)) +
  scale_y_log10()



d <- dat_burden %>% 
  filter(Immunocompromised == 0) %>% 
  filter(!is.na(p_phn) & p_phn > 0)




fit1<-lm(p_phn~Age, data=d)
fit2<-lm(p_phn~Age+I(Age^2), data=d)
fit3<-betareg(p_phn~Age, data = d)
fit4<-betareg(p_phn~Age+I(Age^2), data = d)
fit5<-betareg(p_phn~Age+I(Age^2)+I(Age^3), data = d)


AIC(fit1, fit2, fit3, fit4, fit5)

plot(predict(fit5), type = "l")
lines(predict(fit3))
points(1:49, d$p_phn)
