library(tidyverse)
library(readxl)


theme_set(theme_bw())


dat_ve <- read_xlsx(here::here("data", "VE.xlsx"), sheet = 1) %>% 
  filter(Use) %>% 
  filter(Type == "HZ") %>% 
  filter(Vaccine == "Shingrix") %>% 
  mutate(
    M = M / 100,
    L = L / 100, 
    U = U / 100,
    Yr = gsub("yr", "", `Sub-group`),
    Yr = ifelse(is.na(Yr), 1.5, as.numeric(Yr))
  )


dat_ve %>% 
  ggplot() + 
  geom_pointrange(aes(x = Yr, y = M, ymax = U, ymin = L, colour = Source)) + 
  expand_limits(y = 0)



fit_loglin <- lm(log(M) ~ Yr, data = dat_ve %>% filter(!Realworld))
fit_step2<- lm(log(M) ~ Yr + (Yr >= 9), data = dat_ve %>% filter(!Realworld))

fit_ce <- read_csv(here::here("pars", "sims_ve_rzv_zlg.csv")) %>% 
  group_by(Yr) %>% 
  summarise(f_ga = mean(VE))


proj <- tibble(Yr = 0:100) %>% 
  mutate(
    f_l = exp(predict(fit_loglin, .)),
    f_l = pmin(1, f_l),
    f_s2 = exp(predict(fit_step2, .)),
    f_s2 = pmin(1, f_s2),
    f_l_d10 = ifelse(Yr > 10, 0, f_s2),
    f_s2_d10 = ifelse(Yr > 10, 0, f_l)
  ) %>% 
  left_join(fit_ce)


proj %>% 
  filter(Yr <= 20) %>% 
  ggplot() +
  geom_point(data = dat_ve, aes(x = Yr, y = M, colour = Source)) +
  geom_line(aes(x = Yr, y = f_l, linetype = "Linear")) +
  geom_line(aes(x = Yr, y = f_s2, linetype = "2Step Linear")) +
  # geom_line(aes(x = Yr, y = f_l * 0.87, linetype = "Linear")) +
  # geom_line(aes(x = Yr, y = f_s2 * 0.87, linetype = "2Step Linear")) +
  # geom_line(aes(x = Yr, y = f_ga * 0.90, linetype = "Coxian-Erlang")) +
  geom_line(aes(x = Yr, y = f_l_d10, linetype = "Linear, drop at 10")) +
  geom_line(aes(x = Yr, y = f_s2_d10, linetype = "2Step Linear, drop at 10")) +
  geom_line(aes(x = Yr, y = f_ga, linetype = "Coxian-Erlang")) +
  expand_limits(y = 0)


## Output to parameters -----

ve0 <- crossing(Age = 0:100, AgeVac = 0:100) %>% 
  filter(Age >= AgeVac) %>% 
  mutate(
    TypeVac = "Shingrix",
    Yr = Age - AgeVac
  )

ve_nic <- ve0 %>% 
  left_join(proj %>% select(Yr, VE_Trial = f_l) %>% mutate(VE_Real = VE_Trial * 0.87)) %>% 
  pivot_longer(starts_with("VE"), names_pattern = "VE_(\\w+)", names_to = "Type", values_to = "VE") %>% 
  mutate(IC = F) %>% 
  select(-Yr) %>% 
  arrange(Type, Age, AgeVac)

save(ve_nic, file = here::here("pars", "ves_rzv_linear.rdata"))


ve_nic <- ve0 %>% 
  left_join(proj %>% select(Yr, VE_Trial = f_s2) %>% mutate(VE_Real = VE_Trial * 0.87)) %>% 
  pivot_longer(starts_with("VE"), names_pattern = "VE_(\\w+)", names_to = "Type", values_to = "VE") %>% 
  mutate(IC = F) %>% 
  select(-Yr) %>% 
  arrange(Type, Age, AgeVac)

save(ve_nic, file = here::here("pars", "ves_rzv_2step.rdata"))


ve_nic <- ve0 %>% 
  left_join(proj %>% select(Yr, VE_Trial = f_ga) %>% mutate(VE_Real = VE_Trial * 0.90)) %>% 
  pivot_longer(starts_with("VE"), names_pattern = "VE_(\\w+)", names_to = "Type", values_to = "VE") %>% 
  mutate(IC = F) %>% 
  select(-Yr) %>% 
  arrange(Type, Age, AgeVac)

save(ve_nic, file = here::here("pars", "ves_rzv_ce.rdata"))


ve_nic <- ve0 %>% 
  left_join(proj %>% select(Yr, VE_Trial = f_l_d10) %>% mutate(VE_Real = VE_Trial * 0.87)) %>% 
  pivot_longer(starts_with("VE"), names_pattern = "VE_(\\w+)", names_to = "Type", values_to = "VE") %>% 
  mutate(IC = F) %>% 
  select(-Yr) %>% 
  arrange(Type, Age, AgeVac)

save(ve_nic, file = here::here("pars", "ves_rzv_linear_drop10.rdata"))


ve_nic <- ve0 %>% 
  left_join(proj %>% select(Yr, VE_Trial = f_s2_d10) %>% mutate(VE_Real = VE_Trial * 0.87)) %>% 
  pivot_longer(starts_with("VE"), names_pattern = "VE_(\\w+)", names_to = "Type", values_to = "VE") %>% 
  mutate(IC = F) %>% 
  select(-Yr) %>% 
  arrange(Type, Age, AgeVac)

save(ve_nic, file = here::here("pars", "ves_rzv_2step_drop10.rdata"))

