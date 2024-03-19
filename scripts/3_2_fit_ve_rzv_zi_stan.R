library(rstan)
library(tidyverse)
library(readxl)

theme_set(theme_bw())


options(mc.cores = 6)
rstan_options(auto_write = TRUE)


model <- stan_model(here::here("models", "coxian_gamma.stan"))



dat_ve <- read_xlsx(here::here("data", "VE.xlsx"), sheet = 1) %>% 
  filter(Use) %>% 
  filter(Type == "HZ") %>% 
  mutate(
    M = M / 100,
    L = L / 100, 
    U = U / 100,
    Yr = gsub("yr", "", `Sub-group`),
    Yr = ifelse(is.na(Yr), 1.5, as.numeric(Yr))
  )


dat_ve %>% 
  mutate(
    sd = (U - L) / 2 / 1.96,
    n = M * (1 - M) / sd ** 2,
    n = round(n),
    l = qbinom(0.025, size =n, M) / n,
    u = qbinom(0.975, size =n, M) / n
  )


ds <- dat_ve %>% 
  filter(Source == "Strezova 2023") %>% 
  mutate(
    # sd = (U - L) / 2 / 1.96,
    # n = M * (1 - M) / sd ** 2,
    # n = round(n),
    n = c(14035, 13564, 13074, 12517, 7277, 7100, 6878, 6648, 6258),
    y = round(n * M)
  ) %>% 
  select(n, yr = Yr, y) %>% 
  filter(yr > 1) %>% 
  as.list()

ds$N <- length(ds$yr)


post <- sampling(model, data = ds, iter = 5000)

post

save(post, file = here::here("outputs", "temp", "pars_ve_rzv_zlgamma.rdata"))


sel <- data.frame(rstan::extract(post, pars = c("p0", "alpha", "beta"))) %>% 
  as_tibble() %>% 
  mutate(Key = 1:n())


sel %>%
  filter(Key <= 3000) %>%
  write_csv(here::here("pars", "pars_ve_rzv_zlgamma.csv"))


sims <- sel %>% 
  filter(Key <= 1000) %>% 
  full_join(crossing(Key = 1:1000, Yr = 1:30)) %>% 
  mutate(
    VE = p0 * (1 - pgamma(Yr, alpha, beta))
  )  %>% 
  select(Key, Yr, VE) %>% 
  write_csv(here::here("pars", "sims_ve_rzv_zlgamma.csv"))


g_gof <- sims %>% 
  group_by(Yr) %>% 
  summarise(
    M = mean(VE),
    L = quantile(VE, 0.025),
    U = quantile(VE, 0.975)
  ) %>% 
  ggplot() +
  geom_ribbon(aes(x = Yr, ymin = L, ymax = U), alpha = 0.2) +
  geom_line(aes(x = Yr, y = M)) +
  geom_pointrange(data = dat_ve %>% filter(!Realworld), aes(x = Yr, y = M, ymin = L, ymax = U)) +
  scale_y_continuous("Vaccine efficacy, %", label = scales::percent) +
  scale_x_continuous("Year since vaccinated") +
  expand_limits(y = 0)

g_gof

ggsave(g_gof, filename = here::here("outputs", "figs", "g_pars_ve_rzv_zlgamma.png"), width = 7, height = 5.5)



