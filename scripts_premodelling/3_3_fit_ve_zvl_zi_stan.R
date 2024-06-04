library(rstan)
library(tidyverse)
library(readxl)

theme_set(theme_bw())


options(mc.cores = 6)
rstan_options(auto_write = TRUE)


dat_ve <- read_xlsx(here::here("data", "VE.xlsx"), sheet = 1) %>% 
  filter(Use) %>% 
  filter(Type == "HZ") %>% 
  filter(Vaccine == "Zostavax") %>% 
  filter(Source == "Klein 2023") %>% 
  mutate(
    M = M / 100,
    L = L / 100, 
    U = U / 100,
    Yr = gsub("yr", "", `Sub-group`),
    Yr = ifelse(is.na(Yr), 1.5, as.numeric(Yr))
  )


dat_ve_agp <- read_xlsx(here::here("data", "VE.xlsx"), sheet = 1) %>% 
  filter(Use) %>% 
  filter(Type == "HZ") %>% 
  filter(Vaccine == "Zostavax") %>% 
  filter(Source == "Mbinta 2022") %>%
  filter(!is.na(`Age-group`)) %>% 
  mutate(
    M = M / 100,
    L = L / 100, 
    U = U / 100,
    Yr = gsub("yr", "", `Sub-group`),
    Yr = ifelse(is.na(Yr), 1.5, as.numeric(Yr))
  )


dat_ve_agp <- read_xlsx(here::here("data", "VE.xlsx"), sheet = 1) %>% 
  filter(Use) %>% 
  filter(Type == "HZ") %>% 
  filter(Vaccine == "Zostavax") %>% 
  filter(Source == "Mbinta 2022") %>% 
  filter(!is.na(`Age-group`)) %>% 
  mutate(
    Agp = c(55, 65, 75, 85),
    M = M / 100,
    odd = log(M / (1 - M)),
    or = (odd - odd[2])
  ) %>% 
  select(Agp, or, M)


dat_ve %>% 
  mutate(
    sd = (U - L) / 2 / 1.96,
    n = M * (1 - M) / sd ** 2,
    n = round(n),
    l = qbinom(0.025, size =n, M) / n,
    u = qbinom(0.975, size =n, M) / n
  )


ds <- dat_ve %>% 
  filter(Follow_up == 1) %>% 
  mutate(
    sd = (U - L) / 2 / 1.96,
    n = M * (1 - M) / sd ** 2,
    n = round(n / n()),
    y = round(n * M)
  ) %>% 
  select(n, yr = Yr, y) %>% 
  as.list()

ds$N <- length(ds$yr)


for (key_model in c("zl_exp", "zl_gamma")) {
  model <-  stan_model(here::here("models", key_model + glue::as_glue(".stan")))
  
  post <- sampling(model, data = ds, chains = 3, iter = 2000, warmup = floor(2000 - 1000))
  
  
  if (key_model == "zl_gamma") {
    sel <- data.frame(rstan::extract(post, pars = c("p0", "alpha", "beta"))) %>%
      as_tibble()
  } else if (key_model == "zl_exp") {
    sel <- data.frame(rstan::extract(post, pars = c("p0", "beta"))) %>%
      as_tibble() %>%
      mutate(alpha = 1)
  }
  
  sel <- sel %>% mutate(Key = 1:n())
  
  sims <- sel %>%
    filter(Key <= 1000) %>%
    full_join(crossing(Key = 1:1000, Yr = 1:30)) %>%
    mutate(
      VE = p0 * (1 - pgamma(Yr, alpha, beta))
    )  %>%
    select(Key, Yr, VE)
  
  
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
    geom_pointrange(data = dat_ve, aes(x = Yr, y = M, ymin = L, ymax = U)) +
    scale_y_continuous("Vaccine efficacy, %", label = scales::percent) +
    scale_x_continuous("Year since vaccinated") +
    expand_limits(y = 0)
  
  
  save(post, file = here::here("out", "pars_ve_zvl_" + glue::as_glue(key_model) + ".rdata"))
  save(sel, file = here::here("pars", "pars_ve_zvl_" + glue::as_glue(key_model) + ".rdata"))
  write_csv(sims, here::here("pars", "sims_ve_zvl_" + glue::as_glue(key_model) + ".csv"))
  ggsave(g_gof, filename = here::here("docs", "figs", "g_pars_ve_zvl_" + glue::as_glue(key_model) + ".png"), width = 7, height = 5.5)
  
}



d_agp <- tibble(Age = 50:100) %>% 
  mutate(
    Agp = case_when(
      Age < 60 ~ 55,
      Age < 70 ~ 65, 
      Age < 80 ~ 75,
      T ~ 85
    )
  ) %>% 
  left_join(dat_ve_agp) %>% 
  mutate(
    odd = log(M / (1 - M)),
    or = odd - odd[Age == 70],
    or_spline = splinefun(unique(Agp), unique(or), method = "natural")(Age)
  )


g_test_agp <- d_agp %>% 
  mutate(
    M2 = splinefun(unique(Agp), unique(M), method = "natural")(Age),
    odd3 = splinefun(unique(Agp), unique(odd), method = "natural")(Age),
    M3 = 1 / (1 + exp(-odd3)),
    or4 = splinefun(unique(Agp), unique(or), method = "natural")(Age),
    odd4 = or4 + odd[Age == 70],
    M4 = 1 / (1 + exp(-odd4)),
  ) %>%   
  ggplot() +
  geom_line(aes(x = Age, y = M)) +
  geom_line(aes(x = Age, y = M2)) +
  geom_line(aes(x = Age, y = M3)) +
  geom_line(aes(x = Age, y = M4)) +
  expand_limits(y = 0:1)

g_test_agp


d_agp %>% 
  select(Age, or70 = or, or70spline = or_spline) %>%
  write_csv(here::here("pars", "pars_ve_zvl_agp.csv"))
