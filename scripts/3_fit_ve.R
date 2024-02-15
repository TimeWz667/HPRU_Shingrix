library(tidyverse)
library(readxl)




dat_ve <- read_xlsx(here::here("data", "Shingrix VE.xlsx"), sheet = 1) %>% 
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
  ggplot() + 
  geom_pointrange(aes(x = Yr, y = M, ymax = U, ymin = L, colour = Source)) + 
  expand_limits(y = 0)



dat_ve


fit_loglin <- lm(log(M) ~ Yr, data = dat_ve %>% filter(!Realworld))
fit_loglin2 <- lm(log(M) ~ Yr + (Yr ** 2), data = dat_ve %>% filter(!Realworld))
fit_step2<- lm(log(M) ~ Yr + (Yr >= 9), data = dat_ve %>% filter(!Realworld))


dat_ve$f_l <- predict(fit_loglin, dat_ve)
dat_ve$f_l2 <- predict(fit_loglin2, dat_ve)
dat_ve$f_s2 <- predict(fit_step2, dat_ve)


dat_ve %>% 
  ggplot() + 
  geom_point(aes(x = Yr, y = M, colour = Source)) +
  geom_line(aes(x = Yr, y = exp(f_l), colour = Source)) +
  geom_line(aes(x = Yr, y = exp(f_l2), colour = Source)) +
  geom_line(aes(x = Yr, y = exp(f_s2), colour = Source)) +
  expand_limits(y = 0)

dat_ve %>% 
  mutate(
    Fit = 1 - pgamma(Yr, 20, .2)
  ) %>% 
  ggplot() + 
  geom_point(aes(x = Yr, y = - log(1 - M) / Yr, colour = Source)) + 
  #geom_line(aes(x = Yr, y = Fit * 0.7)) +
  geom_line(aes(x = Yr, y = Fit)) +
  expand_limits(y = 0)




dat_ve %>% 
  mutate(
    Fit = 1 - 
  ) %>% 
  ggplot() + 
  geom_point(aes(x = Yr, y = M / Yr, colour = Source)) +
  geom_line(aes(x = Yr, y = Fit)) +
  expand_limits(y = 0)

