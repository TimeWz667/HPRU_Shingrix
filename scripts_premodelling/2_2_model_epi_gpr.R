library(tidyverse)
library(tidybayes)
library(readxl)
library(GauPro)
library(ggpubr)




epi_hz <- read_xlsx(here::here("data", "raw", "Zoster_Burden_Results_Nov19_Trimmed.xlsx"), 1) %>% 
  select(Age, IC = Immunocompromised, X = `Num zoster events`, N = `Person- years`,
         M = `Incidence per 1,000 pyrs`, L = `Incidence 95%CI lower`, U = `Incidence 95%CI upper`) %>% 
  mutate(across(M:U, function(x) x / 1000)) %>% 
  filter(Age != "101+") %>% 
  mutate(Age = as.numeric(Age))
  

epi_gp <- read_xlsx(here::here("data", "raw", "Zoster_Burden_Results_Nov19_Trimmed.xlsx"), 2) %>% 
  select(Age, IC = Immunocompromised, X = `Num zoster events`, N = `Person- years`,
         M = `Incidence per 1,000 pyrs`, L = `Incidence 95%CI lower`, U = `Incidence 95%CI upper`) %>% 
  mutate(across(M:U, function(x) x / 1000)) %>% 
  filter(Age != "101+") %>% 
  mutate(Age = as.numeric(Age))


epi_phn <- read_xlsx(here::here("data", "raw", "Zoster_Burden_Results_Nov19_Trimmed.xlsx"), 3) %>% 
  select(Age, IC = Immunocompromised, X = `PHN events`, N = `Total events`,
         M = `Prop events with PHN`, L = `Prop 95% CI Lower`, U = `Prop 95% CI Upper`) %>% 
  filter(Age != "101+") %>% 
  mutate(Age = as.numeric(Age))


epi <- epi_hz %>% select(Age, IC, r_hz = M) %>% 
  left_join(epi_gp %>% select(Age, IC, r_hz_gp = M)) %>% 
  left_join(epi_phn %>% select(Age, IC, p_phn = M)) %>% 
  mutate(
    p_gp = r_hz_gp / r_hz
  ) %>% 
  select(Age, IC, r_hz, p_gp, p_phn)

save(epi_hz, epi_gp, epi_phn, epi, file = here::here("data", "processed_epi", "Epi_HZ_CPRD_23Nov19.rdata"))



### Projection with GPR
logit <- function(p) log(p / (1 - p))
expit <- function(x) 1 / (1 + exp(-x)) 

n_sims <- 2e3


gp_hz <- with(epi %>% filter(IC == 0), { gpkm(Age, log(r_hz)) })
gp_hz$plot()

gp_gp <- with(epi %>% filter(IC == 0), { gpkm(Age, logit(p_gp)) })
gp_gp$plot()

gp_phn <- with(epi %>% filter(IC == 0) %>% filter(Age <= 90), { gpkm(Age, logit(p_phn)) })
gp_phn$plot()

sims0 <- tibble(
  IC = 0,
  Key = rep(1:n_sims, 51),
  Age = rep(50:100, each = n_sims),
  r_hz = exp(c(gp_hz$sample(50:100, n_sims))),
  p_gp = expit(c(gp_gp$sample(50:100, n_sims))),
  p_phn = expit(c(gp_phn$sample(50:100, n_sims)))
)
  
  
gp_hz <- with(epi %>% filter(IC == 1) %>% filter(Age <= 90), { gpkm(Age, log(r_hz)) })
gp_hz$plot()

gp_gp <- with(epi %>% filter(IC == 1) %>% filter(Age <= 90), { gpkm(Age, logit(p_gp)) })
gp_gp$plot()

gp_phn <- with(epi %>% filter(IC == 1) %>% filter(p_phn > 0), { gpkm(Age, (p_phn)) })
gp_phn$plot()


sims1 <- tibble(
  IC = 1,
  Key = rep(1:n_sims, 51),
  Age = rep(50:100, each = n_sims),
  r_hz = exp(c(gp_hz$sample(50:100, n_sims))),
  p_gp = expit(c(gp_gp$sample(50:100, n_sims))),
  p_phn = (c(gp_phn$sample(50:100, n_sims)))
)


Epi_HZ <- bind_rows(sims0, sims1)

save(Epi_HZ, file = here::here("data", "processed_epi", "Epi_HZ_CPRD_GPR.rdata"))

pars_epi <- Epi_HZ
save(pars_epi, file = here::here("pars", "pars_epi_GPR.rdata"))


### Check inputs
theme_set(theme_bw())



g_r_hz <- Epi_HZ %>% 
  ggplot() +
  stat_interval(aes(x = Age, y = r_hz)) +
  geom_pointinterval(data = epi_hz %>% filter(Age <= 90), aes(x = Age, y = M, ymin = L, ymax = U)) +
  scale_colour_brewer() +
  facet_wrap(.~ IC, scales = "free_y", labeller = label_both) +
  scale_y_continuous("Incidence HZ per 1,000", labels = scales::number_format(scale = 1e3)) +
  expand_limits(y = 0)

g_r_hz

g_r_hz_gp <- Epi_HZ %>% 
  ggplot() +
  stat_interval(aes(x = Age, y = p_gp * r_hz)) +
  geom_pointinterval(data = epi_gp %>% filter(Age <= 90), aes(x = Age, y = M, ymin = L, ymax = U)) +
  scale_colour_brewer() +
  facet_wrap(.~ IC, scales = "free_y", labeller = label_both) +
  scale_y_continuous("Incidence HZ, GP-only, per 1,000", labels = scales::number_format(scale = 1e3)) +
  expand_limits(y = 0)

g_r_hz_gp


g_p_phn <- Epi_HZ %>% 
  ggplot() +
  stat_interval(aes(x = Age, y = p_phn)) +
  geom_pointinterval(data = epi_phn %>% filter(Age <= 90), aes(x = Age, y = M, ymin = L, ymax = U)) +
  scale_colour_brewer() +
  facet_wrap(.~ IC, scales = "free_y", labeller = label_both) +
  scale_y_continuous("Postherpetic neuralgia, %", labels = scales::percent) +
  expand_limits(y = 0)

g_p_phn


g_epi <- ggarrange(g_r_hz + theme(legend.position = "none"), 
                   g_r_hz_gp + theme(legend.position = "none"), 
                   g_p_phn + theme(legend.position = "none"), nrow = 3)

g_epi

ggsave(g_epi, filename = here::here("docs", "figs", "processed", "g_epi_gpr_fit.png"), width = 7, height = 10)


