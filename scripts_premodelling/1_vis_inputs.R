library(tidyverse)
library(tidybayes)

theme_set(theme_bw())


folder_outputs <- function(x) here::here("docs", "figs", "processed", x)

set.seed(11667)


N_Iter <- 1000


## Population / Background Mortality -----

load(here::here("data", "processed_demography", "Population_IC_Ons_2015.rdata"))


g <- Pop %>% 
  filter(age >= 18) %>% 
  ggplot(aes(x = age, y = Pop)) +
  geom_line() +
  geom_point() +
  scale_y_continuous("Population size, thousands", labels = scales::number_format(scale = 1e-3)) +
  expand_limits(y=0) +
  labs(title = "Population size", subtitle = "IC, ONS, 2015")
g
ggsave(g, filename = folder_outputs("g_Pop_IC_Ons_2015.png"), width = 6, height = 4)


g <- Pop %>% 
  filter(age >= 18) %>% 
  ggplot(aes(x = age, y = Background_mortality)) +
  geom_line() +
  geom_point() +
  scale_y_log10("Background mortality, percent", labels = scales::percent) +
  labs(title = "Background mortality", subtitle = "IC, ONS, 2015")
g
ggsave(g, filename = folder_outputs("g_DeathBg_IC_Ons_2015.png"), width = 6, height = 4)


load(here::here("data", "processed_demography", "Population_NIC_Ons_2015.rdata"))
tag <- "NIC, ONS, 2015"

g <- Pop %>% 
  filter(Age >= 18) %>% 
  ggplot(aes(x = Age, y = Pop)) +
  geom_line() +
  geom_point() +
  scale_y_continuous("Population size, thousands", labels = scales::number_format(scale = 1e-3)) +
  expand_limits(y=0) +
  labs(title = "Population size", subtitle = tag)
g
ggsave(g, filename = folder_outputs("g_Pop_NIC_Ons_2015.png"), width = 6, height = 4)


g <- Pop %>% 
  filter(Age >= 18) %>% 
  ggplot(aes(x = Age, y = Background_mortality)) +
  geom_line() +
  geom_point() +
  scale_y_log10("Background mortality, percent", labels = scales::percent) +
  labs(title = "Background mortality", subtitle = tag)
g
ggsave(g, filename = folder_outputs("g_DeathBg_NIC_Ons_2015.png"), width = 6, height = 4)


## Epidemiology -----
local({
  load(here::here("data", "processed_epi", "Epi_HZ_IC_CPRD.rdata"))

  tag <- "IC CPRD 16/04/2019"
  
  
  g <- Incidence_HZ %>% 
    filter(Key <= N_Iter) %>% 
    filter(Age >= 18) %>% 
    ggplot(aes(x = Age, y = Incidence_HZ)) +
    stat_lineribbon(.width = c(.99, .95, .8, .5), color = "#08519C") +
    scale_y_continuous("Incidence HZ, %", labels = scales::percent) +
    expand_limits(y = 0) +
    labs(title = "Incidence HZ, all cases", subtitle = tag) +
    scale_fill_brewer()
  
  g
  ggsave(g, filename = folder_outputs("g_Incidence_HZ_All_IC_CPRD.png"), width = 6, height = 4)
  
  
  g <- Incidence_HZ_GP_only %>% 
    filter(Key <= N_Iter) %>% 
    filter(Age >= 18) %>% 
    ggplot(aes(x = Age, y = Incidence_HZ_GP_only)) +
    stat_lineribbon(.width = c(.99, .95, .8, .5), color = "#08519C") +
    scale_y_continuous("Incidence HZ, %", labels = scales::percent) +
    expand_limits(y = 0) +
    labs(title = "Incidence HZ, GP cases", subtitle = tag) +
    scale_fill_brewer()
  
  g
  ggsave(g, filename = folder_outputs("g_Incidence_HZ_GP_IC_CPRD.png"), width = 6, height = 4)
  
  
  g <- Mortality_HZ %>% 
    filter(Key <= N_Iter) %>% 
    filter(Age >= 18) %>% 
    ggplot(aes(x = Age, y = Death_HZ)) +
    stat_lineribbon(.width = c(.99, .95, .8, .5), color = "#08519C") +
    scale_y_continuous("Mortality HZ, %", labels = scales::percent) +
    expand_limits(y = 0) +
    labs(title = "Mortality HZ", subtitle = tag) +
    scale_fill_brewer()
  
  ggsave(g, filename = folder_outputs("g_Mortality_HZ_IC_CPRD.png"), width = 6, height = 4)
  
  
  g <- P_PHN %>% 
    filter(Key <= N_Iter) %>% 
    filter(Age >= 18) %>% 
    ggplot(aes(x = Age, y = p_phn)) +
    stat_lineribbon(.width = c(.99, .95, .8, .5), color = "#08519C") +
    scale_y_continuous("Proportion PHN | HZ, %", labels = scales::percent) +
    expand_limits(y = 0) +
    labs(title = "Proportion PHN per HZ", subtitle = tag) +
    scale_fill_brewer()
  
  ggsave(g, filename = folder_outputs("g_P_PHN_IC_CPRD.png"), width = 6, height = 4)
  
})


local({
  load(here::here("data", "processed_epi", "Epi_HZ_NIC_CPRD.rdata"))
  tag <- "NIC CPRD 16/04/2019"
  
  
  g <- Incidence_HZ %>% 
    filter(Key <= N_Iter) %>% 
    filter(Age >= 18) %>% 
    ggplot(aes(x = Age, y = Incidence_HZ)) +
    stat_lineribbon(aes(y = Incidence_HZ), .width = c(.99, .95, .8, .5), color = "#08519C") +
    scale_y_continuous("Incidence HZ, %", labels = scales::percent) +
    expand_limits(y = 0) +
    labs(title = "Incidence HZ, all cases", subtitle = tag) +
    scale_fill_brewer()
  
  ggsave(g, filename = folder_outputs("g_Incidence_HZ_All_NIC_CPRD.png"), width = 6, height = 4)
  
  
  g <- Incidence_HZ_GP_only %>% 
    filter(Key <= N_Iter) %>% 
    filter(Age >= 18) %>% 
    ggplot(aes(x = Age, y = Incidence_HZ_GP_only)) +
    stat_lineribbon(aes(y = Incidence_HZ_GP_only), .width = c(.99, .95, .8, .5), color = "#08519C") +
    scale_y_continuous("Incidence HZ, %", labels = scales::percent) +
    expand_limits(y = 0) +
    labs(title = "Incidence HZ, GP cases", subtitle = tag) +
    scale_fill_brewer()
  
  ggsave(g, filename = folder_outputs("g_Incidence_HZ_GP_NIC_CPRD.png"), width = 6, height = 4)
  
  
  
  g <- Mortality_HZ %>% 
    filter(Key <= N_Iter) %>% 
    filter(Age >= 18) %>% 
    ggplot(aes(x = Age, y = Death_HZ)) +
    stat_lineribbon(aes(y = Death_HZ), .width = c(.99, .95, .8, .5), color = "#08519C") +
    scale_y_continuous("Mortality HZ, %", labels = scales::percent) +
    expand_limits(y = 0) +
    labs(title = "Mortality HZ", subtitle = tag) +
    scale_fill_brewer()
  
  ggsave(g, filename = folder_outputs("g_Mortality_HZ_NIC_CPRD.png"), width = 6, height = 4)
  
  
  g <- P_PHN %>% 
    filter(Key <= N_Iter) %>% 
    filter(Age >= 18) %>% 
    ggplot(aes(x = Age, y = p_phn)) +
    stat_lineribbon(width = c(.99, .95, .8, .5), color = "#08519C") +
    scale_y_continuous("Proportion PHN | HZ, %", labels = scales::percent) +
    expand_limits(y = 0) +
    labs(title = "Proportion PHN per HZ", subtitle = tag) +
    scale_fill_brewer()
  
  ggsave(g, filename = folder_outputs("g_P_PHN_NIC_CPRD.png"), width = 6, height = 4)
  
})


## Hospitalisation -----

local({
  load(here::here("data", "processed_epi", "Epi_HZ_IC_CPRD.rdata"))
  tag <- "IC CPRD 16/04/2019"
  
  
  g <- Hospitalisation_HZ %>% 
    filter(Key <= N_Iter) %>% 
    filter(Age >= 18) %>% 
    ggplot(aes(x = Age, y = r_hospitalisation_hz)) +
    stat_lineribbon(.width = c(.99, .95, .8, .5), color = "#08519C") +
    scale_y_continuous("Hospitalisation HZ, per 100 000", labels = scales::number_format(scale = 1e5)) +
    expand_limits(y = 0) +
    labs(title = "Hospitalisation HZ", subtitle = tag) +
    scale_fill_brewer()
  
  g
  ggsave(g, filename = folder_outputs("g_Hospitalisation_HZ_IC_CPRD.png"), width = 6, height = 4)
})


local({
  load(here::here("data", "processed_epi", "Epi_HZ_NIC_CPRD.rdata"))
  tag <- "NIC CPRD 16/04/2019"
  
  
  g <- Hospitalisation_HZ %>% 
    filter(Key <= N_Iter) %>% 
    filter(Age >= 18) %>% 
    ggplot(aes(x = Age, y = r_hospitalisation_hz)) +
    stat_lineribbon(.width = c(.99, .95, .8, .5), color = "#08519C") +
    scale_y_continuous("Hospitalisation HZ, per 100 000", labels = scales::number_format(scale = 1e5)) +
    expand_limits(y = 0) +
    labs(title = "Hospitalisation HZ", subtitle = tag) +
    scale_fill_brewer()
  
  g
  ggsave(g, filename = folder_outputs("g_Hospitalisation_HZ_NIC_CPRD.png"), width = 6, height = 4)
})


## VE -----
local({
  load(here::here("data", "processed_vaccine", "VE_Shingrix_IC.rdata"))
  
  tag <- "IC 31/07/2018"
  
  ves <- bind_rows(lapply(c(20, 40, 60, 70, 80), function(va) {
    bind_rows(lapply(1:N_Iter, function(k) {
      sample_ve(VE, vaccination_age = va) %>% mutate(VA = va, Key = k, duration = age - VA) %>% 
        filter(duration >= 0 & duration <= 15)
    }))
  }))
  
  g <- ves %>% 
    ggplot(aes(x = duration, y = VE)) +
    stat_lineribbon(aes(y = VE), .width = c(.99, .95, .8, .5), color = "#08519C") +
    scale_y_continuous("Vaccine efficacy, %", labels = scales::percent) +
    scale_x_continuous("Years since vaccination") +
    expand_limits(y = 0:1) +
    facet_grid(.~VA)  +
    labs(title = "Vaccine efficacy, Shingrix", subtitle = tag) +
    scale_fill_brewer()
  
  g
  ggsave(g, filename = folder_outputs("g_VE_IC.png"), width = 8, height = 4)
    
})


local({
  load(here::here("data", "processed_vaccine", "VE_Shingrix_NIC.rdata"))
  
  tag <- "NIC 31/07/2018"
  
  ves <- bind_rows(lapply(c(20, 40, 60, 70, 80), function(va) {
    bind_rows(lapply(1:N_Iter, function(k) {
      sample_ve(VE, vaccination_age = va) %>% mutate(VA = va, Key = k, duration = age - VA) %>% 
        filter(duration >= 0 & duration <= 15)
    }))
  }))
  
  g <- ves %>% 
    ggplot(aes(x = duration, y = VE)) +
    stat_lineribbon(aes(y = VE), .width = c(.99, .95, .8, .5), color = "#08519C") +
    scale_y_continuous("Vaccine efficacy, %", labels = scales::percent) +
    scale_x_continuous("Years since vaccination") +
    expand_limits(y = 0:1) +
    facet_grid(.~VA)  +
    labs(title = "Vaccine efficacy, Shingrix", subtitle = tag) +
    scale_fill_brewer()
  
  g
  ggsave(g, filename = folder_outputs("g_VE_NIC.png"), width = 8, height = 4)
  
})

## QOL -----

local({
  load(here::here("data", "processed_ce", "QOL_LE.rdata"))
  load(here::here("data", "processed_vaccine", "VE_Shingrix_NIC.rdata"))
  
  tag <- "LE 11/07/2018"
  
  g <- QL %>% 
    filter(Key <= N_Iter) %>% 
    filter(Age >= 18) %>% 
    ggplot(aes(x = Age, y = QL_y1)) +
    stat_lineribbon(.width = c(.99, .95, .8, .5), color = "#08519C") +
    scale_y_continuous("QALY loss, first year with HZ") +
    expand_limits(y = 0) +
    labs(title = "QALY loss, first year with HZ", subtitle = tag) +
    scale_fill_brewer()
  
  g
  ggsave(g, filename = folder_outputs("g_QL_yr1.png"), width = 8, height = 4)
  
  g <- QL %>% 
    filter(Key <= N_Iter) %>% 
    filter(Age >= 18) %>% 
    ggplot(aes(x = Age, y = QL_y2)) +
    stat_lineribbon(.width = c(.99, .95, .8, .5), color = "#08519C") +
    scale_y_continuous("QALY loss, second year with HZ") +
    expand_limits(y = 0) +
    labs(title = "QALY loss, second year with HZ", subtitle = tag) +
    scale_fill_brewer()
  
  g
  ggsave(g, filename = folder_outputs("g_QL_yr2.png"), width = 8, height = 4)
  
})


## Costs -----

local({
  load(here::here("data", "processed_ce", "Cost_Hospitalisation_IC.rdata"))
  tag = "IC"

  g <- Cost_Hospitalisation_HZ %>% 
    filter(Key <= N_Iter) %>% 
    filter(age >= 18) %>% 
    ggplot(aes(x = age, y = Hospitalisation_costs_pp_HZ )) +
    stat_lineribbon(.width = c(.99, .95, .8, .5), color = "#08519C") +
    scale_y_continuous("Cost per patient, GBP") +
    expand_limits(y = 0) +
    labs(title = "HZ-related hospitalisation cost per patient, GBP", subtitle = tag) +
    scale_fill_brewer()
  
  ggsave(g, filename = folder_outputs("g_Cost_hospitalisation_IC.png"), width = 8, height = 4)
})


local({
  load(here::here("data", "processed_ce", "Cost_Hospitalisation_NIC.rdata"))
  tag = "NIC"
  
  g <- Cost_Hospitalisation_HZ %>% 
    filter(Key <= N_Iter) %>% 
    filter(age >= 18) %>% 
    ggplot(aes(x = age, y = Hospitalisation_costs_pp_HZ )) +
    stat_lineribbon(.width = c(.99, .95, .8, .5), color = "#08519C") +
    scale_y_continuous("Cost per patient, GBP") +
    expand_limits(y = 0) +
    labs(title = "HZ-related hospitalisation cost per patient, GBP", subtitle = tag) +
    scale_fill_brewer()
  
  ggsave(g, filename = folder_outputs("g_Cost_hospitalisation_NIC.png"), width = 8, height = 4)
})



