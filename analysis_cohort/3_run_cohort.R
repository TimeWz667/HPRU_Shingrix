library(tidyverse)
library(tidybayes)

theme_set(theme_bw())

source(here::here("models", "m_dy_hz.R"))


## Parameters: Demography -----
load(here::here("pars", "pars_demo.rdata"))



## Parameters: CE -----
load(here::here("data", "processed_ce", "Cost_Hospitalisation_NIC.rdata"))
load(here::here("data", "processed_ce", "Cost_GP_Gauthier.rdata"))
load(here::here("data", "processed_ce", "QOL_LE.rdata"))
cost_vac <- read_csv(here::here("data", "processed_ce", "Cost_Vac.csv"))

pars_ce <- QL %>% 
  select(- Key) %>%
  group_by(Age) %>% 
  summarise(across(everything(), mean)) %>% 
  left_join(Cost_Hospitalisation_HZ %>% select(- Key)) %>% 
  bind_cols(Cost_GP %>% select(- Key) %>% summarise(across(everything(), mean)))

### discount rate costs
discount_rate_costs <- 0.035
### discount rate effects
discount_rate_effects <- 0.035


## Parameters: Epidemiology -----
pars_epi <- local({
  p_mor_hz <- local({
    load(here::here("data", "processed_epi", "Epi_HZ_NIC_CPRD_bind.rdata"))
    Epi_HZ %>% 
      group_by(Age) %>% 
      summarise(p_mor_hz = mean(r_mor_hz) / mean(r_inc_hz))
  })
  load(here::here("pars", "pars_epi_gpr.rdata"))
  
  pars_epi %>% 
    filter(IC == 0) %>% 
    left_join(p_mor_hz) %>% 
    select(-IC) %>% 
    arrange(Key, Age)
})


## Parameters: Vaccination -----
pars_ve <- local({
  load(here::here("pars", "ves_ce.rdata"))
  
  ve_nic %>% filter(Type == "Real") %>% select(Age, AgeVac, Vaccine = TypeVac, Protection = VE)
})


## Simulation -----
keys <- pars_epi %>% pull(Key) %>% unique()
keys <- keys[1:100]

yss <- list()


pb <- txtProgressBar(min = 1, max = length(keys), style = 3,  width = 50, char = "=") 

for(k in keys) {
  pars <- c(pars_demo$England, list(
    Epi = pars_epi %>% filter(Key == k) %>% select(-Key),
    VE = pars_ve
  ))
  
  for (age0 in 50:95) {
    yss[[length(yss) + 1]] <- sim_cohort_hz_vac(pars, age0 = age0, year0 = 2024)  %>% 
      mutate(Key = k, Scenario = glue::as_glue("Vac_") + age0)
  }
  
  setTxtProgressBar(pb, k)
}

yss <- bind_rows(yss)
results <- summarise_cohort_hz(yss, pars_ce, cost_vac, dis_effects = discount_rate_effects, dis_costs = discount_rate_costs)

results$Stats %>% write_csv(here::here("outputs", "tabs", "cohort_vac_stats.csv"))
results$CE %>% filter(Arm == "SOC") %>% write_csv(here::here("outputs", "tabs", "cohort_vac_ce.csv"))




g_icer <- results$CE %>% 
  group_by(Scenario, Arm) %>% 
  summarise_all(mean) %>% 
  extract(Scenario, c("Age0", "Age1"), "ReVac_(\\d+):(\\d+)", remove = F, convert = T) %>% 
  filter(Age0 %in% c(60, 65, 70, 75)) %>% 
  filter(Arm != "SOC") %>% 
  ggplot() +
  geom_line(aes(x = Age1, y = ICER, colour = Arm)) +
  geom_vline(aes(xintercept = Age0), linetype = 2) +
  geom_text(aes(x = Age0, y = 0), label = "First RZV", angle = 90, hjust = 0, vjust = 1.2) +
  scale_x_continuous("Age for RZV revaccination") +
  facet_grid(.~Age0) +
  expand_limits(y = 0)

g_icer

g_e <- results$CE %>% 
  group_by(Scenario, Arm) %>% 
  summarise_all(mean) %>% 
  extract(Scenario, c("Age0", "Age1"), "ReVac_(\\d+):(\\d+)", remove = F, convert = T) %>% 
  filter(Age0 %in% c(60, 65, 70, 75)) %>% 
  filter(Arm != "SOC") %>% 
  ggplot() +
  geom_line(aes(x = Age1, y = dE, colour = Arm)) +
  geom_vline(aes(xintercept = Age0), linetype = 2) +
  geom_text(aes(x = Age0, y = 0), label = "First RZV", angle = 90, hjust = 0, vjust = 1.2) +
  scale_x_continuous("Age for RZV revaccination") +
  scale_y_continuous("QALY gained") +
  facet_grid(.~Age0) +
  expand_limits(y = 0)

g_e


g_c <- results$CE %>% 
  group_by(Scenario, Arm) %>% 
  summarise_all(mean) %>% 
  extract(Scenario, c("Age0", "Age1"), "ReVac_(\\d+):(\\d+)", remove = F, convert = T) %>% 
  filter(Age0 %in% c(60, 65, 70, 75)) %>% 
  filter(Arm != "SOC") %>% 
  ggplot() +
  geom_line(aes(x = Age1, y = dC, colour = Arm)) +
  geom_vline(aes(xintercept = Age0), linetype = 2) +
  geom_text(aes(x = Age0, y = 0), label = "First RZV", angle = 90, hjust = 0, vjust = 1.2) +
  scale_x_continuous("Age for RZV revaccination") +
  scale_y_continuous("Incremental cost, million GBP", 
                     labels = scales::number_format(scale = 1e-6)) +
  facet_grid(.~Age0) +
  expand_limits(y = 0)

g_c

ggsave(g_e, filename = here::here("outputs", "figs", "g_vac_de.png"), width = 8, height = 5)
ggsave(g_c, filename = here::here("outputs", "figs", "g_vac_dc.png"), width = 8, height = 5)
ggsave(g_icer, filename = here::here("outputs", "figs", "g_vac_icer.png"), width = 8, height = 5)
