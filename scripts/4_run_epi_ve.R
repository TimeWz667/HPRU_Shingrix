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
  group_by(age) %>% 
  summarise(across(everything(), mean)) %>% 
  left_join(Cost_Hospitalisation_HZ %>% select(- Key)) %>% 
  bind_cols(Cost_GP %>% select(- Key) %>% summarise(across(everything(), mean))) %>% 
  rename(Age = age)

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
### Eligibility functions -----
scenario_soc <- function(df, p0, yr) find_eligible_default(df, p0, min(yr, 2022))
scenario_full <- find_eligible_default


pars_uptake <- local({
  load(here::here("data", "fitted_coverage.rdata"))
  pred1$pars
})


pars_ves <- local({
  load(here::here("pars", "ves.rdata"))
  
  ves %>% 
    filter(!IC) %>% 
    select(Age, AgeVac, Vaccine = TypeVac, Protection = VE)
})


## Build up scenarios -----
scenarios <- list(
  "Linear" = "ves_linear.rdata",
  "2Step" = "ves_2step.rdata",
  "Linear_D10" = "ves_linear_drop10.rdata",
  "2Step_D10" = "ves_2step_drop10.rdata",
  "Coxian-Erlang" = "ves_ce.rdata"
)

scenarios <- lapply(scenarios, function(f) {
  load(here::here("pars", f))
  
  list(
    Pars = bind_rows(
      pars_ves %>% filter(Vaccine != "Shingrix"),
      ve_nic %>% filter(Type == "Real") %>% select(Age, AgeVac, Vaccine = TypeVac, Protection = VE),
    ),
    Eligible = scenario_full
  )
})


scenarios[["SOC"]] <- list(
  Pars = pars_ves %>% filter(Vaccine != "Shingrix"),
  Eligible = scenario_soc
)


## Simulation -----
keys <- pars_epi %>% pull(Key) %>% unique()


yss <- lapply(scenarios, function(x) list())

i <- 1

for(k in keys[1:200]) {
  print(k)
  
  pars <- c(pars_demo$England, list(
    Epi = pars_epi %>% filter(Key == k) %>% select(-Key),
    Uptake = pars_uptake
  ))
  
  for (ksc in names(scenarios)) {
    pars$VE <- scenarios[[ksc]]$Pars
    yss[[ksc]][[i]] <- sim_dy_hz_vac(pars, year1 = 2040, 
                                rule_eligible = scenarios[[ksc]]$Eligible) %>% 
      mutate(Scenario = ksc, Key = k)
  }
  i <- i + 1
}

yss <- bind_rows(lapply(yss, bind_rows))

results <- summarise_dy_hz(yss, pars_ce, cost_vac, soc = "SOC", age0 = 50)


## Visualise results -----

g_trend <- results$Yss %>% 
  group_by(Year, Scenario) %>% 
  summarise(
    Incidence = sum(HZ) / sum(N)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year + 1, y = Incidence, colour = Scenario)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) + 
  scale_y_continuous("Incidence, 50+, per 100k", labels = scales::number_format(scale = 1e5)) +
  scale_x_continuous("Year", breaks = c(2023, 2028, 2033, 2040)) +
  expand_limits(y = 0)


g_avt_n <- results$Avt %>% 
  ggplot() +
  geom_line(aes(x = Year + 1, y = CumCases0 - CumCases, colour = Scenario)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) +
  scale_y_continuous("Cases averted, million", labels = scales::number_format(scale = 1e-6)) +
  scale_x_continuous("Year", breaks = c(2023, 2028, 2033, 2040))


g_avt_p <- results$Avt %>% 
  ggplot() +
  geom_line(aes(x = Year + 1, y = 1 - CumCases / CumCases0, colour = Scenario)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) +
  scale_x_continuous("Year", breaks = c(2023, 2028, 2033, 2040)) +
  scale_y_continuous("Cases averted, %", labels = scales::percent)



g_q_all <- results$Stats %>% 
  ggplot() +
  geom_line(aes(x = Year + 1, y = Q_All_d, colour = Scenario)) +
  scale_y_continuous("QALY, million, discounted", labels = scales::number_format(scale = 1e-6)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) +
  scale_x_continuous("Year", breaks = c(2023, 2028, 2033, 2040)) +
  expand_limits(y = 0)


g_c_all <- results$Stats %>% 
  ggplot() +
  geom_line(aes(x = Year + 1, y = C_All_d, colour = Scenario)) +
  scale_y_continuous("Medical + Vaccine costs, million GBP, discounted", labels = scales::number_format(scale = 1e-6)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) +
  scale_x_continuous("Year", breaks = c(2023, 2028, 2033, 2040)) +
  expand_limits(y = 0)


g_c_med <-results$Stats %>% 
  ggplot() +
  geom_line(aes(x = Year + 1, y = C_Med_d, colour = Scenario)) +
  scale_y_continuous("Medical costs, million GBP, discounted", labels = scales::number_format(scale = 1e-6)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) +
  scale_x_continuous("Year", breaks = c(2023, 2028, 2033, 2040)) +
  expand_limits(y = 0)


g_dq <- results$Avt %>% 
  ggplot() +
  geom_line(aes(x = Year + 1, y = CumQ - CumQ0, colour = Scenario)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) +
  scale_y_continuous("Incremental effectiveness, thousand QALY", labels = scales::number_format(scale = 1e-3)) +
  scale_x_continuous("Year", breaks = c(2023, 2028, 2033, 2040))


g_dc <- results$Avt %>% 
  ggplot() +
  geom_line(aes(x = Year + 1, y = CumC - CumC0, colour = Scenario)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) +
  scale_y_continuous("Incremental costs, thousand GBP", labels = scales::number_format(scale = 1e-3)) +
  scale_x_continuous("Year", breaks = c(2023, 2028, 2033, 2040))


g_icer <- results$Avt %>% 
  filter(Year >= 2023) %>% 
  ggplot() +
  geom_line(aes(x = Year + 1, y = (CumC - CumC0) / (CumQ - CumQ0), colour = Scenario)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_hline(yintercept = 3e4, linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) +
  scale_y_continuous("Incremental Cost-Effectiveness Ratio,\n thousand GBP per QALY", labels = scales::number_format(scale = 1e-3)) +
  scale_x_continuous("Year", breaks = c(2023, 2028, 2033, 2040)) +
  expand_limits(y = 0)


g_trend
g_avt_n
g_avt_p
g_q_all
g_c_all
g_c_med
g_dc
g_dq
g_icer


ggsave(g_trend, filename = here::here("results", "figs", "g_sens_trend.png"), width = 7, height = 5)
ggsave(g_avt_n, filename = here::here("results", "figs", "g_sens_avtn.png"), width = 7, height = 5)
ggsave(g_avt_p, filename = here::here("results", "figs", "g_sens_avtp.png"), width = 7, height = 5)
ggsave(g_q_all, filename = here::here("results", "figs", "g_sens_q_all.png"), width = 7, height = 5)
ggsave(g_c_all, filename = here::here("results", "figs", "g_sens_c_all.png"), width = 7, height = 5)
ggsave(g_c_med, filename = here::here("results", "figs", "g_sens_c_med.png"), width = 7, height = 5)

ggsave(g_dc, filename = here::here("results", "figs", "g_sens_dc.png"), width = 7, height = 5)
ggsave(g_dq, filename = here::here("results", "figs", "g_sens_dq.png"), width = 7, height = 5)
ggsave(g_icer, filename = here::here("results", "figs", "g_sens_icer.png"), width = 7, height = 5)


