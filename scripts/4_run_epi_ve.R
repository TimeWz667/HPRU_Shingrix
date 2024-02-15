library(tidyverse)
library(tidybayes)

theme_set(theme_bw())

source(here::here("models", "m_dy_hz.R"))



## Eligibility functions -----
scenario_soc <- function(df, p0, yr) find_eligible_default(df, p0, min(yr, 2022))

scenario_full <- find_eligible_default

## Loading parameters -----
load(here::here("pars", "pars_demo.rdata"))


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


pv <- list(
  "Linear" = "ves_linear.rdata",
  "2Step" = "ves_2step.rdata",
  "Linear_D10" = "ves_linear_drop10.rdata",
  "2Step_D10" = "ves_2step_drop10.rdata",
  "Coxian-Erlang" = "ves_ce.rdata"
)

pv <- lapply(pv, function(f) {
  load(here::here("pars", f))
  
  bind_rows(
    pars_ves %>% filter(Vaccine != "Shingrix"),
    ve_nic %>% filter(Type == "Real") %>% select(Age, AgeVac, Vaccine = TypeVac, Protection = VE)
  )
})




## Simulation -----
yss <- list()

keys <- pars_epi %>% pull(Key) %>% unique()

for(k in keys[1:5]) {
  print(k)
  
  pars <- c(pars_demo$England, list(
    Epi = pars_epi %>% filter(Key == k) %>% select(-Key),
    Uptake = pars_uptake,
    VE = pars_ves
  ))
  
  yss[[length(yss) + 1]] <- sim_dy_hz_vac(pars, year1 = 2040, 
                                                  rule_eligible = scenario_soc) %>% 
    mutate(Scenario = "SOC", Key = k, Scenario_VE = "SOC")
  
  for (kve in names(pv)) {
    pars$VE <- pv[[kve]]
    
    yss[[length(yss) + 1]] <- sim_dy_hz_vac(pars, year1 = 2040, 
                                                   rule_eligible = scenario_full) %>% 
      mutate(Scenario = "Full", Key = k, Scenario_VE = kve)
  }
}


yss <- bind_rows(yss)


stats <- yss %>% 
  filter(Age >= 50) %>% 
  mutate(NewUptake = ifelse(is.na(NewUptake), 0, NewUptake)) %>% 
  group_by(Year, Age, Scenario_VE) %>% 
  summarise(
    across(c(N, starts_with("HZ"), NewUptake), mean)
  )


avt <- stats %>% 
  group_by(Year, Scenario_VE) %>% 
  summarise(
    Cases = sum(HZ)
  ) %>% 
  arrange(Year) %>%
  group_by(Scenario_VE) %>% 
  mutate(
    CumCases = cumsum(Cases)
  ) %>% 
  ungroup()




load(here::here("data", "processed_ce", "Cost_Hospitalisation_NIC.rdata"))
load(here::here("data", "processed_ce", "Cost_GP_Gauthier.rdata"))
load(here::here("data", "processed_ce", "QOL_LE.rdata"))


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

### vaccine coverage
vaccine_coverage <- 0.483 
### cost per vaccine dose
cost_vac_per_dose <- 75
### admin cost per vaccine dose (item of service fee for 2018/19, https://www.nhsemployers.org/-/media/Employers/Documents/Primary-care-contracts/V-and-I/201819-Vaccination-and-immunisation-guidance-and-audit-requirements.PDF?la=en&hash=B3DFFE1BE23C5826841A87704FF305964A481A42)
cost_admin_per_dose <- 10
### number of doses
number_courses <- 2

### Vaccine cost per vaccination
cost_vac_pp <- (cost_vac_per_dose + cost_admin_per_dose) * number_courses

stats_ce <- stats %>% 
  left_join(pars_ce) %>% 
  filter(Year >= 2022) %>% 
  mutate(
    dis_ql = 1 / ((1 + discount_rate_effects)^ (Year - 2023)),
    dis_cost = 1 / ((1 + discount_rate_costs)^ (Year - 2023)),
    QL_y2_d = QL_y2 / (1 + discount_rate_effects),
    QL_HZ = QL_y1 + QL_y2,
    QL_HZ_d = QL_y1 + QL_y2_d,
    Q_Life = N * QOL,
    Q_HZ = - HZ * QL_HZ,
    Q_All = Q_Life + Q_HZ,
    Q_Life_d = Q_Life * dis_ql,
    Q_HZ_d = - HZ * QL_HZ_d * dis_ql,
    Q_All_d = Q_Life_d + Q_HZ_d,
    C_Hosp = HZ_Hosp * cost_Hospitalisation_pp_inf,
    C_GP_NonPHN = (HZ_GP - HZ_PHN_GP) * cost_GP_pp_non_PHN_HZ_inf,
    C_GP_PHN = HZ_PHN_GP * cost_GP_pp_PHN_inf,
    C_GP = C_GP_NonPHN + C_GP_PHN,
    C_Vac = cost_vac_pp * NewUptake,
    C_Vac_d = C_Vac * dis_ql,
    C_All = C_Hosp + C_GP + C_Vac,
    C_Hosp_d = C_Hosp * dis_cost,
    C_GP_NonPHN_d = C_GP_NonPHN * dis_cost,
    C_GP_PHN_d = C_GP_PHN * dis_cost,
    C_GP_d = C_GP * dis_cost,
    C_All_d = C_Hosp_d + C_GP_d + C_Vac_d
  )



g_trend <- yss %>% 
  filter(Age >= 50) %>% 
  group_by(Year, Scenario, Scenario_VE) %>% 
  summarise(
    Incidence = sum(HZ) / sum(N)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year + 1, y = Incidence, colour = Scenario_VE)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) + 
  scale_y_continuous("Incidence, 50+, per 100k", labels = scales::number_format(scale = 1e5)) +
  expand_limits(y = 0)

g_avt_n <- avt %>% 
  left_join(avt %>% filter(Scenario_VE == "SOC") %>% select(Year, CumCases0 = CumCases), by = "Year") %>% 
  ggplot() +
  geom_line(aes(x = Year, y = CumCases0 - CumCases, colour = Scenario_VE)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) +
  scale_y_continuous("Cases averted, million", labels = scales::number_format(scale = 1e-6))


g_avt_p <- avt %>% 
  left_join(avt %>% filter(Scenario_VE == "SOC") %>% select(Year, CumCases0 = CumCases), by = "Year") %>% 
  ggplot() +
  geom_line(aes(x = Year, y = 1 - CumCases / CumCases0, colour = Scenario_VE)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) +
  scale_y_continuous("Cases averted, %", labels = scales::percent)



g_q_all <- stats_ce %>% 
  group_by(Year, Scenario_VE) %>% 
  summarise(
    Q_All_d = sum(Q_All_d)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year + 1, y = Q_All_d, colour = Scenario_VE)) +
  scale_y_continuous("QALY, million, discounted", labels = scales::number_format(scale = 1e-6)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) +
  expand_limits(y = 0)


g_c_all <- stats_ce %>% 
  group_by(Year, Scenario_VE) %>% 
  summarise(
    C_All_d = sum(C_All_d)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year + 1, y = C_All_d, colour = Scenario_VE)) +
  scale_y_continuous("Medical + Vaccine costs, million GBP, discounted", labels = scales::number_format(scale = 1e-6)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) +
  expand_limits(y = 0)


g_c_med <- stats_ce %>% 
  group_by(Year, Scenario_VE) %>% 
  summarise(
    C_Med_d = sum(C_All_d - C_Vac_d)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year + 1, y = C_Med_d, colour = Scenario_VE)) +
  scale_y_continuous("Medical costs, million GBP, discounted", labels = scales::number_format(scale = 1e-6)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) +
  expand_limits(y = 0)


stats_ce %>% 
  group_by(Year, Scenario_VE) %>% 
  summarise(
    Q_Life_d = sum(Q_Life_d),
    Q_All_d = sum(Q_All_d),
    C_All_d = sum(C_All_d),
    C_Vac_d = sum(C_Vac_d),
    C_Med_d = C_All_d - C_Vac_d
  ) %>% filter(Year == 2040)




g_trend
g_avt_n
g_avt_p
g_q_all
g_c_all
g_c_med

