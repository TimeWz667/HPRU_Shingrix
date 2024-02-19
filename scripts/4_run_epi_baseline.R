library(tidyverse)
library(tidybayes)

theme_set(theme_bw())

source(here::here("models", "m_dy_hz.R"))


## Eligibility functions -----
scenario_soc <- function(df, p0, yr) find_eligible_default(df, p0, min(yr, 2022))

scenario_p65 <- function(df, p0, yr) find_eligible_default(df, p0, min(yr, 2027))

scenario_full <- find_eligible_default

scenario_2028_90 <- function(df, p0, yr) {
  if (yr < 2028) {
    df <- find_eligible_default(df, p0, yr)
  } else {
    df <- find_eligible_default(df, p0, yr, cap = 90)
  }
  return(df)
}

scenario_2033_90 <- function(df, p0, yr) {
  if (yr < 2033) {
    df <- find_eligible_default(df, p0, yr)
  } else {
    df <- find_eligible_default(df, p0, yr, cap = 90)
  }
  return(df)
}


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


## Simulation -----
yss_soc <- list()
yss_p1 <- list()
yss_p2 <- list()
yss_p1_90 <- list()
yss_p2_90 <- list()


keys <- pars_epi %>% pull(Key) %>% unique()

for(k in keys[1:50]) {
  print(k)
  
  pars <- c(pars_demo$England, list(
    Epi = pars_epi %>% filter(Key == k) %>% select(-Key),
    Uptake = pars_uptake,
    VE = pars_ves
  ))
  
  yss_soc[[length(yss_soc) + 1]] <- sim_dy_hz_vac(pars, year1 = 2040, 
                                                  rule_eligible = scenario_soc) %>% mutate(Scenario = "SOC", Key = k)
  yss_p1[[length(yss_p1) + 1]] <- sim_dy_hz_vac(pars, year1 = 2040, 
                                                rule_eligible = scenario_p65) %>% mutate(Scenario = "To 65 yr", Key = k)
  yss_p2[[length(yss_p2) + 1]] <- sim_dy_hz_vac(pars, year1 = 2040, 
                                                rule_eligible = scenario_full) %>% mutate(Scenario = "Full", Key = k)
  yss_p1_90[[length(yss_p1_90) + 1]] <- sim_dy_hz_vac(pars, year1 = 2040, 
                                                      rule_eligible = scenario_2028_90) %>% mutate(Scenario = "To 90 yr at 2028", Key = k)
  yss_p2_90[[length(yss_p2_90) + 1]] <- sim_dy_hz_vac(pars, year1 = 2040, 
                                                      rule_eligible = scenario_2033_90) %>% mutate(Scenario = "To 90 yr at 2033", Key = k)
}


save(yss_soc, file = here::here("outputs", "temp", "yss_soc.rdata"))
save(yss_p1, file = here::here("outputs", "temp", "yss_p1.rdata"))
save(yss_p2, file = here::here("outputs", "temp", "yss_p2.rdata"))
save(yss_p1_90, file = here::here("outputs", "temp", "yss_p1_90.rdata"))
save(yss_p2_90, file = here::here("outputs", "temp", "yss_p2_90.rdata"))




yss <- bind_rows(c(yss_soc, yss_p1, yss_p2, yss_p1_90, yss_p2_90))


g_cov <- yss %>% 
  filter(Age >= 50) %>% 
  group_by(Year, Scenario) %>% 
  summarise(
    Coverage = weighted.mean((Vaccine != "None"), w = N)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year + 1, y = Coverage, colour = Scenario)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) + 
  scale_y_continuous("Coverage, 50+, %", labels = scales::percent) +
  expand_limits(y = 0)


g_trend <- yss %>% 
  filter(Age >= 50) %>% 
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
  expand_limits(y = 0)

ggsave(g_cov, filename = here::here("outputs", "figs", "g_epi_coverage.png"), width = 6.5, height = 4)
ggsave(g_trend, filename = here::here("outputs", "figs", "g_epi_trend.png"), width = 6.5, height = 4)



stats <- yss %>% 
  filter(Age >= 50) %>% 
  mutate(NewUptake = ifelse(is.na(NewUptake), 0, NewUptake)) %>% 
  group_by(Year, Age, Scenario) %>% 
  summarise(
    across(c(N, starts_with("HZ"), NewUptake), mean)
  )

stats %>% 
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
  expand_limits(y = 0)


avt <- stats %>% 
  group_by(Year, Scenario) %>% 
  summarise(
    Cases = sum(HZ)
  ) %>% 
  arrange(Year) %>%
  group_by(Scenario) %>% 
  mutate(
    CumCases = cumsum(Cases)
  ) %>% 
  ungroup()

g_avt_n <- avt %>% 
  left_join(avt %>% filter(Scenario == "SOC") %>% select(Year, CumCases0 = CumCases), by = "Year") %>% 
  ggplot() +
  geom_line(aes(x = Year, y = CumCases0 - CumCases, colour = Scenario)) +
  scale_y_continuous("Cases averted, million", labels = scales::number_format(scale = 1e-6))


g_avt_p <- avt %>% 
  left_join(avt %>% filter(Scenario == "SOC") %>% select(Year, CumCases0 = CumCases), by = "Year") %>% 
  ggplot() +
  geom_line(aes(x = Year, y = 1 - CumCases / CumCases0, colour = Scenario)) +
  scale_y_continuous("Cases averted, %", labels = scales::percent)





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


g_q_all <- stats_ce %>% 
  group_by(Year, Scenario) %>% 
  summarise(
    Q_All_d = sum(Q_All_d)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year + 1, y = Q_All_d, colour = Scenario)) +
  scale_y_continuous("QALY, million, discounted", labels = scales::number_format(scale = 1e-6)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) +
  expand_limits(y = 0)

g_c_all <- stats_ce %>% 
  group_by(Year, Scenario) %>% 
  summarise(
    C_All_d = sum(C_All_d)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year + 1, y = C_All_d, colour = Scenario)) +
  scale_y_continuous("Medical + Vaccine costs, million GBP, discounted", labels = scales::number_format(scale = 1e-6)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) +
  expand_limits(y = 0)


g_c_vac <- stats_ce %>% 
  group_by(Year, Scenario) %>% 
  summarise(
    C_Vac_d = sum(C_Vac_d)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year + 1, y = C_Vac_d, colour = Scenario)) +
  scale_y_continuous("Vaccine costs, million GBP, discounted", labels = scales::number_format(scale = 1e-6)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) +
  expand_limits(y = c(0, 1e8))

ggsave(g_avt_n, filename = here::here("outputs", "figs", "g_epi_avt_n.png"), width = 6.5, height = 4)
ggsave(g_avt_p, filename = here::here("outputs", "figs", "g_epi_avt_p.png"), width = 6.5, height = 4)
ggsave(g_c_vac, filename = here::here("outputs", "figs", "g_epi_c_vac.png"), width = 6.5, height = 4)
ggsave(g_c_all, filename = here::here("outputs", "figs", "g_epi_c_all.png"), width = 6.5, height = 4)
ggsave(g_q_all, filename = here::here("outputs", "figs", "g_epi_q_all.png"), width = 6.5, height = 4)



