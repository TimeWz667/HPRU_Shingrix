library(tidyverse)
library(tidybayes)

theme_set(theme_bw())

source(here::here("models", "m_dy_hz.R"))


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
  
  load(here::here("pars", "ves_ce.rdata"))
  
  bind_rows(
    ves %>% 
      filter(!IC) %>% 
      filter(TypeVac != "Shingrix") %>% 
      select(Age, AgeVac, Vaccine = TypeVac, Protection = VE),
    ve_nic %>% filter(Type == "Real") %>% select(Age, AgeVac, Vaccine = TypeVac, Protection = VE)
  )
})


k = 5

pars <- c(pars_demo$England, list(
  Epi = pars_epi %>% filter(Key == k) %>% select(-Key),
  Uptake = pars_uptake,
  VE = pars_ves
))




scenario_soc <- function(df, p0, yr) find_eligible_default(df, p0, min(yr, 2022))
scenario_p65 <- function(df, p0, yr) find_eligible_default(df, p0, min(yr, 2027))
scenario_full <- find_eligible_default

scenario_2028_95 <- function(df, p0, yr) {
  if (yr < 2028) {
    df <- find_eligible_default(df, p0, yr)
  } else {
    df <- find_eligible_default(df, p0, yr, cap = 95)
  }
  return(df)
}

scenario_2033_95 <- function(df, p0, yr) {
  if (yr < 2033) {
    df <- find_eligible_default(df, p0, yr)
  } else {
    df <- find_eligible_default(df, p0, yr, cap = 95)
  }
  return(df)
}



yss <- bind_rows(
  sim_dy_hz_vac(pars, year1 = 2040, rule_eligible = scenario_soc) %>% mutate(Scenario = "SOC"),
  sim_dy_hz_vac(pars, year1 = 2040, rule_eligible = scenario_p65) %>% mutate(Scenario = "To 65 yr"),
  sim_dy_hz_vac(pars, year1 = 2040, rule_eligible = scenario_full) %>% mutate(Scenario = "Scheduled"),
  sim_dy_hz_vac(pars, year1 = 2040, rule_eligible = scenario_2028_95) %>% mutate(Scenario = "To 95 yr at 2028"),
  sim_dy_hz_vac(pars, year1 = 2040, rule_eligible = scenario_2033_95) %>% mutate(Scenario = "To 95 yr at 2033")
) %>% 
  mutate(
    Scenario = factor(Scenario, levels = c("SOC", "To 65 yr", "Scheduled", "To 95 yr at 2028", "To 95 yr at 2033")),
  )



yss %>% 
  filter(Age >= 50) %>% 
  group_by(Year, Scenario) %>% 
  summarise(
    Incidence = sum(HZ) / sum(N)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = Incidence, colour = Scenario)) +
  geom_vline(xintercept = c(2023, 2028, 2033) - 1, linetype = 2) +
  geom_text(x = 2023 - 1, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028 - 1, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033 - 1, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) + 
  scale_y_continuous("Incidence, per 100k", labels = scales::number_format(scale = 1e5)) +
  expand_limits(y = 0)


yss %>% 
  filter(Age >= 50) %>% 
  mutate(
    AgeGrp = cut(Age, seq(0, 100, 5), right = F)
  )  %>% 
  filter(!is.na(AgeGrp))


yss %>% 
  filter(Age >= 50) %>% 
  #filter(Scenario == "To 85 yr at 2033") %>% 
  #filter(Year %in% seq(2018, 2040, 10)) %>% 
  filter(Year == 2038) %>% 
  mutate(
    AgeGrp = cut(Age, seq(0, 100, 5), right = F)
  )  %>% 
  filter(!is.na(AgeGrp)) %>% 
  group_by(AgeGrp, Scenario) %>% 
  summarise(Cases = sum(HZ)) %>% 
  #group_by(Year) %>% 
  #mutate(Cases = Cases / sum(Cases)) %>% 
  ggplot() +
  geom_bar(aes(x = AgeGrp, y = Cases, fill = Scenario), stat = "identity", position = "dodge")





ys <- sim_dy_hz_vac(pars, year1 = 2040, rule_eligible = scenario_full)


ys %>% 
  filter(Age >= 70) %>% 
  group_by(Year, Age) %>% 
  summarise(
    Coverage = weighted.mean((Vaccine != "None"), w = N)
  ) %>% 
  mutate(Cohort = Year - Age + 70) %>% 
  filter(Coverage > 0) %>% 
  ggplot() +
  geom_line(aes(x = Age, y = Coverage, colour = as.character(Cohort))) 


ys %>% 
  filter(Age >= 50) %>% 
  mutate(
    AgeGrp = cut(Age, seq(0, 100, 5), right = F)
  )  %>% 
  filter(!is.na(AgeGrp)) %>% 
  group_by(Year, AgeGrp) %>% 
  summarise(
    Coverage = weighted.mean((Vaccine != "None"), w = N)
  ) %>% 
  filter(Year %in% seq(2018, 2040, 5)) %>% 
  ggplot() +
  geom_bar(aes(x = AgeGrp, y = Coverage, fill = as.character(Year)), stat = "identity", position = "dodge", width = 0.8, alpha = 0.8) +
  scale_y_continuous("Coverage, %", labels = scales::percent) +
  # facet_wrap(.~Year) +
  expand_limits(y = 0:1)


ys %>% 
  filter(Age >= 50) %>% 
  mutate(
    AgeGrp = cut(Age, seq(0, 100, 5), right = F)
  )  %>% 
  filter(!is.na(AgeGrp)) %>% 
  group_by(Year, AgeGrp) %>% 
  summarise(
    Coverage = weighted.mean((Vaccine != "None"), w = N)
  ) %>% 
  filter(Year %in% seq(2018, 2040, 5)) %>% 
  ggplot() +
  geom_bar(aes(x = AgeGrp, y = Coverage, fill = as.character(Year)), stat = "identity", position = "dodge", width = 0.8, alpha = 0.8) +
  scale_y_continuous("Coverage, %", labels = scales::percent) +
  # facet_wrap(.~Year) +
  expand_limits(y = 0:1)


ys %>% 
  filter(Age >= 50 & Age <= 100) %>% 
  group_by(Year, Age) %>% 
  summarise(
    Coverage = weighted.mean((Vaccine != "None"), w = N)
  ) %>% 
  filter(Year %in% seq(2018, 2040, 5)) %>% 
  mutate(
    AgeGrp = cut(Age, c(50, 60, 65, 70, 80, 90, 100), right = F)
  ) %>% 
  filter(!is.na(AgeGrp)) %>% 
  ggplot() +
  geom_bar(aes(x = Age, y = Coverage, fill = AgeGrp), stat = "identity", width = 1) +
  #geom_line(aes(x = Age, y = Coverage, colour = as.character(Year))) +
  scale_y_continuous("Coverage, %", labels = scales::percent) +
  facet_wrap(.~Year) +
  expand_limits(y = 0:1)



ys %>% 
  group_by(Year) %>% 
  summarise(
    Incidence = sum(HZ) / sum(N)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = Incidence)) +
  geom_vline(xintercept = c(2023, 2028, 2033) - 1, linetype = 2) +
  geom_text(x = 2023 - 1, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028 - 1, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033 - 1, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) + 
  scale_y_continuous("Incidence, per 100k", labels = scales::number_format(scale = 1e5)) +
  expand_limits(y = 0)


