library(tidyverse)
library(tidybayes)

theme_set(theme_bw())

source(here::here("models", "m_dy_hz.R"))
source(here::here("models", "misc.R"))

# Load inputs
file_inputs <- "pars_nic_programme.rdata"
if (!(file_inputs %in% dir(here::here("analysis_cohort", "inputs")))) {
  source(here::here("analysis_programme", "fn_arrange_inputs.R"))
  pars_set <- load_inputs_nic(discount_costs = 0.035, discount_effects = 0.035, year = 2024, n_sims = 1e3)
  save(pars_set, file = here::here("analysis_programme", "inputs", file_inputs))
} else {
  load(file = here::here("analysis_programme", "inputs", file_inputs))
}



k = 5
pars <- get_pars(pars_set, k)


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
    Incidence = sum(N_HZ) / sum(N)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = Incidence, colour = Scenario)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) + 
  scale_y_continuous("Incidence, per 100k", labels = scales::number_format(scale = 1e5)) +
  expand_limits(y = 0)


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
  summarise(Cases = sum(N_HZ)) %>% 
  #group_by(Year) %>% 
  #mutate(Cases = Cases / sum(Cases)) %>% 
  ggplot() +
  geom_bar(aes(x = AgeGrp, y = Cases, fill = Scenario), stat = "identity", position = "dodge")


ys <- sim_dy_hz_vac(pars, year1 = 2050, rule_eligible = scenario_full)


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
    Incidence = sum(N_HZ) / sum(N)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = Incidence)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) + 
  scale_y_continuous("Incidence, per 100k", labels = scales::number_format(scale = 1e5)) +
  expand_limits(y = 0)


