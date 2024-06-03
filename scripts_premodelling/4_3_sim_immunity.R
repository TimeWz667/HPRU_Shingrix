library(tidyverse)

theme_set(theme_bw())

source(here::here("R", "sim_coverage.R"))


## Inputs loading
load(here::here("data", "fitted_coverage.rdata"))
load(here::here("pars", "ves.rdata"))

ves <- ves %>% mutate(IC = ifelse(IC, "IC", "NonIC"))


pop <- local({
  load(here::here("data", "processed_demography", "Population_ONS.rdata"))
  load(here::here("data", "processed_epi", "Epi_HZ_CPRD_23Nov19.rdata"))
  
  pr_ic <- dat_burden %>% 
    select(Age, IC = Immunocompromised, N) %>% 
    pivot_wider(names_from = IC, names_prefix = "IC", values_from = N) %>% 
    mutate(
      PrIC = IC1 / IC0
    ) %>% 
    select(Age, PrIC)

  bind_rows(
    demo_ons %>% 
      filter(Location == "England") %>% 
      mutate(IC = "NonIC") %>% 
      select(Year, Age, N, IC),
    demo_ons %>% 
      filter(Location == "England") %>% 
      left_join(pr_ic) %>% 
      mutate(
        N = round(N * PrIC),
        N = ifelse(is.na(N), 0, N),
        IC = "IC"
      ) %>% 
      select(Year, Age, N, IC)
  )
})


## Model of vaccine uptake
### Settings
year0 <- 2014
year1 <- 2040


sims_Stay <- bind_rows(lapply(1944:2000, function(year_birth) {
  bind_rows(
    simulate_protection(pred1$pars$p_initial, pred1$pars$p_catchup, year_birth = year_birth, 
                        year0 = year0, year1 = year1, ic = F,
                        al_ic = 70, ah_ic = 80,
                        year_step1 = 2023, al_step1 = 70, ah_step1 = 80,
                        year_step2 = 2028, al_step2 = 70, ah_step2 = 80,
                        year_step3 = 2033),
    simulate_protection(pred1$pars$p_initial, pred1$pars$p_catchup, year_birth = year_birth, 
                        year0 = year0, year1 = year1, ic = T,
                        al_ic = 70, ah_ic = 80,
                        year_step1 = 2023, al_step1 = 70, ah_step1 = 80,
                        year_step2 = 2028, al_step2 = 70, ah_step2 = 80,
                        year_step3 = 2033)
  )
})) %>% 
  mutate(FromVac = Year - YearVac, Scenario = "Stay") %>% 
  left_join(pop, relationship = "many-to-many") %>% 
  left_join(ves, relationship = "many-to-many") %>% 
  filter(!is.na(VE))


sims_Plan <- bind_rows(lapply(1944:2000, function(year_birth) {
  bind_rows(
    simulate_protection(pred1$pars$p_initial, pred1$pars$p_catchup, year_birth = year_birth, 
                        year0 = year0, year1 = year1, ic = F,
                        al_ic = 50, ah_ic = 80,
                        year_step1 = 2023, al_step1 = 65, ah_step1 = 80,
                        year_step2 = 2028, al_step2 = 60, ah_step2 = 80,
                        year_step3 = 2033),
    simulate_protection(pred1$pars$p_initial, pred1$pars$p_catchup, year_birth = year_birth, 
                        year0 = year0, year1 = year1, ic = T,
                        al_ic = 50, ah_ic = 80,
                        year_step1 = 2023, al_step1 = 65, ah_step1 = 80,
                        year_step2 = 2028, al_step2 = 60, ah_step2 = 80,
                        year_step3 = 2033)
  )
})) %>% 
  mutate(FromVac = Year - YearVac, Scenario = "UK_Plan") %>% 
  left_join(pop, relationship = "many-to-many") %>% 
  left_join(ves, relationship = "many-to-many") %>% 
  filter(!is.na(VE))


sims_IcOnly <- bind_rows(lapply(1944:2000, function(year_birth) {
  bind_rows(
    simulate_protection(pred1$pars$p_initial, pred1$pars$p_catchup, year_birth = year_birth, 
                        year0 = year0, year1 = year1, ic = F,
                        al_ic = 50, ah_ic = 80,
                        year_step1 = 2023, al_step1 = 70, ah_step1 = 80,
                        year_step2 = 2028, al_step2 = 70, ah_step2 = 80,
                        year_step3 = 2033),
    simulate_protection(pred1$pars$p_initial, pred1$pars$p_catchup, year_birth = year_birth, 
                        year0 = year0, year1 = year1, ic = T,
                        al_ic = 50, ah_ic = 80,
                        year_step1 = 2023, al_step1 = 70, ah_step1 = 80,
                        year_step2 = 2028, al_step2 = 70, ah_step2 = 80,
                        year_step3 = 2033)
  )
})) %>% 
  mutate(FromVac = Year - YearVac, Scenario = "IC_Only") %>% 
  left_join(pop, relationship = "many-to-many") %>% 
  left_join(ves, relationship = "many-to-many") %>% 
  filter(!is.na(VE))



sims_PlanAndOlder <- bind_rows(lapply(1944:2000, function(year_birth) {
  bind_rows(
    simulate_protection(pred1$pars$p_initial, pred1$pars$p_catchup, year_birth = year_birth, 
                        year0 = year0, year1 = year1, ic = F,
                        al_ic = 50, ah_ic = 90,
                        year_step1 = 2023, al_step1 = 65, ah_step1 = 90,
                        year_step2 = 2028, al_step2 = 60, ah_step2 = 90,
                        year_step3 = 2033),
    simulate_protection(pred1$pars$p_initial, pred1$pars$p_catchup, year_birth = year_birth, 
                        year0 = year0, year1 = year1, ic = T,
                        al_ic = 50, ah_ic = 90,
                        year_step1 = 2023, al_step1 = 65, ah_step1 = 90,
                        year_step2 = 2028, al_step2 = 60, ah_step2 = 90,
                        year_step3 = 2033)
  )
})) %>% 
  mutate(FromVac = Year - YearVac, Scenario = "UK_Plan to 90") %>% 
  left_join(pop, relationship = "many-to-many") %>% 
  left_join(ves, relationship = "many-to-many") %>% 
  filter(!is.na(VE))



save(sims_Stay, sims_Plan, sims_IcOnly, sims_PlanAndOlder, file = here::here("outputs", "temp", "sims_coverage.rdata"))

# save(sims, file = here::here("outputs", "temp", "sims_coverage.rdata"))


vis <- function(sims) {
  list(
    Coverage= sims %>% 
      filter(Year %in% c(2018, 2023, 2028, 2033)) %>% 
      ggplot() +
      geom_bar(aes(x = Age, y = Pr, fill = FromVac), stat = "identity", position = "stack", width = 1) +
      scale_y_continuous("Coverage, %", labels = scales::percent) +
      scale_fill_gradient("Year from vaccined", high = "grey80", low = "grey40", breaks = seq(0, 20, 5)) +
      expand_limits(y = 1, x = 50) +
      facet_grid(Year~IC) +
      theme(axis.text.x = element_text(angle = -60, hjust = 0, vjust = 0)),
    VE = sims %>% 
      filter(Year %in% c(2018, 2023, 2028, 2033)) %>% 
      ggplot() +
      geom_bar(aes(x = Age, y = Pr * VE, fill = FromVac), stat = "identity", position = "stack", width = 1) +
      scale_y_continuous("Protection, %", labels = scales::percent) +
      scale_fill_gradient("Year from vaccined", high = "grey80", low = "grey40", breaks = seq(0, 20, 5)) +
      expand_limits(y = 1, x = 50) +
      facet_grid(Year~IC) +
      theme(axis.text.x = element_text(angle = -60, hjust = 0, vjust = 0)) 
  )
}


vis_Stay = vis(sims_Stay)
vis_Plan = vis(sims_Plan)
vis_IcOnly = vis(sims_IcOnly)
vis_PlanAndOlder = vis(sims_PlanAndOlder)


### Visualisation
sims %>% 
  filter(YearBirth %in% 1944:1950) %>% 
  filter(!is.na(YearVac)) %>% 
  ggplot() +
  geom_bar(aes(x = Year, y = Pr, fill = AgeVac), stat = "identity", position = "stack", width = 1, alpha = 0.7) +
  scale_y_continuous("Coverage, %", labels = scales::percent) +
  scale_fill_gradient("Age vaccined", low = "grey80", high = "grey20", breaks = seq(70, 80, 5)) +
  expand_limits(y = 1) +
  facet_grid(YearBirth~IC) +
  theme(axis.text.x = element_text(angle = -60, hjust = 0, vjust = 0))


sims %>% 
  filter(YearBirth %in% 1944:1950) %>% 
  filter(!is.na(YearVac)) %>%
  ggplot() +
  geom_bar(aes(x = Year, y = Pr, fill = FromVac), stat = "identity", position = "stack", width = 1) +
  scale_y_continuous("Coverage, %", labels = scales::percent) +
  scale_fill_gradient("Year vaccined", low = "grey80", high = "grey20", breaks = seq(0, 20, 5)) +
  expand_limits(y = 1) +
  facet_grid(YearBirth~IC) +
  theme(axis.text.x = element_text(angle = -60, hjust = 0, vjust = 0))


sims %>% 
  filter(YearBirth %in% c(1944, 1960, 1970)) %>% 
  filter(!is.na(YearVac)) %>%
  ggplot() +
  geom_bar(aes(x = Age, y = Pr, fill = FromVac), stat = "identity", position = "stack", width = 1) +
  scale_y_continuous("Coverage, %", labels = scales::percent) +
  scale_fill_gradient("Year vaccined", high = "grey80", low = "grey20", breaks = seq(0, 20, 5)) +
  expand_limits(y = 1) +
  facet_grid(YearBirth~IC) +
  theme(axis.text.x = element_text(angle = -60, hjust = 0, vjust = 0))


## Vaccine uptake and vaccine efficacy, unknown waning rate
sims %>% 
  filter(Year %in% c(2018, 2023, 2028, 2033)) %>% 
  left_join(get_full_ve(0.2)) %>% 
  mutate(
    Protection = Pr * VE
  ) %>% 
  ggplot() +
  geom_bar(aes(x = Age, y = Protection, fill = AgeVac), stat = "identity", position = "stack", width = 1) +
  scale_y_continuous("Vaccine-induced protection, %", labels = scales::percent) +
  scale_fill_gradient("Age vaccined", low = "grey80", high = "grey20", breaks = seq(70, 80, 5)) +
  expand_limits(y = 1) +
  facet_wrap(.~Year, labeller = "label_both") +
  theme(axis.text.x = element_text(angle = -60, hjust = 0, vjust = 0)) +
  labs(caption = "Waning: 20% per year")



## Incidence model and fitted waning rate

Inc_prevac <- local({
  load(here::here("data", "Epi_HZ_NIC.rdata"))
  
  Incidence_HZ %>% 
    group_by(Age = age) %>% 
    summarise(Inc = exp(mean(log(Incidence_HZ))))
})


Inc_zostavax <- local({
  load(here::here("data", "hz_burden_22.rdata"))
  
  dat_burden %>% 
    group_by(Age) %>% 
    summarise(
      X = round(sum(IncR * N)),
      N = sum(N),
      Pr = X / N
    )
})

Inc_prevac %>% ggplot() +
  geom_line(aes(x = Age, y = Inc)) +
  geom_line(data = Inc_zostavax, aes(x = Age, y = Pr * 0.95))


ds <- Inc_zostavax %>% 
  left_join(Inc_prevac, by = "Age") %>% 
  left_join(
    sims %>% 
      filter(Year == 2022) %>% 
      filter(Pr > 0) %>%
      left_join(get_full_ve(r_waning)) %>% 
      filter(!is.na(VE)) %>% 
      group_by(Age) %>% 
      summarise(Protection = sum(Pr * VE)),
    by = "Age"
  ) %>% 
  left_join(
    sims %>% 
      filter(Year == 2015) %>% 
      filter(Pr > 0) %>%
      left_join(get_full_ve(r_waning)) %>% 
      filter(!is.na(VE)) %>% 
      group_by(Age) %>% 
      summarise(Protection0 = sum(Pr * VE)),
    by = "Age"
  ) %>% 
  mutate(
    Protection = ifelse(is.na(Protection), 0, Protection),
    Protection0 = ifelse(is.na(Protection0), 0, Protection0)
  )

ds %>% 
  ggplot() +
  geom_line(aes(x = Age, y = Inc, colour = "2015")) +
  geom_line(aes(x = Age, y = Pr, colour = "2023")) +
  geom_line(aes(x = Age, y = Inc * (1 - Protection), colour = "2015 + protection")) 


