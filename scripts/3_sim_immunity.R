library(tidyverse)

theme_set(theme_bw())

source(here::here("R", "sim_coverage.R"))


## Inputs loading
load(here::here("data", "fit_coverage.csv"))

coverage <- read_csv(here::here("data", "Coverage.csv")) %>% 
  mutate(
    Cov_2018 = ifelse(Cov_2018 == "Not eligible", NA, Cov_2018),
    Cov_2018 = as.numeric(Cov_2018)
  ) %>% 
  pivot_longer(-Age, 
               names_pattern = "Cov_(\\d+)", names_to = "Year", values_to = "Coverage") %>% 
  mutate(Coverage = Coverage / 100) 

load(here::here("data", "VE_Zostavax_NIC_AJ.rdata"))


get_full_ve <- function(r_waning) {
  VE$src %>% 
    rename(AgeVac = age, VE0 = VE) %>% 
    left_join(crossing(AgeVac = 0:100, FromVac = 0:100)) %>% 
    mutate(
      VE = VE0 * exp(- FromVac * r_waning),
      Age = AgeVac + FromVac
    ) %>% 
    filter(Age <= 100) %>% 
    select(- VE0)
}


## Model of vaccine uptake
### Settings
year0 <- 2014
year1 <- 2040


sims <- bind_rows(lapply(1944:2000, function(year_birth) {
  bind_rows(
    simulate_protection(pars = pred1$pars, year_birth = year_birth, ic = F, year0 = year0, year1 = year1),
    simulate_protection(pars = pred1$pars, year_birth = year_birth, ic = T, year0 = year0, year1 = year1)
  )
})) %>% 
  mutate(FromVac = Year - YearVac)

save(sims, file = here::here("outputs", "temp", "sims_coverage.rdata"))


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






