library(tidyverse)

theme_set(theme_bw())


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


## Settings
year0 <- 2014
year1 <- 2035

year_available <- 2014
age_eligible <- 70:80


sims <- bind_rows(lapply(1944:2010, function(year_birth) {
  vac_new <- tibble(
    YearBirth = year_birth,
    Year = year0:year1,
    Age = Year - YearBirth
  ) %>% 
    mutate(
      PrVac = case_when(
        Year < year_available ~ 0,
        Age < min(age_eligible) ~ 0,
        Age == min(age_eligible) ~ pred1$pars$p_initial,
        Age <= max(age_eligible) ~ pred1$pars$p_catchup,
        T ~ 0
      ),
      Coverage = 1 - cumprod(1 - PrVac),
      NewVac = diff(c(0, Coverage))
    ) %>% 
    select(YearBirth, YearVac = Year, NewVac)
  
  vac <- bind_rows(lapply(year0:year1, function(yr) {
    vac_new %>% 
      filter(YearVac <= yr) %>% 
      mutate(Year = yr)
  }))
})) %>% 
  mutate(
    AgeVac = YearVac - YearBirth,
    FromVac = Year - YearVac,
    Age = Year - YearBirth
  )


sims %>% 
  filter(YearBirth %in% 1944:1950) %>% 
  filter(NewVac > 0) %>% 
  ggplot() +
  geom_bar(aes(x = Year, y = NewVac, fill = AgeVac), stat = "identity", position = "stack", width = 1, alpha = 0.7) +
  scale_y_continuous("Coverage, %", labels = scales::percent) +
  scale_fill_gradient("Age vaccined", low = "grey80", high = "grey20", breaks = seq(70, 80, 5)) +
  expand_limits(y = 1) +
  facet_wrap(.~YearBirth, labeller = "label_both") +
  theme(axis.text.x = element_text(angle = -60, hjust = 0, vjust = 0))


sims %>% 
  filter(YearBirth %in% 1944:1950) %>% 
  filter(NewVac > 0) %>% 
  ggplot() +
  geom_bar(aes(x = Year, y = NewVac, fill = FromVac), stat = "identity", position = "stack", width = 1) +
  scale_y_continuous("Coverage, %", labels = scales::percent) +
  scale_fill_gradient("Year vaccined", low = "grey80", high = "grey20", breaks = seq(0, 20, 5)) +
  expand_limits(y = 1) +
  facet_wrap(.~YearBirth, labeller = "label_both") +
  theme(axis.text.x = element_text(angle = -60, hjust = 0, vjust = 0))


sims %>% 
  filter(Year %in% c(2018, 2023, 2028, 2033)) %>% 
  filter(NewVac > 0) %>% 
  ggplot() +
  geom_bar(aes(x = Age, y = NewVac, fill = FromVac), stat = "identity", position = "stack", width = 1) +
  scale_y_continuous("Coverage, %", labels = scales::percent) +
  scale_fill_gradient("Year vaccined", high = "grey80", low = "grey20", breaks = seq(0, 20, 5)) +
  expand_limits(y = 1) +
  facet_wrap(.~Year, labeller = "label_both") +
  theme(axis.text.x = element_text(angle = -60, hjust = 0, vjust = 0))


sims %>% 
  filter(Year %in% c(2018, 2023, 2028, 2033)) %>% 
  filter(NewVac > 0) %>% 
  ggplot() +
  geom_bar(aes(x = Age, y = NewVac, fill = AgeVac), stat = "identity", position = "stack", width = 1) +
  scale_y_continuous("Coverage, %", labels = scales::percent) +
  scale_fill_gradient("Age vaccined", low = "grey80", high = "grey20", breaks = seq(70, 80, 5)) +
  expand_limits(y = 1) +
  facet_wrap(.~Year, labeller = "label_both") +
  theme(axis.text.x = element_text(angle = -60, hjust = 0, vjust = 0))



sims %>% 
  filter(Year %in% c(2018, 2023, 2028, 2033)) %>% 
  filter(NewVac > 0) %>%
  left_join(get_full_ve(0.2)) %>% 
  mutate(
    Protection = NewVac * VE
  ) %>% 
  ggplot() +
  geom_bar(aes(x = Age, y = Protection, fill = AgeVac), stat = "identity", position = "stack", width = 1) +
  scale_y_continuous("Vaccine-induced protection, %", labels = scales::percent) +
  scale_fill_gradient("Age vaccined", low = "grey80", high = "grey20", breaks = seq(70, 80, 5)) +
  expand_limits(y = 1) +
  facet_wrap(.~Year, labeller = "label_both") +
  theme(axis.text.x = element_text(angle = -60, hjust = 0, vjust = 0))


load(here::here("data", "Epi_HZ_NIC.rdata"))



Incidence_HZ %>% 
  group_by(age) %>% 
  summarise(
    Incidence_HZ = exp(mean(log(Incidence_HZ)))
  ) %>% 
  ggplot() + 
  geom_line(aes(x = age, y = Incidence_HZ))



## Model of vaccine uptake

## Vaccine uptake and vaccine efficacy, unknown waning rate

## Incidence model and fitted waning rate

## Future uptake of Shingrix

### Current programme

### Younger age population

### Extreme old age population

### 





p0 <- 0.01 
p_vac <- 0.001


cost <- 10
p0 * (1000 + cost) = (coverage * p_vac + (1 - coverage) * p0) * (1000 + cost) 





