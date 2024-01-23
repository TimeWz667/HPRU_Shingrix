library(tidyverse)


folder_raw <- function(x) here::here("data", "raw", x)
folder_data <- function(x) here::here("data", "processed_epi", x)



### Incidence/Mortality HZ, IC ----


## Data Nick
# Incidence_HZ <- read_csv(file = folder_raw("Incidence_RCGP_2017_Nick.csv"))
## Data Anu-> Incidence in the non-IC
# Incidence_HZ<-read.csv(file=paste(Data,"clean_df_o90_grouped.csv", sep=""), row.names = 1)
# Incidence_HZ_coef<-read.csv(file=paste(Data, "sim_coef_age2degreepoly_1000.csv", sep=""), row.names=1)

## Data Jemma CPRD supplemented by HES
### Incidence NonIC
Incidence_HZ <- read_csv(folder_raw("sim_coef_age2degreepoly_nb_nIC_1000_new.csv"))[-1] %>% 
  rename(beta_age = age, beta_age2 = age2) %>%
  mutate(Key = 1:n()) %>% 
  crossing(tibble(age = 0:100)) %>% 
  arrange(Key, age) %>% 
  mutate(
    Incidence_HZ = exp(Intercept + age * beta_age + age ^ 2 * beta_age2),
    Incidence_HZ = ifelse(age >= 50, Incidence_HZ, NA)
  ) %>% 
  group_by(Key) %>%
  fill(Incidence_HZ, .direction = "updown") %>% 
  mutate(
    p_HZ = 1 - exp(-Incidence_HZ)
  ) %>% 
  select(Key, Age = age, Incidence_HZ, p_HZ) %>% 
  ungroup()


Incidence_HZ_GP_only <- read_csv(folder_raw("sim_coef_age2degreepoly_nb_nIC_onlyGP_1000_new.csv"))[-1] %>% 
  rename(beta_age = age, beta_age2 = age2) %>%
  mutate(Key = 1:n()) %>% 
  crossing(tibble(age = 0:100)) %>% 
  arrange(Key, age) %>% 
  mutate(
    Incidence_HZ_GP_only = exp(Intercept + age * beta_age + age ^ 2 * beta_age2),
    Incidence_HZ_GP_only = ifelse(age >= 50, Incidence_HZ_GP_only, NA)
  ) %>% 
  group_by(Key) %>%
  fill(Incidence_HZ_GP_only, .direction = "updown") %>% 
  mutate(
    p_HZ_GP_only = 1 - exp(-Incidence_HZ_GP_only)
  ) %>% 
  select(Key, Age = age, Incidence_HZ_GP_only, p_HZ_GP_only) %>% 
  ungroup()


## Peter Hobbelen's analysis 2004-05 to 2012-13 mortality (HES, primary diagnosis)
Mortality_HZ <- read_csv(folder_raw("11-06-2018 HZ HES mortality incidence primary cause sim_coef_age_1000.csv"))[-1] %>% 
  rename(beta_age = age) %>% 
  mutate(Key = 1:n()) %>% 
  crossing(tibble(age = 0:100)) %>% 
  arrange(Key, age) %>% 
  mutate(
    Death_HZ = exp(Intercept + beta_age * age),
    Death_HZ = ifelse(age >= 50 & age < 90, Death_HZ, NA)
  ) %>% 
  group_by(Key) %>% 
  fill(Death_HZ, .direction = "updown") %>% 
  select(Key, Age = age, Death_HZ) %>% 
  ungroup()

## Hospitalisation rate Peter Hobbelen analysis 2004-05 to 2012-13
Hospitalisation_HZ <- read_csv(folder_raw("HZ hospitalisation incidence sim_coef_age3degreepoly_1000.csv"))[-1] %>% 
  rename(beta_age = age, beta_age2 = age2, beta_age3 = age3) %>% 
  mutate(Key = 1:n()) %>% 
  full_join(crossing(age = 0:100, Key = 1:1000)) %>% 
  mutate(
    Hospitalisation_rate_HZ = exp(Intercept + beta_age * age + beta_age2 * age ^ 2 + beta_age3 * age ^ 3),
    Hospitalisation_rate_HZ = ifelse(age <= 90 & age >= 50, Hospitalisation_rate_HZ, NA)
  ) %>% 
  group_by(Key) %>% 
  fill(Hospitalisation_rate_HZ, .direction = "updown") %>% 
  ungroup() %>% 
  select(Key, Age = age, r_hospitalisation_hz = Hospitalisation_rate_HZ)


## Data Jemma CPRD p_PHN
P_PHN <-  read_csv(folder_raw("sim_coef_age2degreepoly_binom_PHN_nIC_1000.csv"))[-1] %>% 
  rename(beta_age = age, beta_age2 = age2) %>% 
  mutate(Key = 1:n()) %>% 
  full_join(crossing(age = 0:100, Key = 1:1000)) %>% 
  mutate(
    p_phn = exp(Intercept + beta_age * age + beta_age2 * age ^ 2),
    p_phn = p_phn / (1 + p_phn),
    p_phn = ifelse(age <= 95 & age >= 50, p_phn, NA)
  ) %>% 
  group_by(Key) %>% 
  fill(p_phn, .direction = "updown") %>% 
  ungroup() %>% 
  select(Key, Age = age, p_phn)


save(Incidence_HZ, Incidence_HZ_GP_only, Mortality_HZ, P_PHN, Hospitalisation_HZ, file = folder_data("Epi_HZ_NIC_CPRD.rdata"))


### Incidence IC
Incidence_HZ <- read_csv(folder_raw("sim_coef_age2degreepoly_pois_IC_1000_new.csv"))[-1] %>% 
  rename(beta_age = age, beta_age2 = age2) %>%
  mutate(Key = 1:n()) %>% 
  crossing(tibble(age = 0:100)) %>% 
  arrange(Key, age) %>% 
  mutate(
    Incidence_HZ = exp(Intercept + age * beta_age + age ^ 2 * beta_age2),
    Incidence_HZ = ifelse(age >= 50 & age <= 95, Incidence_HZ, NA)
  ) %>% 
  group_by(Key) %>%
  fill(Incidence_HZ, .direction = "updown") %>% 
  mutate(
    p_HZ = 1 - exp(-Incidence_HZ)
  ) %>% 
  select(Key, Age = age, Incidence_HZ, p_HZ) %>% 
  ungroup()


Incidence_HZ_GP_only <- read_csv(folder_raw("sim_coef_age2degreepoly_pois_IC_onlyGP_1000_new.csv"))[-1] %>% 
  rename(beta_age = age, beta_age2 = age2) %>%
  mutate(Key = 1:n()) %>% 
  crossing(tibble(age = 0:100)) %>% 
  arrange(Key, age) %>% 
  mutate(
    Incidence_HZ_GP_only = exp(Intercept + age * beta_age + age ^ 2 * beta_age2),
    Incidence_HZ_GP_only = ifelse(age >= 50 & age <= 95, Incidence_HZ_GP_only, NA)
  ) %>% 
  group_by(Key) %>% 
  fill(Incidence_HZ_GP_only, .direction = "updown") %>% 
  mutate(
    p_HZ_GP_only = 1 - exp(-Incidence_HZ_GP_only)
  ) %>% 
  select(Key, Age = age, Incidence_HZ_GP_only, p_HZ_GP_only) %>% 
  ungroup()


Mortality_HZ <- read_csv(folder_raw("01-01-2019 HZ HES mortality incidence primary cause IC sim_coef_age_1000.csv"))[-1] %>% 
  rename(beta_age = age, beta_age2 = age2) %>% 
  mutate(Key = 1:n()) %>% 
  crossing(tibble(age = 0:100)) %>% 
  arrange(Key, age) %>% 
  mutate(
    Death_HZ = exp(Intercept + beta_age * age + beta_age2 * age ^ 2),
    Death_HZ = ifelse(age >= 50 & age < 90, Death_HZ, NA)
  ) %>% 
  group_by(Key) %>% 
  fill(Death_HZ, .direction = "updown") %>% 
  select(Key, Age = age, Death_HZ) %>% 
  ungroup()




## IC
Hospitalisation_HZ <- read_csv(folder_raw("HZ hospitalisation incidence IC sim_coef_age4degreepoly_1000.csv"))[-1] %>% 
  rename(beta_age = age, beta_age2 = age2, beta_age3 = age3, beta_age4 = age4) %>% 
  mutate(Key = 1:n()) %>% 
  full_join(crossing(age = 0:100, Key = 1:1000)) %>% 
  mutate(
    Hospitalisation_rate_HZ = exp(Intercept + beta_age * age + beta_age2 * age ^ 2 + beta_age3 * age ^ 3 + beta_age4 * age ^ 4),
    Hospitalisation_rate_HZ = ifelse(age <= 90 & age >= 50, Hospitalisation_rate_HZ, NA)
  ) %>% 
  group_by(Key) %>% 
  fill(Hospitalisation_rate_HZ, .direction = "updown") %>% 
  ungroup() %>% 
  select(Key, Age = age, r_hospitalisation_hz = Hospitalisation_rate_HZ)


P_PHN <-  read_csv(folder_raw("sim_coef_age2degreepoly_binom_PHN_IC_1000.csv"))[-1] %>% 
  rename(beta_age = age, beta_age2 = age2) %>% 
  mutate(Key = 1:n()) %>% 
  full_join(crossing(age = 0:100, Key = 1:1000)) %>% 
  mutate(
    p_phn = exp(Intercept + beta_age * age + beta_age2 * age ^ 2),
    p_phn = p_phn / (1 + p_phn),
    p_phn = ifelse(age <= 90 & age >= 50, p_phn, NA)
  ) %>% 
  group_by(Key) %>% 
  fill(p_phn, .direction = "updown") %>% 
  ungroup() %>% 
  select(Key, Age = age, p_phn)


save(Incidence_HZ, Incidence_HZ_GP_only, Mortality_HZ, Hospitalisation_HZ, P_PHN, file = folder_data("Epi_HZ_IC_CPRD.rdata"))

