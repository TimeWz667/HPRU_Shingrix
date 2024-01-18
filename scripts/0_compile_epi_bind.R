library(tidyverse)


folder_raw <- function(x) here::here("data", "raw", x)
folder_data <- function(x) here::here("data", x)

pick <- function(df, n) {
  ks <- df %>% pull(Key) %>% unique()
  
  tibble(ID = 1:n, Key = sample(ks, n, rep= T)) %>% 
    left_join(df, by = "Key", relationship = "many-to-many") %>% 
    select(-Key) %>% 
    rename(Key = ID)
}


### Notes
## - Build up parameter samples based on the posterior of the statistic models
## - Remove samples with unreasonable features, for example, the overall incidence must be smaller than the GP-only incidence

### Combining all the epidemiological inputs to a single file

n_sim <- 1000
n_sample <- 3000



### Incidence/Mortality HZ, NonIC ----
## Data Jemma CPRD supplemented by HES

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
  select(Key, Age = age, r_inc_hz = Incidence_HZ) %>% 
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
  select(Key, Age = age, r_inc_hz_gp = Incidence_HZ_GP_only) %>% 
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
  select(Key, Age = age, r_mor_hz = Death_HZ) %>% 
  ungroup()

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
  select(Key, Age = age, r_hosp_hz = Hospitalisation_rate_HZ)


epi_hz_nic <- pick(Incidence_HZ, n_sample) %>% 
  left_join(pick(Incidence_HZ_GP_only, n_sample)) %>% 
  left_join(pick(Mortality_HZ, n_sample)) %>% 
  left_join(pick(Hospitalisation_HZ, n_sample)) %>% 
  left_join(pick(P_PHN, n_sample))


eligible <- epi_hz_nic %>% 
  filter(r_inc_hz > r_inc_hz_gp) %>% 
  filter(r_inc_hz > r_hosp_hz) %>%
  filter(r_inc_hz > r_mor_hz) %>%
  group_by(Key) %>% 
  summarise(eli = n()) %>% 
  ungroup() %>% 
  filter(eli == max(eli)) %>% 
  pull(Key)


epi_hz_nic <- tibble(Key = sample(eligible, n_sim), ID = 1:n_sim) %>% 
  left_join(epi_hz_nic) %>% 
  select(- Key) %>% 
  rename(Key = ID)

Epi_HZ <- epi_hz_nic
save(Epi_HZ, file = here::here("data", "Epi_NIC.rdata"))



## IC
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
  select(Key, Age = age, r_inc_hz = Incidence_HZ) %>% 
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
  select(Key, Age = age, r_inc_hz_gp = Incidence_HZ_GP_only) %>% 
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
  select(Key, Age = age, r_mor_hz = Death_HZ) %>% 
  ungroup()


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
  select(Key, Age = age, r_hosp_hz = Hospitalisation_rate_HZ)


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



epi_hz_ic <- pick(Incidence_HZ, n_sample) %>% 
  left_join(pick(Incidence_HZ_GP_only, n_sample)) %>% 
  left_join(pick(Mortality_HZ, n_sample)) %>% 
  left_join(pick(Hospitalisation_HZ, n_sample)) %>% 
  left_join(pick(P_PHN, n_sample))


eligible <- epi_hz_ic %>% 
  filter(r_inc_hz > r_inc_hz_gp) %>% 
  filter(r_inc_hz > r_hosp_hz) %>%
  filter(r_inc_hz > r_mor_hz) %>%
  group_by(Key) %>% 
  summarise(eli = n()) %>% 
  ungroup() %>% 
  filter(eli == max(eli)) %>% 
  pull(Key)


epi_hz_ic <- tibble(Key = sample(eligible, n_sim), ID = 1:n_sim) %>% 
  left_join(epi_hz_ic) %>% 
  select(- Key) %>% 
  rename(Key = ID)

Epi_HZ <- epi_hz_ic
save(Epi_HZ, file = here::here("data", "Epi_IC.rdata"))


Epi_HZ <- bind_rows(
  epi_hz_ic %>% mutate(IC = 1),
  epi_hz_nic %>% mutate(IC = 0)
)

save(Epi_HZ, file = here::here("data", "Epi_HZ.rdata"))
