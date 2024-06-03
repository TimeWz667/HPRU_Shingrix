library(tidyverse)


pick <- function(df, n) {
  ks <- df %>% pull(Key) %>% unique()
  
  tibble(ID = 1:n, Key = sample(ks, n, rep= T)) %>% 
    left_join(df, by = "Key", relationship = "many-to-many") %>% 
    select(-Key) %>% 
    rename(Key = ID)
}


n_sim <- 1000
n_sample <- 3000


#### Non IC ----

load(here::here("data", "processed_epi", "Epi_HZ_NIC_CPRD.rdata"))

epi_hz_nic <- pick(Incidence_HZ, n_sample) %>% 
  left_join(pick(Incidence_HZ_GP_only, n_sample)) %>% 
  left_join(pick(Mortality_HZ, n_sample)) %>% 
  left_join(pick(Hospitalisation_HZ, n_sample)) %>% 
  left_join(pick(P_PHN, n_sample)) %>% 
  rename(r_inc_hz = Incidence_HZ, r_inc_hz_gp = Incidence_HZ_GP_only, 
         r_mor_hz = Death_HZ, r_hosp_hz = r_hospitalisation_hz)


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
save(Epi_HZ, file = here::here("data", "processed_epi", "Epi_HZ_NIC_CPRD_bind.rdata"))


## IC

load(here::here("data", "processed_epi", "Epi_HZ_IC_CPRD.rdata"))

epi_hz_ic <- pick(Incidence_HZ, n_sample) %>% 
  left_join(pick(Incidence_HZ_GP_only, n_sample)) %>% 
  left_join(pick(Mortality_HZ, n_sample)) %>% 
  left_join(pick(Hospitalisation_HZ, n_sample)) %>% 
  left_join(pick(P_PHN, n_sample)) %>% 
  rename(r_inc_hz = Incidence_HZ, r_inc_hz_gp = Incidence_HZ_GP_only, 
         r_mor_hz = Death_HZ, r_hosp_hz = r_hospitalisation_hz)


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
save(Epi_HZ, file = here::here("data", "processed_epi", "Epi_HZ_IC_CPRD_bind.rdata"))


Epi_HZ <- bind_rows(
  epi_hz_ic %>% mutate(IC = 1),
  epi_hz_nic %>% mutate(IC = 0)
)

save(Epi_HZ, file = here::here("data", "processed_epi", "Epi_HZ_CPRD_bind.rdata"))
