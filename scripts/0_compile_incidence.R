library(tidyverse)


folder_raw <- function(x) here::here("data", "raw", x)
folder_data <- function(x) here::here("data", x)



### Incidence HZ, IC ----

#### AJs model ----
Incidence_HZ <- local({
  df <- read_csv(file = folder_raw("Incidence_HZ_old_AJ_model.csv")) %>% 
    rename(age = Age, Incidence_HZ = Incidence)
  
  # false positives when determining incidence
  p_Incidence_false_positives <- 0.05
  # proportion of HZ cases in IC
  p_Incidence_IC <- 0.089523809524
  
  df %>% 
    full_join(tibble(age = 50:100)) %>% 
    arrange(age) %>%
    fill(Incidence_HZ, .direction = "updown") %>% 
    mutate(
      Incidence_HZ_nIC = Incidence_HZ * (1-p_Incidence_false_positives)*(1-p_Incidence_IC)
    )
}) %>% 
  select(age, p_HZ = Incidence_HZ_nIC) %>% 
  mutate(
    p_HZ_GP_only = p_HZ
  )

save(Incidence_HZ, file = folder_data("Incidence_HZ_AJ.rdata"))


## Data Nick
#Incidence_HZ<-read.csv(file=paste(Data,"Incidence_RCGP_2017_Nick.csv", sep=""))
## Data Anu-> Incidence in the non-IC
# Incidence_HZ<-read.csv(file=paste(Data,"clean_df_o90_grouped.csv", sep=""), row.names = 1)
# Incidence_HZ_coef<-read.csv(file=paste(Data, "sim_coef_age2degreepoly_1000.csv", sep=""), row.names=1)
## Data Jemma CPRD supplemented by HES

### Incidence NonIC
Incidence_HZ <- read_csv(folder_raw("sim_coef_age2degreepoly_nb_nIC_1000.csv"))[-1] %>% 
  rename(beta_age = age, beta_age2 = age2) %>%
  mutate(Key = 1:n()) %>% 
  crossing(tibble(age = 0:100)) %>% 
  arrange(Key, age) %>% 
  mutate(
    Incidence_HZ = exp(Intercept + age * beta_age + age ^ 2 * beta_age2),
    Incidence_HZ = ifelse(age >= 50, Incidence_HZ, NA)
  ) %>% 
  fill(Incidence_HZ, .direction = "updown") %>% 
  mutate(
    p_HZ = 1 - exp(-Incidence_HZ)
  ) %>% 
  select(Key, age, Incidence_HZ, p_HZ)


Incidence_HZ_GP_only <- read_csv(folder_raw("sim_coef_age2degreepoly_nb_nIC_onlyGP_1000.csv"))[-1] %>% 
  rename(beta_age = age, beta_age2 = age2) %>%
  mutate(Key = 1:n()) %>% 
  crossing(tibble(age = 0:100)) %>% 
  arrange(Key, age) %>% 
  mutate(
    Incidence_HZ_GP_only = exp(Intercept + age * beta_age + age ^ 2 * beta_age2),
    Incidence_HZ_GP_only = ifelse(age >= 50, Incidence_HZ_GP_only, NA)
  ) %>% 
  fill(Incidence_HZ_GP_only, .direction = "updown") %>% 
  mutate(
    p_HZ_GP_only = 1 - exp(-Incidence_HZ_GP_only)
  ) %>% 
  select(Key, age, Incidence_HZ_GP_only, p_HZ_GP_only)


save(Incidence_HZ, Incidence_HZ_GP_only, file = folder_data("Incidence_HZ_IC.rdata"))



### Incidence IC
Incidence_HZ <- read_csv(folder_raw("sim_coef_age2degreepoly_pois_IC_1000.csv"))[-1] %>% 
  rename(beta_age = age, beta_age2 = age2) %>%
  mutate(Key = 1:n()) %>% 
  crossing(tibble(age = 0:100)) %>% 
  arrange(Key, age) %>% 
  mutate(
    Incidence_HZ = exp(Intercept + age * beta_age + age ^ 2 * beta_age2),
    Incidence_HZ = ifelse(age >= 50 & age <= 95, Incidence_HZ, NA)
  ) %>% 
  fill(Incidence_HZ, .direction = "updown") %>% 
  mutate(
    p_HZ = 1 - exp(-Incidence_HZ)
  ) %>% 
  select(Key, age, Incidence_HZ, p_HZ)


Incidence_HZ_GP_only <- read_csv(folder_raw("sim_coef_age2degreepoly_pois_IC_onlyGP_1000.csv"))[-1] %>% 
  rename(beta_age = age, beta_age2 = age2) %>%
  mutate(Key = 1:n()) %>% 
  crossing(tibble(age = 0:100)) %>% 
  arrange(Key, age) %>% 
  mutate(
    Incidence_HZ_GP_only = exp(Intercept + age * beta_age + age ^ 2 * beta_age2),
    Incidence_HZ_GP_only = ifelse(age >= 50 & age <= 95, Incidence_HZ_GP_only, NA)
  ) %>% 
  fill(Incidence_HZ_GP_only, .direction = "updown") %>% 
  mutate(
    p_HZ_GP_only = 1 - exp(-Incidence_HZ_GP_only)
  ) %>% 
  select(Key, age, Incidence_HZ_GP_only, p_HZ_GP_only)


save(Incidence_HZ, Incidence_HZ_GP_only, file = folder_data("Incidence_HZ_NIC.rdata"))


