library(tidyverse)


folder_raw <- function(x) here::here("data", "raw", x)
folder_data <- function(x) here::here("data", x)


# AJ's old model
Pop_2015 <- read_csv(folder_raw("Background population AJ model.csv"), col_names = c("age", "Pop"))
Background_mortality <- read_csv(folder_raw("Background_mortality_old_AJ_model.csv")) %>% 
  rename(age = Age, Background_mortality = `Background mortality`)

Pop <- Pop_2015 %>% full_join(Background_mortality)


save(Pop, file = folder_data("Population_IC_AJ_2015.rdata"))
save(Pop, file = folder_data("Population_NIC_AJ_2015.rdata"))


# ONS 
Pop_2015 <- read_csv(file=folder_raw("midyearestimates2015 ONS population England.csv")) %>% 
  filter(Age != "All ages") %>% 
  rename(age = Age) %>% 
  mutate(
    Pop = as.numeric(gsub(",", "", Pop)),
    age = as.numeric(age)
  )


# ONS mid-year estimate population England 2015 by sex
Pop_male_female <- read_csv(file = folder_raw("Pop_england_estimates_mid_2015_male_females.csv")) %>% 
  select(age, males = `ENGLAND males`, females = `ENGLAND females`)

Background_mortality <- read_csv(file = folder_raw("Background_mortality_rates_2014_2016_projections_ONS.csv")) %>% 
  select(age, dr_males = `Male mortality rate`, dr_females = `Female mortality rate`) %>% 
  left_join(Pop_male_female) %>% 
  mutate(
    total = males + females,
    p_females = females / total,
    p_males = males / total,
    Background_mortality = dr_males * p_males + dr_females * p_females,
    Background_mortality = ifelse(age >= 100, 1, Background_mortality)
  ) %>% 
  select(age, Background_mortality)


Pop <- Pop_2015 %>% full_join(Background_mortality)
save(Pop, file = folder_data("Population_NIC_Ons_2015.rdata"))


# IC
Pop_IC <- read_csv(folder_raw("01-01-2019 zos denominator - IC - age vs year 1 year brackets 100y.csv"))


Pop_IC_2015 <- Pop_IC %>% 
  select(age = Age, Pop_IC = `2012/13`) %>% 
  filter(age != "all") %>% 
  mutate(
    age = ifelse(age == ">=100", 100, age),
    age = as.numeric(age)
  ) %>% 
  left_join(Pop_2015) %>% 
  mutate(
    p_IC = Pop_IC / Pop
  )


Pop <- Pop_IC_2015 %>% 
  select(age, Pop = Pop_IC) %>% 
  full_join(Background_mortality)


save(Pop, file = folder_data("Population_IC_Ons_2015.rdata"))
  

