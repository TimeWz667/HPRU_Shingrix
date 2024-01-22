library(tidyverse)


folder_raw <- function(x) here::here("data", "raw", x)
folder_data <- function(x) here::here("data", "processed_demography", x)


# AJ's old model
Pop_2015 <- read_csv(folder_raw("Background population AJ model.csv"), col_names = c("age", "Pop"))
Background_mortality <- read_csv(folder_raw("Background_mortality_old_AJ_model.csv")) %>% 
  rename(age = Age, Background_mortality = `Background mortality`)

Pop <- Pop_2015 %>% full_join(Background_mortality)


save(Pop, file = folder_data("Population_IC_AJ_2015.rdata"))
save(Pop, file = folder_data("Population_NIC_AJ_2015.rdata"))
