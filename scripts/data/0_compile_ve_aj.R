library(tidyverse)


folder_raw <- function(x) here::here("data", "raw", x)
folder_data <- function(x) here::here("data", "processed_vaccine", x)


## VE sampler
sample_ve <- function(obj_ve, vaccination_age = 70, vid = NA, ...) {
  UseMethod("sample_ve")
}


#### AJ zostavax ----
VE <- read_csv(folder_raw("VE_old_AJ_model.csv")) %>% 
  full_join(tibble(age = 0:100), by = "age") %>% 
  arrange(age) %>% 
  fill(VE, .direction = "updown")

VE <- list(src = VE)
class(VE) <- "ve_aj"


sample_ve.ve_aj <- function(obj_ve, vaccination_age = 70, ...) {
  obj_ve$src
}


save(VE, sample_ve, sample_ve.ve_aj, file = folder_data("VE_Zostavax_NIC_AJ.rdata"))
save(VE, sample_ve, sample_ve.ve_aj, file = folder_data("VE_Zostavax_IC_AJ.rdata"))
