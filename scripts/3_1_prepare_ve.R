library(tidyverse)



# load(here::here("outputs", "temp", "sims_coverage.rdata"))


ves_zosta_nic <- local({
  load(here::here("data", "VE_Zostavax_NIC.rdata"))
  
  bind_rows(lapply(18:100, function(va) {
    bind_rows(lapply(1:100, function(i) sample_ve(VE, vaccination_age = va))) %>% 
      group_by(Age = age) %>% 
      summarise(VE = mean(VE)) %>% 
      ungroup() %>% 
      mutate(
        AgeVac = va
      )
  })) %>% 
    mutate(
      TypeVac = "Zostavax",
      IC = F
    )
})

ves_zosta_ic <- ves_zosta_nic %>% mutate(IC = T)


ves_shingrix_ic <- local({
  load(here::here("data", "VE_Shingrix_IC.rdata"))
  
  bind_rows(lapply(18:100, function(va) {
    print(va)
    bind_rows(lapply(1:100, function(i) sample_ve(VE, vaccination_age = va))) %>% 
      group_by(Age = age) %>% 
      summarise(VE = mean(VE)) %>% 
      ungroup() %>% 
      mutate(
        AgeVac = va
      )
  })) %>% 
    mutate(
      TypeVac = "Shingrix",
      IC = T
    )
  
})


ves_shingrix_nic <- local({
  load(here::here("data", "VE_Shingrix_NIC.rdata"))
  
  bind_rows(lapply(18:100, function(va) {
    print(va)
    bind_rows(lapply(1:100, function(i) sample_ve(VE, vaccination_age = va))) %>% 
      group_by(Age = age) %>% 
      summarise(VE = mean(VE)) %>% 
      ungroup() %>% 
      mutate(
        AgeVac = va
      )
  })) %>% 
    mutate(
      TypeVac = "Shingrix",
      IC = F
    )
  
})


ves <- bind_rows(
  ves_zosta_ic,
  ves_zosta_nic,
  ves_shingrix_ic,
  ves_shingrix_nic
)

save(ves, file = here::here("pars", "ves.rdata"))
