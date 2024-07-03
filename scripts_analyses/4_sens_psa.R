library(tidyverse)

theme_set(theme_bw())



ve_type <- "realworld"
ve_type <- glue::as_glue(ve_type)

load(file = here::here("out", "yss_rzv_" + ve_type + ".rdata"))



yss_diff <- local({
  temp <- yss %>% 
    pivot_longer(-c(Scenario, Age0, Arm, Key, N0, Year0), names_to = "Index")
  
  temp %>% 
    filter(Arm != "SOC") %>% 
    left_join(
      temp %>% 
        filter(Arm == "SOC") %>% 
        select(Scenario, Age0, Key, Index, value0 = value),
      relationship = "many-to-many"
    ) %>% 
    mutate(
      Index = paste0("d", Index),
      Diff = value - value0
    ) %>% 
    select(-value, -value0) %>% 
    pivot_wider(names_from = Index, values_from = "Diff") %>% 
    mutate(
      ICER = dC_All_d / dQ_All_d,
      Price0 = dC_VacRZV_d / dN_VacRZV_d,
      Thres20 = (dQ_All_d * 2e4 - dC_Med_d) / dN_VacRZV_d,
      Thres30 = (dQ_All_d * 3e4 - dC_Med_d) / dN_VacRZV_d,
    )
  
}) %>% 
  filter(Age0 == 70 & Arm == "Vac")





tibble(Price = seq(0, 200, 5)) %>% 
  mutate(
    PrCE20 = sapply(Price, \(x) mean(yss_diff$Thres20 > x)),
    PrCE30 = sapply(Price, \(x) mean(yss_diff$Thres30 > x))
  ) %>% 
  ggplot() +
  geom_line(aes(x = Price, y = PrCE20, colour = "WTP20k")) +
  geom_line(aes(x = Price, y = PrCE30, colour = "WTP30k"))




