library(tidyverse)

theme_set(theme_bw())


load(here::here("analysis_cohort", "temp", "yss_rzv.rdata"))

dir.create(here::here("outputs", "report_scm"), showWarnings = F)

output_file <- function(file) here::here("outputs", "report_scm", file)




## Threshold price

TresPrice <- yss %>% 
  filter(Arm == "Vac") %>% 
  select(Arm, Age0, C_Vac_d, C_Med_d, C_All_d, C_All_d0, dC_All_d, dQ_All_d) %>% 
  mutate(
    price0 = 75,
    c_vac_pp = (price0 + 10) * 2,
    wtp = 2e4,
    C_Vac_d2e4 = dQ_All_d * wtp + C_All_d0,
    TresP2E4_Vac =  c_vac_pp * C_Vac_d2e4 / C_Vac_d,
    TresP2E4_RZV = TresP2E4_Vac / 2 - 10,
    wtp = 3e4,
    C_Vac_d3e4 = dQ_All_d * wtp + C_All_d0,
    TresP3E4_Vac =  c_vac_pp * C_Vac_d3e4 / C_Vac_d,
    TresP3E4_RZV = TresP3E4_Vac / 2 - 10
  ) %>% 
  group_by(Arm, Age0) %>% 
  summarise(
    across(starts_with("TresP"), list(
      M = median,
      L = function(x) quantile(x, 0.025),
      U = function(x) quantile(x, 0.975)
    ))
  ) 


TresPriceAgp <- yss %>% 
  filter(Arm == "Vac") %>% 
  filter(Age0 < 95) %>% 
  select(Key, Age0, N_NewUptake, C_Vac_d, C_Med_d, C_All_d, C_All_d0, dC_All_d, dQ_All_d) %>% 
  mutate(
    Age0 = cut(Age0, seq(60, 95, 5), right = F)
  ) %>% 
  group_by(Key, Age0) %>% 
  summarise(
    across(-N_NewUptake, function(x) weighted.mean(x, w = N_NewUptake))
  ) %>% 
  mutate(
    price0 = 75,
    c_vac_pp = (price0 + 10) * 2,
    wtp = 2e4,
    C_Vac_d2e4 = dQ_All_d * wtp + C_All_d0,
    TresP2E4_Vac =  c_vac_pp * C_Vac_d2e4 / C_Vac_d,
    TresP2E4_RZV = TresP2E4_Vac / 2 - 10,
    wtp = 3e4,
    C_Vac_d3e4 = dQ_All_d * wtp + C_All_d0,
    TresP3E4_Vac =  c_vac_pp * C_Vac_d3e4 / C_Vac_d,
    TresP3E4_RZV = TresP3E4_Vac / 2 - 10
  ) %>% 
  group_by(Age0) %>% 
  summarise(
    across(starts_with("TresP"), list(
      M = median,
      L = function(x) quantile(x, 0.025),
      U = function(x) quantile(x, 0.975)
    ))
  ) 



gs <- list()

gs$g_tp_vac <- TresPrice %>% 
  ggplot(aes(x = Age0)) +
  geom_ribbon(aes(ymin = TresP3E4_Vac_L, ymax = TresP3E4_Vac_U, fill = "30,000"), alpha = 0.4) +
  geom_line(aes(y = TresP3E4_Vac_M, colour = "30,000")) +
  geom_ribbon(aes(ymin = TresP2E4_Vac_L, ymax = TresP2E4_Vac_U, fill = "20,000"), alpha = 0.4) +
  geom_line(aes(y = TresP2E4_Vac_M, colour = "20,000")) +
  geom_hline(yintercept = 170, linetype = 2) +
  scale_y_continuous("Threshold price, per vaccinated person") +
  scale_x_continuous("Age of RZV vaccination", breaks = seq(60, 95, 5)) +
  scale_fill_discrete("WTP in GBP") +
  guides(colour = guide_none()) +
  expand_limits(y = 0) +
  labs(caption = "Two doses per person; 10 GBP per vaccine administration")


gs$g_tp_rzv <- TresPrice %>% 
  ggplot(aes(x = Age0)) +
  geom_ribbon(aes(ymin = TresP3E4_RZV_L, ymax = TresP3E4_RZV_U, fill = "30,000"), alpha = 0.4) +
  geom_line(aes(y = TresP3E4_RZV_M, colour = "30,000")) +
  geom_ribbon(aes(ymin = TresP2E4_RZV_L, ymax = TresP2E4_RZV_U, fill = "20,000"), alpha = 0.4) +
  geom_line(aes(y = TresP2E4_RZV_M, colour = "20,000")) +
  geom_hline(yintercept = 75, linetype = 2) +
  scale_y_continuous("Threshold price, per dose") +
  scale_x_continuous("Age of RZV vaccination", breaks = seq(60, 95, 5)) +
  scale_fill_discrete("WTP in GBP") +
  guides(colour = guide_none()) +
  expand_limits(y = 0) +
  labs(caption = "Two doses per person; 10 GBP per vaccine administration")



gs$g_tp_vac_agp <- TresPriceAgp %>% 
  ggplot(aes(x = Age0)) +
  geom_pointrange(aes(y = TresP3E4_Vac_M, ymin = TresP3E4_Vac_L, ymax = TresP3E4_Vac_U, colour = "30,000")) +
  geom_pointrange(aes(y = TresP2E4_Vac_M, ymin = TresP2E4_Vac_L, ymax = TresP2E4_Vac_U, colour = "20,000")) +
  geom_hline(yintercept = 170, linetype = 2) +
  scale_y_continuous("Threshold price, per vaccinated person") +
  scale_x_discrete("Age of RZV vaccination") +
  scale_fill_discrete("WTP in GBP") +
  guides(colour = guide_none()) +
  expand_limits(y = 0) +
  labs(caption = "Two doses per person; 10 GBP per vaccine administration")


gs$g_tp_rzv_agp <- TresPriceAgp %>% 
  ggplot(aes(x = Age0)) +
  geom_pointrange(aes(y = TresP3E4_RZV_M, ymin = TresP3E4_RZV_L, ymax = TresP3E4_RZV_U, colour = "30,000")) +
  geom_pointrange(aes(y = TresP2E4_RZV_M, ymin = TresP2E4_RZV_L, ymax = TresP2E4_RZV_U, colour = "20,000")) +
  geom_hline(yintercept = 75, linetype = 2) +
  scale_y_continuous("Threshold price, per dose") +
  scale_x_discrete("Age of RZV vaccination") +
  scale_fill_discrete("WTP in GBP") +
  guides(colour = guide_none()) +
  expand_limits(y = 0) +
  labs(caption = "Two doses per person; 10 GBP per vaccine administration")


gs$g_tp_rzv
gs$g_tp_rzv_agp


ggsave(gs$g_tp_rzv_agp, file = output_file("Fig_ThresRzvAgp.png"), width = 6, height = 4.5)
ggsave(gs$g_tp_vac_agp, file = output_file("Fig_ThresVacAgp.png"), width = 6, height = 4.5)
ggsave(gs$g_tp_rzv, file = output_file("Fig_ThresRzv.png"), width = 6, height = 4.5)
ggsave(gs$g_tp_vac, file = output_file("Fig_ThresVac.png"), width = 6, height = 4.5)



## 
## Build-up single dose scenarios
## Age-groups or initial age of each group


## List of figures -----
## Fig 1 
## ICER and Threshold price
## Fig 2
## ZVL -> RZV (single dose)
## Fig 3
## RZV -> RZV (double -> single dose)


## -----
## Extra Fig 1
## CE plane
## Extra Fig 2
## ZVL -> RZV (two doses)
## Extra Fig 3
## RZV -> RZV (two doses)








