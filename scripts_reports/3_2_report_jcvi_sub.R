library(tidyverse)

theme_set(theme_bw())



dir.create(here::here("docs", "report_scm"), showWarnings = F)
output_file <- function(file) here::here("docs", "report_scm", file)


## Population data
pop <- local({
  load(here::here("data", "processed_demography", "Population_ONS.rdata"))
  demo_ons %>% 
    filter(Location == "England" & Year == 2023) %>% 
    select(Age, N)
  
})

## Vaccine CEA

load(here::here("out", "yss_rzv.rdata"))


yss_ce <- local({
  s0 <- yss %>% 
    filter(Arm == "SOC") %>% 
    select(Scenario, Age0, Key, Risk_HZ0 = Risk_HZ, Risk_Death0 = Risk_Death, 
           Q_HZ_d0 = Q_HZ_d, Q_Life_d0 = Q_Life_d, Q_All_d0 = Q_All_d,
           N_VacRZV_d0 = N_VacRZV_d,
           C_Vac_d0 = C_Vac_d, C_VacRZV_d0 = C_VacRZV_d, C_Med_d0 =  C_Med_d, C_All_d0 = C_All_d
    )
  
  yss %>% 
    filter(Arm == "Vac") %>% 
    select(Scenario, Age0, Arm, Key, N0 = N0, Risk_HZ, Risk_Death, N_VacRZV_d,
           Q_HZ_d, Q_Life_d, Q_All_d, C_Vac_d, C_VacRZV_d, C_Med_d, C_All_d) %>% 
    left_join(s0, by = c("Scenario", "Age0", "Key")) %>% 
    mutate(
      AvtHZ = (Risk_HZ0 - Risk_HZ) / Risk_HZ0,
      AvtDeath = (Risk_Death0 - Risk_Death) / Risk_Death0,
      dQ_HZ_d = Q_HZ_d - Q_HZ_d0, 
      dQ_Life_d = Q_Life_d - Q_Life_d0, 
      dQ_All_d = Q_All_d - Q_All_d0, 
      dC_Vac_d = C_Vac_d - C_Vac_d0, 
      dC_VacRZV_d = C_VacRZV_d - C_VacRZV_d0, 
      dC_Med_d = C_Med_d - C_Med_d0, 
      dC_All_d = C_All_d - C_All_d0,
      dN_VacRZV_d = N_VacRZV_d - N_VacRZV_d0
    ) %>% 
    select(Key, Scenario, Age0, Arm, N0, starts_with(c("Avt", "dQ", "dC", "dN")))
}) %>% 
  mutate(
    Agp = cut(Age0, seq(60, 100, 5), right = F)
  ) %>% 
  left_join(pop %>% select(Age0 = Age, N)) %>% 
  select(-Age0, -Scenario) %>% 
  group_by(Agp, Key) %>% 
  summarise(
    across(starts_with("d"), \(x) sum(x * N)),
    across(c(AvtHZ , AvtDeath), \(x) weighted.mean(x, w = N))
  ) %>% 
  mutate(
    ICER = dC_All_d / dQ_All_d,
    price = dC_VacRZV_d / dN_VacRZV_d,
    Thres20 = (dQ_All_d * 2e4 - dC_Med_d) / dN_VacRZV_d,
    Thres30 = (dQ_All_d * 3e4 - dC_Med_d) / dN_VacRZV_d
  )


tab_price <- bind_rows(lapply(seq(50, 100, 25), \(x) {
  yss_ce %>% 
    mutate(
      Price = x,
      dC_All_d = dC_All_d - dC_VacRZV_d + dN_VacRZV_d * Price,
      ICER = dC_All_d / dQ_All_d
    )
})) %>% 
  group_by(Agp, Price) %>% 
  summarise(
    across(everything(), list(
      M = median,
      L = function(x) quantile(x, 0.025),
      U = function(x) quantile(x, 0.975)
    ))
  )



g <- tab_price %>% 
  ggplot(aes(x = Agp)) +
  geom_pointrange(aes(y = ICER_M, ymin = ICER_L, ymax = ICER_U, colour = as.factor(Price)), position = position_dodge(0.3)) +
  geom_hline(yintercept = 2e4, linetype = 2) +
  geom_hline(yintercept = 3e4, linetype = 2) +
  scale_x_discrete("Age of vaccination") +
  scale_y_continuous("Incremental cost-effectiveness ratio, \ncost per QALY gained, GBP", 
                     breaks = 0:12 * 1e4, labels = scales::label_dollar(prefix = "")) +
  scale_color_discrete("Cost per admin.", guide = guide_legend(reverse = T)) +
  expand_limits(y = 0)


g

ggsave(g, file = output_file("Fig_RZV_CE.png"), width = 6, height = 4.5)

## Threshold price

tab_thres <- yss_ce %>% 
  group_by(Agp) %>% 
  summarise(
    across(everything(), list(
      M = median,
      L = function(x) quantile(x, 0.025),
      U = function(x) quantile(x, 0.975)
    ))
  ) 


g_tp <- tab_thres %>% 
  ggplot(aes(x = Agp)) +
  geom_point(aes(y = Thres20_M, colour = "CE given 20,000 WTP")) +
  geom_point(aes(y = Thres30_L, colour = "95% CE given 30,000 WTP")) +
  scale_y_continuous("Threshold price, per adminstration") +
  scale_x_discrete("Age of RZV vaccination") +
  scale_colour_discrete("Scenario") +
  expand_limits(y = c(0, 100))
  

g_tpr <- tab_thres %>% 
  ggplot(aes(x = Agp)) +
  geom_pointrange(aes(y = Thres20_M, ymin = Thres20_L, ymax = Thres20_U, colour = "20,000")) +
  geom_pointrange(aes(y = Thres30_M, ymin = Thres30_L, ymax = Thres30_U, colour = "30,000")) +
  scale_y_continuous("Threshold price, per adminstration") +
  scale_x_discrete("Age of RZV vaccination") +
  scale_colour_discrete("WTP in GBP") +
  expand_limits(y = 0)

  
ggsave(g_tp, file = output_file("Fig_RZV_ThresPrice.png"), width = 7, height = 4.5)
ggsave(g_tpr, file = output_file("Fig_RZV_ThresPriceRange.png"), width = 7, height = 4.5)



## [Fig 3] Incremental cost-effectiveness ratios (ICERs) of RZV revaccination for ZVL covered population by age of revaccination.  (cohort models)


load(here::here("out", "yss_zvl2rzv.rdata"))



yss_ce <- local({
  s0 <- yss %>% 
    filter(Scenario != "Overall") %>% 
    filter(Arm == "Vac") %>% 
    select(Scenario, Age0, Key, Risk_HZ0 = Risk_HZ, Risk_Death0 = Risk_Death, 
           Q_HZ_d0 = Q_HZ_d, Q_Life_d0 = Q_Life_d, Q_All_d0 = Q_All_d,
           N_VacRZV_d0 = N_VacRZV_d,
           C_Vac_d0 = C_Vac_d, C_VacRZV_d0 = C_VacRZV_d, C_Med_d0 =  C_Med_d, C_All_d0 = C_All_d
    )
  
  dy <- yss %>% 
    filter(Scenario != "Overall") %>% 
    filter(Arm %in% c("ReVac_RZV1", "ReVac_RZV2")) %>% 
    select(Scenario, Age0, Arm, Age1, Arm, Key, N0 = N0, Risk_HZ, Risk_Death, N_VacRZV_d,
           Q_HZ_d, Q_Life_d, Q_All_d, C_Vac_d, C_VacRZV_d, C_Med_d, C_All_d) %>% 
    left_join(s0, by = c("Scenario", "Age0", "Key")) %>% 
    mutate(Type = "Second")
  
  dy %>% 
    mutate(
      AvtHZ = (Risk_HZ0 - Risk_HZ) / Risk_HZ0,
      AvtDeath = (Risk_Death0 - Risk_Death) / Risk_Death0,
      dQ_HZ_d = Q_HZ_d - Q_HZ_d0, 
      dQ_Life_d = Q_Life_d - Q_Life_d0, 
      dQ_All_d = Q_All_d - Q_All_d0, 
      dC_Vac_d = C_Vac_d - C_Vac_d0, 
      dC_VacRZV_d = C_VacRZV_d - C_VacRZV_d0, 
      dC_Med_d = C_Med_d - C_Med_d0, 
      dC_All_d = C_All_d - C_All_d0,
      dN_VacRZV_d = N_VacRZV_d - N_VacRZV_d0,
      ICER = dC_All_d / dQ_All_d,
      price = dC_VacRZV_d / dN_VacRZV_d,
      Thres20 = (dQ_All_d * 2e4 - dC_Med_d) / dN_VacRZV_d,
      Thres30 = (dQ_All_d * 3e4 - dC_Med_d) / dN_VacRZV_d
    ) %>% 
    select(Key, Scenario, Age0, Age1, Arm, Type, N0, starts_with(c("Avt", "dQ", "dC", "dN", "Thres"))) 
}) %>% 
  filter(Age1 >= 80) %>% 
  mutate(
    Agp = cut(Age1, seq(60, 100, 5), right = F)
  ) %>% 
  left_join(pop %>% select(Age1 = Age, N)) %>% 
  select(-Age1, -Scenario) %>% 
  group_by(Age0, Agp, Arm, Key) %>% 
  summarise(
    across(starts_with("d"), \(x) sum(x * N / N0)),
    across(c(AvtHZ , AvtDeath), \(x) weighted.mean(x, w = N / N0))
  ) %>% 
  mutate(
    ICER = dC_All_d / dQ_All_d,
    price = dC_VacRZV_d / dN_VacRZV_d,
    Thres20 = (dQ_All_d * 2e4 - dC_Med_d) / dN_VacRZV_d,
    Thres30 = (dQ_All_d * 3e4 - dC_Med_d) / dN_VacRZV_d
  )



tab_price <- bind_rows(lapply(seq(50, 100, 25), \(x) {
  yss_ce %>% 
    mutate(
      Price = x,
      dC_All_d = dC_All_d - dC_VacRZV_d + dN_VacRZV_d * Price,
      ICER = dC_All_d / dQ_All_d
    )
})) %>% 
  group_by(Agp, Age0, Arm, Price) %>% 
  summarise(
    across(everything(), list(
      M = median,
      L = function(x) quantile(x, 0.025),
      U = function(x) quantile(x, 0.975)
    ))
  )


g <- tab_price %>% 
  filter(Age0 %in% c(70, 75)) %>% 
  mutate(
    a0 = paste0("Age of ZVL vaccination: ", Age0)
  ) %>%
  ggplot(aes(x = Agp)) +
  geom_pointrange(aes(y = ICER_M, ymin = ICER_L, ymax = ICER_U, colour = as.factor(Price)), position = position_dodge(0.4)) +
  geom_hline(yintercept = 2e4, linetype = 2) +
  geom_hline(yintercept = 3e4, linetype = 2) +
  scale_x_discrete("Age of re-vaccination") +
  scale_y_continuous("Incremental cost-effectiveness ratio, \ncost per QALY gained, GBP", 
                     breaks = 0:15 * 1e4, labels = scales::label_dollar(prefix = "")) +
  scale_color_discrete("Cost per admin.", guide = guide_legend(reverse = T)) +
  expand_limits(y = 0) +
  facet_grid(Arm~a0, labeller = labeller(Arm = c("ReVac_RZV1"="Single dose RZV", "ReVac_RZV2"="Two doses RZV")))


ggsave(g, file = output_file("Fig_ZVL2RZV_CE.png"), width = 9, height = 6.5)




tab_thres <- yss_ce %>% 
  filter(Age0 %in% c(70, 75)) %>% 
  mutate(
    a0 = paste0("Age of ZVL vaccination: ", Age0)
  ) %>%
  group_by(Agp, a0, Arm) %>% 
  summarise(
    across(everything(), list(
      M = median,
      L = function(x) quantile(x, 0.025),
      U = function(x) quantile(x, 0.975)
    ))
  ) 



g_tp <- tab_thres %>% 
  ggplot(aes(x = Agp)) +
  geom_point(aes(y = Thres20_M, colour = "CE given 20,000 WTP")) +
  geom_point(aes(y = Thres30_L, colour = "95% CE given 30,000 WTP")) +
  scale_y_continuous("Threshold price, per adminstration") +
  scale_x_discrete("Age of RZV vaccination") +
  scale_colour_discrete("Scenario") +
  expand_limits(y = c(0, 100)) +
  facet_grid(Arm~a0, labeller = labeller(Arm = c("ReVac_RZV1"="Single dose RZV", "ReVac_RZV2"="Two doses RZV")))


g_tpr <- tab_thres %>% 
  ggplot(aes(x = Agp)) +
  geom_pointrange(aes(y = Thres20_M, ymin = Thres20_L, ymax = Thres20_U, colour = "20,000")) +
  geom_pointrange(aes(y = Thres30_M, ymin = Thres30_L, ymax = Thres30_U, colour = "30,000")) +
  scale_y_continuous("Threshold price, per adminstration") +
  scale_x_discrete("Age of RZV vaccination") +
  scale_colour_discrete("WTP in GBP") +
  expand_limits(y = 0) +
  facet_grid(Arm~a0, labeller = labeller(Arm = c("ReVac_RZV1"="Single dose RZV", "ReVac_RZV2"="Two doses RZV")))



ggsave(g_tp, file = output_file("Fig_ZVL2RZV_ThresPrice.png"), width = 9, height = 6.5)
ggsave(g_tpr, file = output_file("Fig_ZVL2RZV_ThresPriceRange.png"), width = 9, height = 6.5)


