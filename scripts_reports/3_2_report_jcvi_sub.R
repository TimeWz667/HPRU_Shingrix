library(tidyverse)

theme_set(theme_bw())



dir.create(here::here("docs", "report_scm"), showWarnings = F)

output_file <- function(file) here::here("docs", "report_scm", file)


## Vaccine CEA

load(here::here("out", "yss_rzv.rdata"))

tab <- yss %>% 
  filter(Arm == "Vac") %>% 
  filter(Age0 >= 70) %>% 
  mutate(
    Agp = cut(Age0, seq(70, 100, 5), right = F)
  ) %>% 
  group_by(Agp, Key) %>% 
  summarise(across(c(dC_All_d, dQ_All_d), sum)) %>% 
  mutate(ICER = dC_All_d / dQ_All_d) %>% 
  group_by(Agp) %>% 
  summarise(
    across(c(dC_All_d, dQ_All_d, ICER), list(
      M = median,
      L = function(x) quantile(x, 0.025),
      U = function(x) quantile(x, 0.975)
    ))
  )


g <- tab %>% 
  ggplot(aes(x = Agp)) +
  geom_pointrange(aes(y = ICER_M, ymin = ICER_L, ymax = ICER_U)) +
  geom_hline(yintercept = 2e4, linetype = 2) +
  geom_hline(yintercept = 3e4, linetype = 2) +
  scale_x_discrete("Age of vaccination") +
  scale_y_continuous("Incremental cost-effectiveness ratio, \ncost per QALY gained, GBP", 
                     breaks = 0:9 * 1e4, labels = scales::label_dollar(prefix = "")) +
  expand_limits(y = 0)


g

ggsave(g, file = output_file("Fig_CEA_RZV.png"), width = 6, height = 4.5)

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
  filter(Age0 >= 70) %>% 
  #filter(Age0 < 95) %>% 
  select(Key, Age0, N_NewUptake, C_Vac_d, C_Med_d, C_All_d, C_All_d0, dC_All_d, dQ_All_d) %>% 
  mutate(
    Age0 = cut(Age0, seq(60, 100, 5), right = F)
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
  scale_colour_discrete("WTP in GBP") +
  expand_limits(y = 0) +
  labs(caption = "Two doses per person; 10 GBP per vaccine administration")


gs$g_tp_rzv_agp <- TresPriceAgp %>% 
  ggplot(aes(x = Age0)) +
  geom_pointrange(aes(y = TresP3E4_RZV_M, ymin = TresP3E4_RZV_L, ymax = TresP3E4_RZV_U, colour = "30,000")) +
  geom_pointrange(aes(y = TresP2E4_RZV_M, ymin = TresP2E4_RZV_L, ymax = TresP2E4_RZV_U, colour = "20,000")) +
  geom_hline(yintercept = 75, linetype = 2) +
  scale_y_continuous("Threshold price, per dose") +
  scale_x_discrete("Age of RZV vaccination") +
  scale_colour_discrete("WTP in GBP") +
  expand_limits(y = 0) +
  labs(caption = "Two doses per person; 10 GBP per vaccine administration")


gs$g_tp_vac_agp
gs$g_tp_rzv_agp


ggsave(gs$g_tp_rzv_agp, file = output_file("Fig_ThresRzvAgp.png"), width = 6, height = 4.5)
ggsave(gs$g_tp_vac_agp, file = output_file("Fig_ThresVacAgp.png"), width = 6, height = 4.5)
ggsave(gs$g_tp_rzv, file = output_file("Fig_ThresRzv.png"), width = 6, height = 4.5)
ggsave(gs$g_tp_vac, file = output_file("Fig_ThresVac.png"), width = 6, height = 4.5)



## 
## Build-up single dose scenarios
## Age-groups or initial age of each group

## [Fig 3] Incremental cost-effectiveness ratios (ICERs) of RZV revaccination for ZVL covered population by age of revaccination.  (cohort models)



tab <- bind_rows(local({
  load(here::here("analysis_cohort", "temp", "yss_zvl2rzv_single.rdata"))
  
  yss %>% 
    filter(Type == "Second_d") %>% 
    filter(Arm == "ReVac") %>% 
    filter(Age0 %in% c(70, 75)) %>%
    filter(Age1 %in% seq(80, 95, 5)) %>% 
    group_by(Age0, Age1, Type) %>% 
    summarise(
      across(c(N_NewUptake, dC_All_d, dQ_All_d, ICER), list(
        M = median,
        L = function(x) quantile(x, 0.025),
        U = function(x) quantile(x, 0.975)
      ))
    ) %>% 
    mutate(Dose = "Single dose")
}), local({
  load(here::here("analysis_cohort", "temp", "yss_zvl2rzv.rdata"))
  
  yss %>% 
    filter(Type == "Second_d") %>% 
    filter(Arm == "ReVac") %>% 
    filter(Age0 %in% c(70, 75)) %>%
    filter(Age1 %in% seq(80, 95, 5)) %>% 
    group_by(Age0, Age1, Type) %>% 
    summarise(
      across(c(N_NewUptake, dC_All_d, dQ_All_d, ICER), list(
        M = median,
        L = function(x) quantile(x, 0.025),
        U = function(x) quantile(x, 0.975)
      ))
    ) %>% 
    mutate(Dose = "Two doses")
}))
  



g <- tab %>% 
  mutate(
    a0 = paste0("Age of ZVL vaccination: ", Age0),
    Dose = factor(Dose, rev(unique(Dose)))
  ) %>%
  ggplot(aes(x = Age1)) +
  geom_pointrange(aes(y = ICER_M, ymin = ICER_L, ymax = ICER_U, colour = Dose), position = "dodge") +
  geom_hline(yintercept = 2e4, linetype = 2) +
  geom_hline(yintercept = 3e4, linetype = 2) +
  scale_x_continuous("Age of re-vaccination", breaks = seq(70, 95, 5)) +
  scale_y_continuous("Incremental cost-effectiveness ratio, \ncost per QALY gained, GBP", 
                     breaks = 0:9 * 1e4, labels = scales::label_dollar(prefix = "")) +
  expand_limits(y = 0, x = 80) +
  facet_grid(.~a0)


g
ggsave(g, file = output_file("Fig_ReVac.png"), width = 7, height = 4.5)





tab <- bind_rows(local({
  load(here::here("analysis_cohort", "temp", "yss_zvl2rzv_single.rdata"))
  
  yss %>% 
    filter(Type == "Second_d") %>% 
    filter(Arm == "ReVac") %>% 
    filter(Age0 %in% c(70, 75)) %>%
    filter(Age1 %in% seq(80, 95, 5)) %>% 
    group_by(Age0, Age1, Type) %>% 
    mutate(Dose = "Single dose")
}), local({
  load(here::here("analysis_cohort", "temp", "yss_zvl2rzv.rdata"))
  
  yss %>% 
    filter(Type == "Second_d") %>% 
    filter(Arm == "ReVac") %>% 
    filter(Age0 %in% c(70, 75)) %>%
    filter(Age1 %in% seq(80, 95, 5)) %>% 
    mutate(Dose = "Two doses")
}))


TresPriceAgp <- tab %>% 
  select(Key, Age0, Age1, Dose, C_Vac_d, C_Med_d, C_All_d, C_All_d0, dC_All_d, dQ_All_d) %>% 
  mutate(
    price0 = 75,
    doses = ifelse(Dose == "Two doses", 2, 1),
    c_vac_pp = (price0 + 10) * doses,
    wtp = 2e4,
    C_Vac_d2e4 = dQ_All_d * wtp + C_All_d0 - C_Med_d,
    TresP2E4_Vac =  c_vac_pp * C_Vac_d2e4 / C_Vac_d,
    TresP2E4_RZV = TresP2E4_Vac / doses - 10,
    wtp = 3e4,
    C_Vac_d3e4 = dQ_All_d * wtp + C_All_d0 - C_Med_d,
    TresP3E4_Vac =  c_vac_pp * C_Vac_d3e4 / C_Vac_d,
    TresP3E4_RZV = TresP3E4_Vac / doses - 10
  ) %>% 
  group_by(Age0, Age1, Type, Dose) %>% 
  summarise(
    across(starts_with("TresP"), list(
      M = median,
      L = function(x) quantile(x, 0.025),
      U = function(x) quantile(x, 0.975)
    ))
  ) %>% 
  mutate(
    a0 = paste0("Age of ZVL vaccination: ", Age0),
    Dose = factor(Dose, unique(Dose))
  ) 
  


g_tp_vac_agp <- TresPriceAgp %>% 
  ggplot(aes(x = Age1)) +
  geom_pointrange(aes(y = TresP3E4_Vac_M, ymin = TresP3E4_Vac_L, ymax = TresP3E4_Vac_U, colour = "30,000")) +
  geom_pointrange(aes(y = TresP2E4_Vac_M, ymin = TresP2E4_Vac_L, ymax = TresP2E4_Vac_U, colour = "20,000")) +
  geom_hline(data = tibble(Dose = c("Two doses", "Single dose"), y = c(170, 85)), aes(yintercept = y), linetype = 2) +
  scale_y_continuous("Threshold price, per dose") +
  scale_x_continuous("Age of re-vaccination") +
  scale_colour_discrete("WTP in GBP") +
  expand_limits(y = 0) +
  labs(caption = "10 GBP per vaccine administration") +
  facet_grid(Dose~a0)


g_tp_rzv_agp <- TresPriceAgp %>% 
  ggplot(aes(x = Age1)) +
  geom_pointrange(aes(y = TresP3E4_RZV_M, ymin = TresP3E4_RZV_L, ymax = TresP3E4_RZV_U, colour = "30,000")) +
  geom_pointrange(aes(y = TresP2E4_RZV_M, ymin = TresP2E4_RZV_L, ymax = TresP2E4_RZV_U, colour = "20,000")) +
  geom_hline(yintercept = 75, linetype = 2) +
  scale_y_continuous("Threshold price, per dose") +
  scale_x_continuous("Age of re-vaccination") +
  scale_colour_discrete("WTP in GBP") +
  expand_limits(y = 0) +
  labs(caption = "10 GBP per vaccine administration") +
  facet_grid(Dose~a0)

g_tp_vac_agp

g_tp_rzv_agp

ggsave(g_tp_rzv_agp, file = output_file("Fig_ThresReRzvAgp_Re.png"), width = 6, height = 4.5)
ggsave(g_tp_vac_agp, file = output_file("Fig_ThresVacAgp_Re.png"), width = 6, height = 4.5)

