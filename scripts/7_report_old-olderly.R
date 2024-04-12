library(tidyverse)

theme_set(theme_bw())



dir.create(here::here("outputs", "report_oo"), showWarnings = F)


output_file <- function(file) here::here("outputs", "report_oo", file)


## [Fig 1] Baseline vaccine-induced immunity in 2023. (epidemiology models) 

load(here::here("analysis_programme", "temp", "yss_soc.rdata"))

ys <- yss_soc[[1]]

tab <- ys %>% 
  filter(Year == 2023) %>% 
  filter(Age < 100) %>% 
  mutate(
    AgeGrp = cut(Age, c(0, 60, 70, 80, 90, 101), labels = c("<60", "60-69", "70-79", "80-89", "90+"), right = F)
  ) %>% 
  group_by(AgeGrp) %>% 
  summarise(
    N_Pop = sum(N),
    N_Vac = sum((Vaccine == "ZVL") * N),
    N_Unvac = N_Pop - N_Vac,
    N_Im = sum(Protection * N),
    Pr_Vac = weighted.mean((Vaccine == "ZVL"), N),
    Pr_Im = weighted.mean(Protection, N)
  ) %>% 
  ungroup() %>% 
  mutate(
    Prop_Pop = N_Pop / sum(N_Pop)
  )

g <- tab %>% 
  ggplot(aes(x = AgeGrp)) +
  geom_bar(aes(y = 1), stat = "identity", alpha = 0.3) +
  geom_bar(aes(y = Pr_Vac, fill = "Vaccinated"), stat = "identity", width = 0.8) +
  geom_bar(aes(y = Pr_Im, fill = "Immuned"), stat = "identity", width = 0.6) +
  geom_text(aes(y = 1, label = scales::percent(Prop_Pop)), vjust = -0.3) +
  scale_x_discrete("Age group") +
  scale_y_continuous("Proportion", labels = scales::percent) +
  scale_fill_discrete("Immune status")
  

ggsave(g, file = output_file("Fig1.png"), width = 6, height = 5)

## [Fig 2] Incremental cost-effectiveness ratios (ICERs) of RZV revaccination by age of vaccination. (cohort models) 

load(here::here("analysis_cohort", "temp", "yss_rzv.rdata"))

tab <- yss %>% 
  filter(Arm == "Vac") %>% 
  group_by(Age0) %>% 
  summarise(
    across(c(dC_All_d, dQ_All_d, ICER, NMB), list(
      M = median,
      L = function(x) quantile(x, 0.025),
      U = function(x) quantile(x, 0.975)
    ))
  )

g <- tab %>% 
  ggplot(aes(x = Age0)) +
  geom_ribbon(aes(ymin = ICER_L, ymax = ICER_U), alpha = 0.2) + 
  geom_line(aes(y = ICER_M)) + 
  geom_hline(yintercept = 2e4, linetype = 2) +
  geom_hline(yintercept = 3e4, linetype = 2) +
  scale_x_continuous("Age of vaccination", breaks = c(60, 70, seq(80, 90, 2), 95)) +
  scale_y_continuous("Incremental cost-effectiveness ratio, \ncost per QALY gained, GBP", 
                     breaks = 0:6 * 1e4, labels = scales::label_dollar(prefix = "")) +
  expand_limits(y = 0)


g
ggsave(g, file = output_file("Fig2.png"), width = 6, height = 4.5)
 
## [Fig 3] Incremental cost-effectiveness ratios (ICERs) of RZV revaccination for ZVL covered population by age of revaccination.  (cohort models)


load(here::here("analysis_cohort", "temp", "yss_zvl2rzv.rdata"))


tab <- yss %>% 
  filter(Arm == "ReVac") %>% 
  group_by(Age0, Age1, Type) %>% 
  summarise(
    across(c(dC_All_d, dQ_All_d, ICER, NMB), list(
      M = median,
      L = function(x) quantile(x, 0.025),
      U = function(x) quantile(x, 0.975)
    ))
  )


g <- tab %>% 
  filter(Type == "Second") %>% 
  filter(Age0 %in% c(70, 75)) %>%
  mutate(
    a0 = paste0("Age of ZVL vaccination: ", Age0)
  ) %>% 
  ggplot(aes(x = Age1)) +
  geom_ribbon(aes(ymin = ICER_L, ymax = ICER_U), alpha = 0.2) + 
  geom_line(aes(y = ICER_M)) + 
  geom_hline(yintercept = 2e4, linetype = 2) +
  geom_hline(yintercept = 3e4, linetype = 2) +
  scale_x_continuous("Age of re-vaccination", breaks = c(70, 75, seq(80, 90, 2), 95)) +
  scale_y_continuous("Incremental cost-effectiveness ratio, \ncost per QALY gained, GBP", 
                     breaks = 0:6 * 1e4, labels = scales::label_dollar(prefix = "")) +
  expand_limits(y = 0, x = 70) +
  facet_grid(.~a0)


g
ggsave(g, file = output_file("Fig3.png"), width = 7, height = 4.5)


## [Fig 4] Immunity gaps

load(file = here::here("analysis_cohort", "inputs", "pars_nic.rdata"))


ve <- pars_set$VE_RZV %>% 
  group_by(TimeVac) %>% 
  summarise(
    M = median(Protection),
    L = quantile(Protection, 0.025),
    U = quantile(Protection, 0.975)
  )


g <- crossing(Age = c(75, 80, 85, 90), AgeVac = c(60, 65, 70, 75)) %>% 
  mutate(
    TimeVac = Age - AgeVac - 1, 
    a0 = paste0("Age: ", Age)
  ) %>% 
  filter(TimeVac > 1) %>% 
  left_join(ve) %>% 
  ggplot() +
  geom_bar(aes(x = AgeVac, y = M), stat = "identity", alpha = 0.4) +
  geom_linerange(aes(x = AgeVac, ymin = L, ymax = U)) + 
  scale_y_continuous("Vaccine-induced immunity, %", labels = scales::percent) +
  scale_x_continuous("Age of vaccination, RZV") +
  facet_grid(.~a0) +
  expand_limits(y = 1)

ggsave(g, file = output_file("Fig4.png"), width = 10, height = 4.5)


## [Fig 5] Incremental cost-effectiveness ratios (ICERs) of RZV revaccination by age of revaccination. (cohort model) 

load(here::here("analysis_cohort", "temp", "yss_rzv2rzv.rdata"))


tab <- yss %>% 
  filter(Arm == "ReVac") %>% 
  group_by(Age0, Age1, Type) %>% 
  summarise(
    across(c(dC_All_d, dQ_All_d, ICER, NMB), list(
      M = median,
      L = function(x) quantile(x, 0.025),
      U = function(x) quantile(x, 0.975)
    ))
  )


g <- tab %>% 
  filter(Type == "Second") %>% 
  filter(Age0 %in% c(60, 65, 70, 75)) %>%
  filter(Age1 - Age0 > 10) %>% 
  mutate(
    a0 = paste0("Age of RZV vaccination: ", Age0)
  ) %>% 
  ggplot(aes(x = Age1)) +
  geom_ribbon(aes(ymin = ICER_L, ymax = ICER_U), alpha = 0.2) + 
  geom_line(aes(y = ICER_M)) + 
  geom_hline(yintercept = 2e4, linetype = 2) +
  geom_hline(yintercept = 3e4, linetype = 2) +
  scale_x_continuous("Age of re-vaccination", breaks = c(60, 65, 70, 75, seq(80, 90, 2), 95)) +
  scale_y_continuous("Incremental cost-effectiveness ratio, \ncost per QALY gained, GBP", 
                     breaks = c(0:3, 5, 10) * 1e4, labels = scales::label_dollar(prefix = "")) +
  expand_limits(y = 0, x = 60) +
  facet_wrap(a0~., ncol = 2)


ggsave(g, file = output_file("Fig5.png"), width = 10, height = 7.5)



## [Fig 6] Trend of 60+ incidence rates by intervention scenarios. (epidemiology models) 

root <- "analysis_programme"


load(here::here(root, "temp", "yss_soc.rdata"))
load(here::here(root, "temp", "yss_p1.rdata"))
load(here::here(root, "temp", "yss_p2.rdata"))
load(here::here(root, "temp", "yss_p1_95.rdata"))
load(here::here(root, "temp", "yss_p2_95.rdata"))


yss <- bind_rows(c(yss_soc, yss_p1, yss_p2, yss_p1_95, yss_p2_95)) %>% 
  mutate(
    Scenario = factor(Scenario, levels = c("SOC", "Phase 1 only", "Scheduled", "To 95 yr since 2028", "To 95 yr since 2033"))
  ) %>% 
  filter(Year >= 2020 & Year <= 2045) %>% 
  filter(Age >= 60) 


labs_scenarios <- list(
  "SOC" = "SOC", 
  "Phase 1 only" = "Phase 1 only", 
  "Scheduled" = "Scheduled", 
  "To 95 yr since 2028" = "Scheduled + 95 yo since 2028", 
  "To 95 yr since 2033" = "Scheduled + 95 yo since 2033"
)


g <- yss %>% 
  filter(Year <= 2045) %>% 
  group_by(Year, Scenario) %>% 
  summarise(
    Incidence = sum(N_HZ) / sum(N)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = Incidence, colour = Scenario)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2) + 
  scale_y_continuous("Incidence of HZ, 60+, per 100,000", labels = scales::number_format(scale = 1e5)) +
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040, 2045)) +
  expand_limits(y = c(0, 0.008))


g
ggsave(g, file = output_file("Fig6.png"), width = 6, height = 4.5)


## [Fig 7] The trend of the vaccinated and those with vaccine-induced immunity from 2023 by intervention scenarios. (epidemiology models) 

g0 <- yss %>% 
  filter(Year <= 2045) %>% 
  group_by(Year, Scenario) %>% 
  summarise(
    Vaccinated = sum(N * (Vaccine != "None")) / sum(N)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = Vaccinated, colour = Scenario)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2) + 
  scale_y_continuous("Vaccinated, 60+, %", labels = scales::percent)+
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040, 2045)) +
  expand_limits(y = c(0, 1))
g0


g1 <- yss %>% 
  filter(Year <= 2045) %>% 
  group_by(Year, Scenario) %>% 
  summarise(
    Vaccinated = sum(N * Protection) / sum(N)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = Vaccinated, colour = Scenario)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  #geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) + 
  #geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) + 
  #geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2) + 
  scale_y_continuous("Vaccine-induced immunity, 60+, %", labels = scales::percent)+
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040, 2045)) +
  expand_limits(y = c(0, 1))
g1

g <- ggpubr::ggarrange(g0 + labs(subtitle="(A)"), g1 + labs(subtitle="(B)"), ncol = 1, common.legend = T, legend = "right")

g

ggsave(g, file = output_file("Fig7.png"), width = 6, height = 7.5)


## [Fig 8] The impacts of providing 80-95 population RZV vaccination/revaccination by intervention scenarios. (epidemiology models) 
# (A) Accumulative case averted in proportion. 
# (B) Time-series of QALY gained by intervention scenarios. 
# (C) Time-series of costing. 
# (D) Incremental net monetary benefit, cumulating since the end of 2023.  



avt <- local({
  temp <- yss %>% 
    group_by(Year, Scenario, Key) %>% 
    summarise(
      NewUptake = sum(NewUptake), 
      Cases = sum(N_HZ),
      C_All_d = sum(C_All_d),
      Q_All_d = sum(Q_All_d),
      Q_Life_d = sum(Q_Life_d)
    ) %>% 
    group_by(Year, Scenario) %>%
    select(-Key) %>% 
    summarise_all(mean) %>% 
    arrange(Year) %>%
    group_by(Scenario) %>% 
    mutate(
      CumUptake = cumsum(NewUptake),
      CumCases = cumsum(Cases),
      CumCost = cumsum(C_All_d),
      CumQol = cumsum(Q_All_d)
    ) %>% 
    ungroup()
  
  temp <- temp %>% 
    left_join(temp %>% filter(Scenario == "SOC") %>% 
                select(Year, CumCases0 = CumCases, CumCost0 = CumCost, CumQol0 = CumQol, CumUptake0 = CumUptake,
                       C_All_d0 = C_All_d, Q_All_d0 = Q_All_d), by = "Year")
  
}) 


g_dose <- avt %>% 
  #filter(Year <= 2045) %>% 
  filter(Year >= 2023) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = CumUptake * 2, colour = Scenario)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2) + 
  scale_y_continuous("Cumulative doses, RZV, million", labels = scales::number_format(scale = 1e-6)) +
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040, 2045)) +
  scale_color_discrete("Scenario", labels = labs_scenarios) +
  expand_limits(y = 0, x = 2020)


g_avt_p <- avt %>% 
  filter(Year <= 2045) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = 1 - CumCases / CumCases0, colour = Scenario)) +
  scale_y_continuous("Cases averted, 60+, %", labels = scales::percent) +
  scale_color_discrete("Scenario", labels = labs_scenarios) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  # geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) + 
  # geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) + 
  # geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2)  +
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040, 2045))


g_dq_all <- avt %>% 
  filter(Year <= 2045) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = Q_All_d - Q_All_d0, colour = Scenario)) +
  scale_y_continuous("Incremental QALYs, 60+, discounted, thousands", labels = scales::number_format(scale = 1e-3)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2)  +
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040, 2045)) +
  scale_color_discrete("Scenario", labels = labs_scenarios) +
  expand_limits(y = 0) +
  labs(subtitle = "Breakdown by year")


g_cdq_all <- avt %>% 
  #filter(Year <= 2045) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = CumQol - CumQol0, colour = Scenario)) +
  scale_y_continuous("Incremental QALYs, \ndiscounted, thousands", labels = scales::number_format(scale = 1e-3)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) +
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) +
  geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2)  +
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040, 2045)) +
  scale_color_discrete("Scenario", labels = labs_scenarios) +
  expand_limits(y = 0)


g_dc_all <- avt %>% 
  filter(Year <= 2045) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = C_All_d - C_All_d0, colour = Scenario)) +
  scale_y_continuous("Incremental costs, discounted, million GBP", labels = scales::number_format(scale = 1e-6)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2)  +
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040, 2045)) +
  scale_color_discrete("Scenario", labels = labs_scenarios) +
  expand_limits(y = 0) +
  labs(subtitle = "Breakdown by year")


g_cdc_all <- avt %>% 
  #filter(Year <= 2045) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = CumCost - CumCost0, colour = Scenario)) +
  scale_y_continuous("Incremental costs, \ndiscounted, million GBP", labels = scales::number_format(scale = 1e-6)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  # geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) + 
  # geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) + 
  # geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2)  +
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040, 2045)) +
  scale_color_discrete("Scenario", labels = labs_scenarios) +
  expand_limits(y = 0)


g_nmb <- avt %>% 
  ggplot() +
  geom_line(aes(x = Year, y = (CumQol - CumQol0) * 3e4 - (CumCost - CumCost0), colour = Scenario)) +
  geom_hline(yintercept = 3e4, linetype = 3) +
  scale_y_continuous("INMB, million GBP", labels = scales::number_format(scale = 1e-6)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  # geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) + 
  # geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) + 
  # geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2)  +
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040, 2045)) +
  scale_color_discrete("Scenario", labels = labs_scenarios) +
  expand_limits(y = 0)


g <- ggpubr::ggarrange(g_cdq_all + labs(subtitle="(A)"), 
                        g_cdc_all + labs(subtitle="(B)"), 
                        g_nmb + labs(subtitle="(C)"), ncol = 1, common.legend = T, legend = "right")

g
ggsave(g, file = output_file("Fig8.png"), width = 6, height = 8)

