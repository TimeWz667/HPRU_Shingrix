library(tidyverse)

theme_set(theme_bw())


root <- "analysis_programme"


load(here::here(root, "temp", "yss_soc.rdata"))
load(here::here(root, "temp", "yss_p1.rdata"))
load(here::here(root, "temp", "yss_p2.rdata"))
load(here::here(root, "temp", "yss_p1_95.rdata"))
load(here::here(root, "temp", "yss_p2_95.rdata"))


yss <- bind_rows(c(yss_soc, yss_p1, yss_p2, yss_p1_95, yss_p2_95)) %>% 
  mutate(
    Scenario = factor(Scenario, levels = c("SOC", "Phase 1 only", "Scheduled", "To 95 yr since 2028", "To 95 yr since 2033"))
  )


labs_scenarios <- list(
  "SOC" = "SOC", 
  "Phase 1 only" = "Phase 1 only", 
  "Scheduled" = "Scheduled", 
  "To 95 yr since 2028" = "Scheduled + 95 yo since 2028", 
  "To 95 yr since 2033" = "Scheduled + 95 yo since 2033"
)


stats <- yss %>% 
  filter(Age >= 60) %>% 
  mutate(NewUptake = ifelse(is.na(NewUptake), 0, NewUptake)) %>% 
  group_by(Year, Age, Scenario, Key) %>%
  summarise(
    across(c(N, starts_with("HZ"), NewUptake), sum)
  ) %>% 
  group_by(Year, Age, Scenario) %>% 
  summarise(
    across(c(N, starts_with("HZ"), NewUptake), mean)
  ) %>% 
  ungroup()



load(here::here("data", "processed_ce", "Cost_Hospitalisation_NIC.rdata"))
load(here::here("data", "processed_ce", "Cost_GP_Gauthier.rdata"))
load(here::here("data", "processed_ce", "QOL_LE.rdata"))
cost_vac <- read_csv(here::here("data", "processed_ce", "Cost_Vac.csv"))

pars_ce <- QL %>% 
  select(- Key) %>%
  group_by(Age) %>% 
  summarise(across(everything(), mean)) %>% 
  left_join(Cost_Hospitalisation_HZ %>% select(- Key)) %>% 
  bind_cols(Cost_GP %>% select(- Key) %>% summarise(across(everything(), mean)))


### discount rate costs
discount_rate_costs <- 0.035
### discount rate effects
discount_rate_effects <- 0.035


stats_ce <- stats %>% 
  left_join(pars_ce) %>% 
  filter(Year >= 2022) %>% 
  mutate(
    dis_ql = 1 / ((1 + discount_rate_effects)^ (Year - 2023)),
    dis_cost = 1 / ((1 + discount_rate_costs)^ (Year - 2023)),
    QL_y2_d = QL_y2 / (1 + discount_rate_effects),
    QL_HZ = QL_y1 + QL_y2,
    QL_HZ_d = QL_y1 + QL_y2_d,
    Q_Life = N * QOL,
    Q_HZ = - HZ * QL_HZ,
    Q_All = Q_Life + Q_HZ,
    Q_Life_d = Q_Life * dis_ql,
    Q_HZ_d = - HZ * QL_HZ_d * dis_ql,
    Q_All_d = Q_Life_d + Q_HZ_d,
    C_Hosp = HZ_Hosp * cost_Hospitalisation_pp_inf,
    C_GP_NonPHN = (HZ_GP - HZ_PHN_GP) * cost_GP_pp_non_PHN_HZ_inf,
    C_GP_PHN = HZ_PHN_GP * cost_GP_pp_PHN_inf,
    C_GP = C_GP_NonPHN + C_GP_PHN,
    C_Vac = cost_vac$cost_vac_pp[2] * NewUptake,
    C_Vac_d = C_Vac * dis_ql,
    C_All = C_Hosp + C_GP + C_Vac,
    C_Hosp_d = C_Hosp * dis_cost,
    C_GP_NonPHN_d = C_GP_NonPHN * dis_cost,
    C_GP_PHN_d = C_GP_PHN * dis_cost,
    C_GP_d = C_GP * dis_cost,
    C_All_d = C_Hosp_d + C_GP_d + C_Vac_d
  )


stats %>% 
  group_by(Year, Scenario) %>% 
  summarise(
    Incidence = sum(HZ) / sum(N)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = Incidence, colour = Scenario)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2) + 
  scale_y_continuous("Incidence, 60+, per 100k", labels = scales::number_format(scale = 1e5)) +
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040)) +
  expand_limits(y = 0)


avt <- stats_ce %>% 
  group_by(Year, Scenario) %>% 
  summarise(
    Cases = sum(HZ),
    C_All_d = sum(C_All_d),
    Q_All_d = sum(Q_All_d),
    Q_Life_d = sum(Q_Life_d)
  ) %>% 
  arrange(Year) %>%
  group_by(Scenario) %>% 
  mutate(
    CumCases = cumsum(Cases),
    CumCost = cumsum(C_All_d),
    CumQol = cumsum(Q_All_d)
  ) %>% 
  ungroup()

avt <- avt %>% 
  left_join(avt %>% filter(Scenario == "SOC") %>% 
              select(Year, CumCases0 = CumCases, CumCost0 = CumCost, CumQol0 = CumQol, 
                     C_All_d0 = C_All_d, Q_All_d0 = Q_All_d), by = "Year")



g_cov <- yss %>% 
  filter(Age >= 60) %>% 
  group_by(Year, Scenario) %>% 
  summarise(
    Coverage = weighted.mean((Vaccine != "None"), w = N)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = Coverage, colour = Scenario)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2) + 
  scale_y_continuous("Coverage, 60+, %", labels = scales::percent) +
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040)) +
  scale_color_discrete("Scenario", labels = labs_scenarios) +
  expand_limits(y = 0)


stat_coverage <- yss %>% 
  mutate(
    AgeGrp = cut(Age, c(0, seq(50, 100, 5), 101), right = F)
  ) %>% 
  filter(Age >= 60 & Age < 100) %>% 
  group_by(Year, Scenario, AgeGrp) %>% 
  summarise(
    Coverage = weighted.mean((Vaccine != "None"), w = N)
  )


g_cov_age <- stat_coverage %>% 
  filter(Year %in% c(2023, 2028, 2033, 2038)) %>% 
  mutate(Year = factor(Year)) %>% 
  ggplot() +
  geom_bar(aes(x = AgeGrp, y = Coverage, fill = Scenario), 
           colour = 1, width = 0.9, stat = "identity", position = "dodge") +
  facet_grid(Year~.)


g_trend <- yss %>% 
  filter(Age >= 60) %>% 
  group_by(Year, Scenario) %>% 
  summarise(
    Incidence = sum(HZ) / sum(N)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = Incidence, colour = Scenario)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2) + 
  scale_y_continuous("Incidence, 60+, per 100k", labels = scales::number_format(scale = 1e5)) +
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040)) +
  scale_color_discrete("Scenario", labels = labs_scenarios) +
  expand_limits(y = 0)


g_trend_age <- bind_rows(yss_p1) %>% 
  filter(Age >= 60 & Age < 100) %>%
  mutate(
    AgeGrp = cut(Age, c(0, seq(50, 100, 5), 101), right = F)
  ) %>% 
  group_by(Year, Scenario, AgeGrp) %>% 
  summarise(
    Incidence = sum(HZ) / sum(N)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = Incidence, colour = AgeGrp)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2) + 
  scale_y_continuous("Incidence, 60+, per 100k", labels = scales::number_format(scale = 1e5)) +
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040)) +
  expand_limits(y = 0)


g_dose <- stats %>% 
  group_by(Year, Scenario) %>% 
  summarise(
    Doses = sum(NewUptake) * 2
  ) %>% 
  filter(Year >= 2023) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = Doses, colour = Scenario)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2) + 
  scale_y_continuous("Doses, Shingrix, million", labels = scales::number_format(scale = 1e-6)) +
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040)) +
  scale_color_discrete("Scenario", labels = labs_scenarios) +
  expand_limits(y = 0)


g_avt_n <- avt %>% 
  ggplot() +
  geom_line(aes(x = Year, y = CumCases0 - CumCases, colour = Scenario)) +
  scale_y_continuous("Cases averted, million", labels = scales::number_format(scale = 1e-6)) +
  scale_color_discrete("Scenario", labels = labs_scenarios) +
  labs(subtitle = "Cumulative")


g_avt_p <- avt %>% 
  ggplot() +
  geom_line(aes(x = Year, y = 1 - CumCases / CumCases0, colour = Scenario)) +
  scale_y_continuous("Cases averted, %", labels = scales::percent) +
  scale_color_discrete("Scenario", labels = labs_scenarios) +
  labs(subtitle = "Cumulative")


g_dq_all <- avt %>% 
  ggplot() +
  geom_line(aes(x = Year, y = Q_All_d - Q_All_d0, colour = Scenario)) +
  scale_y_continuous("Incremental QALYs, discounted, thousands", labels = scales::number_format(scale = 1e-3)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2)  +
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040)) +
  scale_color_discrete("Scenario", labels = labs_scenarios) +
  expand_limits(y = 0) +
  labs(subtitle = "Breakdown by year")

g_cdq_all <- avt %>% 
  ggplot() +
  geom_line(aes(x = Year, y = CumQol - CumQol0, colour = Scenario)) +
  scale_y_continuous("Incremental QALYs, discounted, thousands", labels = scales::number_format(scale = 1e-3)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2)  +
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040)) +
  scale_color_discrete("Scenario", labels = labs_scenarios) +
  expand_limits(y = 0) +
  labs(subtitle = "Cumulative")


g_dc_all <- avt %>% 
  ggplot() +
  geom_line(aes(x = Year, y = C_All_d - C_All_d0, colour = Scenario)) +
  scale_y_continuous("Incremental costs, discounted, million GBP", labels = scales::number_format(scale = 1e-6)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2)  +
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040)) +
  scale_color_discrete("Scenario", labels = labs_scenarios) +
  expand_limits(y = 0) +
  labs(subtitle = "Breakdown by year")


g_cdc_all <- avt %>% 
  ggplot() +
  geom_line(aes(x = Year, y = CumCost - CumCost0, colour = Scenario)) +
  scale_y_continuous("Incremental costs, discounted, million GBP", labels = scales::number_format(scale = 1e-6)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2)  +
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040)) +
  scale_color_discrete("Scenario", labels = labs_scenarios) +
  expand_limits(y = 0) +
  labs(subtitle = "Cumulative")


g_icer <- avt %>% 
  ggplot() +
  geom_line(aes(x = Year, y = (CumCost - CumCost0) / (CumQol - CumQol0), colour = Scenario)) +
  geom_hline(yintercept = 3e4) +
  scale_y_continuous("ICER, Cost per QALY gained, thousand", labels = scales::number_format(scale = 1e-3)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2)  +
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040)) +
  scale_color_discrete("Scenario", labels = labs_scenarios) +
  expand_limits(y = 0) +
  labs(subtitle = "Cumulative")


avt %>% 
  mutate(
    NMB4 = (CumQol - CumQol0) * 4e4 - (CumCost - CumCost0),
    NMB3 = (CumQol - CumQol0) * 3e4 - (CumCost - CumCost0),
    NMB2 = (CumQol - CumQol0) * 2e4 - (CumCost - CumCost0)
  ) %>% 
  group_by(Scenario) %>% 
  summarise(
    Yield2 = min(Year[NMB2 > 0]),
    Yield3 = min(Year[NMB3 > 0]),
    Yield4 = min(Year[NMB4 > 0])
  )

g_nmb <- avt %>% 
  ggplot() +
  geom_line(aes(x = Year, y = (CumQol - CumQol0) * 3e4 - (CumCost - CumCost0), colour = Scenario, linetype = "30k")) +
  geom_line(aes(x = Year, y = (CumQol - CumQol0) * 2e4 - (CumCost - CumCost0), colour = Scenario, linetype = "20k")) +
  scale_y_continuous("Net Monetary benefit", labels = scales::number_format(scale = 1e-3)) +
  geom_vline(xintercept = c(2023, 2028, 2033), linetype = 2) +
  geom_text(x = 2023, y = 0, label = "Phase 1", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2028, y = 0, label = "Phase 2", angle = 90, hjust = -0.1, vjust = 1.2) + 
  geom_text(x = 2033, y = 0, label = "Continuation", angle = 90, hjust = -0.1, vjust = 1.2)  +
  scale_x_continuous("Year", breaks = c(2020, 2023, 2028, 2033, 2040)) +
  expand_limits(y = 0) +
  labs(subtitle = "Cumulative")



ggsave(g_cov, filename = here::here(root, "figs", "g_proj_coverage.png"), width = 6.5, height = 4)
ggsave(g_cov_age, filename = here::here(root, "figs", "g_proj_coverage_agp.png"), width = 6.5, height = 4)
ggsave(g_trend, filename = here::here(root, "figs", "g_proj_trend.png"), width = 6.5, height = 4)
ggsave(g_trend_age, filename = here::here(root, "figs", "g_proj_trend_agp.png"), width = 6.5, height = 4)
ggsave(g_avt_n, filename = here::here(root, "figs", "g_proj_avt_n.png"), width = 6.5, height = 4)
ggsave(g_avt_p, filename = here::here(root, "figs", "g_proj_avt_p.png"), width = 6.5, height = 4)
ggsave(g_dose, filename = here::here(root, "figs", "g_proj_dose.png"), width = 6.5, height = 4)
ggsave(g_dc_all, filename = here::here(root, "figs", "g_proj_dc_all.png"), width = 6.5, height = 4)
ggsave(g_cdc_all, filename = here::here(root, "figs", "g_proj_cdc_all.png"), width = 6.5, height = 4)
ggsave(g_dq_all, filename = here::here(root, "figs", "g_proj_dq_all.png"), width = 6.5, height = 4)
ggsave(g_cdq_all, filename = here::here(root, "figs", "g_proj_cdq_all.png"), width = 6.5, height = 4)

