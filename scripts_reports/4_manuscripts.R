library(tidyverse)

theme_set(theme_bw())



dat <- read_csv(here::here("docs", "tabs", "stats_ce_rzv_realworld.csv"))


labs_comp <- c(
  "dQ_HZ_d" = "HZ prevention",
  "dQ_Life_d" = "Survival", 
  "dC_GP_d" = "GP care",
  "dC_Hosp_d" = "Hospitialisation",
  "dC_VacRZV_d" = "Vaccination"
)


waterfall <- dat %>% 
  select(Age0, Arm, 
         dQ_HZ_d = dQ_HZ_d_M, dQ_Life_d = dQ_Life_d_M, 
         dC_Hosp_d = dC_Hosp_d_M, dC_GP_NonPHN_d = dC_GP_NonPHN_d_M,
         dC_GP_PHN_d = dC_GP_PHN_d_M, 
         dC_VacRZV_d = dC_VacRZV_d_M, dN_VacRZV_d = dN_VacRZV_d_M) %>%
  mutate(
    dC_GP_d = dC_GP_NonPHN_d + dC_GP_PHN_d,
    Thres = (2e4 * (dQ_HZ_d + dQ_Life_d) - dC_Hosp_d - dC_GP_d) / dN_VacRZV_d,
    dC_VacRZV_d = Thres * dN_VacRZV_d
  ) %>% 
  select(Age0, Arm, dQ_HZ_d, dQ_Life_d, dC_Hosp_d, dC_GP_d, dC_VacRZV_d) %>% 
  pivot_longer(-c(Age0, Arm), names_to = "Index") %>% 
  mutate(
    Arm = factor(Arm, c("Vac1", "Vac")),
    Type = ifelse(startsWith(Index, "dQ"), "QoL", "Cost"),
    Index = factor(Index, names(labs_comp)),
    id = as.numeric(Index),
    MB = ifelse(Type == "QoL", value * 2e4, -value)
  )



g_wf <- waterfall %>% 
  group_by(Age0, Arm) %>% 
  arrange(Index) %>% 
  mutate(
    y1 = cumsum(MB),
    y0 = c(0, y1[-n()]),
    Direction = ifelse(MB > 0, "Beneficial", "Harmful")
  ) %>% 
  filter(Age0 %in% c(70, 80)) %>% 
  ggplot() +
  geom_rect(aes(x = Index, xmin = id - 0.4, xmax = id + 0.4, ymin = y0, ymax = y1, fill = Direction), alpha = 0.5) +
  geom_segment(aes(x=ifelse(id == last(id), last(id) - 0.5, id - 0.4), 
                   xend=ifelse(id == last(id), last(id) + 0.5, id + 1.4), 
                   y=y1, 
                   yend=y1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 2.5) +
  annotate("text", x = 2.3, y = 170, label = "QoL", hjust = 1) +
  annotate("text", x = 2.7, y = 170, label = "Cost", hjust = 0) +
  scale_y_continuous("Net montary benefit (WTP = 20,000 GBP)") + 
  scale_x_discrete("Source", labels = labs_comp) +
  facet_grid(Age0~Arm, labeller = labeller(
    Arm = c(Vac = "Two doses", Vac1 = "Single dose"),
    Age0 = c("70" = "Vaccinated at 70 YOA", "80" = "Vaccinated at 80 YOA")
  )) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


g_comp_stack <- waterfall %>% 
  group_by(Age0, Arm) %>% 
  filter(Index != "dC_VacRZV_d") %>% 
  #mutate(Index = fct_rev(Index)) %>% 
  filter(Age0 <= 95) %>% 
  ggplot(aes(x = Age0)) +
  geom_bar(aes(y = MB, fill = Index), stat = "identity", position = "stack", colour = NA, width = 1) +
  scale_y_continuous("Monetary benefit in GBP") +
  scale_fill_discrete("Components", labels = labs_comp) +
  facet_grid(.~Arm, labeller = labeller(Arm = c(Vac = "Two doses", Vac1 = "Single dose"))) +
  scale_x_continuous("Age of vaccination") +
  labs(caption = "1 QALY = 20,000 GBP")


g_comp_fill <- waterfall %>% 
  group_by(Age0, Arm) %>% 
  filter(Index != "dC_VacRZV_d") %>% 
  #mutate(Index = fct_rev(Index)) %>% 
  filter(Age0 <= 95) %>% 
  ggplot(aes(x = Age0)) +
  geom_bar(aes(y = MB, fill = Index), stat = "identity", position = "fill", colour = NA, width = 1) +
  scale_fill_discrete("Components", labels = labs_comp) +
  scale_y_continuous("Share of monetary benefit, %", labels = scales::percent) +
  facet_grid(.~Arm, labeller = labeller(Arm = c(Vac = "Two doses", Vac1 = "Single dose"))) +
  scale_x_continuous("Age of vaccination") +
  labs(caption = "1 QALY = 20,000 GBP")

g_wf
g_comp_stack
g_comp_fill

ggsave(g_wf, filename = here::here("docs", "figs", "g_rzv_waterfall.png"), width = 8, height = 7.5)
ggsave(g_comp_stack, filename = here::here("docs", "figs", "g_rzv_mb_stack.png"), width = 8, height = 5.5)
ggsave(g_comp_fill, filename = here::here("docs", "figs", "g_rzv_mb_fill.png"), width = 8, height = 5.5)


  