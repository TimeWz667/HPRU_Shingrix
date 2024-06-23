library(tidyverse)

theme_set(theme_bw())



stats_ce <- read_csv(here::here("docs", "tabs", "stats_ce_rzv.csv"))


g_icer <- stats_ce %>% 
  filter(Index == "ICER") %>% 
  filter(Arm == "Vac" | Age0 >= 80) %>% 
  ggplot(aes(x = Age0)) +
  geom_ribbon(aes(ymin = L, ymax = U, fill = Arm), alpha = 0.2) +
  geom_line(aes(y = M, colour = Arm)) +
  geom_hline(yintercept = 2e4, linetype = 2) +
  geom_hline(yintercept = 3e4, linetype = 3) +
  scale_y_continuous("ICER, GBP", breaks = 0:15 * 1e4) +
  scale_x_continuous("Age of RZV vaccination") +
  scale_fill_discrete("Intervention", labels = c(Vac = "Double doses", Vac1 = "Single dose")) +
  guides(colour = guide_none())

g_icer


g_e <- stats_ce %>% 
  filter(Index == "dQ_All_d") %>% 
  filter(Arm == "Vac" | Age0 >= 80) %>% 
  ggplot(aes(x = Age0)) +
  geom_ribbon(aes(ymin = L, ymax = U, fill = Arm), alpha = 0.2) +
  geom_line(aes(y = M, colour = Arm)) +
  scale_y_continuous("Utility gained, QALY per capita") +
  scale_x_continuous("Age of RZV vaccination") +
  scale_fill_discrete("Intervention", labels = c(Vac = "Double doses", Vac1 = "Single dose")) +
  guides(colour = guide_none()) +
  expand_limits(y = 0)

g_e


g_c <- stats_ce %>% 
  filter(Index == "dC_All_d") %>% 
  filter(Arm == "Vac" | Age0 >= 80) %>% 
  ggplot(aes(x = Age0)) +
  geom_ribbon(aes(ymin = L, ymax = U, fill = Arm), alpha = 0.2) +
  geom_line(aes(y = M, colour = Arm)) +
  scale_y_continuous("Incremental cost, GBP per capita") +
  scale_x_continuous("Age of RZV vaccination") +
  scale_fill_discrete("Intervention", labels = c(Vac = "Double doses", Vac1 = "Single dose")) +
  guides(colour = guide_none()) +
  expand_limits(y = 0)

g_thres <- stats_ce %>% 
  filter(Index == "Thres20") %>% 
  filter(Arm == "Vac" | Age0 >= 80) %>% 
  ggplot(aes(x = Age0)) +
  geom_ribbon(aes(ymin = L, ymax = U, fill = Arm), alpha = 0.2) +
  geom_line(aes(y = M, colour = Arm)) +
  scale_y_continuous("Threshold price of RZV, GBP per adminstration") +
  scale_x_continuous("Age of RZV vaccination") +
  scale_fill_discrete("Intervention", labels = c(Vac = "Double doses", Vac1 = "Single dose")) +
  guides(colour = guide_none()) +
  expand_limits(y = 0) +
  labs(caption = "WTP = 20,000 GBP")

g_thres



ggsave(g_e, filename = here::here("docs", "figs", "g_rzv_de.png"), width = 8, height = 5)
ggsave(g_c, filename = here::here("docs", "figs", "g_rzv_dc.png"), width = 8, height = 5)
ggsave(g_icer, filename = here::here("docs", "figs", "g_rzv_icer.png"), width = 8, height = 5)
ggsave(g_thres, filename = here::here("docs", "figs", "g_rzv_thres.png"), width = 8, height = 5)




stats_ce <- read_csv(here::here("docs", "tabs", "stats_ce_zvl2rzv.csv"))

labs_arm <- c(ReVac_RZV2 = "Double doses", ReVac_RZV1 = "Single dose")

g_icer <- stats_ce %>% 
  filter(Age0 %in% c(60, 65, 70, 75, 80, 85, 90)) %>% 
  filter(Index == "ICER") %>% 
  filter(Type == "Second") %>% 
  filter(Arm %in% c("ReVac_RZV1", "ReVac_RZV2")) %>% 
  ggplot(aes(x = Age1)) +
  geom_ribbon(aes(ymin = L, ymax = U, fill = Arm), alpha = 0.2) +
  geom_line(aes(x = Age1, y = M, colour = Arm)) +
  geom_vline(aes(xintercept = Age0), linetype = 2) +
  geom_text(aes(x = Age0, y = 0), label = "ZVL vaccination", angle = 90, hjust = 0, vjust = 1.2) +
  geom_hline(yintercept = 2e4, linetype = 2) +
  geom_hline(yintercept = 3e4, linetype = 2) +
  scale_x_continuous("Age for RZV revaccination") +
  scale_y_continuous("ICER") +
  scale_fill_discrete("RZV \nvaccination", labels = labs_arm) +
  guides(colour = guide_none()) +
  facet_wrap(.~Age0) +
  expand_limits(y = 0)

g_icer


g_e <- stats_ce %>% 
  filter(Age0 %in% c(60, 65, 70, 75, 80, 85, 90)) %>% 
  filter(Index == "dQ_All_d") %>% 
  filter(Type == "Second") %>% 
  filter(Arm %in% c("ReVac_RZV1", "ReVac_RZV2")) %>% 
  ggplot(aes(x = Age1)) +
  geom_ribbon(aes(ymin = L, ymax = U, fill = Arm), alpha = 0.2) +
  geom_vline(aes(xintercept = Age0), linetype = 2) +
  geom_text(aes(x = Age0, y = 0), label = "ZVL vaccination", angle = 90, hjust = 0, vjust = 1.2) +
  geom_line(aes(y = M, colour = Arm)) +
  scale_y_continuous("Utility gained, QALY per capita") +
  scale_x_continuous("Age for RZV revaccination") +
  scale_fill_discrete("RZV \nvaccination", labels = labs_arm) +
  guides(colour = guide_none()) +
  facet_wrap(.~Age0) +
  expand_limits(y = 0)

g_e


g_c <- stats_ce %>% 
  filter(Age0 %in% c(60, 65, 70, 75, 80, 85, 90)) %>% 
  filter(Index == "dC_All_d") %>% 
  filter(Type == "Second") %>% 
  filter(Arm %in% c("ReVac_RZV1", "ReVac_RZV2")) %>% 
  ggplot(aes(x = Age1)) +
  geom_ribbon(aes(ymin = L, ymax = U, fill = Arm), alpha = 0.2) +
  geom_vline(aes(xintercept = Age0), linetype = 2) +
  geom_text(aes(x = Age0, y = 0), label = "ZVL vaccination", angle = 90, hjust = 0, vjust = 1.2) +
  geom_line(aes(y = M, colour = Arm)) +
  scale_y_continuous("Incremental cost, GBP per capita") +
  scale_x_continuous("Age for RZV revaccination") +
  scale_fill_discrete("RZV \nvaccination", labels = labs_arm) +
  guides(colour = guide_none()) +
  facet_wrap(.~Age0) +
  expand_limits(y = 0)

g_c



g_thres <- stats_ce %>% 
  filter(Age0 %in% c(60, 65, 70, 75, 80, 85, 90)) %>% 
  filter(Index == "Thres20") %>% 
  filter(Type == "Second") %>% 
  filter(Arm %in% c("ReVac_RZV1", "ReVac_RZV2")) %>% 
  ggplot(aes(x = Age1)) +
  geom_ribbon(aes(ymin = L, ymax = U, fill = Arm), alpha = 0.2) +
  geom_vline(aes(xintercept = Age0), linetype = 2) +
  geom_text(aes(x = Age0, y = 0), label = "ZVL vaccination", angle = 90, hjust = 0, vjust = 1.2) +
  geom_line(aes(y = M, colour = Arm)) +
  scale_y_continuous("Threshold price of RZV, GBP per adminstration") +
  scale_x_continuous("Age for RZV revaccination") +
  scale_fill_discrete("RZV \nvaccination", labels = labs_arm) +
  guides(colour = guide_none()) +
  facet_wrap(.~Age0) +
  expand_limits(y = 0) +
  labs(caption = "WTP = 20,000 GBP")

g_thres



ggsave(g_e, filename = here::here("docs", "figs", "g_zvl2rzv_de.png"), width = 10, height = 5)
ggsave(g_c, filename = here::here("docs", "figs", "g_zvl2rzv_dc.png"), width = 10, height = 5)
ggsave(g_icer, filename = here::here("docs", "figs", "g_zvl2rzv_icer.png"), width = 10, height = 5)
ggsave(g_thres, filename = here::here("docs", "figs", "g_zvl2rzv_thres.png"), width = 10, height = 5)




## Programme-based profile
profile <- read_csv(here::here("docs", "tabs", "tab_programme.csv"))

profile <- profile %>% 
  mutate(
    Group = paste(Eligibility, ifelse(Agp != "[80,85)", "", Agp)),
    Group = factor(Group, rev(unique(Group)))
  )


g_pro <- profile %>% 
  mutate(N = c(pr_cum_N[1], diff(pr_cum_N))) %>% 
  select(Arm, Group, N, dC_All_d, dQ_All_d) %>% 
  pivot_longer(c(N, dC_All_d, dQ_All_d)) %>% 
  ggplot() +
  geom_histogram(aes(x = value, y = name, fill = Group), stat = "identity", position = "fill") +
  scale_y_discrete("", labels = c(N = "Population", dC_All_d="Cost", dQ_All_d="QALY")) +
  scale_x_continuous("Shares, %", labels = scales::percent) +
  scale_fill_discrete("", labels = c(
    "ZVL_85 [80,85)" = "Vaccinated with ZVL, 80-85",
    "UV_85 [80,85)" = "Unvaccinated, 80-85",
    "New2023 " = "2023-2028 eligibility",
    "SOC " = "~2023 eligibility",
    "SOC " = "~2023 eligibility"
  )) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.position = "bottom")

g_pro


ggsave(g_pro, filename = here::here("docs", "figs", "g_profile_burden.png"), width = 8, height = 4)



profile

