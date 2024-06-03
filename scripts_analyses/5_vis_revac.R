library(tidyverse)

theme_set(theme_bw())


load(here::here("out", "stats_rzv2rzv.rdata"))


g_icer0 <- stats_ce %>% 
  filter(Age1 == Age0 + 1) %>% 
  filter(Arm == "Vac") %>% 
  filter(Index == "ICER") %>% 
  ggplot() +
  geom_ribbon(aes(x = Age0, ymin = L, ymax = U), alpha = 0.2) +
  geom_smooth(aes(x = Age0, y = Avg), se = F) +
  geom_hline(yintercept = 2e4, linetype = 2) +
  geom_hline(yintercept = 3e4, linetype = 2) +
  scale_x_continuous("Age for RZV vaccination") +
  scale_y_continuous("ICER") +
  expand_limits(y = 0)


g_icer <- stats_ce %>% 
  filter(Age0 %in% c(60, 65, 70, 75, 80, 85, 90)) %>% 
  filter(Index == "ICER") %>% 
  ggplot() +
  geom_line(aes(x = Age1, y = Avg, colour = Arm)) +
  geom_vline(aes(xintercept = Age0), linetype = 2) +
  geom_text(aes(x = Age0, y = 0), label = "First RZV", angle = 90, hjust = 0, vjust = 1.2) +
  geom_hline(yintercept = 2e4, linetype = 2) +
  geom_hline(yintercept = 3e4, linetype = 2) +
  scale_x_continuous("Age for RZV revaccination") +
  scale_y_continuous("ICER") +
  facet_wrap(.~Age0) +
  expand_limits(y = 0)

g_icer



g_e <- stats_ce %>% 
  filter(Age0 %in% c(60, 65, 70, 75, 80, 85, 90)) %>% 
  filter(Index == "dQ_All_d") %>% 
  ggplot() +
  geom_line(aes(x = Age1, y = Avg, colour = Arm)) +
  geom_vline(aes(xintercept = Age0), linetype = 2) +
  geom_text(aes(x = Age0, y = 0), label = "First RZV", angle = 90, hjust = 0, vjust = 1.2) +
  scale_x_continuous("Age for RZV revaccination") +
  scale_y_continuous("QALY gained") +
  facet_wrap(.~Age0) +
  expand_limits(y = 0)

g_e


g_c <- stats_ce %>% 
  filter(Age0 %in% c(60, 65, 70, 75, 80, 85, 90)) %>% 
  filter(Index == "dC_All_d") %>% 
  ggplot() +
  geom_line(aes(x = Age1, y = Avg, colour = Arm)) +
  geom_vline(aes(xintercept = Age0), linetype = 2) +
  geom_text(aes(x = Age0, y = 0), label = "First RZV", angle = 90, hjust = 0, vjust = 1.2) +
  scale_x_continuous("Age for RZV revaccination") +
  scale_y_continuous("Incremental cost, million GBP", 
                     labels = scales::number_format(scale = 1e-6)) +
  facet_wrap(.~Age0) +
  expand_limits(y = 0)

g_c

ggsave(g_icer0, filename = here::here("docs", "figs", "g_rzv_icer.png"), width = 5, height = 3.5)
ggsave(g_e, filename = here::here("docs", "figs", "g_rzv2rzv_de.png"), width = 8, height = 5)
ggsave(g_c, filename = here::here("docs", "figs", "g_rzv2rzv_dc.png"), width = 8, height = 5)
ggsave(g_icer, filename = here::here("docs", "figs", "g_rzv2rzv_icer.png"), width = 8, height = 5)



load(here::here("analysis_cohort", "temp", "stats_zvl2rzv.rdata"))


g_icer0 <- stats_ce %>% 
  filter(Age1 == Age0 + 1) %>% 
  filter(Arm == "Vac") %>% 
  filter(Index == "ICER") %>% 
  ggplot() +
  geom_ribbon(aes(x = Age0, ymin = L, ymax = U), alpha = 0.2) +
  geom_smooth(aes(x = Age0, y = Avg), se = F) +
  geom_hline(yintercept = 2e4, linetype = 2) +
  geom_hline(yintercept = 3e4, linetype = 2) +
  scale_x_continuous("Age for ZVL vaccination") +
  scale_y_continuous("ICER") +
  expand_limits(y = 0)


g_icer <- stats_ce %>% 
  filter(Age0 %in% c(60, 65, 70, 75, 80, 85, 90)) %>% 
  filter(Index == "ICER") %>% 
  ggplot() +
  geom_line(aes(x = Age1, y = Avg, colour = Arm)) +
  geom_vline(aes(xintercept = Age0), linetype = 2) +
  geom_text(aes(x = Age0, y = 0), label = "First RZV", angle = 90, hjust = 0, vjust = 1.2) +
  geom_hline(yintercept = 2e4, linetype = 2) +
  geom_hline(yintercept = 3e4, linetype = 2) +
  scale_x_continuous("Age for RZV revaccination") +
  scale_y_continuous("ICER") +
  facet_wrap(.~Age0) +
  expand_limits(y = 0)

g_icer



g_e <- stats_ce %>% 
  filter(Age0 %in% c(60, 65, 70, 75, 80, 85, 90)) %>% 
  filter(Index == "dQ_All_d") %>% 
  ggplot() +
  geom_line(aes(x = Age1, y = Avg, colour = Arm)) +
  geom_vline(aes(xintercept = Age0), linetype = 2) +
  geom_text(aes(x = Age0, y = 0), label = "First RZV", angle = 90, hjust = 0, vjust = 1.2) +
  scale_x_continuous("Age for RZV revaccination") +
  scale_y_continuous("QALY gained") +
  facet_wrap(.~Age0) +
  expand_limits(y = 0)

g_e


g_c <- stats_ce %>% 
  filter(Age0 %in% c(60, 65, 70, 75, 80, 85, 90)) %>% 
  filter(Index == "dC_All_d") %>% 
  ggplot() +
  geom_line(aes(x = Age1, y = Avg, colour = Arm)) +
  geom_vline(aes(xintercept = Age0), linetype = 2) +
  geom_text(aes(x = Age0, y = 0), label = "First RZV", angle = 90, hjust = 0, vjust = 1.2) +
  scale_x_continuous("Age for RZV revaccination") +
  scale_y_continuous("Incremental cost, million GBP", 
                     labels = scales::number_format(scale = 1e-6)) +
  facet_wrap(.~Age0) +
  expand_limits(y = 0)

g_c


ggsave(g_icer0, filename = here::here("docs", "figs", "g_zvl_icer.png"), width = 5, height = 3.5)
ggsave(g_e, filename = here::here("docs", "figs", "g_zvl2rzv_de.png"), width = 8, height = 5)
ggsave(g_c, filename = here::here("docs", "figs", "g_zvl2rzv_dc.png"), width = 8, height = 5)
ggsave(g_icer, filename = here::here("docs", "figs", "g_zvl2rzv_icer.png"), width = 8, height = 5)
