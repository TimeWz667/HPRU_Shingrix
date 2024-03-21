library(tidyverse)


load(here::here("analysis_cohort", "temp", "stats_rzv2rzv.rdata"))

g_icer <- stats_ce %>% 
  filter(Age0 %in% c(60, 65, 70, 75, 80)) %>% 
  filter(Index == "ICER") %>% 
  ggplot() +
  geom_line(aes(x = Age1, y = Avg, colour = Arm)) +
  geom_vline(aes(xintercept = Age0), linetype = 2) +
  geom_text(aes(x = Age0, y = 0), label = "First RZV", angle = 90, hjust = 0, vjust = 1.2) +
  scale_x_continuous("Age for RZV revaccination") +
  scale_y_continuous("ICER") +
  facet_grid(.~Age0) +
  expand_limits(y = 0)

g_icer



g_e <- stats_ce %>% 
  filter(Age0 %in% c(60, 65, 70, 75, 80)) %>% 
  filter(Index == "dQ_All_d") %>% 
  ggplot() +
  geom_line(aes(x = Age1, y = Avg, colour = Arm)) +
  geom_vline(aes(xintercept = Age0), linetype = 2) +
  geom_text(aes(x = Age0, y = 0), label = "First RZV", angle = 90, hjust = 0, vjust = 1.2) +
  scale_x_continuous("Age for RZV revaccination") +
  scale_y_continuous("QALY gained") +
  facet_grid(.~Age0) +
  expand_limits(y = 0)

g_e


g_c <- stats_ce %>% 
  filter(Age0 %in% c(60, 65, 70, 75, 80)) %>% 
  filter(Index == "dC_All_d") %>% 
  ggplot() +
  geom_line(aes(x = Age1, y = Avg, colour = Arm)) +
  geom_vline(aes(xintercept = Age0), linetype = 2) +
  geom_text(aes(x = Age0, y = 0), label = "First RZV", angle = 90, hjust = 0, vjust = 1.2) +
  scale_x_continuous("Age for RZV revaccination") +
  scale_y_continuous("Incremental cost, million GBP", 
                     labels = scales::number_format(scale = 1e-6)) +
  facet_grid(.~Age0) +
  expand_limits(y = 0)

g_c


ggsave(g_e, filename = here::here("analysis_cohort", "figs", "g_rzv2rzv_de.png"), width = 8, height = 5)
ggsave(g_c, filename = here::here("analysis_cohort", "figs", "g_rzv2rzv_dc.png"), width = 8, height = 5)
ggsave(g_icer, filename = here::here("analysis_cohort", "figs", "g_rzv2rzv_icer.png"), width = 8, height = 5)



load(here::here("analysis_cohort", "temp", "stats_zvl2rzv.rdata"))

g_icer <- stats_ce %>% 
  filter(Age0 %in% c(60, 65, 70, 75, 80)) %>% 
  filter(Index == "ICER") %>% 
  ggplot() +
  geom_line(aes(x = Age1, y = Avg, colour = Arm)) +
  geom_vline(aes(xintercept = Age0), linetype = 2) +
  geom_text(aes(x = Age0, y = 0), label = "First RZV", angle = 90, hjust = 0, vjust = 1.2) +
  scale_x_continuous("Age for RZV revaccination") +
  scale_y_continuous("ICER") +
  facet_grid(.~Age0) +
  expand_limits(y = 0)

g_icer



g_e <- stats_ce %>% 
  filter(Age0 %in% c(60, 65, 70, 75, 80)) %>% 
  filter(Index == "dQ_All_d") %>% 
  ggplot() +
  geom_line(aes(x = Age1, y = Avg, colour = Arm)) +
  geom_vline(aes(xintercept = Age0), linetype = 2) +
  geom_text(aes(x = Age0, y = 0), label = "First RZV", angle = 90, hjust = 0, vjust = 1.2) +
  scale_x_continuous("Age for RZV revaccination") +
  scale_y_continuous("QALY gained") +
  facet_grid(.~Age0) +
  expand_limits(y = 0)

g_e


g_c <- stats_ce %>% 
  filter(Age0 %in% c(60, 65, 70, 75, 80)) %>% 
  filter(Index == "dC_All_d") %>% 
  ggplot() +
  geom_line(aes(x = Age1, y = Avg, colour = Arm)) +
  geom_vline(aes(xintercept = Age0), linetype = 2) +
  geom_text(aes(x = Age0, y = 0), label = "First RZV", angle = 90, hjust = 0, vjust = 1.2) +
  scale_x_continuous("Age for RZV revaccination") +
  scale_y_continuous("Incremental cost, million GBP", 
                     labels = scales::number_format(scale = 1e-6)) +
  facet_grid(.~Age0) +
  expand_limits(y = 0)

g_c


ggsave(g_e, filename = here::here("analysis_cohort", "figs", "g_zvl2rzv_de.png"), width = 8, height = 5)
ggsave(g_c, filename = here::here("analysis_cohort", "figs", "g_zvl2rzv_dc.png"), width = 8, height = 5)
ggsave(g_icer, filename = here::here("analysis_cohort", "figs", "g_zvl2rzv_icer.png"), width = 8, height = 5)
