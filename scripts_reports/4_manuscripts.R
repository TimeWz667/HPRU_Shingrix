library(tidyverse)

theme_set(theme_bw())



stats_yss <- read_csv(here::here("docs", "tabs", "stats_ys_rzv.csv"))

dat <- read_csv(here::here("docs", "tabs", "stats_ce_rzv.csv"))


dat %>% 
  select(-c(L, U)) %>%
  filter(Index %in% c("dQ_HZ_d", "dQ_Life_d", "dC_Vac_d", "dC_Med_d")) %>% 
  group_by(Age0, Arm) %>% 
  mutate(
    M = ifelse(Index == "dC_Vac_d", M / 85 * 50, M),
    Type = ifelse(startsWith(Index, "dQ"), "QoL", "Cost"),
    Index = factor(Index, c("dQ_Life_d", "dQ_HZ_d", "dC_Med_d", "dC_Vac_d")),
    id = as.numeric(Index)
  ) %>%
  arrange(Index) %>% 
  mutate(
    M = ifelse(Type == "QoL", M * 2e4, -M),
    y1 = cumsum(M),
    y0 = c(0, y1[-n()]),
    Direction = ifelse(M > 0, "Beneficial", "Harmful")
  ) %>% 
  filter(Age0 %in% c(65, 70, 75, 80, 85)) %>% 
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
  scale_x_discrete("Source", labels = c("dQ_Life_d" = "Survival", "dQ_HZ_d" = "HZ prevention", "dC_Med_d" = "Medical cost", "dC_Vac_d" = "Vaccination")) +
  facet_grid(Age0~Arm) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

  