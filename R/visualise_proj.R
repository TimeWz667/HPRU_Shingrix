
vis_proj <- function(stats_proj, prefix, ext = ".png") {
  require(ggplot2)
  
  gs <- list()
  
  tags <- c(
    "Null" = "No vaccine for HZ",
    "Stay" = "No new programme (keep ZVL)",
    "ToRZV" = "(1) Replace ZVL by RZV",
    "Sch65" = "(2) = (1) + RZV to 65 from 2028",
    "Sch" = "(3) Scheduled programme",
    "Sch1d85" = "(3) + Single RZV to 85",
    "Sch1d95" = "(3) + Single RZV to 95",
    "Sch2d85" = "(3) + Two RZV to 85",
    "Sch2d95" = "(3) + Two RZV to 95"
  )
  
  
  gs$g_inc_all <- stats_proj$Stats_All %>% 
    filter(Index == "IncR_HZ") %>% 
    filter(Year <= 2040 & Year >= 2022) %>%
    filter(Scenario %in% c("Stay", "ToRZV", "Sch65", "Sch", "Sch1d85", "Sch2d85")) %>% 
    mutate(Scenario = factor(Scenario, names(tags))) %>% 
    ggplot() +
    #geom_ribbon(aes(x = Year, ymin = L, ymax = U, fill = Scenario), alpha = 0.2) +
    geom_line(aes(x = Year, y = M, colour = Scenario)) +
    scale_color_discrete(labels = tags) +
    scale_y_continuous("Incidence, per 100,000", labels = scales::number_format(scale = 1e5)) +
    scale_x_continuous("Year", breaks = c(2023, 2028, 2033, 2040, 2045, 2050)) +
    expand_limits(y = 0)
  
  
  gs$g_inc_agp <- stats_proj[[2]] %>% 
    filter(Index == "IncR_HZ") %>% 
    filter(Year <= 2040 & Year >= 2022) %>%
    filter(Scenario %in% c("Stay", "ToRZV", "Sch65", "Sch", "Sch1d85", "Sch2d85")) %>% 
    mutate(Scenario = factor(Scenario, names(tags))) %>% 
    ggplot() +
    #geom_ribbon(aes(x = Year, ymin = L, ymax = U, fill = Scenario), alpha = 0.2) +
    geom_line(aes(x = Year, y = M, colour = Scenario)) +
    scale_color_discrete(labels = tags) +
    scale_y_continuous("Incidence, per 100,000", labels = scales::number_format(scale = 1e5)) +
    scale_x_continuous("Year", breaks = c(2023, 2028, 2033, 2040, 2045, 2050)) +
    facet_grid(.~Agp) +
    expand_limits(y = 0)
  
  
  gs$g_inc_68 <- stats_proj[[3]] %>% 
    filter(Index == "IncR_HZ") %>% 
    filter(Year <= 2040 & Year >= 2022) %>%
    filter(Scenario %in% c("Stay", "ToRZV", "Sch65", "Sch", "Sch1d85", "Sch2d85")) %>% 
    mutate(Scenario = factor(Scenario, names(tags))) %>% 
    ggplot() +
    #geom_ribbon(aes(x = Year, ymin = L, ymax = U, fill = Scenario), alpha = 0.2) +
    geom_line(aes(x = Year, y = M, colour = Scenario)) +
    scale_color_discrete(labels = tags) +
    scale_y_continuous("Incidence, per 100,000", labels = scales::number_format(scale = 1e5)) +
    scale_x_continuous("Year", breaks = c(2023, 2028, 2033, 2040, 2045, 2050)) +
    facet_grid(.~Agp, labeller = labeller(Agp = c("60_80" = "Aged 60 - 80", "80+" = "Aged 80+"))) +
    expand_limits(y = 0)
  
  
  gs$g_avt_sch <- stats_proj[[4]] %>%
    filter(Year <= 2040 & Year >= 2022) %>%
    filter(Index == "Avt_Inc") %>%
    filter(Scenario %in% c("Stay", "ToRZV", "Sch65", "Sch")) %>% 
    mutate(Scenario = factor(Scenario, names(tags))) %>% 
    ggplot() +
    geom_line(aes(x = Year, y = M, colour = Scenario)) +
    scale_x_continuous("Year", breaks = c(2023, 2028, 2033, 2040, 2045, 2050)) +
    scale_y_continuous("Averted cases, %", labels = scales::percent) +
    scale_colour_discrete("Scenario", labels = tags) +
    guides(colour = guide_legend(reverse = T))
  
  
  gs$g_avt_old <- stats_proj[[5]] %>%
    filter(Year <= 2040 & Year >= 2022) %>%
    filter(Index == "Avt_Inc") %>%
    filter(Scenario %in% c("Sch", "Sch1d85", "Sch2d85", "Sch1d95", "Sch2d95")) %>% 
    mutate(Scenario = factor(Scenario, names(tags))) %>% 
    ggplot() +
    geom_line(aes(x = Year, y = M, colour = Scenario)) +
    scale_x_continuous("Year", breaks = c(2023, 2028, 2033, 2040, 2045, 2050)) +
    scale_y_continuous("Averted cases, %", labels = scales::percent) +
    scale_colour_discrete("Scenario", labels = tags) +
    guides(colour = guide_legend(reverse = T))
  
  
  ggsave(gs$g_inc_all, filename = here::here("docs", "figs", paste0(prefix, "_inc_all", ext)), width = 6, height = 4)  
  ggsave(gs$g_inc_agp, filename = here::here("docs", "figs", paste0(prefix, "_inc_agp", ext)), width = 15, height = 4)  
  ggsave(gs$g_inc_68, filename = here::here("docs", "figs", paste0(prefix, "_inc_68", ext)), width = 8, height = 4)  
  ggsave(gs$g_avt_sch, filename = here::here("docs", "figs", paste0(prefix, "_avt_sch", ext)), width = 6, height = 4)  
  ggsave(gs$g_avt_old, filename = here::here("docs", "figs", paste0(prefix, "_avt_oo", ext)), width = 6, height = 4)  
  
  
  return(gs)
}





