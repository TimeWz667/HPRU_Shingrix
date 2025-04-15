
vis_proj <- function(stats_proj) {
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
  
  
  gs$g_inc_all <- stats_proj$stats_proj_all %>% 
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
  
  
  gs$g_inc_agp <- stats_proj$stats_proj_agp %>% 
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
  
  
  gs$g_inc_68 <- stats_proj$stats_proj_68 %>% 
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
  
  gs$g_inc_bind <- ggpubr::ggarrange(
    gs$g_inc_all + labs(subtitle = "(A)") + theme(legend.position = "None"),
    gs$g_inc_68 + labs(subtitle = "(B)") + theme(legend.position = "right"),
    nrow = 2, common.legend = F
  )
  
  gs$g_avt_sch <- stats_proj$diff_proj_zvl %>%
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
  
  
  gs$g_avt_old <- stats_proj$diff_proj_ch %>%
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
  
  
  return(gs)
}


save_fig_proj <- function(gs, prefix = "", folder = NA, ext = ".pdf") {
  require(tidyverse)
  
  if (!is.na(folder)) {
    root <- here::here("docs", "figs", folder)
    dir.create(root, showWarnings = F)
  } else {
    root <- here::here("docs", "figs")
  }
  
  if (prefix != "") {
    prefix <- glue::as_glue("g_") + prefix + "_"
  } else {
    prefix <- glue::as_glue("g_")
  }
  
  ggsave(gs$g_inc_all, filename = here::here(root, prefix + prefix + "inc_all" + ext), width = 6, height = 4)  
  ggsave(gs$g_inc_agp, filename = here::here(root, prefix + "inc_agp" + ext), width = 15, height = 4)  
  ggsave(gs$g_inc_68, filename = here::here(root, prefix + "inc_68" + ext), width = 8, height = 4)  
  ggsave(gs$g_inc_bind, filename = here::here(root, prefix + "inc_bind" + ext), width = 8, height = 7)  
  ggsave(gs$g_avt_sch, filename = here::here(root, prefix + "avt_sch" + ext), width = 6, height = 4)  
  ggsave(gs$g_avt_old, filename = here::here(root, prefix + "avt_oo" + ext), width = 6, height = 4)
  
}




