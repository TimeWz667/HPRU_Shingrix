

vis_thres <- function(stats_uv, stats_re, prefix, ext = ".png") {
  require(ggplot2)

  gs <- list()

  d_main <- bind_rows(
    stats_uv$stats_ce %>% 
      filter(Index %in% c("Thres20_50", "Thres30_90")) %>% 
      filter(Age0 >= 60) %>% 
      filter((Arm == "RZV_1d" & Age0 >= 80) | Arm == "RZV_2d") %>% 
      mutate(
        Arm = case_when(
          Age0 < 80 & Arm == "RZV_2d" ~ "Target",
          Arm == "RZV_2d" ~ Arm,
          Arm == "RZV_1d" ~ Arm,
          T ~ NA
        )
      ) %>% 
      filter(!is.na(Arm)) %>% 
      select(Age = Age0, Arm, Index, M),
    stats_re$stats_ce %>% 
      filter(Scenario != "Overall") %>% 
      filter(Index %in% c("Thres20_50", "Thres30_90")) %>% 
      filter(Age1 >= 80) %>% 
      filter(Age0 %in% c(70, 75)) %>%
      mutate(Arm = paste(Arm, Age0)) %>% 
      select(Age = Age1, Arm, Index, M)
  )
  
  bound <- d_main %>% 
      filter(Arm == "Target") %>% 
      pivot_wider(names_from = Index, values_from = M) %>% 
      mutate(thres = pmin(Thres20_50, Thres30_90)) %>% 
      pull(thres) %>% range()
    
  tags <- c(
    "Target" = "RZV target\n",
    "RZV_2d" = "Two-doses RZV\n",
    "RZV_1d" = "Single-dose RZV\n",
    "ReRZV_2d 70" = "Two-doses RZV\nZVL at 70 YOA",
    "ReRZV_1d 70" = "Single-doses RZV\nZVL at 70 YOA"
  )
  
  gs$g_panel <- d_main %>% 
    filter(Age <= 95) %>% 
    filter(Arm %in% names(tags)) %>%
    mutate(Arm = factor(Arm, names(tags))) %>% 
    ggplot() +
    geom_line(aes(x = Age, y = M, colour = Index)) +
    geom_hline(yintercept = bound, linetype = 2) +
    scale_y_continuous("Threshold price per adminstration, GBP") +
    scale_color_discrete("Threshold", labels = c(Thres20_50 = "50% CE given 20,000 WTP", Thres30_90 = "90% CE given 30,000 WTP")) +
    facet_grid(.~Arm, scales = "free_x", labeller = labeller(Arm = tags)) +
    expand_limits(y = c(0, 200)) +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 60, hjust = 1))
  
  ggsave(gs$g_panel, filename = here::here("docs", "figs", paste0(prefix, "_panel", ext)), width = 12, height = 5)
  
}


