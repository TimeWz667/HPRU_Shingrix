sens_price <- function(yss_uv) {
  # yss_uv <- tar_read(yss_uv, 2)
  require(tidyverse)
  
  yss_diff <- local({
    temp <- yss_uv %>% 
      pivot_longer(-c(Scenario, Age0, Arm, Key, N0, Year0), names_to = "Index")
    
    temp %>% 
      filter(Arm != "SOC") %>% 
      left_join(
        temp %>% 
          filter(Arm == "SOC") %>% 
          select(Scenario, Age0, Key, Index, value0 = value),
        relationship = "many-to-many"
      ) %>% 
      mutate(
        Index = paste0("d", Index),
        Diff = value - value0
      ) %>% 
      select(-value, -value0) %>% 
      pivot_wider(names_from = Index, values_from = "Diff") %>% 
      mutate(
        ICER = dC_All_d / dQ_All_d,
        Price0 = dC_VacRZV_d / dN_VacRZV_d,
        Thres20 = (dQ_All_d * 2e4 - dC_Med_d) / dN_VacRZV_d,
        Thres30 = (dQ_All_d * 3e4 - dC_Med_d) / dN_VacRZV_d,
      )
    
  }) %>% 
    filter(Age0 == 70)
  
  
  yd_v1 <- yss_diff %>% filter(Arm == "RZV_1d")
  yd_v2 <- yss_diff %>% filter(Arm == "RZV_2d")
  
  
  sens_price <- bind_rows(
    tibble(Price = seq(0, 300, 5), Arm = "Vac1") %>% 
      mutate(
        PrCE20 = sapply(Price, \(x) mean(yd_v1$Thres20 > x)),
        PrCE30 = sapply(Price, \(x) mean(yd_v1$Thres30 > x))
      ),
    tibble(Price = seq(0, 300, 5), Arm = "Vac2") %>% 
      mutate(
        PrCE20 = sapply(Price, \(x) mean(yd_v2$Thres20 > x)),
        PrCE30 = sapply(Price, \(x) mean(yd_v2$Thres30 > x))
      )
  ) 
    
  

  return(sens_price)
  
}



summarise_sens_price <- function(sens_price, prefix = "", folder = NA, ext = ".pdf") {
  require(tidyverse)
  
  if (!is.na(folder)) {
    root_tab <- here::here("docs", "tabs", folder)
    root_fig <- here::here("docs", "figs", folder)
    dir.create(root_tab, showWarnings = F)
    dir.create(root_fig, showWarnings = F)
  } else {
    root_tab <- here::here("docs", "tabs")
    root_fig <- here::here("docs", "figs")
  }
  
  if (prefix != "") {
    prefix_tab <- glue::as_glue(prefix) + "_"
    prefix_fig <- glue::as_glue("g_") + prefix + "_"
  } else {
    prefix_tab <- glue::as_glue("")
    prefix_fig <- glue::as_glue("g_")
  }
  
  write_csv(sens_price, here::here(root_tab, prefix_tab + "sens_price.csv"))

  
  g_psa <- ggplot(sens_price) +
    geom_line(aes(x = Price, y = PrCE20, colour = Arm, linetype = "£20,000")) +
    geom_line(aes(x = Price, y = PrCE30, colour = Arm, linetype = "£30,000")) + 
    scale_y_continuous("Probability of cost-effecive, %", label = scales::percent, breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)) +
    scale_x_continuous("Cost per administration, £") +
    scale_linetype_discrete("Willingness to pay") +
    scale_color_discrete("Type", label = c(Vac1 = "Single dose", Vac2 = "Two doses")) +
    theme(legend.position = c(1, 1), legend.justification = c(1.2, 1.2))
  
  ggsave(g_psa, filename = here::here(root_fig, prefix_fig + "sens_price" + ext), width = 8, height = 5.5)
}

