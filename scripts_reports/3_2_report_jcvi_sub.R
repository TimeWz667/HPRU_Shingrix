library(tidyverse)

theme_set(theme_bw())


dir.create(here::here("docs", "report_scm"), showWarnings = F)
output_file <- function(file) here::here("docs", "report_scm", file)


## Population data
pop <- local({
  load(here::here("data", "processed_demography", "Population_ONS.rdata"))
  demo_ons %>% 
    filter(Location == "England" & Year == 2023) %>% 
    select(Age, N)
  
})



for (ve_type in c("realworld", "trial")) {
  ve_type <- glue::as_glue(ve_type)
  
  stats_icer <- read_csv(here::here("docs", "tabs", "stats_icer_5yr_rzv_" + ve_type + ".csv")) %>% 
    select(Scenario, Agp0, Arm, matches("ICER(\\d+)_(M|L|U)")) %>% 
    pivot_longer(-c(Scenario, Agp0, Arm), 
                 names_to = c("Price", "name"), names_pattern = "ICER(\\d+)_(\\S)") %>% 
    pivot_wider()
  
  
  g_ce <- stats_icer %>%
    mutate(Price = factor(as.numeric(Price))) %>% 
    filter(Arm == "Vac") %>% 
    ggplot(aes(x = Agp0)) +
    geom_pointrange(aes(y = M, ymin = L, ymax = U, colour = Price), position = position_dodge(0.5)) +
    geom_hline(yintercept = 2e4, linetype = 2) +
    geom_hline(yintercept = 3e4, linetype = 2) +
    scale_x_discrete("Age of vaccination") +
    scale_y_continuous("Incremental cost-effectiveness ratio, \ncost per QALY gained, GBP", 
                       breaks = 0:15 * 1e4, labels = scales::label_dollar(prefix = "")) +
    scale_color_discrete("RZV price\nper admin.", guide = guide_legend(reverse = T)) +
    expand_limits(y = 0)

  ggsave(g_ce, file = output_file("Fig_RZV_ICER_" + ve_type + ".png"), width = 5.5, height = 4.5)
  

  ## Threshold price
  stats_ce <- read_csv(here::here("docs", "tabs", "stats_ce_5yr_rzv_" + ve_type + ".csv"))
  
  g_tp <- stats_ce %>%
    filter(Arm == "Vac") %>% 
    ggplot(aes(x = Agp0)) +
    geom_point(aes(y = Thres20_50, colour = "50% CE given 20,000 WTP")) +
    geom_point(aes(y = Thres30_90, colour = "90% CE given 30,000 WTP")) +
    scale_y_continuous("Threshold price, per adminstration") +
    scale_x_discrete("Age of RZV vaccination") +
    scale_colour_discrete("Scenario") +
    expand_limits(y = c(0, 150))
  
  ggsave(g_tp, file = output_file("Fig_RZV_Thres_" + ve_type + ".png"), width = 7, height = 4.5)
  
  
  
  ## [Fig 3] Incremental cost-effectiveness ratios (ICERs) of RZV revaccination for ZVL covered population by age of revaccination.  (cohort models)
  stats_icer <- read_csv(here::here("docs", "tabs", "stats_icer_5yr_zvl2rzv_" + ve_type + ".csv")) %>% 
    select(Scenario, Age0, Age1, Arm, matches("ICER(\\d+)_(M|L|U)")) %>% 
    pivot_longer(-c(Scenario, Age0, Age1, Arm), 
                 names_to = c("Price", "name"), names_pattern = "ICER(\\d+)_(\\S)") %>% 
    pivot_wider()
  
  
  g_ce <- stats_icer %>%
    filter(Age0 %in% c(70, 75)) %>% 
    mutate(
      a0 = paste0("Age of ZVL vaccination: ", Age0),
      Price = factor(as.numeric(Price))
    ) %>% 
    ggplot(aes(x = Age1)) +
    geom_pointrange(aes(y = M, ymin = L, ymax = U, colour = Price), position = position_dodge(0.5)) +
    geom_hline(yintercept = 2e4, linetype = 2) +
    geom_hline(yintercept = 3e4, linetype = 2) +
    scale_x_discrete("Age of vaccination") +
    scale_y_continuous("Incremental cost-effectiveness ratio, \ncost per QALY gained, GBP", 
                       breaks = 0:15 * 1e4, labels = scales::label_dollar(prefix = "")) +
    scale_color_discrete("RZV price\nper admin.", guide = guide_legend(reverse = T)) +
    expand_limits(y = 0) +
    facet_grid(Arm~a0, labeller = labeller(Arm = c("ReVac_RZV1"="Single dose RZV", "ReVac_RZV2"="Two doses RZV")))
  
  ggsave(g_ce, file = output_file("Fig_ZVL2RZV_ICER_" + ve_type + ".png"), width = 9, height = 6.5)

  
  ## Threshold price
  stats_ce <- read_csv(here::here("docs", "tabs", "stats_ce_5yr_zvl2rzv_" + ve_type + ".csv"))
  
  g_tp <- stats_ce %>%
    filter(Age0 %in% c(70, 75)) %>% 
    mutate(
      a0 = paste0("Age of ZVL vaccination: ", Age0)
    ) %>% 
    ggplot(aes(x = Age1)) +
    geom_point(aes(y = Thres20_50, colour = "50% CE given 20,000 WTP")) +
    geom_point(aes(y = Thres30_90, colour = "90% CE given 30,000 WTP")) +
    scale_y_continuous("Threshold price, per adminstration") +
    scale_x_discrete("Age of RZV vaccination") +
    scale_colour_discrete("Scenario") +
    expand_limits(y = c(0, 100)) +
    facet_grid(Arm~a0, labeller = labeller(Arm = c("ReVac_RZV1"="Single dose RZV", "ReVac_RZV2"="Two doses RZV")))
  
  g_tp
  
  ggsave(g_tp, file = output_file("Fig_ZVL2RZV_Thres_" + ve_type + ".png"), width = 9, height = 6.5)
  
}







