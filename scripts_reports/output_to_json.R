library(tidyverse)
library(jsonlite)


for (vtype in c("rw", "tr")) {
  for (dis in c("0", "15", "35")) {
    folder <- glue::as_glue(vtype) + "_" + dis
    
    read_csv(here::here("docs", "tabs", folder, "stats_uv_ce.csv")) %>% 
      select(-c(A, L, U)) %>% 
      group_by(Scenario, Arm, Age0) %>% 
      pivot_wider(names_from = Index, values_from = M) %>% 
      write_json(here::here("json", "stats_ce_uv_" + folder + ".json"))
    
    read_csv(here::here("docs", "tabs", folder, "stats_re_ce.csv")) %>% 
      filter(Index != "N0") %>% 
      select(-c(A, L, U)) %>% 
      group_by(Scenario, Arm, Age0, Age1) %>% 
      pivot_wider(names_from = Index, values_from = M) %>%
      write_json(here::here("json", "stats_ce_re_" + folder + ".json"))
    
    
    read_csv(here::here("docs", "tabs", folder, "stats_uv_ys.csv")) %>% 
      select(-c(A, L, U)) %>% 
      group_by(Scenario, Arm, Age0) %>% 
      pivot_wider(names_from = Index, values_from = M) %>% 
      write_json(here::here("json", "stats_ys_uv_" + folder + ".json"))
    
    read_csv(here::here("docs", "tabs", folder, "stats_re_ys.csv")) %>% 
      select(-c(A, L, U)) %>% 
      group_by(Scenario, Arm, Age0, Age1) %>% 
      pivot_wider(names_from = Index, values_from = M) %>% 
      write_json(here::here("json", "stats_ys_re_" + folder + ".json"))
    
    
  }
}

