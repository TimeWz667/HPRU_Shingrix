library(tidyverse)
library(jsonlite)


ds <- dir("docs/tabs")
ds <- ds[endsWith(ds, "_ce.csv") | endsWith(ds, "_ys.csv")]


for (file in ds[endsWith(ds, "_ce.csv")]) {
  tab <- read_csv(here::here("docs", "tabs", file))

  if (startsWith(file, "tab_re")) {
    tab %>% 
      select(-c(A, L, U)) %>% 
      group_by(Scenario, Arm, Age0, Age1) %>% 
      pivot_wider(names_from = Index, values_from = M) %>% 
      write_json(here::here("json", gsub(".csv", ".json", file)))
  } else {
    tab %>% 
      select(-c(A, L, U)) %>% 
      group_by(Scenario, Arm, Age0) %>% 
      pivot_wider(names_from = Index, values_from = M) %>% 
      write_json(here::here("json", gsub(".csv", ".json", file)))
  }
}


for (file in ds[endsWith(ds, "_ys.csv")]) {
  tab <- read_csv(here::here("docs", "tabs", file))
  
  if (startsWith(file, "tab_re")) {
    tab %>% 
      select(-c(A, L, U)) %>% 
      group_by(Scenario, Arm, Age0, Age1) %>% 
      pivot_wider(names_from = Index, values_from = M) %>% 
      write_json(here::here("json", gsub(".csv", ".json", file)))
  } else {
    tab %>% 
      select(-c(A, L, U)) %>% 
      group_by(Scenario, Arm, Age0) %>% 
      pivot_wider(names_from = Index, values_from = M) %>% 
      write_json(here::here("json", gsub(".csv", ".json", file)))
  }
}

