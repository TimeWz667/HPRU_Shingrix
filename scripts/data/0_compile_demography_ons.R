library(tidyverse)
library(readxl)



demo_proj <- read_xls(here::here("data", "raw_demography", "ukpppopendata2020.xls"), sheet = "Population") %>% 
  pivot_longer(-c(Sex, Age), names_to = "Year", values_to = "N") %>% 
  mutate(
    Age = ifelse(Age %in% as.character(0:100), Age, "100"),
    Age = as.numeric(Age),
    Year = as.numeric(Year)
  ) %>% 
  group_by(Age, Year) %>% 
  summarise(N = sum(N)) %>% 
  ungroup() %>% 
  arrange(Year, Age, N)



demo_pre <- bind_rows(lapply(2012:2020, function(yr) {
  raw_pre <- read_xlsx(here::here("data", "raw_demography", "PopulationByAge.xlsx"), sheet = as.character(yr))
  raw_pre %>% 
    filter(geogcode %in% c("E92000001", "K02000001")) %>% 
    pivot_longer(starts_with(c("m_", "f_"))) %>% 
    separate(name, c("Sex", "Year", "Age")) %>% 
    filter(Age != "al") %>% 
    mutate(
      Age = as.numeric(Age),
      Year = 2000 + as.numeric(Year)
    ) %>% 
    select(Location = variable, Year, Age, Sex, N = value) %>% 
    group_by(Location, Year, Age) %>% 
    summarise(N = sum(N)) %>% 
    ungroup()
})) %>% 
  mutate(
    Location = ifelse(Location == "ENGLAND", "England", "UK")
  )


rat <- demo_pre %>% 
  group_by(Year, Location) %>%
  summarise(N = sum(N)) %>% 
  pivot_wider(names_from = Location, values_from = N) %>% 
  mutate(rat = England / UK) %>% 
  pull(rat) %>% 
  tail(4) %>% 
  mean


demo_ons <- bind_rows(
  demo_pre %>% mutate(Type = "History"),
  demo_proj %>% mutate(Location = "UK", Type = "Projection"),
  demo_proj %>% mutate(Location = "England", N = round(N * rat), Type = "Projection")
)

demo_ons

save(demo_ons, file = here::here("data", "processed_demography", "Population_ONS.rdata"))
