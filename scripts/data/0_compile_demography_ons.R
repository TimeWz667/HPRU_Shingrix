library(tidyverse)
library(readxl)



demo_proj_as <- read_xls(here::here("data", "raw_demography", "ukpppopendata2020.xls"), sheet = "Population") %>% 
  pivot_longer(-c(Sex, Age), names_to = "Year", values_to = "N") %>% 
  mutate(
    Age = ifelse(Age %in% as.character(0:100), Age, "100"),
    Age = as.numeric(Age),
    Year = as.numeric(Year),
    Sex = ifelse(Sex == 1, "m", "f")
  ) 


demo_proj <- demo_proj_as %>% 
  group_by(Age, Year) %>% 
  summarise(N = sum(N)) %>% 
  ungroup() %>% 
  arrange(Year, Age, N)


demo_pre_as <- bind_rows(lapply(2012:2020, function(yr) {
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
    select(Location = variable, Year, Age, Sex, N = value)
})) %>% 
  mutate(
    Location = ifelse(Location == "ENGLAND", "England", "UK")
  )


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


mor_proj <- bind_rows(
  read_xlsx(here::here("data", "raw_demography", "ukppp20qx.xlsx"), sheet = "males period qx", skip = 4) %>% 
    pivot_longer(-age, names_to = "Year", values_to = "mortality") %>% 
    rename(Age = age) %>% 
    mutate(Sex = "m"),
  read_xlsx(here::here("data", "raw_demography", "ukppp20qx.xlsx"), sheet = "females period qx", skip = 4) %>% 
    pivot_longer(-age, names_to = "Year", values_to = "mortality") %>% 
    rename(Age = age) %>% 
    mutate(Sex = "f")
) %>% 
  mutate(
    mortality = mortality * 1e-5,
    Year = as.numeric(Year)
  ) 


demo_ons <- mor_proj %>% 
  inner_join(
    bind_rows(
      demo_pre_as %>% mutate(Type = "History"),
      demo_proj_as %>% filter(Year > 2020) %>% mutate(Location = "UK", Type = "Projection"),
      demo_proj_as %>% filter(Year > 2020) %>% mutate(Location = "England", N = round(N * rat), Type = "Projection")
    )
  ) %>% 
  group_by(Location, Year, Age, Type) %>% 
  summarise(
    mortality = weighted.mean(mortality, N),
    N = sum(N)
  ) %>% 
  ungroup() %>% 
  arrange(Location, Year, Age)

save(demo_ons, file = here::here("data", "processed_demography", "Population_ONS.rdata"))
