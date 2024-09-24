library(tidyverse)
library(readxl)
library(ggpubr)

theme_set(theme_bw())


apply_lor <- function(p0, lor) 1 / (1 + exp(-log(p0 / (1 - p0)) - lor))


dat_ve <- read_xlsx(here::here("data", "VE.xlsx"), sheet = 1) %>% 
  filter(Use) %>% 
  filter(Type == "HZ") %>% 
  mutate(
    M = M / 100,
    L = L / 100, 
    U = U / 100,
    Yr = gsub("yr", "", `Sub-group`),
    Yr = ifelse(is.na(Yr), 1.5, as.numeric(Yr))
  )



load(here::here("pars", "pars_ve_lor.rdata"))
load(here::here("pars", "pars_ve_zvl_rwa.rdata"))
load(here::here("pars", "pars_ve_rzv_rw_zlg.rdata"))



## ZVL VE -----
pars_ve_zvl %>% 
  mutate(AgeVac = Age - Yr) %>% 
  filter(AgeVac %in% seq(70, 75, 5)) %>% 
  filter(Yr < 20) %>% 
  #filter(Yr %in% c(5, 10)) %>% 
  group_by(IC, Vaccine, AgeVac, Yr) %>% 
  summarise(
    VE = mean(VE)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Yr, y = VE, colour = as.character(AgeVac))) +
  scale_y_continuous("Vaccine effectiveness, %", labels = scales::percent) +
  scale_x_continuous("Year since vaccination",) +
  expand_limits(y = 0:1, x = 0)



ve_zvl <- pars_ve_zvl %>% 
  mutate(
    AgeVac = Age - Yr,
    Tag = paste0("ZVL vaccinated at ", AgeVac)
  ) %>% 
  filter(AgeVac %in% seq(70, 75, 5)) %>% 
  filter(Yr <= 20) %>% 
  #filter(Yr %in% c(5, 10)) %>% 
  group_by(Tag, Yr) %>% 
  summarise(
    VE = mean(VE)
  )



g_zvl_gof <- pars_ve_zvl%>%
  filter(Age - Yr == 70) %>% 
  filter(Yr <= 20) %>% 
  group_by(Yr) %>% 
  summarise(
    M = mean(VE),
    L = quantile(VE, 0.025),
    U = quantile(VE, 0.975)
  ) %>% 
  ggplot() +
  geom_ribbon(aes(x = Yr, ymin = L, ymax = U), alpha = 0.2) +
  geom_line(aes(x = Yr, y = M)) +
  geom_pointrange(data = dat_ve %>% filter(Source == "Klein 2023"), aes(x = Yr, y = M, ymin = L, ymax = U)) +
  scale_y_continuous("Vaccine effectiveness, %", label = scales::percent) +
  scale_x_continuous("Year since vaccinated") +
  expand_limits(y = 0:1)


g_zvl_var <- ve_zvl %>% 
  ggplot() +
  geom_line(aes(x = Yr, y = VE, colour = Tag)) +
  geom_pointrange(data = dat_ve %>% filter(Source == "Klein 2023"), aes(x = Yr, y = M, ymin = L, ymax = U)) +
  scale_y_continuous("Vaccine effectiveness, %", labels = scales::percent) +
  scale_x_continuous("Year since vaccinated", breaks = c(1, seq(5, 20, 5))) +
  scale_colour_discrete("") +
  expand_limits(y = 0:1, x = 20) +
  theme(legend.position = c(0, 0), legend.justification = c(-0.1, -0.1))



g_zvl <- ggpubr::ggarrange(
  g_zvl_gof + labs(subtitle = "(A) Goodness of fit"), 
  g_zvl_var + labs(subtitle = "(B) Vaccine effectiveness by age of vaccination")
)


g_zvl


## RZV VE -----
d <- pars_ve_rzv%>% 
  filter(Yr <= 20) %>% 
  #filter(Yr %in% c(5, 10)) %>% 
  group_by(Vaccine, Yr) %>% 
  summarise(
    VE = median(VE)
  ) %>% 
  mutate(
    Tag = "Real-world, two doses (Baseline)"
  )


ve_rzv <- bind_rows(
  d, 
  d %>% 
    mutate(
      VE = apply_lor(VE, - lor_rw),
      Tag = "Trial, two doses"
    ),
  d %>% 
    mutate(
      VE = apply_lor(VE, lor_single),
      Tag = "Real-world, single dose"
    ),
  d %>%
    mutate(
      VE = apply_lor(VE, lor_re),
      Tag = "Real-world, two doses after ZVL"
    ),
  d %>%
    mutate(
      VE = apply_lor(VE, lor_re + lor_single),
      Tag = "Real-world, single dose after ZVL"
    )
) %>% 
  mutate(
    Tag = factor(Tag, c("Trial, two doses", 
                        "Real-world, two doses (Baseline)", 
                        "Real-world, two doses after ZVL", 
                        "Real-world, single dose",
                        "Real-world, single dose after ZVL"))
  )


g_rzv_gof <- pars_ve_rzv %>% 
  filter(Yr <= 20) %>% 
  group_by(Yr) %>% 
  summarise(
    M = mean(apply_lor(VE, - lor_rw)),
    L = quantile(apply_lor(VE, - lor_rw), 0.025),
    U = quantile(apply_lor(VE, - lor_rw), 0.975)
  ) %>% 
  ggplot() +
  geom_ribbon(aes(x = Yr, ymin = L, ymax = U), alpha = 0.2) +
  geom_line(aes(x = Yr, y = M)) +
  geom_pointrange(data = dat_ve %>% filter(!Realworld), aes(x = Yr, y = M, ymin = L, ymax = U)) +
  scale_y_continuous("Vaccine efficacy, %", label = scales::percent) +
  scale_x_continuous("Year since vaccinated") +
  expand_limits(y = 0)


g_rzv_var <- ve_rzv %>% 
  ggplot() +
  geom_line(aes(x = Yr, y = VE, colour = Tag)) +
  geom_pointrange(data = dat_ve %>% filter(!Realworld), aes(x = Yr, y = M, ymin = L, ymax = U)) +
  scale_y_continuous("Vaccine efficacy/effectiveness, %", labels = scales::percent) +
  scale_x_continuous("Year since vaccinated", breaks = c(1, seq(5, 20, 5))) +
  scale_colour_discrete("") +
  expand_limits(y = 0:1, x = 20) +
  theme(legend.position = c(0, 0), legend.justification = c(-0.1, -0.1))



g_rzv <- ggpubr::ggarrange(
  g_rzv_gof + labs(subtitle = "(A) Goodness of fit"), 
  g_rzv_var + labs(subtitle = "(B) Variants of implementation")
)

g_rzv

# Plot focusing on main VE estimate
d_rw <- pars_ve_rzv %>% 
  filter(Yr <= 20) %>% 
  group_by(Yr) %>% 
  summarise(
    M = mean(VE),
    L = quantile(VE, 0.025),
    U = quantile(VE, 0.975)
  )
d_rw$Tag <- "Real-world, two doses (Baseline)"

g_rzv_gof_alt <- pars_ve_rzv %>% 
  filter(Yr <= 20) %>% 
  group_by(Yr) %>% 
  summarise(
    M = mean(apply_lor(VE, - lor_rw)),
    L = quantile(apply_lor(VE, - lor_rw), 0.025),
    U = quantile(apply_lor(VE, - lor_rw), 0.975)
  ) %>% 
  ggplot() +
  geom_ribbon(aes(x = Yr, ymin = L, ymax = U, fill = "Trial, two doses"), alpha = 0.2) +
  geom_line(aes(x = Yr, y = M, colour = "Trial, two doses", linetype = "Trial, two doses")) +
  geom_pointrange(data = dat_ve %>% filter(!Realworld), aes(x = Yr, y = M, ymin = L, ymax = U)) +
  scale_y_continuous("Vaccine efficacy, %", label = scales::percent) +
  scale_x_continuous("Year since vaccinated") +
  scale_colour_manual(NULL, values = c("black"), aesthetics = c("fill", "colour")) +
  scale_linetype_manual(NULL, values = c("32")) +
  expand_limits(y = 0) +
  theme(legend.position = c(0, 0), legend.justification = c(-0.1, -0.1))

g_rzv_var_alt <- ggplot(ve_rzv) +
  geom_ribbon(data = d_rw, aes(x = Yr, ymin = L, ymax = U, fill = Tag), alpha = 0.2) +
  geom_line(aes(x = Yr, y = VE, colour = Tag, linetype = Tag)) +
  scale_y_continuous("Vaccine efficacy/effectiveness, %", labels = scales::percent) +
  scale_x_continuous("Year since vaccinated", breaks = c(1, seq(5, 20, 5))) +
  scale_colour_manual(NULL, values = c("black", "#36f", "#36f", "#e3a", "#e3a"), aesthetics = c("fill", "colour")) +
  scale_linetype_manual(NULL, values = c("32", "solid", "32", "solid", "32")) +
  expand_limits(y = 0:1, x = 20) +
  theme(legend.position = c(0, 0), legend.justification = c(-0.1, -0.1))

g_rzv_alt <- ggpubr::ggarrange(
  g_rzv_gof_alt + labs(subtitle = "(A) Goodness of fit"), 
  g_rzv_var_alt + labs(subtitle = "(B) Variants of implementation")
)




## ZVL uptake


load(here::here("data", "processed_vaccine", "coverage.rdata"))
load(here::here("data", "fitted_coverage.rdata"))


d <- coverage %>% 
  mutate(
    Age = Age - 1,
    Year = as.numeric(Year),
    Cohort = Year - Age + 70
  ) %>% 
  filter(Cohort >= 2014)



g_uptake_gof <- pred1$pred %>% 
  filter(Cohort == "2014") %>% 
  ggplot(aes(x = Age - 1)) +
  geom_ribbon(aes(ymin = Coverage_l, ymax = Coverage_u), alpha = 0.2) +
  geom_line(aes(y = Coverage)) +
  geom_point(data = d, aes(x = Age, y = value, colour = as.character(Cohort)), size = rel(2)) +
  geom_line(data = d, aes(x = Age, y = value, colour = as.character(Cohort))) +
  scale_y_continuous("Coverage, %", labels = scales::percent) +
  scale_colour_discrete("Cohort (year of 70 YOA)") +
  scale_x_continuous("Age", breaks = seq(70, 80, 2)) +
  expand_limits(y = c(0, 1)) +
  theme(legend.position = c(1, 0), legend.justification = c(1.1, -0.1))



g_uptake_gof


ggsave(g_zvl, filename = here::here("docs", "figs", "g_vaccine_ve_zvl.png"), width = 12, height = 6)
ggsave(g_rzv, filename = here::here("docs", "figs", "g_vaccine_ve_rzv.png"), width = 12, height = 6)
ggsave(g_rzv_alt, filename = here::here("docs", "figs", "g_vaccine_ve_rzv_alt.png"), width = 12, height = 6)
ggsave(g_uptake_gof, filename = here::here("docs", "figs", "g_vaccine_uptake.png"), width = 6, height = 4.5)


## Epidemiology



load(file = here::here("pars", "parset_nic_c35q35y24n1k_realworld.rdata"))

pars_set$Epidemiology %>% 
  ggplot(aes(x = Age, y = r_hz * (1 - p_gp))) +
  stat_lineribbon() +
  scale_y_continuous("Incidence of HZ-related hospitalisation, per 100k", labels = scales::number_format(scale = 1e5)) +
  scale_fill_brewer()




