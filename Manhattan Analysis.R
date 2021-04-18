## BAC @ MC 2021 
## This data stems from the NYC Dept of Health COVID data repository on github

library(tidyverse)
library(readxl)
library(sf)
library(glue)
library(ggtext)
library(showtext)
font_add_google(name = "Lato", family = "lato")
showtext::showtext_auto()

path = "C:/RScripts/BAC@MC 2021/BAC-MC-2021"

shape_file = read_sf("C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021\\Shapefiles\\zcta_borocd_merged.geojson") 
            # This file contains ZCTA, BoroCD, and COVID data as of 2021-04-15

shape_file %>% 
  mutate(q_covid_rate = ntile(COVID_CASE_COUNT, 4)) %>% 
  ggplot() +
    geom_sf(aes(fill = q_covid_rate)) +
    scale_fill_gradient(high = "#9C1F2E", low = "white") +
    theme_void() +
    coord_sf() +
    theme(legend.position = 'bottom',
          text = element_text(family = 'lato', size = 30)) +
    labs(title = "Sub-borough Quartiled Rate of COVID-19", 
         caption = "Data as of April 14, 2021",
         fill = "Quartile: ")
 ggsave(glue("{path}/Plots/covid_rate_4tile.png"), height = 6, width = 6, units = 'in')
 
## Capacity --------------------------------------------------------------------------------------- 

h_units = read_xlsx(glue(path, "/Data/NYC-housing-data.xlsx"), sheet = 'housing units') %>% 
  select(!18:last_col()) %>% 
  summarise(across(4:last_col(), sum)) %>% 
  pivot_longer(everything(), names_to = 'year', values_to = "value") %>% 
  mutate(year = as.numeric(year)-2000,
         value = value/1000000)

lm_housing = lm(value~year, data = h_units)
lm_housing %>% summary()

h_units = h_units %>% 
  bind_rows(lm_housing %>% predict(tibble('year' = c(19:25))) %>% 
              tibble() %>% 
              rename('value' = 1) %>% 
              mutate(year = 19:25,
                     value = value) %>% 
              select(year, value)) %>% 
  mutate(value = value * 2.6)


## Population -------------------------------------------------------------------------------------

pop = read_xlsx(glue(path, "/Data/NYC-demographic-other-data.xlsx"), sheet = 'population') %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  pivot_longer(everything(), names_to = 'year', values_to = 'pop') %>% 
  mutate(year = as.numeric(year)-2000,
         pop = pop/1000000)

lm_pop = lm(pop ~ year, data = pop)

pop = pop %>% 
  bind_rows(lm_pop %>% predict(tibble('year' = c(19:25))) %>% 
              tibble() %>% 
              rename('pop' = 1) %>% 
              mutate(year = 19:25) %>% 
              select(year, pop)) %>% 
  mutate(type = ifelse(year > 18, 1, 0)) %>% 
  inner_join(., h_units) %>% 
  mutate(y2 = year + 2000)

pop %>% 
  ggplot() +
  aes(x = y2, y = pop, linetype = factor(type)) +
  geom_line(color = "#9C1F2E", size = 3, lineend = "round") +
  geom_line(aes(y = value), color = "#53565A", size = 3, lineend = "round") +
  scale_y_continuous(labels = scales::unit_format(unit = "M")) +
  theme_minimal() +
  theme(legend.position = 'none', 
        text = element_text(family = 'lato', size = 70),
        plot.title = element_markdown(lineheight = 1.1)) +
  labs(title = "<span style='color:#53565A;'><b>Capactiy</b></span> vs <span style='color:#9C1F2E'><b>Population</b></span> Forecasted Through 2025",
       x = "Year",
       y = "Residents")
ggsave(glue("{path}/Plots/pop_vs_capacity.png"), height = 7, width = 11, units = 'in')


## Homeownership Rate and Trend -------------------------------------------------------------------

hra