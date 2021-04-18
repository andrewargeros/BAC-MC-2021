## BAC @ MC 2021 
## This data stems from the NYC Dept of Health COVID data repository on github

library(tidyverse)
library(tidymodels)
library(readxl)
library(sf)
library(glue)
library(nominatim) # devtools::install_github("hrbrmstr/nominatim")
library(ggtext)
library(recipeselectors) # devtools::install_github("stevenpawley/recipeselectors")
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


## Home Ownership Rate and Trend ------------------------------------------------------------------

get_slope = function(group){
  dt = hrate %>% 
    filter(`Sub-Borough Area` == group) %>% 
    select(4:last_col()) %>% 
    pivot_longer(everything(), names_to = 'year', values_to = 'value') %>% 
    mutate(year = as.numeric(year)-2000) %>% 
    filter(year != 0)
  
  l = lm(value~year, data = dt) %>% summary()
  ret = l$coefficients[2] %>% as.numeric()
  return(ret)
  }
  
hrate = read_xlsx(glue(path, "/Data/NYC-housing-data.xlsx"), 
                  sheet = 'home ownership rate') %>% 
  select(!18:last_col()) %>% 
  janitor::clean_names('snake')

hrate_2 = hrate %>% 
  select(sub_borough_area, `x2017`) %>% 
  distinct() %>% 
  rename('sba' = 1,
         'h17' = 2) %>% 
  mutate(slope = map(sba, get_slope) %>% as.numeric()) %>% 
  mutate(slope_scale = scale(slope),
         h17_scale = scale(h17))

hrate_2 %>% filter(str_detect(sba, 'Stuyvesant'))

## SBA to Community -------------------------------------------------------------------------------

crd = read_xlsx(glue(path, "/Data/NYC-housing-data.xlsx"), 
                  sheet = 'Crowding') %>% 
  select(1:8) %>% 
  janitor::clean_names('snake') %>% 
  mutate(geography = str_remove_all(geography, '^borough/PUMA\\) - '))

get_coord = function(place) {
  t = osm_search(place, key = 'KgJM1qj6eHENOw7mMwdfd8qdKXn6Mto8')
  
  if (nrow(t) == 0){
    
    return(NA)
    
  } else if (nrow(t) > 1){
    
    return("Multiple")
    
  } else {
    
    coords = t %>% 
      mutate(coords = glue('{lon}, {lat}') %>% as.character()) %>% 
      select(coords) %>% 
      as.character()
    
    return(coords)
    
  }
}

sub_boro_centers = hrate %>% 
  mutate(name = ifelse(str_count(sub_borough_area, "/") > 0, 
                       str_extract(sub_borough_area, "(.*?)/") %>% str_remove_all('/'),
                       sub_borough_area),
         name = ifelse(name == "Sheepshead Bay", "Brighton Beach", name),
         name2 = glue('{name}, New York City') %>% as.character()) %>% 
  select(sub_borough_area, name, name2) %>% 
  mutate(coords = map(name2, get_coord) %>% as.character()) %>% 
  mutate(coords = ifelse(name == 'South Crown Heights', '-73.944866, 40.671955', coords),
         coords = ifelse(name == 'North Crown Heights', '-73.968728, 40.676602', coords), 
         coords = ifelse(name == 'South Shore', '-74.202289, 40.545318', coords),
         coords = ifelse(name == 'North Shore', '-74.089670, 40.634671', coords),
         coords = ifelse(name == 'Mid-Island', '-74.185937, 40.582574', coords)) %>% 
  mutate(lon = str_extract(coords, "^(.*?),") %>% str_remove(","),
         lat = str_extract(coords, ", (.*?)$") %>% str_remove(", ")) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326, agr = "constant")

joined_df = shape_file %>% 
  st_join(sub_boro_centers, join = st_contains) %>% 
  tibble() %>% 
  select(NEIGHBORHOOD_NAME, name) %>% 
  distinct()

missing_sb = sub_boro_centers %>% 
  as_tibble() %>% 
  select(name) %>% 
  distinct() %>% 
  anti_join(., joined_sf %>% 
                as_tibble() %>% 
                select(name))

shape_file %>% 
  ggplot() +
  geom_sf(color = "black") +
  geom_sf(data = sub_boro_centers)

## 2020 Data File ---------------------------------------------------------------------------------

dat = read_csv(glue(path, "/Data/nyc_agg_data.csv")) %>% 
  mutate(across(where(is.character), ~replace_na(.x, "Missing")))

