## BAC @ MC 2021 
## This data stems from the NYC Dept of Health COVID data repository on github

library(tidyverse)
library(tidymodels)
library(readxl)
library(sf)
library(glue)
library(embed)
library(fpc)
library(nominatim) # devtools::install_github("hrbrmstr/nominatim")
library(ggtext)
library(patchwork)
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

joined_sf = shape_file %>% st_join(sub_boro_centers, join = st_contains)

missing_sb = sub_boro_centers %>% 
  as_tibble() %>% 
  select(name) %>% 
  distinct() %>% 
  anti_join(., joined_sf %>% 
                as_tibble() %>% 
                select(name))

pl = shape_file %>% 
  ggplot() +
  geom_sf(color = "black") +
  geom_sf(data = sub_boro_centers)

p2 = joined_sf %>% 
  left_join(., hrate, by = 'sub_borough_area') %>%
  mutate(rate = (COVID_CASE_COUNT/POP_DENOMINATOR)/x2017) %>% 
  group_by(BoroCD) %>% 
  mutate(rate = mean(rate, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(n = ntile(rate, 4)) %>% 
  ggplot() +
  geom_sf(aes(fill = rate))

pl|p2

names = read_xlsx(glue(path, "/Data/sub_boro_cd_conversion.xlsx"), 
                  sheet = 1) %>% 
  mutate(sub_borough_area = ifelse(is.na(sub_borough_area), census_name, sub_borough_area)) %>% 
  mutate(boro_code = strsplit(as.character(boro_cd), ", ")) %>% 
  unnest(boro_code) %>% 
  transform(boro_code = as.numeric(boro_code))

hrate %>% 
  select(sub_borough_area, x2017) %>% 
  left_join(., names, by = 'sub_borough_area') %>% 
  left_join(., shape_file, by = c('boro_code' = 'BoroCD')) %>%
  ungroup() %>% 
  mutate(n = ntile(x2017, 4)) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_gradient(high = "#9C1F2E", low = "white") +
  theme_void() +
  coord_sf() +
  theme(legend.position = 'bottom',
        text = element_text(family = 'lato', size = 30)) +
  labs(title = "Community District Quartiled Homeownership Rate", 
       # caption = "Data as of April 14, 2021",
       fill = "Quartile: ")

hrate %>% 
  select(sub_borough_area, x2017) %>% 
  left_join(., names, by = 'sub_borough_area') %>% 
  left_join(., shape_file, by = c('boro_code' = 'BoroCD')) %>%
  ungroup() %>% 
  mutate(rate = (COVID_CASE_COUNT/POP_DENOMINATOR)/x2017,
         n = ntile(rate, 4)) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_gradient(high = "#9C1F2E", low = "white") +
  theme_void() +
  coord_sf() +
  theme(legend.position = 'bottom',
        text = element_text(family = 'lato', size = 30)) +
  labs(title = "Community District Quartiled Homeownership COVID-19 Index", 
       caption = "Data as of April 14, 2021",
       fill = "Quartile: ")

## Building Footprint -----------------------------------------------------------------------------

building = read_sf("C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021\\Shapefiles\\Building Footprints.geojson")

building %>% 
  ggplot() +
  geom_sf()

## 2020 Data File ---------------------------------------------------------------------------------

dat = read_csv(glue(path, "/Data/nyc_agg_data.csv")) %>% 
  mutate(across(where(is.character), ~replace_na(.x, "Missing")))

## Similarity Algorithm ---------------------------------------------------------------------------

get_slope_all = function(group){
  dt = temp %>% 
    filter(boro_code == group) %>% 
    relocate(contains('code'), .before = 1) %>% 
    select(starts_with('x')) %>% 
    pivot_longer(everything(), names_to = 'year', values_to = 'value') %>% 
    mutate(year = str_remove(year, '^x'),
           year = as.numeric(year)-2000) %>% 
    filter(year != 0) %>% 
    mutate(value = replace_na(value, mean(value, na.rm = T)))
  
  l = lm(value~year, data = dt) %>% summary()
  ret = l$coefficients[2] %>% as.numeric()
  return(ret)
}

data = names %>% select(boro_code) %>% distinct()

for (sheet in readxl::excel_sheets(glue(path, "/Data/NYC-housing-data.xlsx"))){
  
  print(sheet)
  
  sheet_sub = sheet %>% 
    str_extract("[a-z ]{10}") %>% 
    str_replace_all(' ', '_')
  
  temp = read_xlsx(glue(path, "/Data/NYC-housing-data.xlsx"), sheet = sheet) %>% 
    janitor::clean_names('snake') %>% 
    select(-starts_with('short_'), -starts_with('long_')) %>% 
    janitor::remove_empty("cols")
  
  if ("community_district" %in% variable.names(temp)){
    
    temp = temp %>% 
      mutate(code_prefix = str_extract(community_district, pattern = "^[A-Za-z]{2}"),
             code_suffix = str_extract(community_district, pattern = "\\d{2}")) %>% 
      mutate(code_prefix = case_when(code_prefix== "MN" ~ 1,
                                     code_prefix== "BX" ~ 2,
                                     code_prefix== "BK" ~ 3,
                                     code_prefix== "QN" ~ 4,
                                     code_prefix== "SI" ~ 5),
             boro_code = glue("{code_prefix}{code_suffix}") %>% as.numeric()) %>% 
      select(-community_district, -code_prefix, -code_suffix)
    
  } else if ("sub_borough_area" %in% variable.names(temp)) {
    
    temp2 = temp %>% left_join(., names, by = 'sub_borough_area') 
    
    test = temp2 %>% 
      select('sub_borough_area') %>% 
      anti_join(., names) %>% 
      select(sub_borough_area) %>% 
      filter(!is.na(sub_borough_area)) %>% 
      distinct()
    
    if (nrow(test) > 1){
      temp2 = test %>% left_join(., names, by = c('sub_borough_area' = 'census_name'))
    }
    
    temp = temp2
    rm(temp2)
    
  }
  
  if (sheet == "Crowding"){
    temp = temp %>% 
      mutate(geography = str_remove_all(geography, '^borough/PUMA\\) - ')) %>% 
      filter(geo_type_name == 'Neighborhood (Community District)') %>% 
      mutate(code_suffix = str_extract(geography, pattern = "\\d{1,2}"),
             code_suffix = ifelse(nchar(code_suffix) == 1, 
                                  glue("0{code_suffix}") %>% as.character(),
                                  code_suffix)) %>% 
      mutate(code_prefix = case_when(borough == "Manhattan" ~ 1,
                                     borough == "Bronx" ~ 2,
                                     borough == "Brooklyn" ~ 3,
                                     borough == "Queens" ~ 4,
                                     borough == "Staten" ~ 5),
             boro_code = glue("{code_prefix}{code_suffix}") %>% as.numeric()) %>% 
      select(boro_code, number, percent_of_households)
    
  }
  
  if ('x2010' %in% variable.names(temp)) {
    
    temp = temp %>%
      relocate(contains('c'), .before = 1) %>% 
      select(boro_code, last_col()) %>% 
      mutate(slope = map(boro_code, ~get_slope_all(.x)) %>% as.numeric()) %>%
      mutate(m_dir = ifelse(slope > 0, 'POSITIVE', 'NEGATIVE')) %>% 
      dplyr::rename_with(~glue('{sheet_sub}_{.x}'), !starts_with('boro'))
    
  } else {
    
    temp = temp %>% rename_with(~glue('{sheet_sub}_{.x}'), !starts_with('boro'))
    
  }
  
  data = data %>% left_join(., temp, by = "boro_code")  
}

for (sheet in readxl::excel_sheets(glue(path, "/Data/NYC-demographic-other-data.xlsx"))[1:12]){
  
  print(sheet)
  
  sheet_sub = sheet %>% 
    str_extract("[a-z ]{10}") %>% 
    str_replace_all(' ', '_')
  
  temp = read_xlsx(glue(path, "/Data/NYC-demographic-other-data.xlsx"), sheet = sheet) %>% 
    janitor::clean_names('snake') %>% 
    select(-starts_with('short_'), -starts_with('long_')) %>% 
    janitor::remove_empty("cols")
  
  if ("community_district" %in% variable.names(temp)){
    temp = temp %>% 
      mutate(code_prefix = str_extract(community_district, pattern = "^[A-Za-z]{2}"),
             code_suffix = str_extract(community_district, pattern = "\\d{2}")) %>% 
      mutate(code_prefix = case_when(code_prefix== "MN" ~ 1,
                                     code_prefix== "BX" ~ 2,
                                     code_prefix== "BK" ~ 3,
                                     code_prefix== "QN" ~ 4,
                                     code_prefix== "SI" ~ 5),
             boro_code = glue("{code_prefix}{code_suffix}") %>% as.numeric()) %>% 
      select(-community_district, -code_prefix, -code_suffix)
    
  } else if ("sub_borough_area" %in% variable.names(temp)) {
    
    temp2 = temp %>% left_join(., names, by = 'sub_borough_area') 
    
    test = temp2 %>% 
      select('sub_borough_area') %>% 
      anti_join(., names) %>% 
      select(sub_borough_area) %>% 
      filter(!is.na(sub_borough_area)) %>% 
      distinct()
    
    if (nrow(test) > 1){
      temp2 = test %>% left_join(., names, by = c('sub_borough_area' = 'census_name'))
    }
    
    temp = temp2
    rm(temp2)
    
  }
  
  if ("geography" %in% variable.names(temp)){
    
    temp = temp %>% 
      mutate(geography = str_remove_all(geography, '^borough/PUMA\\) - ')) %>% 
      filter(geo_type_name == 'Neighborhood (Community District)') %>% 
      mutate(code_suffix = str_extract(geography, pattern = "\\d{1,2}"),
             code_suffix = ifelse(nchar(code_suffix) == 1, 
                                  glue("0{code_suffix}") %>% as.character(),
                                  code_suffix)) %>% 
      mutate(code_prefix = case_when(borough == "Manhattan" ~ 1,
                                     borough == "Bronx" ~ 2,
                                     borough == "Brooklyn" ~ 3,
                                     borough == "Queens" ~ 4,
                                     borough == "Staten" ~ 5),
             boro_code = glue("{code_prefix}{code_suffix}") %>% as.numeric()) %>% 
      select(boro_code, number, percent)
    
  }
  
  if ('x2010' %in% variable.names(temp) | 'x2014' %in% variable.names(temp)) {
    
    temp = temp %>%
      relocate(contains('c'), .before = 1) %>% 
      select(boro_code, last_col()) %>% 
      mutate(slope = map(boro_code, ~get_slope_all(.x)) %>% as.numeric()) %>%
      mutate(m_dir = ifelse(slope > 0, 'POSITIVE', 'NEGATIVE')) %>% 
      dplyr::rename_with(~glue('{sheet_sub}_{.x}'), !starts_with('boro'))
    
  } else {
    
    temp = temp %>% rename_with(~glue('{sheet_sub}_{.x}'), !starts_with('boro'))
    
  }
  
  data = data %>% left_join(., temp, by = "boro_code")  
}

homes = read_csv("https://raw.githubusercontent.com/andrewargeros/BAC-MC-2020/master/Data/Housing_New_York_Units_by_Building.csv") %>% 
  janitor::clean_names("snake") %>% 
  mutate(community_board = str_remove(community_board, "-")) %>% 
  transform(project_start_date = as.Date(project_start_date, "%m/%d/%Y")) %>% 
  mutate(code_prefix = str_extract(community_board, pattern = "^[A-Za-z]{2}"),
         code_suffix = str_extract(community_board, pattern = "\\d{2}")) %>% 
  mutate(code_prefix = case_when(code_prefix== "MN" ~ 1,
                                 code_prefix== "BX" ~ 2,
                                 code_prefix== "BK" ~ 3,
                                 code_prefix== "QN" ~ 4,
                                 code_prefix== "SI" ~ 5),
         boro_code = glue("{code_prefix}{code_suffix}") %>% as.numeric()) %>% 
  group_by(boro_code) %>% 
  mutate(lowinc = (extremely_low_income_units + very_low_income_units + low_income_units),
         ownshare = (counted_homeownership_units/total_units)) %>% 
  summarise(total_lowinc = sum(lowinc),
            ownshare = mean(ownshare),
            total_units = sum(total_units),
            lowshare = total_lowinc/total_units,
            total_ownable = sum(counted_homeownership_units),
            projects = n_distinct(project_id))

data = data %>% 
  mutate(across(ends_with('m_dir'), as.factor)) %>% 
  mutate(across(ends_with('suffix'), as.factor)) %>% 
  left_join(., homes, by = 'boro_code') %>% 
  mutate(across(total_lowinc:last_col(), ~replace_na(.x, 0)))

fixed_data = recipe(home_owner_x2018 ~ ., data = data) %>% 
  update_role(boro_code, new_role = 'ID') %>% 
  step_scale(all_numeric(), -all_outcomes(), -boro_code) %>% 
  step_center(all_numeric(), -all_outcomes(), -boro_code) %>% 
  step_knnimpute(all_numeric(), -boro_code) %>%
  step_rm(all_nominal()) %>% 
  step_umap(all_predictors(), -boro_code) %>%
  # step_pca(all_predictors(), num_comp = 5) %>% 
  prep(data, retain = T) %>% 
  bake(new_data = NULL)

fixed_data %>% 
  mutate(bp = str_extract(as.character(boro_code), ".")) %>% 
  ggplot() +
  aes(umap_1, umap_2, color = factor(bp)) +
  geom_point(size = 5, alpha = 0.7)

dbs = dbscan(fixed_data, eps = 0.8, MinPts = 7, scale = TRUE, method = 'raw')[["cluster"]] %>% 
  bind_cols(fixed_data %>% select(boro_code, umap_1, umap_2)) %>% 
  rename('cluster' = 1)

dbs %>% 
  ggplot() +
  aes(umap_1, umap_2, color = factor(cluster)) +
  geom_point(size = 5, alpha = 0.7)
