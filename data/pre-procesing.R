library(tidyverse) ; library(httr) ; library(readxl) ; library(sf)

# Mid-2019 population estimates for local authorities in England

# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala
# Licence: OGL 3.0

ltla_population <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1820327937...1820328318&date=latest&gender=0&c_age=200&measures=20100&select=geography_code,obs_value") %>% 
  rename(area_code = GEOGRAPHY_CODE, population = OBS_VALUE) %>% 
  filter(str_detect(area_code, "^E")) %>% 
  # combine population estimates for Hackney and City of London / Cornwall and Isles of Scilly 
  mutate(area_code = case_when(
    as.character(area_code) %in% c("E09000012", "E09000001") ~ "E09000012", 
    as.character(area_code) %in% c("E06000052", "E06000053") ~ "E06000052", 
    TRUE ~ area_code)) %>% 
  group_by(area_code) %>% 
  summarise(population = sum(population)) 

# Mid-2019 population estimates for England and GM

# Source: ONS
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
# Licence: OGL 3.0

tmp <- tempfile(fileext = ".xls")
GET(url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls",
    write_disk(tmp))
ca_country_population <- read_xls(tmp, sheet = 6, skip = 4) %>% 
  filter(Name %in% c("Greater Manchester (Met County)", "ENGLAND")) %>% 
  select(area_code = Code, population = `All ages`)

bind_rows(ltla_population, ca_country_population) %>% 
  write_csv("population.csv")

# MSOAs in Greater Manchester

# Source: Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-areas-december-2011-boundaries-ew-bsc
# Licence: OGL 3.0

msoa <- st_read("https://opendata.arcgis.com/datasets/87aa4eb6393644768a5f85929cc704c2_0.geojson") %>% 
  filter(str_detect(MSOA11NM, "Bolton|Bury|Manchester|Oldham|Rochdale|Salford|Stockport|Tameside|Trafford|Wigan")) %>%
  select(msoa11cd = MSOA11CD, msoa11nm = MSOA11NM)

# Mid-2018 population estimates by MSOA

# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoaoa
# Licence: OGL 3.0

population <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography=1245709240...1245709350,1245715048,1245715058...1245715063,1245709351...1245709382,1245715006,1245709383...1245709577&date=latest&gender=0&c_age=200&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>% 
  select(msoa11cd = GEOGRAPHY_CODE, population = OBS_VALUE) 

left_join(msoa, population, by = "msoa11cd") %>%
  st_write("msoa.geojson")
  