library(tidyverse) ; library(sf)

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
  