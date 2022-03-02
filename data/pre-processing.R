library(tidyverse) ; library(httr) ; library(readxl) ; library(sf)

# Mid-2020 population estimates for local authorities in England

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

# Mid-2020 population estimates for England and GM

# Source: ONS
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
# Licence: OGL 3.0
# NOTE: you will likely have to amend the "sheet" and "skip" values below as they can change between publications

tmp <- tempfile(fileext = ".xls")
GET(url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2020/ukpopestimatesmid2020on2021geography.xls",##
    write_disk(tmp))
ca_country_population <- read_xls(tmp, sheet = 7, skip = 7) %>% 
  filter(Name %in% c("Greater Manchester (Met County)", "ENGLAND")) %>% 
  select(area_code = Code, population = `All ages`)

bind_rows(ltla_population, ca_country_population) %>% 
  write_csv("population.csv")

# Trafford local authority boundary

# Source: Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-may-2020-boundaries-uk-bgc-1
# Licence: OGL 3.0

st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_May_2020_UK_BGC_V3/FeatureServer/0/query?where=LAD20NM%20%3D%20'TRAFFORD'&outFields=LAD20CD,LAD20NM&outSR=4326&f=geojson") %>% 
  select(lad20cd = LAD20CD, lad20nm = LAD20NM) %>% 
  st_write("la.geojson")

# MSOAs in Trafford

# Source: Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-areas-december-2011-boundaries-ew-bgc
# Licence: OGL 3.0

msoa <- st_read("https://opendata.arcgis.com/datasets/abfccdf1071c43dd981a49eb7da13d2b_0.geojson") %>%
  filter(str_detect(MSOA11NM, "Trafford")) %>%
  # correct invalid polygon geometry (SOURCE CHANGE FOR GEOGRAPHY SEEMS TO MAKE THIS REDUNDANT 2022-03-02)
  #st_buffer(dist = 0) %>% 
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  select(msoa11cd = MSOA11CD, msoa11nm = MSOA11NM, lon, lat)

# Dissolve into local authority boundary (THIS NOW SEEMS REDUNDANT 2022-03-02)
msoa %>% 
  st_union() %>% 
  st_write("la.geojson")

# Mid-2020 population estimates by MSOA

# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoaoa
# Licence: OGL 3.0

population <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography=1245709510...1245709537&date=latest&gender=0&c_age=200&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>% 
  select(msoa11cd = GEOGRAPHY_CODE, population = OBS_VALUE) 

left_join(msoa, population, by = "msoa11cd") %>% 
  select(msoa11cd, msoa11nm, population, lon, lat) %>% 
  st_write("msoa.geojson")

# Mid-2020 population estimates for Trafford
# Source: Nomis
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala

read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1811939363&date=latest&gender=0&c_age=101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>% 
  select(age = C_AGE_NAME, n = OBS_VALUE) %>% 
  mutate(age = parse_number(age),
         ageband = cut(age,
                       breaks = c(0,15,30,45,60,75,120),
                       labels = c("0-14","15-29","30-44","45-59","60-74","75+"),
                       right = FALSE)) %>% 
  group_by(ageband) %>% 
  summarise(population = sum(n)) %>% 
  write_csv("trafford_population.csv")

# House of Commons Library MSOA Names - uses the static URL to get the latest available
read_csv("https://visual.parliament.uk/msoanames/static/MSOA-Names-Latest.csv") %>%
  filter(Laname == "Trafford") %>%
  select(msoa11cd, msoa11hclnm) %>%
  write_csv("msoa_names.csv")
