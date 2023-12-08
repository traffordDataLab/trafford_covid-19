library(tidyverse) ; library(httr) ; library(readxl) ; library(sf)

# -------------------------------------------
# Mid-2022 population estimates (released 2023-11-29) for Local Authorities, Greater Manchester Combined Authority and England as of April 2021
# -------------------------------------------

# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala -> https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=2002
# NOMIS selections:
#   - Geography:
#       - combined authorities: [Some] Greater Manchester
#       - country: [Some] England
#       - local authorities: district / unitary (as of April 2021) [All]
#   - Date [2022]
#   - Age [All Ages]
#   - Sex [Total]
# Licence: OGL 3.0

population <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1853882369,2092957699,1811939329...1811939332,1811939334...1811939336,1811939338...1811939428,1811939436...1811939442,1811939768,1811939769,1811939443...1811939497,1811939499...1811939501,1811939503,1811939505...1811939507,1811939509...1811939517,1811939519,1811939520,1811939524...1811939570,1811939575...1811939599,1811939601...1811939628,1811939630...1811939634,1811939636...1811939647,1811939649,1811939655...1811939664,1811939667...1811939680,1811939682,1811939683,1811939685,1811939687...1811939704,1811939707,1811939708,1811939710,1811939712...1811939717,1811939719,1811939720,1811939722...1811939730,1811939757...1811939767&date=latest&gender=0&c_age=200&measures=20100") %>% 
    rename(area_code = GEOGRAPHY_CODE, population = OBS_VALUE) %>% 
    mutate(area_code = case_when(area_code %in% c("E06000052", "E06000053") ~ "E06000052", # combine population estimates for Cornwall and Isles of Scilly
                                 area_code == "E47000001" ~ "E11000001", # convert Greater Manchester Combined Authority code into Greater Manchester geographic area code
                                 TRUE ~ area_code)) %>% 
    # The following 2 lines are to get the combined total population for Cornwall and the Isles of Scilly as they will now have the same area_code: E06000052
    group_by(area_code) %>%
    summarise(population = sum(population)) %>%
    write_csv("population.csv")


# -------------------------------------------
# Mid-2022 population estimates (released 2023-11-29) for Trafford by age bands
# -------------------------------------------

# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala -> https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=2002
# NOMIS selections:
#   - Geography: local authorities: district / unitary (as of April 2021) [Some] Trafford
#   - Date [2022]
#   - Age [All single year of age column]
#   - Sex [Total]
# Licence: OGL 3.0

trafford_population <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1811939363&date=latest&gender=0&c_age=101...191&measures=20100") %>% 
    select(age = C_AGE_NAME, n = OBS_VALUE) %>% 
    mutate(age = parse_number(age),
           ageband = cut(age,
                         breaks = c(0,15,30,45,60,75,120),
                         labels = c("0-14","15-29","30-44","45-59","60-74","75+"),
                         right = FALSE)) %>% 
    group_by(ageband) %>% 
    summarise(population = sum(n)) %>% 
    write_csv("trafford_population.csv")


# -------------------------------------------
# 2021 Census population estimates - sex by age at MSOA in Trafford (NOTE 2023-12-08: mid-year 2022 not available at this geography)
# -------------------------------------------

# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoaoa -> https://www.nomisweb.co.uk/query/construct/components/simpleapicomponent.aspx?menuopt=22210&subcomp=
# NOMIS selections:
#   - Geography: Super Output Areas - Mid Layer [Some] Trafford
#   - Age [All]
#   - Sex [Total]
# Licence: OGL 3.0

population <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_2221_1.data.csv?date=latest&geography=637535406...637535433&c2021_age_24=0&c_sex=0&measures=20100") %>% 
  select(area_code = GEOGRAPHY_CODE, population = OBS_VALUE) 


# -------------------------------------------
# MSOA Boundary Geography for Trafford
# -------------------------------------------

# MSOAs in Trafford
# Generalised resolution
# Source: https://geoportal.statistics.gov.uk/datasets/ons::middle-layer-super-output-areas-2021-boundaries-ew-bgc/about
# Licence: OGL 3.0

msoa <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/MSOA_2021_EW_BGC_V2/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>%
    filter(str_detect(MSOA21NM, "Trafford")) %>%
    select(area_code = MSOA21CD, area_name = MSOA21NM) %>% 
    st_as_sf(crs = 4326, coords = c("long", "lat")) %>%
    mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
           lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
    select(area_code, area_name, lon, lat)

# Merge the population data into the MSOA geography properties
left_join(msoa, population, by = "area_code") %>% 
  select(area_code, area_name, population, lon, lat) %>% 
  st_write("msoa.geojson")


# House of Commons Library MSOA Names - uses the specific versioned URL for reproducibility
read_csv("https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-2.2.csv") %>%
  filter(localauthorityname == "Trafford") %>%
  select(area_code = msoa21cd, area_name_hcl = msoa21hclnm) %>%
  write_csv("msoa_names.csv")
