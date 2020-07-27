# MSOAs in Greater Manchester

# Source: Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-areas-december-2011-boundaries-ew-bsc
# Licence: OGL 3.0

st_read("https://opendata.arcgis.com/datasets/87aa4eb6393644768a5f85929cc704c2_0.geojson") %>% 
  filter(str_detect(MSOA11NM, "Bolton|Bury|Manchester|Oldham|Rochdale|Salford|Stockport|Tameside|Trafford|Wigan")) %>%
  select(msoa11cd = MSOA11CD, msoa11nm = MSOA11NM) %>%
  st_write("msoa.geojson")

  
  