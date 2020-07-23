# Weekly confirmed coronavirus cases by MSOA #

# Source: Public Health England
# URL: https://coronavirus-staging.data.gov.uk/cases
# Licence: OGL 3.0

# load packages
library(tidyverse) ; library(lubridate) ; library(sf) ; library(scales) ; library(ggtext) ; 
library(classInt) ; library(leaflet) ; library(leafpop) ; library(htmlwidgets)

# create a string object with your local authority name
la <- "Trafford"

# retrieve MSOA boundaries from geoportal.statistics.gov.uk
codes <- read_csv("https://opendata.arcgis.com/datasets/6ecda95a83304543bc8feedbd1a58303_0.csv") %>% 
  filter(LAD11NM == la) %>% 
  distinct(MSOA11CD) %>% 
  pull(MSOA11CD)

msoa <- st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Census_Boundaries/Middle_Super_Output_Areas_December_2011_Boundaries/MapServer/2/query?where=", 
                       URLencode(paste0("msoa11cd IN (", paste(shQuote(codes), collapse = ", "), ")")), 
                       "&outFields=msoa11cd&outSR=4326&f=geojson"))

# retrieve confirmed cases
cases <- read_csv("https://coronavirus.data.gov.uk/downloads/msoa_data/MSOAs_latest.csv") %>% 
  filter(lad19_nm == la) %>% 
  select(lad19_nm, msoa11_cd, msoa11_hclnm, starts_with("wk")) %>% 
  pivot_longer(-c(lad19_nm, msoa11_cd, msoa11_hclnm), 
               names_to = "week", 
               names_pattern = "wk_(.*)", 
               values_to = "n") %>% 
  mutate(week = parse_number(week),
         date = ymd("2019-12-29") + weeks(week),
         n = replace_na(n, 0)) %>% 
  relocate(n, .after = last_col()) 

# create plots for each MSOA
plots <- cases %>% 
  nest(data = -msoa11_cd) %>%
  mutate(plots = map(data, 
                     ~ggplot(data = .x, aes(x = factor(date), y = n, group = 1)) + 
                       geom_col(fill = "#39809E", alpha = 0.8) +
                       geom_hline(yintercept = 0, size = 0.3, colour = "#333333") +
                       scale_y_continuous(expand = c(0.005, 0.005), breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))), position = "right") +
                       labs(x = NULL, y = NULL, 
                            title = "Weekly confirmed cases",
                            subtitle = paste0("<span style = 'color:#757575;'>", .x$msoa11_hclnm,"</span>"),
                            caption = "Source: Public Health England") +
                       theme_minimal() +
                       theme(plot.margin = unit(rep(0.5, 4), "cm"),
                             panel.grid.major.x = element_blank(),
                             panel.grid.minor = element_blank(),
                             plot.title.position = "plot",
                             plot.title = element_text(size = 14, face = "bold"),
                             plot.subtitle = element_markdown(size = 12, margin = margin(b = 20)),
                             plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10)),
                             axis.text.x = element_text(angle = 90))
  )) %>% 
  select(-data)

# join latest weekly cases
msoa_cases <- left_join(msoa, group_by(cases, lad19_nm, msoa11_cd, msoa11_hclnm) %>% 
                          summarise(total_cases = sum(n)), by = c("msoa11cd" = "msoa11_cd")) %>% 
  mutate(total_cases = replace_na(total_cases, 0)) %>% 
  # and plots to MSOA boundaries
  left_join(plots, by = c("msoa11cd" = "msoa11_cd"))

# calculate class intervals for choropleth map
breaks <- classIntervals(msoa_cases$total_cases, n = 5, style = "jenks")$brks
pal <- colorBin(palette = "PuBu", domain = NULL, bins = breaks, na.color = "#D7D8D7")

# create map
leaflet(data = msoa_cases) %>%
  addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png", attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a> | <a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2020)</a> | Data: <a href="https://www.gov.uk/government/publications/covid-19-track-coronavirus-cases" target="_blank">Public Health England</a>') %>% 
  addPolygons(fillColor = ~pal(total_cases), fillOpacity = 0.8, smoothFactor = 0.5, stroke = TRUE, weight = 0.5, color = "#bdbdbd", opacity = 1, 
              label = ~msoa11_hclnm, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"), highlightOptions = highlightOptions(color = "#212121", weight = 2, bringToFront = TRUE),
              popup = popupGraph(msoa_cases$plots, type = "svg")) %>% 
  addLegend(pal = pal, values = ~total_cases, opacity = 0.7, title = "Confirmed cases", position = "bottomright") %>% 
  addControl(paste0("<strong>Total confirmed coronavirus cases</strong><br/><em>", unique(msoa_cases$lad19_nm), ", to week ending ", format(as.Date(max(cases$date)), "%d %B"), "</em>"), position = 'topright') %>% 
  onRender(paste0("function(el, x) {$('head').append(","\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\'",");}"))

# with wards
lookup <- read_csv("https://opendata.arcgis.com/datasets/e169bb50944747cd83dcfb4dd66555b1_0.csv") %>% 
  filter(LAD19NM == la) %>% 
  pull(WD19CD)

wards <- st_read(paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/WD_DEC_2019_UK_BGC/FeatureServer/0/query?where=", 
                        URLencode(paste0("wd19cd IN (", paste(shQuote(lookup), collapse = ", "), ")")), 
                        "&outFields=*&outSR=4326&f=geojson")) %>% 
  select(area_name = WD19NM, lon = LONG, lat = LAT)

leaflet(data = msoa_cases) %>%
  addTiles(urlTemplate = "", attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a> | <a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2020)</a> | Data: <a href="https://www.gov.uk/government/publications/covid-19-track-coronavirus-cases" target="_blank">Public Health England</a>') %>% 
  addPolylines(data = wards, fill = "transparent", stroke = TRUE, weight = 1, color = "#000000", opacity = 1) %>% 
  addPolygons(fillColor = ~pal(total_cases), fillOpacity = 0.6, smoothFactor = 0.5, stroke = FALSE,
              label = ~msoa11_hclnm, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"), 
              popup = popupGraph(msoa_cases$plots, type = "svg")) %>% 
  addLabelOnlyMarkers(data = wards, lng = ~lon, lat = ~lat, label = ~as.character(area_name), labelOptions = labelOptions(noHide = T, textOnly = T, direction = "auto", style = list("color" = "white", "text-shadow" = "-1px -1px 10px #757575, 1px -1px 10px #757575, 1px 1px 10px #757575, -1px 1px 10px #757575"))) %>% 
  addLegend(pal = pal, values = ~total_cases, opacity = 0.7, title = "Confirmed cases", position = "bottomright") %>% 
  addControl(paste0("<strong>Total confirmed coronavirus cases</strong><br/><em>", unique(msoa_cases$lad19_nm), ", to week ending ", format(as.Date(max(cases$date)), "%d %B"), "</em>"), position = 'topright') %>% 
  onRender(
    "function(el, t) {var myMap = this;myMap._container.style['background'] = '#ffffff';}",
    paste0("function(el, x) {$('head').append(","\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\'",");}"))
