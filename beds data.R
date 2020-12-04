library(tidyverse)
library(sf)
library(leaflet)
library(sp)

library(gganimate)
install.packages("plotly")
library(plotly)
library(lubridate)

library(scales)

## Reading data and adding health board column for shape join

wales_beds <- read_csv("C:/Users/Student User/Downloads/wales_beds.csv") %>%  # all wales data
  mutate(`General beds percentage occupied` = `General and acute beds occupied`/`General and acute beds available`,
         `General beds percentage occupied` = `General beds percentage occupied` * 100,
         `Percentage of general occupied beds by COVID` = `General and acute beds occupied by COVID-19 case`/`General and acute beds occupied`,
         `Percentage of general occupied beds by COVID` = scales::percent(`Percentage of general occupied beds by COVID`),
         `Invasive beds occupied percentage` = `Invasive ventilated bed occupied`/`Invasive ventilated beds available`,
         `Invasive beds occupied percentage` = `Invasive beds occupied percentage` * 100,
  ldate = lubridate::parse_date_time(`Date`, orders = "d-b-Y", locale = "uk"))
  #wales_beds$Date <- as.Date(wales_beds$Date, format = "%d-%b-%y")

abu_beds <- read_csv("C:/Users/Student User/Downloads/abu_beds.csv") %>%
  mutate(health_board = "Aneurin Bevan University Health Board") # AB HB data

bcu_beds <- read_csv("C:/Users/Student User/Downloads/bcu_beds.csv") %>%
  mutate(health_board = "Betsi Cadwaladr University Health Board") # BCU HB data

cv_beds <- read_csv("C:/Users/Student User/Downloads/cv_beds.csv") %>%
  mutate(health_board = "Cardiff and Vale University Health Board") # Cardiff & Vale HB data

ctm_beds <- read_csv("C:/Users/Student User/Downloads/ctm_beds.csv") %>%
  mutate(health_board = "Cwm Taf Morgannwg University Health Board") # CTM HB data

hd_beds <- read_csv("C:/Users/Student User/Downloads/hd_beds.csv") %>%
  mutate(health_board = "Hywel Dda University Health Board") # HD HB data

powys_beds <- read_csv("C:/Users/Student User/Downloads/powys_beds.csv") %>%
  filter(`Invasive ventilated bed occupied` != ".") %>%
  mutate(health_board = "Powys Teaching Health Board") # Powys data - Cutting rows with no data from Powys data - columns treated as character otherwise

swansea_beds <- read_csv("C:/Users/Student User/Downloads/swansea_beds.csv") %>%
  mutate(health_board = "Swansea Bay University Health Board")# Swansea HB data

#velindre_beds <- read_csv("C:/Users/Student User/Downloads/velindre_beds.csv") # Velindre data

# Shape file for health boards in Wales
shapes <- read_sf("C:/Users/Student User/Downloads/Local_Health_Boards__April_2019__Boundaries_WA_BGC-shp") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

# Bind data frames ready to join to shape file and add percentage columns
allhb_beds <- rbind(abu_beds, bcu_beds, cv_beds, ctm_beds, hd_beds, powys_beds, swansea_beds) %>%
mutate(`General beds percentage occupied` = `General and acute beds occupied`/`General and acute beds available`,
       `General beds percentage occupied` = `General beds percentage occupied` * 100,
       `Percentage of general occupied beds by COVID` = `General and acute beds occupied by COVID-19 case`/`General and acute beds occupied`,
       `Percentage of general occupied beds by COVID` = `Percentage of general occupied beds by COVID` * 100,
       `Invasive beds occupied percentage` = `Invasive ventilated bed occupied`/`Invasive ventilated beds available`,
       `Invasive beds occupied percentage` = `Invasive beds occupied percentage` * 100,
       ldate = lubridate::parse_date_time(`Date`, orders = "d-b-y", locale = "uk"))

## Plotting

# plot line charts of general beds occupied percentage for each HB
p1 <- ggplot(allhb_beds, aes(x = `ldate`, y = `General beds percentage occupied`, group = 1)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~health_board, scales = "free") +
  labs(
    x = "Date",
    y = "Percentage of beds occupied",
    title = "Percentage of general & acute beds occupied in health boards"
  )
ggplotly(
  p = p1
)

# plot line charts of general beds occupied percentage for Wales
p2 <- ggplot(wales_beds, aes(x = `ldate`, y = `General beds percentage occupied`, group = 1)) +
  geom_line() +
  theme_bw() +
  labs(
    x = "Date",
    y = "Percentage of beds occupied",
    title = "Percentage of general & acute beds occupied in Wales"
  )
ggplotly(
  p = p2
)

# plot line charts of ventilated beds percentage for health boards
p3 <- ggplot(allhb_beds, aes(x = `ldate`, y = `Invasive beds occupied percentage`, group = 1)) +
  geom_line() +
  facet_wrap(~health_board, scales = "free") +
  theme_bw() +
  labs(
    x = "Date",
    y = "Percentage of beds occupied",
    title = "Percentage of ventilated beds occupied in health boards"
  )
ggplotly(
  p = p3
)

# plot line charts of ventilated beds percentage for wales
p4 <- ggplot(wales_beds, aes(x = `ldate`, y = `Invasive beds occupied percentage`, group = 1)) +
  geom_line() +
  theme_bw() +
  labs(
    x = "Date",
    y = "Percentage of beds occupied",
    title = "Percentage of ventilated beds occupied in Wales"
  )
ggplotly(
  p = p4
)

# Joining all HB data to shape file
joined_data <- sp::merge(shapes, allhb_beds, by.x = "lhb19nm", by.y = "health_board")

# Attempt at making interactive map
pal <- colorNumeric(c("green", "red"), joined_data$`General and acute beds occupied`)

# Leaflet map
longitude <- c(-3.0318, -4.1293, -3.3000, -3.3700, -4.9713, -3.2323, -3.8142)
latitude <- c(51.6497, 53.2274, 51.4000, 51.6475, 51.8005, 51.9959, 51.6200)
popup_text <- c("AB", "BCU", "C&V", "CTM", "HD", "Powys", "Swansea")
leaflet(joined_data) %>%
  addProviderTiles(providers$CartoDB) %>%
  addPolygons(fillColor = ~pal(`General and acute beds occupied`), fillOpacity = 0.3) %>%
  addMarkers(lng = longitude, lat = latitude, popup = popup_text) %>%
  addLegend("topleft", pal = pal, values = joined_data$`General and acute beds occupied`, opacity = 1)
