library(tidyverse)
library(sf)
library(leaflet)
library(sp)
install.packages("gifski")
library(gifski)
install.packages("gganimate")
library(gganimate)
install.packages("plotly")
library(plotly)
library(lubridate)

## Reading data and adding health board column for shape join

wales_beds <- read_csv("C:/Users/Student User/Downloads/wales_beds.csv") %>%  # all wales data
  mutate(`General beds percentage occupied` = `General and acute beds occupied`/`General and acute beds available`,
         `Percentage of general occupied beds by COVID` = `General and acute beds occupied by COVID-19 case`/`General and acute beds occupied`)
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
       `Percentage of general occupied beds by COVID` = `General and acute beds occupied by COVID-19 case`/`General and acute beds occupied`)
  #dmy(allhb_beds$Date)
  #as.Date(allhb_beds$Date, format = "%d-%b-%y")


## Plotting

# plot line charts of beds occupied percentage for each HB
p1 <- ggplot(allhb_beds, aes(x = `Date`, y = `General beds percentage occupied`, group = 1)) +
        geom_line() +
        facet_wrap(~health_board)
ggplotly(
  p = p1
)

# Plot bar charts of ventilated beds available for each HB
allhb_beds %>%
  ggplot(aes(x = Date, y = `Invasive ventilated beds available`)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~health_board)

# Plot All Wales percentage beds occupied line chart
wales_beds %>%
  ggplot(aes(x = Date, y = `General beds percentage occupied`, group = 1)) +
  geom_line()
  #add a line for April 20th Nightingale hosp.



# Joining all HB data to shape file
joined_data <- sp::merge(shapes, allhb_beds, by.x = "lhb19nm", by.y = "health_board")

# Attempt at making interactive map
pal <- colorNumeric(c("red", "green"), joined_data$`General and acute beds occupied`)

# Leaflet map
longitude <- c(-3.0318, -4.1293, -3.3000, -3.3700, -4.9713, -3.2323, -3.8142)
latitude <- c(51.6497, 53.2274, 51.4000, 51.6475, 51.8005, 51.9959, 51.6200)
popup_text <- c("AB", "BCU", "C&V", "CTM", "HD", "Powys", "Swansea")
leaflet(joined_data) %>%
  addProviderTiles(providers$CartoDB) %>%
  addPolygons(fillColor = ~pal(`General and acute beds occupied`), fillOpacity = 0.3) %>%
  addMarkers(lng = longitude, lat = latitude, popup = popup_text) %>%
  addLegend("topleft", pal = pal, values = joined_data$`General and acute beds occupied`, opacity = 1)