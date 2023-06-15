if (!require(sf)) {
  install.packages("sf")
}
if (!require(leaflet)) {
  install.packages("leaflet")
}
if (!require(dplyr)) {
  install.packages("dplyr")
}
if (!require(stringr)) {
  install.packages("stringr")
}
if (!require(stringi)) {
  install.packages("stringi")
}
library(sf)
library(leaflet)
library(dplyr)
library(stringr)
library(stringi)
library(devtools)
install_github("wch/webshot")
library(htmlwidgets)
library(webshot)

# get data 
region_geojson_url <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions.geojson"
region_geojson <- st_read("data/regions.geojson")
correspondance_region <- read.csv("data/correspondance-code-insee-code-postal.csv", sep = ";")
stat_acc_V3_cleared <- read.csv("data/stat_acc_V3_cleared.csv", sep = ";")
correspondance_regions <- read.csv("data/anciennes-nouvelles-regions.csv", sep = ";")



# Special character fix
merged_data <- left_join(stat_acc_V3_cleared, correspondance_region, by = c("id_code_insee" = "CodeINSEE"))
merged_data$region <- gsub("\\['|'\\]", "", merged_data$region)
merged_data$region <- str_replace_all(merged_data$region, fixed("PROVENCE-ALPES-COTE D'AZUR"), "PROVENCE-ALPES-CÔTE D'AZUR")
correspondance_regions$AnciensNom <- str_replace_all(correspondance_regions$AnciensNom, fixed("PROVENCE-ALPES-COTE D'AZUR"), "PROVENCE-ALPES-CÔTE D'AZUR")
regions_to_normalize <- c("Île-de-France")
region_geojson$nom[region_geojson$nom %in% regions_to_normalize] <- stri_trans_general(region_geojson$nom[region_geojson$nom %in% regions_to_normalize], "Latin-ASCII")
merged_data$region <- gsub("\\[\"|\"\\]", "", merged_data$region)
merged_data$region <- toupper(trimws(merged_data$region))
correspondance_regions$AnciensNom <- toupper(trimws(correspondance_regions$AnciensNom))

merged_data <- left_join(merged_data, correspondance_regions, by = c("region" = "AnciensNom"))

# 1 indemme 
# 2 tué
# 3 blessé hospitalisé
# 4 blessé léger
# sum blessé hospitalisé + tué / sum accident total

# Sum accident by region
accidents_region <- merged_data %>%
  group_by(NouveauNom) %>%
  summarise(total_accidents = n(),
            grave_accidents = sum(descr_grav %in% c(2, 3)),
            taux_accidents_graves = grave_accidents / total_accidents) %>%
  ungroup()


region_geojson <- merge(region_geojson, accidents_region, by.x = "nom", by.y = "NouveauNom", all.x = TRUE)
# if NA --> 0
#region_geojson$mean_gravite[is.na(region_geojson$mean_gravite)] <- 0
region_geojson$grave_accidents[is.na(region_geojson$taux_accidents_graves)] <- 0
# Color
thresholds <- c(-1, 0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,1)
colors <- c("#00FF00","#00ff9d","#0000FF","#8400ff" ,"#FFFF00", "#FFA500", "#FF0000","#000000")
#color_category <- cut(region_geojson$mean_gravite, breaks = thresholds, labels = colors)
color_category <- cut(region_geojson$taux_accidents_graves, breaks = thresholds, labels = colors)
# Create map leaflet
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = 2, lat = 47, zoom = 5) %>%
  addPolygons(data = region_geojson,
              fillColor = ~color_category,
              color = "black", fillOpacity = 0.5,
              popup = ~paste(nom, sprintf("%.2f", taux_accidents_graves), "gravité", "(", grave_accidents, "accidents graves)", sep = ": "),
              highlightOptions = highlightOptions(weight = 0.5, fillOpacity = 0.4))

map <- addLegend(map, position = "bottomright", colors = colors, labels = c("0", "0.1", "0.2", "0.3", "0.4","0.5","0.6","1"), title = "Accidents")

print(map)

## save html to png
saveWidget(map, "temp.html", selfcontained = FALSE)
webshot("temp.html", file = "image/mapTauxAccGravRegion.png",
        cliprect = "viewport")

