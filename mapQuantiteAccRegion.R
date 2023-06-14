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

# get data
region_geojson_url <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions.geojson"
region_geojson <- st_read("data/regions.geojson")
correspondance_region <- read.csv("data/correspondance-code-insee-code-postal.csv", sep = ";")
stat_acc_V3_cleared <- read.csv("data/stat_acc_V3_cleared.csv", sep = ";")
correspondance_regions <- read.csv("data/anciennes-nouvelles-regions.csv", sep = ";")

# relie code_insee de stat_acc_V3_cleared avec CodeINSEE de correspondance_region
merged_data <- left_join(stat_acc_V3_cleared, correspondance_region, by = c("id_code_insee" = "CodeINSEE"))

# Special character and other things fix
merged_data$region <- gsub("\\['|'\\]", "", merged_data$region)
merged_data$region <- str_replace_all(merged_data$region, fixed("PROVENCE-ALPES-COTE D'AZUR"), "PROVENCE-ALPES-CÔTE D'AZUR")
correspondance_regions$AnciensNom <- str_replace_all(correspondance_regions$AnciensNom, fixed("PROVENCE-ALPES-COTE D'AZUR"), "PROVENCE-ALPES-CÔTE D'AZUR")
regions_to_normalize <- c("Île-de-France")
region_geojson$nom[region_geojson$nom %in% regions_to_normalize] <- stri_trans_general(region_geojson$nom[region_geojson$nom %in% regions_to_normalize], "Latin-ASCII")
merged_data$region <- gsub("\\[\"|\"\\]", "", merged_data$region)
merged_data$region <- toupper(trimws(merged_data$region))
correspondance_regions$AnciensNom <- toupper(trimws(correspondance_regions$AnciensNom))

merged_data <- left_join(merged_data, correspondance_regions, by = c("region" = "AnciensNom"))

# sum accdidens by new region
accidents_region <- merged_data %>%
  group_by(NouveauNom) %>%
  summarise(total_accidents = n()) %>%
  ungroup()



region_geojson <- merge(region_geojson, accidents_region, by.x = "nom", by.y = "NouveauNom", all.x = TRUE)


# if NA --> 0
region_geojson$total_accidents[is.na(region_geojson$total_accidents)] <- 0



# color
thresholds <- c(0, 3000, 6000, 9000, 12000, 15000, Inf)
colors <- c("#00FF00", "#0000FF", "#FFFF00", "#FFA500", "#FF0000", "#000000")
color_category <- cut(region_geojson$total_accidents, breaks = thresholds, labels = colors)

# Create Leaflet map
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = 2, lat = 47, zoom = 5) %>%
  addPolygons(data = region_geojson,
              fillColor = ~color_category,
              color = "black", fillOpacity = 0.5,
              popup = ~paste(nom, total_accidents, "accidents", sep = ": "),
              highlightOptions = highlightOptions(weight = 2, fillOpacity = 0.8))


# add legend
map <- addLegend(map, position = "bottomright", colors = colors, labels = c("0", "3000", "6000", "9000", "12000", "16000+"), title = "Accidents")


print(map)

## save html to png
saveWidget(map, "temp.html", selfcontained = FALSE)
webshot("temp.html", file = "image/mapQteAccGravRegion.png",
        cliprect = "viewport")


