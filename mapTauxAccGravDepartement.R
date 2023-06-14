if (!require(sf)) {
  install.packages("sf")
}
if (!require(leaflet)) {
  install.packages("leaflet")
}
if (!require(dplyr)) {
  install.packages("dplyr")
}
if (!require(dplyr)) {
  install.packages("stringr")
}
if (!require(dplyr)) {
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

# database
department_geojson <- st_read("data/departements.geojson")
correspondance_region <- read.csv("data/correspondance-code-insee-code-postal.csv", sep = ";")
stat_acc_V3_cleared <- read.csv("data/stat_acc_V3_cleared.csv", sep = ";")

# relie code_insee de stat_acc_V3_cleared avec CodeINSEE de correspondance_region

merged_data <- left_join(stat_acc_V3_cleared, correspondance_region, by = c("id_code_insee" = "CodeINSEE"))


# special character normalization
merged_data$region <- gsub("\\[\"|\"\\]", "", merged_data$region)
merged_data$region <- toupper(trimws(merged_data$region))

# summ of gravity by department
accidents_departement <- merged_data %>%
  group_by(Département) %>%
  summarise(mean_gravite = mean(descr_grav)) %>%
  ungroup()


# Special character normalization
accidents_departement$Département <- toupper(gsub("\\['|'\\]", "", accidents_departement$Département))
department_geojson$nom <- str_replace(department_geojson$nom, "\\W", "")
department_geojson$nom <- iconv(department_geojson$nom, "UTF-8", "ASCII//TRANSLIT")
department_geojson$nom <- stri_unescape_unicode(department_geojson$nom)
department_geojson$nom <- str_to_title(department_geojson$nom)
department_geojson$nom <- toupper(department_geojson$nom)

str(accidents_departement)
str(department_geojson)

# Merge data 
department_geojson <- merge(department_geojson, accidents_departement, by.x = "nom", by.y = "Département", all.x = TRUE)
# if NA --> 0
department_geojson$mean_gravite[is.na(department_geojson$mean_gravite)] <- 0


# Color mapping
thresholds <- c(0, 1, 2, 3, 4,5)
colors <- c("#00FF00", "#0000FF", "#FFFF00", "#FFA500", "#FF0000")
color_category <- cut(department_geojson$mean_gravite, breaks = thresholds, labels = colors)

# Create Leaflet map
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = 2, lat = 47, zoom = 5) %>%
  addPolygons(data = department_geojson,
              fillColor = ~color_category,
              color = "black", fillOpacity = 0.5,
              popup = ~paste(nom, sprintf("%.2f", mean_gravite), "gravité", sep = ": "),
              highlightOptions = highlightOptions(weight = 2, fillOpacity = 0.8))



map <- addLegend(map, position = "bottomright", colors = colors, labels = c("0", "1", "2", "3", "4"), title = "Accidents")


print(map)

## save html to png
saveWidget(map, "temp.html", selfcontained = FALSE)
webshot("temp.html", file = "image/mapTauxAccGravDepart.png",
        cliprect = "viewport")

