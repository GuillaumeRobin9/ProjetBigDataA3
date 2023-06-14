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

# get data
department_geojson <- st_read("data/departements.geojson")
correspondance_departement <- read.csv("data/correspondance-code-insee-code-postal.csv", sep = ";")
stat_acc_V3_cleared <- read.csv("data/stat_acc_V3_cleared.csv", sep = ";")


merged_data <- left_join(stat_acc_V3_cleared, correspondance_departement, by = c("id_code_insee" = "CodeINSEE"))
#merged_data$departement <- gsub("\\['|'\\]", "",merged_data$departement)


merged_data$Département <- toupper(trimws(merged_data$Département))

# sum accident by department
accidents_departement <- merged_data %>%
  group_by(Département) %>%
  summarise(total_accidents = n()) %>%
  ungroup()


# Special character and other things fix

accidents_departement$Département <- toupper(gsub("\\['|'\\]", "", accidents_departement$Département))
#accidents_departement$Département <- toupper(accidents_departement$Département)
department_geojson$nom <- iconv(department_geojson$nom, "UTF-8", "ASCII//TRANSLIT")
department_geojson$nom <- stri_unescape_unicode(department_geojson$nom)
department_geojson$nom <- str_to_title(department_geojson$nom)
department_geojson$nom <- gsub("\\['|'\\]", "", department_geojson$nom)
department_geojson$nom <- toupper(department_geojson$nom)

department_geojson <- merge(department_geojson, accidents_departement, by.x = "nom", by.y = "Département", all.x = TRUE)

#str(department_geojson)
str(accidents_departement$Département)


# if NA --> 0 
department_geojson$total_accidents[is.na(department_geojson$total_accidents)] <- 0


# color
thresholds <- c(-1, 0, 300, 600, 900, 1200, 1500, Inf)
colors <- c("#FFFFFF","#00FF00", "#0000FF", "#FFFF00", "#FFA500", "#FF0000", "#000000")
color_category <- cut(department_geojson$total_accidents, breaks = thresholds, labels = colors)

# Create leaflet map
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = 2, lat = 47, zoom = 5) %>%
  addPolygons(data = department_geojson,
              fillColor = ~color_category,
              color = "black", fillOpacity = 0.5,
              popup = ~paste(nom, total_accidents, "accidents", sep = ": "),
              highlightOptions = highlightOptions(weight = 2, fillOpacity = 0.8))

# add legend
map <- addLegend(map, position = "bottomright", colors = colors, labels = c("0","1", "300", "600", "900", "1200", "1600+"), title = "Accidents")

print(map)

## save html to png
saveWidget(map, "temp.html", selfcontained = FALSE)
webshot("temp.html", file = "image/mapQteAccGravDep.png",
        cliprect = "viewport")


