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

# database
department_geojson <- st_read("data/departements.geojson")
correspondance_region <- read.csv("data/correspondance-code-insee-code-postal.csv", sep = ";")
stat_acc_V3_cleared <- read.csv("data/stat_acc_V3_cleared.csv", sep = ";")

# relie code_insee de stat_acc_V3_cleared avec CodeINSEE de correspondance_region

merged_data <- left_join(stat_acc_V3_cleared, correspondance_region, by = c("id_code_insee" = "CodeINSEE"))


# special character and other normalization
merged_data$region <- gsub("\\[\"|\"\\]", "", merged_data$region)
merged_data$region <- toupper(trimws(merged_data$region))

# 1 indemme 
# 2 tué
# 3 blessé hospitalisé
# 4 blessé léger
# sum blessé hospitalisé + tué / sum accident total
# summ of gravity by department
accidents_departement <- merged_data %>%
  group_by(Département) %>%
  summarise(total_accidents = as.numeric(n()),
            grave_accidents = as.numeric(sum(descr_grav %in% c(2, 3))),
            taux_accidents_graves = grave_accidents / total_accidents) %>%
  ungroup()



# Special character and other things normalization
accidents_departement$Département <- toupper(gsub("\\['|'\\]", "", accidents_departement$Département))
#accidents_departement$Département <- toupper(accidents_departement$Département)
department_geojson$nom <- iconv(department_geojson$nom, "UTF-8", "ASCII//TRANSLIT")
department_geojson$nom <- stri_unescape_unicode(department_geojson$nom)
department_geojson$nom <- str_to_title(department_geojson$nom)
department_geojson$nom <- gsub("\\['|'\\]", "", department_geojson$nom)
department_geojson$nom <- toupper(department_geojson$nom)


# Merge data 
department_geojson <- merge(department_geojson, accidents_departement, by.x = "nom", by.y = "Département", all.x = TRUE)
# if NA --> 0
department_geojson$taux_accidents_graves[is.na(department_geojson$taux_accidents_graves)] <- 0
department_geojson$grave_accidents[is.na(department_geojson$grave_accidents)] <- 0


# Color mapping
thresholds <- c(-1, 0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,1)
colors <- c("#00FF00","#00ff9d","#0000FF","#8400ff" ,"#FFFF00", "#FFA500", "#FF0000","#000000")
color_category <- cut(department_geojson$taux_accidents_graves, breaks = thresholds, labels = colors)

# Create Leaflet map
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = 2, lat = 47, zoom = 5) %>%
  addPolygons(data = department_geojson,
              fillColor = ~color_category,
              color = "black", fillOpacity = 0.5,
              popup = ~paste(nom, sprintf("%.2f", taux_accidents_graves), "gravité", "(", grave_accidents, "accidents graves)", sep = ": "),
              highlightOptions = highlightOptions(weight = 2, fillOpacity = 0.8))

map <- addLegend(map, position = "bottomright", colors = colors, labels = c("0", "0.1", "0.2", "0.3", "0.4","0.5","0.6","1"), title = "Accidents")


print(map)

## save html to png
saveWidget(map, "temp.html", selfcontained = FALSE)
webshot("temp.html", file = "image/mapTauxAccGravDepart.png",
        cliprect = "viewport")

