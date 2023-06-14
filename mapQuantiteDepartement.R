library(sf)
library(leaflet)
library(dplyr)
library(stringr)
library(stringi)

# Chargement des données
department_geojson <- st_read("data/departements.geojson")
correspondance_departement <- read.csv("data/correspondance-code-insee-code-postal.csv", sep = ";")
stat_acc_V3_cleared <- read.csv("data/stat_acc_V3_cleared.csv", sep = ";")


merged_data <- left_join(stat_acc_V3_cleared, correspondance_departement, by = c("id_code_insee" = "CodeINSEE"))
#merged_data$departement <- gsub("\\['|'\\]", "",merged_data$departement)

#str(merged_data)
merged_data$Département <- toupper(trimws(merged_data$Département))
#correspondance_regions$AnciensNom <- toupper(trimws(correspondance_regions$AnciensNom))

#merged_data <- left_join(merged_data, correspondance_departement, by = c("Département" = "AnciensNom"))

accidents_departement <- merged_data %>%
  group_by(Département) %>%
  summarise(total_accidents = n()) %>%
  ungroup()



# Convertir en lettres majuscules + delete crochet
accidents_departement$Département <- toupper(gsub("\\['|'\\]", "", accidents_departement$Département))

# Supprimer les caractères spéciaux du nom des départements
department_geojson$nom <- str_replace(department_geojson$nom, "\\W", "")
department_geojson$nom <- iconv(department_geojson$nom, "UTF-8", "ASCII//TRANSLIT")
department_geojson$nom <- stri_unescape_unicode(department_geojson$nom)
department_geojson$nom <- str_to_title(department_geojson$nom)

# Convertir les noms des départements en lettres majuscules
department_geojson$nom <- toupper(department_geojson$nom)

# Fusionner les données
department_geojson <- merge(department_geojson, accidents_departement, by.x = "nom", by.y = "Département", all.x = TRUE)

#str(department_geojson)





#str(merged_data)


# Remplacer les valeurs manquantes dans la colonne total_accidents par 0
department_geojson$total_accidents[is.na(department_geojson$total_accidents)] <- 0



# Définition des seuils pour chaque catégorie
thresholds <- c(0, 3000, 6000, 9000, 12000, 15000, Inf)

# Définition des couleurs correspondantes à chaque catégorie
colors <- c("#00FF00", "#0000FF", "#FFFF00", "#FFA500", "#FF0000", "#000000")

# Création de la fonction pour assigner une couleur fixe en fonction de la catégorie
color_category <- cut(department_geojson$total_accidents, breaks = thresholds, labels = colors)

# Création de la carte Leaflet avec les couleurs fixes
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = 2, lat = 47, zoom = 5) %>%
  addPolygons(data = department_geojson,
              fillColor = ~color_category,
              color = "black", fillOpacity = 0.5,
              popup = ~paste(nom, total_accidents, "accidents", sep = ": "),
              highlightOptions = highlightOptions(weight = 2, fillOpacity = 0.8))

# ...

# Ajout de la légende avec les couleurs et les étiquettes
map <- addLegend(map, position = "bottomright", colors = colors, labels = c("0", "3000", "6000", "9000", "12000", "16000+"), title = "Accidents")

# Affichage de la carte
print(map)

## save html to png
saveWidget(map, "temp.html", selfcontained = FALSE)
webshot("temp.html", file = "image/mapQteAccGravDep.png",
        cliprect = "viewport")


