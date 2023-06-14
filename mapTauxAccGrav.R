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

# Chargement des données
region_geojson_url <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions.geojson"
region_geojson <- st_read("data/regions.geojson")
correspondance_region <- read.csv("data/correspondance-code-insee-code-postal.csv", sep = ";")
stat_acc_V3_cleared <- read.csv("data/stat_acc_V3_cleared.csv", sep = ";")
correspondance_regions <- read.csv("data/anciennes-nouvelles-regions.csv", sep = ";")

# relie code_insee de stat_acc_V3_cleared avec CodeINSEE de correspondance_region

merged_data <- left_join(stat_acc_V3_cleared, correspondance_region, by = c("id_code_insee" = "CodeINSEE"))
merged_data$region <- gsub("\\['|'\\]", "", merged_data$region)

#str(merged_data)

# relie region de merged_data avec AnciensNom de correspondance_regions
# Correction de l'accent manquant dans le nom de la région
merged_data$region <- str_replace_all(merged_data$region, fixed("PROVENCE-ALPES-COTE D'AZUR"), "PROVENCE-ALPES-CÔTE D'AZUR")
correspondance_regions$AnciensNom <- str_replace_all(correspondance_regions$AnciensNom, fixed("PROVENCE-ALPES-COTE D'AZUR"), "PROVENCE-ALPES-CÔTE D'AZUR")


# Création d'un vecteur de noms de région à normaliser
regions_to_normalize <- c("Île-de-France")

# Normalisation des noms de région dans region_geojson pour les régions sélectionnées
region_geojson$nom[region_geojson$nom %in% regions_to_normalize] <- stri_trans_general(region_geojson$nom[region_geojson$nom %in% regions_to_normalize], "Latin-ASCII")

# Normalisation des noms de région dans region_geojson pour les régions sélectionnées
region_geojson$nom[region_geojson$nom %in% regions_to_normalize] <- stri_trans_general(region_geojson$nom[region_geojson$nom %in% regions_to_normalize], "Latin-ASCII")

# Fusion des données géographiques avec les données d'accidents par région
#region_geojson <- merge(region_geojson, accidents_region, by.x = "nom", by.y = "NouveauNom", all.x = TRUE)

# Agrégation des données par nouvelle région
names(merged_data)

merged_data$region <- gsub("\\[\"|\"\\]", "", merged_data$region)
merged_data$region <- toupper(trimws(merged_data$region))
correspondance_regions$AnciensNom <- toupper(trimws(correspondance_regions$AnciensNom))

merged_data <- left_join(merged_data, correspondance_regions, by = c("region" = "AnciensNom"))

#str(merged_data)
#unique_regions <- sort(unique(merged_data$region))
#unique_anciens_noms <- sort(unique(correspondance_regions$AnciensNom))
#print(unique_regions)
#print(unique_anciens_noms)



# Agrégation des données par nouvelle région
accidents_region <- merged_data %>%
  group_by(NouveauNom) %>%
  summarise(mean_gravite = mean(descr_grav)) %>%
  ungroup()

# Fusion des données géographiques avec les données d'accidents par région
region_geojson <- merge(region_geojson, accidents_region, by.x = "nom", by.y = "NouveauNom", all.x = TRUE)
# Remplacer les valeurs manquantes dans la colonne total_accidents par 0
region_geojson$mean_gravite[is.na(region_geojson$mean_gravite)] <- 0




# Définition des seuils pour chaque catégorie
thresholds <- c(0, 1, 2, 3, 4,5)

# Définition des couleurs correspondantes à chaque catégorie
colors <- c("#00FF00", "#0000FF", "#FFFF00", "#FFA500", "#FF0000")

# Création de la fonction pour assigner une couleur fixe en fonction de la catégorie
color_category <- cut(region_geojson$mean_gravite, breaks = thresholds, labels = colors)

# Création de la carte Leaflet avec les couleurs fixes
# Création de la carte Leaflet avec les couleurs fixes
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = 2, lat = 47, zoom = 5) %>%
  addPolygons(data = region_geojson,
              fillColor = ~color_category,
              color = "black", fillOpacity = 0.5,
              popup = ~paste(nom, sprintf("%.2f", mean_gravite), "gravité", sep = ": "),
              highlightOptions = highlightOptions(weight = 2, fillOpacity = 0.8))

# ...

# Ajout de la légende avec les couleurs et les étiquettes
map <- addLegend(map, position = "bottomright", colors = colors, labels = c("0", "1", "2", "3", "4"), title = "Accidents")

# Affichage de la carte
print(map)

## save html to png
saveWidget(map, "temp.html", selfcontained = FALSE)
webshot("temp.html", file = "Rplot.png",
        cliprect = "viewport")

