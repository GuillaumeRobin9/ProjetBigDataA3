library(geojsonio)
department_geojson <- geojson_read("data/regions.geojson", what = "sp")
department_geojson <- geojson_read("data/departements.geojson", what = "sp")
data <- read.csv("data/stat_acc_V3_cleared.csv", sep = ";")

# ---------------------Quantité d'accidents ----------------------------
# By regions 
library(sp)
par(mar=c(0,0,0,0))
plot(region_geojson, col="grey")
points(data$longitude, data$latitude, col = "#0008ff", pch = 20)


# Transforme ce code en utilisant leaflet sprintf polygon


# By departements
library(sp)
par(mar=c(2,2,2,2))
plot(department_geojson, col="grey")
points(data$longitude, data$latitude, col = "#0008ff", pch = 20)

# ---------------------Taux d'accidents grave ------------------------------


