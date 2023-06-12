#lecture csv file "stat_acc_V3.csv"
data <- read.csv("data/stat_acc_V3.csv", sep=";")

#print les diffrentes valeurs de chaque colonne multimodale
# print(unique(data$descr_cat_veh))
# print(unique(data$descr_grav))

#creation d'un vecteur de valeurs a remplacer a partir des valeurs de unique(data$colonne)
old_cat_veh_values <- unique(data$descr_cat_veh)
old_descr_grav_values <- unique(data$descr_grav)

#creation d'un vecteur de valeurs de remplacement
new_cat_veh_values <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
new_descr_grav_values <- c(1,2,3,4)

#boucle for pour remplacer les valeurs de old_values par les valeurs de new_values
for (i in 1:length(old_cat_veh_values)) {
  data$descr_cat_veh <- replace(data$descr_cat_veh, data$descr_cat_veh == old_cat_veh_values[i], new_cat_veh_values[i])
  data$descr_grav <- replace(data$descr_grav, data$descr_grav == old_descr_grav_values[i], new_descr_grav_values[i])
}


#---------------------------------------------------------------------------------------------------------------
#-----------------------------------------------MODIF CASES VIDES-----------------------------------------------
#---------------------------------------------------------------------------------------------------------------

#boucle pour remplacer les cases NULL par 10 dans la colonne "place"
for (i in 1:length(data$place)) {
  if (data$place[i] == "NULL") {
    data$place[i] <- 10
  }
}

#recuperation des lignes avec "id_code_insee" commence par 97
verif <- data[grepl("^97", data$id_code_insee),]

#inversion des valeurs de latitude et longitude pour les lignes avec "id_code_insee" commence par 97
for(i in 1:length(data$latitude)) {
  if (grepl("^97", data$id_code_insee[i])) {
    temp <- data$latitude[i]
    data$latitude[i] <- data$longitude[i]
    data$longitude[i] <- temp
  }
}


#stockage des villes correspondant aux valeurs de lat et long nulles
cities <- unique(data$ville[data$lat == 0 & data$long == 0])

#creation d'une matrice pour stocker les valeurs de lat et long correspondant aux villes de cities
lat <- c("ST LOUIS" = -21.28572763902781, "ST JOSEPH"=-21.378782647907983, "STE SUZANNE"=-20.908309395323386, "ST PHILIPPE"=-21.35762401984637 , "STE ROSE"=-21.12755905365339, "ST LEU"=-21.164569631240063, "SCHOELCHER"=14.609053)
long <- c("ST LOUIS" = 55.412336476095966, "ST JOSEPH"=55.61860810608779, "STE SUZANNE"=55.605217149551564, "ST PHILIPPE"=55.76726935607828, "STE ROSE"=55.791576628965366, "ST LEU"=55.29234022106118, "SCHOELCHER"=-61.076913)

#boucle pour remplacer les valeurs de latitude et longitude nulles par les valeurs correspondantes dans la matrice
for(i in 1:length(data$latitude)) {
  if (data$latitude[i] == 0 & data$longitude[i] == 0) {
    for(j in 1:length(cities)) {
      if (data$ville[i] == cities[j]) {
        data$latitude[i] <- lat[j]
        data$longitude[i] <- long[j]
      }
    }
  }
}

#suppression des lignes avec des valeurs nulles de "age" et "an_nais"
data <- subset(data, data$age != "NULL")
data <- subset(data, data$an_nais != "NULL")


#remplacement des longitudes et latitudes des villes avec des valeurs de latitude et longitude == 2009... par leur valeurs réelles
#stock une seule fois chaque ville ou data$latitude == 2009
arr <- unique(data$ville[data$latitude == 2009])

#stock les arrondissements de paris, de marsseille et de lyon dans 3 vecteurs
arr_paris <- c("PARIS 20", "PARIS 19", "PARIS 18", "PARIS 17", "PARIS 16", "PARIS 15", "PARIS 14", "PARIS 13", "PARIS 12", "PARIS 11", "PARIS 10", "PARIS 09", "PARIS 08", "PARIS 07", "PARIS 06", "PARIS 05", "PARIS 04", "PARIS 03", "PARIS 02", "PARIS 01")
arr_marseille <- c("MARSEILLE 15", "MARSEILLE 14", "MARSEILLE 13", "MARSEILLE 12", "MARSEILLE 11", "MARSEILLE 10", "MARSEILLE 9", "MARSEILLE 8", "MARSEILLE 7", "MARSEILLE 6", "MARSEILLE 5", "MARSEILLE 4", "MARSEILLE 3", "MARSEILLE 2", "MARSEILLE 1")
arr_lyon <- c("LYON 9", "LYON 8", "LYON 7", "LYON 6", "LYON 5", "LYON 4", "LYON 3", "LYON 2", "LYON 1")

#stockage des valeurs de latitude et longitude correspondant aux villes de arr
arr_lat_paris <- c("PARIS 20"=48.864527, "PARIS 19"=48.864527, "PARIS 18"=48.864527, "PARIS 17"=48.864527, "PARIS 16"=48.864527, "PARIS 15"=48.864527, "PARIS 14"=48.864527, "PARIS 13"=48.864527, "PARIS 12"=48.864527, "PARIS 11"=48.864527, "PARIS 10"=48.864527, "PARIS 09"=48.864527, "PARIS 08"=48.864527, "PARIS 07"=48.864527, "PARIS 06"=48.864527, "PARIS 05"=48.864527, "PARIS 04"=48.864527, "PARIS 03"=48.864527, "PARIS 02"=48.864527, "PARIS 01"=48.864527)
arr_long_paris <- c("PARIS 20"=2.416170, "PARIS 19"=2.416170, "PARIS 18"=2.416170, "PARIS 17"=2.416170, "PARIS 16"=2.416170, "PARIS 15"=2.416170, "PARIS 14"=2.416170, "PARIS 13"=2.416170, "PARIS 12"=2.416170, "PARIS 11"=2.416170, "PARIS 10"=2.416170, "PARIS 09"=2.416170, "PARIS 08"=2.416170, "PARIS 07"=2.416170, "PARIS 06"=2.416170, "PARIS 05"=2.416170, "PARIS 04"=2.416170, "PARIS 03"=2.416170, "PARIS 02"=2.416170, "PARIS 01"=2.416170)

arr_lat_lyon <- c("LYON 9"=45.772519, "LYON 8"=45.772519, "LYON 7"=45.772519, "LYON 6"=45.772519, "LYON 5"=45.772519, "LYON 4"=45.772519, "LYON 3"=45.772519, "LYON 2"=45.772519, "LYON 1"=45.772519)
arr_long_lyon <- c("LYON 9"=4.858519, "LYON 8"=4.858519, "LYON 7"=4.858519, "LYON 6"=4.858519, "LYON 5"=4.858519, "LYON 4"=4.858519, "LYON 3"=4.858519, "LYON 2"=4.858519, "LYON 1"=4.858519)

arr_lat_marseille <- c("MARSEILLE 15"=43.292519, "MARSEILLE 14"=43.292519, "MARSEILLE 13"=43.292519, "MARSEILLE 12"=43.292519, "MARSEILLE 11"=43.292519, "MARSEILLE 10"=43.292519, "MARSEILLE 9"=43.292519, "MARSEILLE 8"=43.292519, "MARSEILLE 7"=43.292519, "MARSEILLE 6"=43.292519, "MARSEILLE 5"=43.292519, "MARSEILLE 4"=43.292519, "MARSEILLE 3"=43.292519, "MARSEILLE 2"=43.292519, "MARSEILLE 1"=43.292519)
arr_long_marseille <- c("MARSEILLE 15"=5.372519, "MARSEILLE 14"=5.372519, "MARSEILLE 13"=5.372519, "MARSEILLE 12"=5.372519, "MARSEILLE 11"=5.372519, "MARSEILLE 10"=5.372519, "MARSEILLE 9"=5.372519, "MARSEILLE 8"=5.372519, "MARSEILLE 7"=5.372519, "MARSEILLE 6"=5.372519, "MARSEILLE 5"=5.372519, "MARSEILLE 4"=5.372519, "MARSEILLE 3"=5.372519, "MARSEILLE 2"=5.372519, "MARSEILLE 1"=5.372519)

#modification des valeurs de latitude et longitude de Paris
for(i in 1:length(data$latitude)) {
  for(j in 1:length(arr_paris)) {
    if (data$ville[i] == arr_paris[j]) {
      data$latitude[i] <- arr_lat_paris[j]
      data$longitude[i] <- arr_long_paris[j]
    }
  }
}

#modification des valeurs de latitude et longitude de Lyon
for(i in 1:length(data$latitude)) {
  for(j in 1:length(arr_lyon)) {
    if (data$ville[i] == arr_lyon[j]) {
      data$latitude[i] <- arr_lat_lyon[j]
      data$longitude[i] <- arr_long_lyon[j]
    }
  }
}

#modification des valeurs de latitude et longitude de Marseille
for(i in 1:length(data$latitude)) {
  for(j in 1:length(arr_marseille)) {
    if (data$ville[i] == arr_marseille[j]) {
      data$latitude[i] <- arr_lat_marseille[j]
      data$longitude[i] <- arr_long_marseille[j]
    }
  }
}




#export csv
write.table(data, file = "data/stat_acc_V3_modifModal_nullValues.csv", sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)