#install dplyr package si il n'est pas installé sur la machine
if (!require("dplyr")){install.packages("dplyr")}

library(dplyr)

#lecture csv file "stat_acc_V3.csv"
data <- read.csv("data/stat_acc_V3.csv", sep=";")

#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------

#Les valeurs numériques des dictionnaires suivants sont issus de la documentation de la base de données
#https://www.data.gouv.fr/fr/datasets/bases-de-donnees-annuelles-des-accidents-corporels-de-la-circulation-routiere-annees-de-2005-a-2021/#/resources
#Seuls les dispositifs de sécurité suivants ont été fixés manuellement car ils ne sont pas présents dans la documentation


#--------------------------------------------------------------------------------------------------------------
#-------------------------------------MODIF CAT VEHICULES + GRAVITE--------------------------------------------
#--------------------------------------------------------------------------------------------------------------

#On veut remplacer les valeurs de la colonne "descr_cat_veh" et "descr_grav" par des valeurs numériques
#Afin de pouvoir mieux les exploiter par la suite
#On remplace ensuite les valeurs de la colonne par les valeurs du dictionnaire correspondantes

# Catégorie de véhicule :
# 14 – PL seul > 7,5T
# 10 – VU seul 1,5T <= PTAC <= 3,5T avec ou sans remorque
# 07 – VL seul
# 38 – Autocar
# 15 – PL > 3,5T + remorque
# 02 – Cyclomoteur <50cm3
# 33 – Motocyclette > 125 cm3
# 17 – Tracteur routier + semi-remorque
# 21 – Tracteur agricole
# 13 – PL seul 3,5T <PTCA <= 7,5T
# 37 – Autobus
# 32 – Scooter > 50 cm3 et <= 125 cm3
# 39 – Train
# 34 – Scooter > 125 cm3
# 30 – Scooter < 50 cm3
# 03 – Voiturette (Quadricycle à moteur carrossé) (anciennement "voiturette ou tricycle à moteur")
# 99 – Autre véhicule
# 01 – Bicyclette
# 31 – Motocyclette > 50 cm3 et <= 125 cm3
# 20 – Engin spécial
# 36 – Quad lourd > 50 cm3 (Quadricycle à moteur non carrossé)
# 19 – Tramway
# 16 – Tracteur routier seul
# 35 – Quad léger <= 50 cm3 (Quadricycle à moteur non carrossé)

# Gravité de l'accident :
# 1 – Indemne
# 2 – Tué
# 3 – Blessé hospitalisé
# 4 – Blessé léger


data$descr_cat_veh <- recode(data$descr_cat_veh, "PL seul > 7,5T" = 14, "VU seul 1,5T <= PTAC <= 3,5T avec ou sans remorque " = 10, "VL seul" = 07, "Autocar" = 38, "PL > 3,5T + remorque" = 15, "Cyclomoteur <50cm3" = 02, "Motocyclette > 125 cm3" = 33, "Tracteur routier + semi-remorque" = 17, "Tracteur agricole" = 21, "PL seul 3,5T <PTCA <= 7,5T" = 13, "Autobus" = 37, "Scooter > 50 cm3 et <= 125 cm3" = 32, "Train" = 39, "Scooter > 125 cm3" = 34, "Scooter < 50 cm3" = 30, "Voiturette (Quadricycle à moteur carrossé) (anciennement \"voiturette ou tricycle à moteur\")" = 03, "Autre véhicule" = 99, "Bicyclette" = 01, "Motocyclette > 50 cm3 et <= 125 cm3" = 31, "Engin spécial" = 20, "Quad lourd > 50 cm3 (Quadricycle à moteur non carrossé)" = 36, "Tramway" = 19, "Tracteur routier seul" = 16, "Quad léger <= 50 cm3 (Quadricycle à moteur non carrossé)" = 35)

data$descr_grav <- recode(data$descr_grav, "Indemne" = 1, "Tué" = 2, "Blessé hospitalisé" = 3, "Blessé léger" = 4)

#---------------------------------------------------------------------------------------------------------------
#-----------------------------------------------MODIF CASES VIDES-----------------------------------------------
#---------------------------------------------------------------------------------------------------------------

#boucle pour remplacer les cases NULL par 10 dans la colonne "place" (10 correspondant aux piétons)
for (i in 1:length(data$place)) {
  if (data$place[i] == "NULL") {
    data$place[i] <- 10
  }
}


#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------MODIF DESCR AGGLO-------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

#On cherche à remplacer les valeurs de la colonne "descr_agglo" par des valeurs numériques correspondantes
#On remplace ensuite les valeurs de la colonne par les valeurs du dictionnaire correspondantes

# Agglomération :
# 1 – Hors agglomération
# 2 – En agglomération

data$descr_agglo <- recode(data$descr_agglo, "Hors agglomération" = 1, "En agglomération" = 2)


#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------MODIF DESCR ATHMO-------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

#On cherche à remplacer les valeurs de la colonne "descr_atm" par des valeurs numériques correspondantes
#On remplace ensuite les valeurs de la colonne par les valeurs du dictionnaire correspondantes

# Conditions atmosphériques :
# 1 – Normale
# 2 – Pluie légère
# 3 – Pluie forte
# 4 – Neige - grêle
# 5 – Brouillard - fumée
# 6 – Vent fort - tempête
# 7 – Temps éblouissant
# 8 – Temps couvert
# 9 – Autre 

data$descr_athmo <- recode(data$descr_athmo, "Normale" = 1, "Pluie légère" = 2, "Pluie forte" = 3, "Neige – grêle" = 4, "Brouillard – fumée" = 5, "Vent fort – tempête" = 6, "Temps éblouissant" = 7, "Temps couvert" = 8, "Autre" = 9)

#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------MODIF DESCR LUMI--------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

#On cherche à remplacer les valeurs de la colonne "descr_lum" par des valeurs numériques correspondantes
#On remplace ensuite les valeurs de la colonne par les valeurs du dictionnaire correspondantes

# Lumière : conditions d’éclairage dans lesquelles l'accident s'est produit :
# 1 – Plein jour
# 2 – Crépuscule ou aube
# 3 – Nuit sans éclairage public
# 4 – Nuit avec éclairage public non allumé
# 5 – Nuit avec éclairage public allumé 

data$descr_lum <- recode(data$descr_lum, "Plein jour" = 1, "Crépuscule ou aube" = 2, "Nuit sans éclairage public" = 3, "Nuit avec éclairage public non allumé" = 4, "Nuit avec éclairage public allumé" = 5)

#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------MODIF DESCR SURF--------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

#On cherche à remplacer les valeurs de la colonne "descr_etat_surf" par des valeurs numériques correspondantes
#On remplace ensuite les valeurs de la colonne par les valeurs du dictionnaire correspondantes

# Etat de la surface :
# 1 – Normale
# 2 – Mouillée
# 3 – Flaques
# 4 – Inondée
# 5 – Enneigée
# 6 – Boue
# 7 – Verglacée
# 8 – Corps gras – huile
# 9 – Autre 

#utilisation de recode pour remplacer les valeurs de la colonne "descr_etat_surf" par les valeurs du dictionnaire correspondantes
data$descr_etat_surf <- recode(data$descr_etat_surf, "Normale" = 1, "Mouillée" = 2, "Flaques" = 3, "Inondée" = 4, "Enneigée" = 5, "Boue" = 6, "Verglacée" = 7, "Corps gras – huile" = 8, "Autre" = 9)

#---------------------------------------------------------------------------------------------------------------
#-----------------------------------------INVERSION LAT/LONG DOM TOM--------------------------------------------
#---------------------------------------------------------------------------------------------------------------

#On cherche à modifier les valeurs de latitude et longitude pour les lignes avec "id_code_insee" commence par 97 (DOM TOM)
#Ces valeurs sont inversées dans le fichier csv
#On récupère les lignes avec "id_code_insee" commence par 97
#On inverse les valeurs de latitude et longitude pour les lignes avec "id_code_insee" commence par 97

#inversion des valeurs de latitude et longitude pour les lignes avec "id_code_insee" commence par 97
for(i in 1:length(data$latitude)) {
  if (grepl("^97", data$id_code_insee[i])) {
    temp <- data$latitude[i]
    data$latitude[i] <- data$longitude[i]
    data$longitude[i] <- temp
  }
}

#---------------------------------------------------------------------------------------------------------------
#-------------------------------------------MODIF LAT/LONG NULLES-----------------------------------------------
#---------------------------------------------------------------------------------------------------------------

#On cherche à modifier les valeurs de latitude et longitude qui sont nulles par leurs valeurs correspondantes
#On récupère les villes correspondant aux valeurs de latitude et longitude nulles
#On crée une matrice pour stocker les valeurs de latitude et longitude correspondant aux villes de cities
#On remplace les valeurs de latitude et longitude nulles par les valeurs de la matrice correspondantes


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

#---------------------------------------------------------------------------------------------------------------
#----------------------------------------SUPPR AGE / AN_NAIS NULL-----------------------------------------------
#---------------------------------------------------------------------------------------------------------------

#On cherche à supprimer les lignes avec des valeurs nulles de "age" et "an_nais" car on ne peut pas les remplacer

#suppression des lignes avec des valeurs nulles de "age" et "an_nais"
data <- subset(data, data$age != "NULL")
data <- subset(data, data$an_nais != "NULL")

#---------------------------------------------------------------------------------------------------------------
#----------------------------------------------MODIF LAT/LONG---------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

#On cherche à modifier les valeurs de latitude et longitude qui sont égales à 2009... par leurs valeurs correspondantes
#On récupère les villes correspondant aux valeurs de latitude et longitude égales à 2009...
#On stock une seule fois chaque ville ou data$latitude == 2009...
#On stock les arrondissements de paris, de marsseille et de lyon dans 3 vecteurs
#On remplace les valeurs de latitude et longitude égales à 2009... par les valeurs correspondantes dans les vecteurs


#remplacement des longitudes et latitudes des villes avec des valeurs de latitude et longitude == 2009... par leur valeurs réelles
#stock une seule fois chaque ville ou data$latitude == 2009
arr <- unique(data$ville[data$latitude == 2009])

#print si il y a des NA dans latitude ou longitude

#stock les arrondissements de paris, de marsseille et de lyon dans 3 vecteurs
arr_paris <- c("PARIS 20", "PARIS 19", "PARIS 18", "PARIS 17", "PARIS 16", "PARIS 15", "PARIS 14", "PARIS 13", "PARIS 12", "PARIS 11", "PARIS 10", "PARIS 09", "PARIS 08", "PARIS 07", "PARIS 06", "PARIS 05", "PARIS 04", "PARIS 03", "PARIS 02", "PARIS 01")
arr_marseille <- c("MARSEILLE 16", "MARSEILLE 15", "MARSEILLE 14", "MARSEILLE 13", "MARSEILLE 12", "MARSEILLE 11", "MARSEILLE 10", "MARSEILLE 09", "MARSEILLE 08", "MARSEILLE 07", "MARSEILLE 06", "MARSEILLE 05", "MARSEILLE 04", "MARSEILLE 03", "MARSEILLE 02", "MARSEILLE 01")
arr_lyon <- c("LYON 09", "LYON 08", "LYON 07", "LYON 06", "LYON 05", "LYON 04", "LYON 03", "LYON 02", "LYON 01")

#stockage des valeurs de latitude et longitude correspondant aux villes de arr
arr_lat_paris <- c("PARIS 20"=48.864527, "PARIS 19"=48.864527, "PARIS 18"=48.864527, "PARIS 17"=48.864527, "PARIS 16"=48.864527, "PARIS 15"=48.864527, "PARIS 14"=48.864527, "PARIS 13"=48.864527, "PARIS 12"=48.864527, "PARIS 11"=48.864527, "PARIS 10"=48.864527, "PARIS 09"=48.864527, "PARIS 08"=48.864527, "PARIS 07"=48.864527, "PARIS 06"=48.864527, "PARIS 05"=48.864527, "PARIS 04"=48.864527, "PARIS 03"=48.864527, "PARIS 02"=48.864527, "PARIS 01"=48.864527)
arr_long_paris <- c("PARIS 20"=2.416170, "PARIS 19"=2.416170, "PARIS 18"=2.416170, "PARIS 17"=2.416170, "PARIS 16"=2.416170, "PARIS 15"=2.416170, "PARIS 14"=2.416170, "PARIS 13"=2.416170, "PARIS 12"=2.416170, "PARIS 11"=2.416170, "PARIS 10"=2.416170, "PARIS 09"=2.416170, "PARIS 08"=2.416170, "PARIS 07"=2.416170, "PARIS 06"=2.416170, "PARIS 05"=2.416170, "PARIS 04"=2.416170, "PARIS 03"=2.416170, "PARIS 02"=2.416170, "PARIS 01"=2.416170)

arr_lat_lyon <- c("LYON 09"=45.760339, "LYON 08"=45.760339, "LYON 07"=45.760339, "LYON 06"=45.760339, "LYON 05"=45.760339, "LYON 04"=45.760339, "LYON 03"=45.760339, "LYON 02"=45.760339, "LYON 01"=45.760339)
arr_long_lyon <- c("LYON 09"=4.836629, "LYON 08"=4.836629, "LYON 07"=4.836629, "LYON 06"=4.836629, "LYON 05"=4.836629, "LYON 04"=4.836629, "LYON 03"=4.836629, "LYON 02"=4.836629, "LYON 01"=4.836629)

arr_lat_marseille <- c("MARSEILLE 16"=43.296482, "MARSEILLE 15"=43.296482, "MARSEILLE 14"=43.296482, "MARSEILLE 13"=43.296482, "MARSEILLE 12"=43.296482, "MARSEILLE 11"=43.296482, "MARSEILLE 10"=43.296482, "MARSEILLE 09"=43.296482, "MARSEILLE 08"=43.296482, "MARSEILLE 07"=43.296482, "MARSEILLE 06"=43.296482, "MARSEILLE 05"=43.296482, "MARSEILLE 04"=43.296482, "MARSEILLE 03"=43.296482, "MARSEILLE 02"=43.296482, "MARSEILLE 01"=43.296482)
arr_long_marseille <- c("MARSEILLE 16"=5.369780, "MARSEILLE 15"=5.369780, "MARSEILLE 14"=5.369780, "MARSEILLE 13"=5.369780, "MARSEILLE 12"=5.369780, "MARSEILLE 11"=5.369780, "MARSEILLE 10"=5.369780, "MARSEILLE 09"=5.369780, "MARSEILLE 08"=5.369780, "MARSEILLE 07"=5.369780, "MARSEILLE 06"=5.369780, "MARSEILLE 05"=5.369780, "MARSEILLE 04"=5.369780, "MARSEILLE 03"=5.369780, "MARSEILLE 02"=5.369780, "MARSEILLE 01"=5.369780)


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

#-------------------------------------------------------------------------------------------------------------
#--------------------------------------------MODIFICATION DESCR INTERSEC--------------------------------------
#-------------------------------------------------------------------------------------------------------------

# Intersection :
# 1 – Hors intersection
# 2 – Intersection en X
# 3 – Intersection en T
# 4 – Intersection en Y
# 5 – Intersection à plus de 4 branches
# 6 – Giratoire
# 7 – Place
# 8 – Passage à niveau
# 9 – Autre intersection 

data$description_intersection <- recode(data$description_intersection, "Hors intersection" = 1, "Intersection en X" = 2, "Intersection en T" = 3, "Intersection en Y" = 4, "Intersection à plus de 4 branches" = 5, "Giratoire" = 6, "Place" = 7, "Passage à niveau" = 8, "Autre intersection" = 9)


#-------------------------------------------------------------------------------------------------------------
#----------------------------------------------MODIFICATION MOTIF TRAJ----------------------------------------
#-------------------------------------------------------------------------------------------------------------

# Motif du déplacement au moment de l’accident :
# 0 – Non renseigné
# 1 – Domicile – travail
# 2 – Domicile – école
# 3 – Courses – achats
# 4 – Utilisation professionnelle
# 5 – Promenade – loisirs
# 9 – Autre 

data$descr_motif_traj <- recode(data$descr_motif_traj, "Non renseigné" = 0, "Domicile – travail" = 1, "Domicile – école" = 2, "Courses – achats" = 3, "Utilisation professionnelle" = 4, "Promenade – loisirs" = 5, "Autre" = 9)

#-------------------------------------------------------------------------------------------------------------
#----------------------------------------------MODIFICATION DESCR COL-----------------------------------------
#-------------------------------------------------------------------------------------------------------------

# Collision :
# 1 – Deux véhicules - Frontale
# 2 – Deux véhicules – Par l’arrière
# 3 – Deux véhicules – Par le coté
# 4 – Trois véhicules et plus – En chaîne
# 5 – Trois véhicules et plus – Collisions multiples
# 6 – Autre collision
# 7 – Sans collision

data$descr_type_col <- recode(data$descr_type_col, "Deux véhicules - Frontale" = 1, "Deux véhicules – Par l’arrière" = 2, "Deux véhicules – Par le coté" = 3, "Trois véhicules et plus – En chaîne" = 4, "Trois véhicules et plus – Collisions multiples" = 5, "Autre collision" = 6, "Sans collision" = 7)

#-------------------------------------------------------------------------------------------------------------
#----------------------------------------------MODIFICATION DISPO SECU----------------------------------------
#-------------------------------------------------------------------------------------------------------------
# Dispositif de sécurité :
# 1 – Utilisation d'une ceinture de sécurité 
# 2 – Utilisation d'un casque 
# 3 – Présence d'une ceinture de sécurité - Utilisation non déterminable
# 4 – Présence de ceinture de sécurité non utilisée 
# 5 – Autre - Non déterminable
# 6 – Présence d'un équipement réfléchissant non utilisé
# 7 – Présence d'un casque non utilisé 
# 8 – Utilisation d'un dispositif enfant
# 9 – Présence d'un casque - Utilisation non déterminable
# 10 – Présence dispositif enfant - Utilisation non déterminable
# 11 – Autre - Utilisé
# 12 – Utilisation d'un équipement réfléchissant 
# 13 – Autre - Non utilisé
# 14 – Présence équipement réfléchissant - Utilisation non déterminable
# 15 – Présence d'un dispositif enfant non utilisé

data$descr_dispo_secu <- recode(data$descr_dispo_secu, "Utilisation d'une ceinture de sécurité " = 1, "Utilisation d'un casque " = 2, "Présence d'une ceinture de sécurité - Utilisation non déterminable" = 3, "Présence de ceinture de sécurité non utilisée " = 4, "Autre - Non déterminable" = 5, "Présence d'un équipement réfléchissant non utilisé" = 6, "Présence d'un casque non utilisé " = 7, "Utilisation d'un dispositif enfant" = 8, "Présence d'un casque - Utilisation non déterminable" = 9, "Présence dispositif enfant - Utilisation non déterminable" = 10, "Autre - Utilisé" = 11, "Utilisation d'un équipement réfléchissant " = 12, "Autre - Non utilisé" = 13, "Présence équipement réfléchissant - Utilisation non déterminable" = 14, "Présence d'un dispositif enfant non utilisé" = 15)

#--------------------------------------------------------------------------------------------------------------
#----------------------------------------------FIX FORMAT COLONNES---------------------------------------------
#--------------------------------------------------------------------------------------------------------------

#On fixe le format date pour la colonne "date"w
data$date <- as.POSIXct(data$date, format = "%Y-%m-%d %H:%M")

#On fixe la colonne "age" en tant que integer
data$age <- as.integer(data$age)

#On fixe la colonne "an_nais" en tant que integer
data$an_nais <- as.integer(data$an_nais)

#On fixe la colonne "place" en tant que integer
data$place <- as.integer(data$place)


#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------MODIFICATION AGE--------------------------------------------
#-------------------------------------------------------------------------------------------------------------

#On remplace la colonne age, calculée à partir de maintenant, par leur age au moment de l'accident
data$age <- (2009 - data$an_nais)


#-------------------------------------------------------------------------------------------------------------
#--------------------------------------------------EXPORT CSV-------------------------------------------------
#-------------------------------------------------------------------------------------------------------------


#export csv
write.table(data, file = "data/stat_acc_V3_cleared.csv", sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)