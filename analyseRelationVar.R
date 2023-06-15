# Installation des librairies
if (!require("ggplot2")){install.packages("ggplot2")}
if (!require("lubridate")){install.packages("lubridate")}
if (!require("caret")){install.packages("caret")}

library(ggplot2)
library(lubridate)
library(caret)

# Lecture du CSV
data <- readLines("data/stat_acc_V3.csv", encoding = "latin1")
data <- iconv(data, from = "latin1", to = "UTF-8")
data <- read.csv(text = data, sep = ";")


# ------------------------------------------------------------------------------------------------
# --------------------------------- [ Etude des relations entre variables qualitatives ]
# ------------------------------------------------------------------------------------------------

# Créer un vecteur de couleurs personnalisées pour chaque catégorie
couleurs <- c("#00FF00", "black", "red", "orange")

# ------------------------ tableaux croisées et des tests d’indépendance du chi2 sur les tableaux entre les différentes variables

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

# deux variables : descr_cat_veh et descr_grav
table_croisee <- table(data$descr_cat_veh, data$descr_grav)

# Afficher le tableau croisé
print(table_croisee)

# Créer le graphique mosaic plot
mosaicplot(table_croisee, main = "Mosaic Plot : gravité accident / catégorie du véhicule", 
           xlab="Catégorie du véhicule", ylab="Gravité de l'accident", color = couleurs)

# Effectuer le test d'indépendance du chi2
test_chi2 <- chisq.test(table_croisee)

# Afficher les résultats du test
print(test_chi2)

# ---------- Interprétation

# La statistique du chi2 élevée (14992) indique une différence significative entre les variables descr_cat_veh et descr_grav.
# Les degrés de liberté (69) représentent le nombre de catégories moins 1 pour chaque variable. Dans ce cas, il y a 69 degrés de liberté, ce qui est cohérent avec la taille du tableau croisé.
# La valeur p très faible (< 2.2e-16) suggère que l'association entre les variables est statistiquement significative. La probabilité d'obtenir une telle différence importante par hasard est extrêmement faible.
# En conclusion, nous pouvons rejeter l'hypothèse nulle d'indépendance entre les variables descr_cat_veh et descr_grav. Les résultats suggèrent qu'il existe une relation significative entre ces deux variables.

# ---------- 


# deux variables : age et descr_grav
table_croisee <- table(data$age, data$descr_grav)

# Afficher le tableau croisé
print(table_croisee)

# Créer le graphique mosaic plot
mosaicplot(table_croisee, main = "Mosaic Plot : gravité accident / age", xlab="Age"
           , ylab="Gravité de l'accident",color = couleurs)

# Effectuer le test d'indépendance du chi2
test_chi2 <- chisq.test(table_croisee)

# Afficher les résultats du test
print(test_chi2)


# ---------- Interprétation

# La statistique du chi2 élevée (4289.5) indique une différence significative entre les variables considérées dans le tableau croisé.
# Les degrés de liberté (297) représentent le nombre de catégories moins 1 pour chaque variable dans le tableau croisé. Dans ce cas, il y a 297 degrés de liberté, ce qui est cohérent avec la taille du tableau croisé.
# La valeur p très faible (< 2.2e-16) suggère que l'association entre les variables est statistiquement significative. La probabilité d'obtenir une telle différence importante par hasard est extrêmement faible.
# En conclusion, nous pouvons rejeter l'hypothèse nulle d'indépendance entre les variables considérées dans le tableau croisé. Les résultats indiquent qu'il existe une relation significative entre ces variables.

# ----------

# Lumière : conditions d’éclairage dans lesquelles l'accident s'est produit :
# 1 – Plein jour
# 2 – Crépuscule ou aube
# 3 – Nuit sans éclairage public
# 4 – Nuit avec éclairage public non allumé
# 5 – Nuit avec éclairage public allumé 

# deux variables : descr_lum et descr_grav
table_croisee <- table(data$descr_lum, data$descr_grav)

# Afficher le tableau croisé
print(table_croisee)

# Créer le graphique mosaic plot
mosaicplot(table_croisee, main = "Mosaic Plot : gravité de l'accident / conditions lumineuses",
           xlab="Condition lumineuse", ylab="Gravité de l'accident", color = couleurs)

# Effectuer le test d'indépendance du chi2
test_chi2 <- chisq.test(table_croisee)

# Afficher les résultats du test
print(test_chi2)


# ---------- Interprétation

# Pearson's Chi-squared test
# data:  table_croisee
# X-squared = 1868.8, df = 12, p-value < 2.2e-16
# Conclusion : corrélation forte entre condition lumineuses et gravité de l'accident

# ----------

# deux variables : heure et descr_grav

# Calcul du nombre d'accidents par tranches d'heure
heures <- hour(data$date)
accidents_par_heure <- heures

# Renommer les colonnes pour correspondre à votre dataframe
names(accidents_par_heure) <- c("heure", "count")

table_croisee <- table(accidents_par_heure, data$descr_grav)

# Afficher le tableau croisé
print(table_croisee)

# Créer le graphique mosaic plot
mosaicplot(table_croisee, main = "Mosaic Plot : gravité de l'accident / heure", 
           xlab="Heure", ylab="Gravité de l'accident", color = couleurs)

# Effectuer le test d'indépendance du chi2
test_chi2 <- chisq.test(table_croisee)

# Afficher les résultats du test
print(test_chi2)


# ---------- Interprétation

# Pearson's Chi-squared test
# data:  table_croisee
# X-squared = 1868.8, df = 12, p-value < 2.2e-16
# Conclusion : corrélation forte entre condition lumineuses et gravité de l'accident

# ----------

# deux variables : descr_dispo_secu et descr_grav

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

table_croisee <- table(data$descr_dispo_secu, data$descr_grav)

# Afficher le tableau croisé
print(table_croisee)

# Créer le graphique mosaic plot
mosaicplot(table_croisee, main = "Mosaic Plot : gravité de l'accident / dispositif de sécurité", 
           xlab="Dispositif de sécurité", ylab="Gravité de l'accident", color = couleurs)

# Effectuer le test d'indépendance du chi2
test_chi2 <- chisq.test(table_croisee)

# Afficher les résultats du test
print(test_chi2)


# ---------- Interprétation

# Pearson's Chi-squared test
# data:  table_croisee
# X-squared = 18907, df = 42, p-value < 2.2e-16
# Conclusion : corrélation forte entre dispositif de sécurité et gravité de l'accident

# ----------

# deux variables : descr_athmo et descr_grav

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

table_croisee <- table(data$descr_athmo, data$descr_grav)

# Afficher le tableau croisé
print(table_croisee)

# Créer le graphique mosaic plot
mosaicplot(table_croisee, main = "Mosaic Plot : gravité de l'accident / conditions athmosphériques", 
           xlab="Condition arhmosphérique", ylab="Gravité de l'accident", color = couleurs)

# Effectuer le test d'indépendance du chi2
test_chi2 <- chisq.test(table_croisee)

# Afficher les résultats du test
print(test_chi2)


# ---------- Interprétation

# Pearson's Chi-squared test
# data:  table_croisee
# X-squared = 311.49, df = 24, p-value < 2.2e-16
# Conclusion : corrélation forte entre conditions athmosphériques et gravité de l'accident

# ----------

# deux variables : descr_type_col et descr_grav

# Collision :
# 1 – Deux véhicules - Frontale
# 2 – Deux véhicules – Par l’arrière
# 3 – Deux véhicules – Par le coté
# 4 – Trois véhicules et plus – En chaîne
# 5 – Trois véhicules et plus – Collisions multiples
# 6 – Autre collision
# 7 – Sans collision

table_croisee <- table(data$descr_type_col, data$descr_grav)

# Afficher le tableau croisé
print(table_croisee)

# Créer le graphique mosaic plot
mosaicplot(table_croisee, main = "Mosaic Plot : gravité de l'accident / type de collision", 
           xlab="Type de collision", ylab="Gravité de l'accident", color = couleurs)

# Effectuer le test d'indépendance du chi2
test_chi2 <- chisq.test(table_croisee)

# Afficher les résultats du test
print(test_chi2)


# ---------- Interprétation

# Pearson's Chi-squared test
# data:  table_croisee
# X-squared = 4843, df = 18, p-value < 2.2e-16
# Conclusion : corrélation forte entre type de collision et gravité de l'accident

# ----------

# deux variables : description_intersection et descr_grav

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

table_croisee <- table(data$description_intersection, data$descr_grav)

# Afficher le tableau croisé
print(table_croisee)

# Créer le graphique mosaic plot
mosaicplot(table_croisee, main = "Mosaic Plot : gravité de l'accident / type d'intersection", 
           xlab="Type dintersection", ylab="Gravité de l'accident", color = couleurs)

# Effectuer le test d'indépendance du chi2
test_chi2 <- chisq.test(table_croisee)

# Afficher les résultats du test
print(test_chi2)


# ---------- Interprétation

# Pearson's Chi-squared test
# data:  table_croisee
# X-squared = 1494.1, df = 24, p-value < 2.2e-16
# Conclusion : corrélation forte entre description_intersection et gravité de l'accident

# ----------
