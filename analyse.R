# Installation des librairies
if (!require("ggplot2")){install.packages("ggplot2")}
if (!require("lubridate")){install.packages("lubridate")}

library(ggplot2)
library(lubridate)

# Lecture du CSV
data <- readLines("~/ISEN/CIR_3/projet_BigData_IA_Web/stat_acc_V3_cleared.csv", encoding = "latin1")
data <- iconv(data, from = "latin1", to = "UTF-8")
data <- read.csv(text = data, sep = ";")


# ------------------------------------------------------------------------------------------------
# --------------------------------- [ Etude des relations entre variables qualitatives ]
# ------------------------------------------------------------------------------------------------

# Créer un vecteur de couleurs personnalisées pour chaque catégorie
couleurs <- c("#00FF00", "black", "red", "orange")

# ------------------------ tableaux croisées et des tests d’indépendance du chi2 sur les tableaux entre les différentes variables

# deux variables : descr_cat_veh et descr_grav
table_croisee <- table(data$descr_cat_veh, data$descr_grav)

# Afficher le tableau croisé
print(table_croisee)

# Créer le graphique mosaic plot
mosaicplot(table_croisee, main = "Mosaic Plot : descr_grav / descr_cat_veh", color = couleurs)

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
mosaicplot(table_croisee, main = "Mosaic Plot : descr_grav / age", , color = couleurs)

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

par(mar = c(1, 1, 1, 1))

# deux variables : descr_lum et descr_grav
table_croisee <- table(data$descr_lum, data$descr_grav)

# Afficher le tableau croisé
print(table_croisee)

# Créer le graphique mosaic plot
mosaicplot(table_croisee, main = "Mosaic Plot : descr_grav / descr_lum", color = couleurs)

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
heures <- format(data$date, "%H")
accidents_par_heure <- as.data.frame(table(heures))

# Renommer les colonnes pour correspondre à votre dataframe
names(accidents_par_heure) <- c("heure", "count")

table_croisee <- table(accidents_par_heure, data$descr_grav)

# Afficher le tableau croisé
print(table_croisee)

# Créer le graphique mosaic plot
mosaicplot(table_croisee, main = "Mosaic Plot : heure / descr_lum", color = couleurs)

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
table_croisee <- table(data$descr_dispo_secu, data$descr_grav)

# Afficher le tableau croisé
print(table_croisee)

# Créer le graphique mosaic plot
mosaicplot(table_croisee, main = "Mosaic Plot : descr_grav / descr_dispo_secu", , color = couleurs)

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
table_croisee <- table(data$descr_athmo, data$descr_grav)

# Afficher le tableau croisé
print(table_croisee)

# Créer le graphique mosaic plot
mosaicplot(table_croisee, main = "Mosaic Plot : descr_grav / descr_athmo", , color = couleurs)

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
table_croisee <- table(data$descr_type_col, data$descr_grav)

# Afficher le tableau croisé
print(table_croisee)

# Créer le graphique mosaic plot
mosaicplot(table_croisee, main = "Mosaic Plot : descr_grav / descr_type_col", , color = couleurs)

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
table_croisee <- table(data$description_intersection, data$descr_grav)

# Afficher le tableau croisé
print(table_croisee)

# Créer le graphique mosaic plot
mosaicplot(table_croisee, main = "Mosaic Plot : descr_grav / description_intersection", , color = couleurs)

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


# ------------------------------------------------------------------------------------------------
# --------------------------------- [ Régression linéaire ]
# ------------------------------------------------------------------------------------------------


# ---------- Régression par mois 

# Extraction du mois
data$mois <- as.POSIXlt(data$date)$mon + 1

# Calcul du nombre d'accidents par mois
accidents_par_mois <- aggregate(data$Num_Acc, by = list(mois = data$mois), FUN = length)

# Calcul de la régression
regression <- lm(accidents_par_mois$x ~ accidents_par_mois$mois)

# Affichage du résultat de la régression
summary(regression)


# ---------- Régression par semaine

# Extraction de la semaine
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$semaine <- as.numeric(format(data$date, "%U"))

# Calcul du nombre d'accidents par semaine
accidents_par_semaine <- aggregate(data$Num_Acc, by = list(semaine = data$semaine), FUN = length)

# Calcul de la régression
regression <- lm(accidents_par_semaine$x ~ accidents_par_semaine$semaine)

# Affichage du résultat de la régression
summary(regression)






