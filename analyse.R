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
# --------------------------------- [ Régression linéaire (valeurs non cumulées)]
# ------------------------------------------------------------------------------------------------


# ---------- Régression par mois 

# Extraction du mois
data$mois <- as.POSIXlt(data$date)$mon + 1

# Calcul du nombre d'accidents par mois
accidents_par_mois <- aggregate(data$Num_Acc, by = list(mois = data$mois), FUN = length)
print(accidents_par_mois)

# Calcul de la régression
regression_mois <- lm(accidents_par_mois$x ~ accidents_par_mois$mois)

# Affichage du résultat de la régression
summary(regression_mois)

# Tracer la courbe de régression
ggplot(data = accidents_par_mois, aes(x = mois, y = x)) +
  #geom_point(color = "blue") +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) +
  labs(x = "Mois", y = "Nombre d'accidents") +
  ggtitle("Régression des accidents par mois") +
  theme_minimal()


# ---------- Régression par semaine

# Extraction de la semaine
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$semaine <- as.numeric(format(data$date, "%U"))

# Calcul du nombre d'accidents par semaine
accidents_par_semaine <- aggregate(data$Num_Acc, by = list(semaine = data$semaine), FUN = length)

# Calcul de la régression
regression_semaine <- lm(accidents_par_semaine$x ~ accidents_par_semaine$semaine)

# Affichage du résultat de la régression
summary(regression_semaine)

# Tracer la courbe de régression
ggplot(data = accidents_par_semaine, aes(x = semaine, y = x)) +
  #geom_point(color = "blue") +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) +
  labs(x = "Semaines", y = "Nombre d'accidents") +
  ggtitle("Régression des accidents par semaines") +
  theme_minimal()


# ---------- Comparaison des résultats des deux régressions


# Analyse de la variance pour la régression par semaine
anova_semaine <- anova(regression_semaine)

# Analyse de la variance pour la régression par mois
anova_mois <- anova(regression_mois)

# Erreurs types associées aux estimateurs et intervalles de confiance (semaine)
errors_semaine <- summary(regression_semaine)$coefficients[, "Std. Error"]
confidence_intervals_semaine <- confint(regression_semaine)

# Erreurs types associées aux estimateurs et intervalles de confiance (mois)
errors_mois <- summary(regression_mois)$coefficients[, "Std. Error"]
confidence_intervals_mois <- confint(regression_mois)

# R² et R² ajusté (semaine)
r_squared_semaine <- summary(regression_semaine)$r.squared
adjusted_r_squared_semaine <- summary(regression_semaine)$adj.r.squared

# R² et R² ajusté (mois)
r_squared_mois <- summary(regression_mois)$r.squared
adjusted_r_squared_mois <- summary(regression_mois)$adj.r.squared


# ------  Affichage des résultats


print(anova_semaine)
# L'analyse de variance (ANOVA) indique que la variable semaine n'est pas significativement liée au nombre d'accidents par semaine, avec une valeur p de 0.2121. 
# Cela signifie qu'il n'y a pas suffisamment de preuves pour affirmer que le numéro de la semaine a un impact significatif sur le nombre d'accidents.

print(anova_mois)
# L'analyse de variance (ANOVA) indique que la variable mois n'est pas significativement liée au nombre d'accidents par mois, avec une valeur p de 0.3609. 
# Cela suggère qu'il n'y a pas suffisamment de preuves pour affirmer que le mois de l'année a un impact significatif sur le nombre d'accidents.

print(errors_semaine)
print(confidence_intervals_semaine)
# Pour la régression par semaine, l'erreur type associée à l'estimateur de la variable semaine est de 2.290, avec un intervalle de confiance à 95% allant de -1.704 à 7.492.

print(errors_mois)
print(confidence_intervals_mois)
# Pour la régression par mois, l'erreur type associée à l'estimateur de la variable mois est de 61.825, avec un intervalle de confiance à 95% allant de -78.553 à 196.958.

print(r_squared_semaine)
print(adjusted_r_squared_semaine)
# Pour la régression par semaine, le R² est de 0.0304 et le R² ajusté est de 0.0113. Ces valeurs indiquent que seulement environ 3.04% de la variance dans le nombre 
# d'accidents par semaine est expliquée par le modèle de régression.

print(r_squared_mois)
print(adjusted_r_squared_mois)
# Pour la régression par mois, le R² est de 0.084 et le R² ajusté est de -0.0076. Ces valeurs indiquent que seulement environ 8.4% de la variance dans le nombre 
# d'accidents par mois est expliquée par le modèle de régression.

# ------ Qualité de régression

# Les prédictions basées sur les modèles de régression par semaine et par mois ne semblent pas être de bonne qualité, avec des valeurs R² très faibles et des erreurs élevées. 
# Cela suggère que les modèles ne capturent pas de manière significative la variation dans le nombre d'accidents.
# C'est conclusion sont confirmées par les tracés des courbes de régression superposées aus nombres d'accidents par mois et par semaines.
# Cela peut s'expliquer par le fait que nous ne prenons pas les valeurs cumulées des accidents. 
# Nous allons donc réiterer les régressions et les analyses pour les valeurs cumulées d'accidents pas mois et par semaines


# ------------------------------------------------------------------------------------------------
# --------------------------------- [ Régression linéaire (valeurs cumulées)]
# ------------------------------------------------------------------------------------------------


# ---------- Régression par mois 

# Extraction du mois
data$mois <- as.POSIXlt(data$date)$mon + 1

# Calcul de l'effectif cumulé des accidents par mois
accidents_cumul_mois <- cumsum(accidents_par_mois$x)

# Calcul de la régression
regression_cumul_mois <- lm(accidents_cumul_mois ~ accidents_par_mois$mois)

# Affichage du résultat de la régression
summary(regression_cumul_mois)

# Création du data.frame pour tracer la courbe de régression
plot_data <- data.frame(mois = unique(accidents_cumul_mois), accidents_cumul_mois = accidents_cumul_mois)

# Tracer la courbe de régression
ggplot(data = plot_data, aes(x = mois, y = accidents_cumul_mois)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) +
  labs(x = "Mois", y = "Nombre d'accidents cumulés") +
  ggtitle("Régression des accidents cumulés par mois") +
  theme_minimal()

# -- Interprétation
# Le R² (coefficient de détermination) est de 0.9991. Cela signifie que 99.91% de la variabilité de l'effectif cumulé des accidents par mois est expliquée par le modèle de régression.
# Le R² ajusté est de 0.999, ce qui est très proche du R². Cela indique que le modèle n'a pas de pénalité significative due au nombre de variables explicatives.
# Statistiques de test :
#   
# Le F-statistic est de 1.071e+04 avec un p-value très proche de zéro. Cela suggère que le modèle de régression dans son ensemble est statistiquement significatif.
# En conclusion, la régression linéaire montre que la variable mois a une influence significative sur l'effectif cumulé des accidents par mois. Le modèle de régression est très bien ajusté aux données, expliquant presque toute la variabilité des données.


# ---------- Régression par semaines

# Extraction de la semaine
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$semaine <- as.numeric(format(data$date, "%U"))

# Calcul du nombre d'accidents par semaine
accidents_par_semaine <- aggregate(data$Num_Acc, by = list(semaine = data$semaine), FUN = length)

# Calcul de l'effectif cumulé des accidents par mois
accidents_cumul_semaine <- cumsum(accidents_par_semaine$x)

# Calcul de la régression
regression_semaine <- lm(accidents_cumul_semaine ~ accidents_par_semaine$semaine)

# Affichage du résultat de la régression
summary(regression_semaine)

# Création du data.frame pour tracer la courbe de régression
plot_data <- data.frame(semaine = unique(accidents_cumul_semaine), accidents_cumul_semaine = accidents_cumul_semaine)

# Tracer la courbe de régression
ggplot(data = plot_data, aes(x = semaine, y = accidents_cumul_semaine)) +
  #geom_point(color = "blue") +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) +
  labs(x = "Semaines", y = "Nombre d'accidents") +
  ggtitle("Régression des accidents par semaines") +
  theme_minimal()

# -- Interprétation
# Le R² (coefficient de détermination) est de 0.9991. Cela signifie que 99.91% de la variabilité de l'effectif cumulé des accidents par semaine est expliquée par le modèle de régression.
# Le R² ajusté est également de 0.9991, ce qui suggère que le modèle n'a pas de pénalité significative due au nombre de variables explicatives.
# Statistiques de test :
#   
# Le F-statistic est de 5.859e+04 avec un p-value très proche de zéro. Cela suggère que le modèle de régression dans son ensemble est statistiquement significatif.
# En conclusion, la régression linéaire montre que la variable semaine a une influence significative sur l'effectif cumulé des accidents par semaine. Le modèle de régression est très bien ajusté aux données, expliquant presque toute la variabilité des données.


# ---------- Comparaison des résultats des deux régressions


# Analyse de la variance pour la régression par semaine
anova_semaine <- anova(regression_semaine)

# Analyse de la variance pour la régression par mois
anova_mois <- anova(regression_mois)

# Erreurs types associées aux estimateurs et intervalles de confiance (semaine)
errors_semaine <- summary(regression_semaine)$coefficients[, "Std. Error"]
confidence_intervals_semaine <- confint(regression_semaine)

# Erreurs types associées aux estimateurs et intervalles de confiance (mois)
errors_mois <- summary(regression_mois)$coefficients[, "Std. Error"]
confidence_intervals_mois <- confint(regression_mois)

# R² et R² ajusté (semaine)
r_squared_semaine <- summary(regression_semaine)$r.squared
adjusted_r_squared_semaine <- summary(regression_semaine)$adj.r.squared

# R² et R² ajusté (mois)
r_squared_mois <- summary(regression_mois)$r.squared
adjusted_r_squared_mois <- summary(regression_mois)$adj.r.squared


# ------  Affichage des résultats


print(anova_semaine)
# L'analyse de la variance (ANOVA) montre un F-value élevé de 58586 avec un p-value très proche de zéro. 
# Cela indique que le modèle de régression est statistiquement significatif pour expliquer l'effectif cumulé des accidents par semaine.

print(anova_mois)
# L'analyse de la variance (ANOVA) montre un F-value de 0.917 avec un p-value de 0.3609. 
# Cela indique que le modèle de régression n'est pas statistiquement significatif pour expliquer l'effectif cumulé des accidents par mois.

print(errors_semaine)
# Les erreurs types associées aux estimateurs sont de 182.67 pour l'intercept (constante) et de 6.06 pour la variable semaine.

print(confidence_intervals_semaine)
# Les intervalles de confiance à 95% pour les estimateurs sont de -1637.79 à -904.33 pour l'intercept et de 1453.56 à 1477.87 pour la variable semaine.

print(errors_mois)
# Les erreurs types associées aux estimateurs sont de 455.02 pour l'intercept et de 61.83 pour la variable mois.

print(confidence_intervals_mois)
# Les intervalles de confiance à 95% pour les estimateurs sont de 4737.99 à 6765.70 pour l'intercept et de -78.55 à 196.96 pour la variable mois.

print(r_squared_semaine)
# Le R² (coefficient de détermination) est de 0.9991, ce qui indique que 99.91% de la variabilité de l'effectif cumulé des accidents par semaine est expliquée par le modèle de régression.

print(adjusted_r_squared_semaine)
# Le R² ajusté est également de 0.9991, ce qui suggère que le modèle n'a pas de pénalité significative due au nombre de variables explicatives.

print(r_squared_mois)
# Le R² est de 0.08399, ce qui indique que seulement 8.40% de la variabilité de l'effectif cumulé des accidents par mois est expliquée par le modèle de régression.

print(adjusted_r_squared_mois)
# Le R² ajusté est de -0.00761, ce qui suggère que le modèle ne fournit pas une bonne ajustement aux données et n'est pas capable d'expliquer la variabilité des données.


# ------ Qualité de régression

# En conclusion, la régression par semaine est statistiquement significative et fournit un ajustement très précis aux données, 
# expliquant près de 99.91% de la variabilité de l'effectif cumulé des accidents par semaine. En revanche, la régression par mois 
# n'est pas statistiquement significative et ne fournit pas un bon ajustement aux données, expliquant seulement 8.40% de la variabilité 
# de l'effectif cumulé des accidents par mois.







