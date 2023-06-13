# Installation des librairies
if (!require("ggplot2")){install.packages("ggplot2")}
if (!require("lubridate")){install.packages("lubridate")}

library(ggplot2)
library(lubridate)

# Lecture du CSV
data <- readLines("data/stat_acc_V3.csv", encoding = "latin1")
data <- iconv(data, from = "latin1", to = "UTF-8")
data <- read.csv(text = data, sep = ";")


# ------------------------------------------------------------------------------------------------
# --------------------------------- [ Etude des relations entre variables qualitatives ]
# ------------------------------------------------------------------------------------------------

# ---- tableaux croisées et des tests d’indépendance du chi2 sur les tableaux entre les différentes variables

# deux variables : descr_cat_veh et descr_grav
table_croisee <- table(data$descr_cat_veh, data$descr_grav)

# Afficher le tableau croisé
print(table_croisee)

# Créer le graphique mosaic plot
mosaicplot(table_croisee, main = "Mosaic Plot")

# Effectuer le test d'indépendance du chi2
test_chi2 <- chisq.test(table_croisee)

# Afficher les résultats du test
print(test_chi2)


# ------------------------------------------------------------------------------------------------
# --------------------------------- [ Régression linéaire ]
# ------------------------------------------------------------------------------------------------


# Extraction du mois
data$mois <- as.POSIXlt(data$date)$mon + 1

# Calcul du nombre d'accidents par mois
accidents_par_mois <- aggregate(data$Num_Acc, by = list(mois = data$mois), FUN = length)

# Calcul de la régression
regression <- lm(accidents_par_mois$x ~ accidents_par_mois$mois)

# Affichage du résultat de la régression
summary(regression)


