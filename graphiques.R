# Installation des librairies
if (!require("ggplot2")){install.packages("ggplot2")}
if (!require("lubridate")){install.packages("lubridate")}

library(ggplot2)
library(lubridate)

# Lecture du CSV
data <- readLines("data/stat_acc_V3_cleared.csv", encoding = "latin1")
data <- iconv(data, from = "latin1", to = "UTF-8")
data <- read.csv(text = data, sep = ";")


# ----------------
# ---------------------------------------------------------------- [ REPRESENTATIONS GRAPHIQUES ]
# ----------------

# ------------------------------------------------------------------------------------------------
# -------------------------------- [ Nombre d’accidents en fonction des conditions atmosphériques ]
# ------------------------------------------------------------------------------------------------


# Calcul du nombre d'accidents par condition atmosphérique
accidents_par_condition <- as.data.frame(table(data$descr_athmo))

# Renommer les colonnes pour correspondre à votre dataframe
names(accidents_par_condition) <- c("descr_athmo", "count")

# Légende
legende <- c("Normale", "Pluie légère", "Pluie forte", "Neige - grêle", "Brouillard - fumée", 
             "Vent fort - tempête", "Temps éblouissant", "Temps couvert", "Autre")

# Création du diagramme à barres avec étiquettes
ggplot(data = accidents_par_condition, aes(x = descr_athmo, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = count), vjust = -0.3, size = 3.5) +
  scale_x_discrete(labels = legende) +
  labs(x = "Conditions athmosphériques", y = "Nombre d'accidents", title = "Répartition des accidents selon les conditions athmosphériques") +
  theme_minimal()


# ------------------------------------------------------------------------------------------------
# -------------------------------- [ Nombre d’accidents en fonction de la description de la surface ]
# ------------------------------------------------------------------------------------------------


# Calcul du nombre d'accidents par surface
accidents_par_surface <- as.data.frame(table(data$descr_etat_surf))

# Renommer les colonnes pour correspondre à votre dataframe
names(accidents_par_surface) <- c("descr_etat_surf", "count")

# Légende
legende <- c("Normale", "Mouillée", "Flaques", "Inondée", 
             "Enneigée", "Boue", "Verglacée", "Corps gras - huile", 
             "Autre")

# Création du diagramme à barres avec étiquettes
ggplot(data = accidents_par_surface, aes(x = descr_etat_surf, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = count), vjust = -0.3, size = 3.5) +
  scale_x_discrete(labels = legende) +
  labs(x = "Surfaces", y = "Nombre d'accidents", title = "Répartition des accidents selon les surfaces") +
  theme_minimal()


# ------------------------------------------------------------------------------------------------
# -------------------------------- [ Nombre d’accidents selon la gravité ]
# ------------------------------------------------------------------------------------------------


# Calcul du nombre d'accidents par gravité
accidents_par_gravite <- as.data.frame(table(data$descr_grav))

# Renommer les colonnes pour correspondre à votre dataframe
names(accidents_par_gravite) <- c("descr_grav", "count")

legende <- c("Blessé hospitalisé", "Blessé léger", "Indemne", "Tué")

# Création du diagramme camembert avec étiquettes en conservant les accents de la légende
ggplot(data = accidents_par_gravite, aes(x = "", y = count, fill = descr_grav)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 3.5) +
  coord_polar("y", start = 0) +
  scale_fill_discrete(labels = legende) +
  labs(x = "", y = "", title = "Répartition des accidents selon la gravité") +
  theme_minimal() +
  theme(legend.position = "bottom")


# ------------------------------------------------------------------------------------------------
# -------------------------------- [ Nombre d’accidents par tranches d’heure ]
# ------------------------------------------------------------------------------------------------


# Calcul du nombre d'accidents par tranches d'heure
heures <- hour(data$date)
accidents_par_heure <- as.data.frame(table(heures))

# Renommer les colonnes pour correspondre à votre dataframe
names(accidents_par_heure) <- c("heure", "count")

# Création du diagramme à barres avec étiquettes
ggplot(data = accidents_par_heure, aes(x = heure, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = count), vjust = -0.3, size = 3.5) +
  labs(x = "Heures", y = "Nombre d'accidents", title = "Répartition des accidents par tranches d'heures") +
  theme_minimal()


# ------------------------------------------------------------------------------------------------
# -------------------------------- [ Nombre d’accidents par ville ]
# ------------------------------------------------------------------------------------------------


# Calcul du nombre d'accidents par ville
accidents_par_ville <- as.data.frame(table(data$ville))

# Sélection du top 10 des ville savec le plus d'accidents
top_10_villes <- head(accidents_par_ville[order(accidents_par_ville$Freq, decreasing = TRUE), ], 10)

# Renommer les colonnes pour correspondre à votre dataframe
names(accidents_par_ville) <- c("ville", "count")

# Création du diagramme à barres avec étiquettes
ggplot(data = accidents_par_ville, aes(x = ville, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = count), vjust = -0.3, size = 3.5) +
  theme_minimal()

# Renommer les colonnes pour correspondre à votre dataframe
names(top_10_villes) <- c("ville", "count")

# Création du diagramme pour le top 10 des villes avec le plus d'accidents
ggplot(data = top_10_villes, aes(x = ville, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = count), vjust = -0.3, size = 3.5) +
  labs(x = "Villes", y = "Nombre d'accidents", title = "Top 10 des villes avec le plus d'accidents") +
  theme_minimal()
