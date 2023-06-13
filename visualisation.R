# Installation des librairies
if (!require("ggplot2")){install.packages("ggplot2")}
if (!require("lubridate")){install.packages("lubridate")}

library(ggplot2)
library(lubridate)

# Lecture du CSV
data <- readLines("data/stat_acc_V3.csv", encoding = "latin1")
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

# Création du diagramme à barres avec étiquettes
ggplot(data = accidents_par_condition, aes(x = descr_athmo, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = count), vjust = -0.3, size = 3.5) +
  theme_minimal()


# ------------------------------------------------------------------------------------------------
# -------------------------------- [ Nombre d’accidents en fonction de la description de la surface ]
# ------------------------------------------------------------------------------------------------


# Calcul du nombre d'accidents par surface
accidents_par_surface <- as.data.frame(table(data$descr_etat_surf))

# Renommer les colonnes pour correspondre à votre dataframe
names(accidents_par_surface) <- c("descr_etat_surf", "count")

# Création du diagramme à barres avec étiquettes
ggplot(data = accidents_par_surface, aes(x = descr_etat_surf, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = count), vjust = -0.3, size = 3.5) +
  theme_minimal()


# ------------------------------------------------------------------------------------------------
# -------------------------------- [ Nombre d’accidents selon la gravité ]
# ------------------------------------------------------------------------------------------------


# Calcul du nombre d'accidents par gravité
accidents_par_gravite <- as.data.frame(table(data$descr_grav))

# Renommer les colonnes pour correspondre à votre dataframe
names(accidents_par_gravite) <- c("descr_grav", "count")

# Création du diagramme à barres avec étiquettes
ggplot(data = accidents_par_gravite, aes(x = descr_grav, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = count), vjust = -0.3, size = 3.5) +
  theme_minimal()


# ------------------------------------------------------------------------------------------------
# -------------------------------- [ Nombre d’accidents par tranches d’heure ]
# ------------------------------------------------------------------------------------------------


# Calcul du nombre d'accidents par tranches d'heure
accidents_par_heure <- as.data.frame(table(hour(data$heure)))
                                     
# Renommer les colonnes pour correspondre à votre dataframe
names(accidents_par_heure) <- c("heure", "count")

# Création du diagramme à barres avec étiquettes
ggplot(data = accidents_par_heure, aes(x = heure, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = count), vjust = -0.3, size = 3.5) +
  theme_minimal()


# ------------------------------------------------------------------------------------------------
# -------------------------------- [ Nombre d’accidents par ville ]
# ------------------------------------------------------------------------------------------------


# Calcul du nombre d'accidents par ville
accidents_par_ville <- as.data.frame(table(data$ville))

# Renommer les colonnes pour correspondre à votre dataframe
names(accidents_par_ville) <- c("ville", "count")

# Création du diagramme à barres avec étiquettes
ggplot(data = accidents_par_ville, aes(x = ville, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = count), vjust = -0.3, size = 3.5) +
  theme_minimal()
