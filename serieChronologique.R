if (!require(rlang)) {
  install.packages("rlang")
}
if (!require(lubridate)) {
  install.packages("lubridate")
}
library(rlang)
library(dplyr)
library(lubridate)
data <- read.csv("data/stat_acc_V3.csv", sep=";")

data$date <- as.POSIXct(data$date)  # Convertir la colonne 'date' en format POSIXct car les données ne contiennent pas l'heure

series_mois <- data %>%
  mutate(mois = format(date, "%Y-%B")) %>%
  count(mois)

series_semaines <- data %>%
  mutate(semaine = format(date, "%Y-%a")) %>%
  count(semaine)

print(series_mois)
print(series_semaines)

data <- data %>%
  mutate(mois = format(date, "%B"),
         semaine = format(date, "%a"))

# add in csv 
#write.table(data, file = "data/stat_acc_V3_test.csv", sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)



