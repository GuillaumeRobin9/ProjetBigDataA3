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

data$date <- as.POSIXct(data$date)  # Convertir la colonne 'date' en format POSIXct bc data don't keep the time

series_mois <- data %>%
  mutate(mois = format(date, "%Y-%B")) %>%
  count(mois)

series_semaines <- data %>%
  mutate(semaine = format(date, "%Y-%a")) %>%
  count(semaine)

  
print(series_mois)
print(series_semaines)
