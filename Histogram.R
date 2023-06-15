data <- read.csv("data/stat_acc_V3_cleared.csv", sep = ";")

# Histograme quantité d’accidents en fonction des tranches d’âges
age <- as.numeric(data$age) # age = char -> numeric
hist(age, main = "Histogramme des accidents en fonction des tranches d’âges", xlab = "Tranches d’âges", ylab = "Nombre d’accidents", col = "blue", border = "red", breaks = 10)


# Moyenne mensuelle des accidents
#monthAvg <-avg(data$mois)
data$date <- as.Date(data$date)
data$month <- format(data$date, "%b") # get month
month_order<- c("Jan", "Fev", "Mar", "Avr", "Mai", "Jun", "Jul", "Aou", "Sep", "Oct", "Nov", "Dec")
monthly_avg <- aggregate(data$Num_Acc, by = list(data$month), FUN = length) # get avg
colnames(monthly_avg) <- c("Month", "AccidentCount")
monthly_avg <- monthly_avg[order(monthly_avg$Month, month_order), ] # sort by month and year
barplot(monthly_avg$AccidentCount, names.arg = monthly_avg$Month, main = "Moyenne mensuelle des accidents",  xlab = "Mois", ylab = "Nombre d'accidents", col = "blue", border = "red", breaks = 10)





