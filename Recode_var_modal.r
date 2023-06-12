#lecture csv file "stat_acc_V3.csv"
data <- read.csv("data/stat_acc_V3.csv", sep=";")

#print les diffrentes valeurs de chaque colonne multimodale
# print(unique(data$descr_cat_veh))
print(unique(data$descr_grav))

#creation d'un vecteur de valeurs a remplacer a partir des valeurs de unique(data$colonne)
old_cat_veh_values <- unique(data$descr_cat_veh)
old_descr_grav_values <- unique(data$descr_grav)

#creation d'un vecteur de valeurs de remplacement
new_cat_veh_values <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
new_descr_grav_values <- c(1,2,3,4)

#boucle for pour remplacer les valeurs de old_values par les valeurs de new_values
for (i in 1:length(old_cat_veh_values)) {
  data$descr_cat_veh <- replace(data$descr_cat_veh, data$descr_cat_veh == old_cat_veh_values[i], new_cat_veh_values[i])
  data$descr_grav <- replace(data$descr_grav, data$descr_grav == old_descr_grav_values[i], new_descr_grav_values[i])
}

#export csv
write.csv(data, file = "data/stat_acc_V3_modifModal.csv", sep = ";", row.names = FALSE)