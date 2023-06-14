library(tidyverse)

DataRegion <- read.csv("data/correspondance-code-insee-code-postal.csv", sep = ";")
data <- read.csv("data/stat_acc_V3.csv", sep = ";")

# merge insee code from both db
merged_data <- left_join(data, DataRegion, by = c("id_code_insee" = "CodeINSEE"))

# Nombre d'accidents par région et gravité
accidents_region <- merged_data %>% # merge data
group_by(Région, descr_grav) %>% # group by region and gravity
summarise(nombre_accidents = n()) %>% # count number of accidents
ungroup()   # remove grouping group_by

accidents_region <- left_join(accidents_region, population_par_region, by = "Région")

# Calcul de la gravité par habitant
accidents_region <- accidents_region %>%
mutate(gravite_pop = (nombre_accidents / population) * 100000)

print(accidents_region)



