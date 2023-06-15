DataRegion <- read.csv("data/correspondance-code-insee-code-postal.csv", sep = ";")
data <- read.csv("data/stat_acc_V3_cleared.csv", sep = ";")

# merge both Insee code
accidents_population <- merge(data, DataRegion, by.x = "id_code_insee", by.y = "CodeINSEE")
# nb accident for region and severity
accidents_par_region <- aggregate(accidents_population$Num_Acc, by = list(accidents_population$region, accidents_population$descr_grav), FUN = length)
names(accidents_par_region) <- c("region", "descr_grav", "Nombre_Accidents")
# get total population
population_par_region <- aggregate(accidents_population$Population, by = list(accidents_population$region), FUN = sum)
names(population_par_region) <- c("region", "Population")
print(population_par_region)
str(population_par_region)
# merge nb accident & pop
accidents_par_100k <- merge(accidents_par_region, population_par_region, by = "region")
accidents_par_100k$Gravite_par_100k <- accidents_par_100k$Nombre_Accidents / (accidents_par_100k$Population / 100000)
#only needed columns
accidents_par_100k <- accidents_par_100k[, c("Nombre_Accidents", "Gravite_par_100k", "region")]
print(accidents_par_100k)


