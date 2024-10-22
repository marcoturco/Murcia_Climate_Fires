# Clear the environment
rm(list = ls())
graphics.off()
gc()

# Load necessary libraries
library(Hmisc)
library(dplyr)

# File path
file_path <- "/Users/marco/Downloads/Historico_Actualizado.mdb"
dir_fire <- '/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/data/'


# Read the .mdb file
data <- mdb.get(file_path)

# Filter data for Murcia (idcomunidad = 8)
murcia_fire_data <- data$pif_localizacion[data$pif_localizacion$idcomunidad == 8, ]

# Extract burned area (arbolada and no-arbolada)
superficie_quemada <- data$pif_perdidas

# Extract dates (detección)
fechas_incendio <- data$pif_tiempos[, c("numeroparte", "deteccion")]

# Extract cause of fire
causa_incendio <- data$pif_causa[, c("numeroparte", "idcausa")]

# Usar distinct() para obtener filas únicas basadas solo en 'IdCausa'
CodCausa_unicos <- data$CodCausa %>%
  distinct(IdCausa, .keep_all = TRUE)

# Ver las primeras filas del resultado
head(CodCausa_unicos)
# Guardar el dataframe como un archivo CSV
write.csv(CodCausa_unicos, paste0(dir_fire,"CodCausa_unicos.csv"), row.names = FALSE)

# Filter murcia_fire_data columns
murcia_fire_data <- murcia_fire_data[, c("numeroparte", "idcomunidad")]

# Merge the data
murcia_incendios <- merge(murcia_fire_data, superficie_quemada, by = "numeroparte")
murcia_incendios <- merge(murcia_incendios, fechas_incendio, by = "numeroparte")
murcia_incendios <- merge(murcia_incendios, causa_incendio, by = "numeroparte")

# Ensure deteccion is in Date format
murcia_incendios$deteccion <- as.Date(murcia_incendios$deteccion, format = "%Y-%m-%d")

# Correct year "2068" to "1968"
murcia_incendios$year <- format(murcia_incendios$deteccion, "%Y")
murcia_incendios$year <- as.numeric(ifelse(murcia_incendios$year == 2068, 1968, murcia_incendios$year))

# Filter data from 1980 onwards
murcia_incendios <- murcia_incendios %>%
  filter(year >= 1980)

# Extract day, month, and year from deteccion
murcia_incendios$month <- as.numeric(format(murcia_incendios$deteccion, "%m"))
murcia_incendios$day <- as.numeric(format(murcia_incendios$deteccion, "%d"))

# Create a new column that combines the month and day information
murcia_incendios$month_day <- murcia_incendios$month + (murcia_incendios$day / 31)

# Calculate total burned area (sum of all relevant columns)
murcia_incendios$total_ba <- rowSums(murcia_incendios[, c("superficiearboladatotal", 
                                                          "superficienoarboladatotal", 
                                                          "superficienoarboladaagricola", 
                                                          "superficienoarboladaotras")], na.rm = TRUE)

# Arrange the data in chronological order based on detection date
murcia_incendios <- murcia_incendios[order(murcia_incendios$deteccion), ]

# Filter fires larger than 1 hectare
murcia_incendios <- murcia_incendios %>%
  filter(total_ba >= 1)

# Save the dataset for future use
save(murcia_incendios, file = paste0(dir_fire,"murcia_incendios_filtered.RData"))
