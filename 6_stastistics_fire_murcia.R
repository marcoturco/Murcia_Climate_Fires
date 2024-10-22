# Clear the environment
rm(list = ls())
graphics.off()
gc()

# Load necessary libraries
library(Hmisc)
library(dplyr)
library(ggplot2)

# Define directories
dir_fire <- '/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/data/'
dir_out <- '/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/docs/'

# Load the filtered data
# Define the file path
file_path <- "/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/data/fires_filtered_1980_2023.csv"
# Read the CSV file into a data frame
filtered_fires <- read.csv(file_path)
# View the first few rows to verify the data
head(filtered_fires)
plot(filtered_fires$Total_BA, log = 'y')

# Filter the dataset to include only fires from 1980 onwards
filtered_fires_1991 <- filtered_fires %>%
  filter(Incendio_Detectado >= as.Date("1980-01-01"))

# View the first few rows to verify the filtering
head(filtered_fires_1991)


# Total de registros
total_registros <- nrow(filtered_fires_1991)

# Cálculo de la superficie total quemada (total_ba)
total_ba <- sum(filtered_fires_1991$Total_BA, na.rm = TRUE)

# Superficie total de la región de Murcia (en hectáreas, 1 km² = 100 ha)
superficie_murcia <- 11313 * 100  # Murcia tiene aproximadamente 11,313 km²

# Proporción de superficie quemada respecto a la superficie total de Murcia
proporcion_ba_vs_murcia <- (total_ba / superficie_murcia) * 100

# Cálculo de la superficie arbolada total quemada
superficie_arbolada_total <- sum(filtered_fires_1991$ha_TOTAL_Sup_Arbolada, na.rm = TRUE)

# Cálculo de la superficie no arbolada agrícola total quemada
superficie_no_arbolada_agricola_total <- sum(filtered_fires_1991$ha_TOTAL_Sup_Agricola, na.rm = TRUE)

# Cálculo de la superficie no arbolada total quemada
superficie_no_arbolada_total <- sum(filtered_fires_1991$ha_TOTAL_Sup_NoArbolada, na.rm = TRUE)

# Cálculo de otras superficies no arboladas quemadas
superficie_no_arbolada_otras_total <- sum(filtered_fires_1991$ha_TOTAL_Otra_Superficies, na.rm = TRUE)

# Proporción de superficie arbolada respecto al total quemado
proporcion_superficie_arbolada <- (superficie_arbolada_total / total_ba) * 100

# Proporción de superficie no arbolada agrícola respecto al total quemado
proporcion_superficie_no_arbolada_agricola <- (superficie_no_arbolada_agricola_total / total_ba) * 100

# Proporción de superficie no arbolada total respecto al total quemado
proporcion_superficie_no_arbolada_total <- (superficie_no_arbolada_total / total_ba) * 100

# Proporción de otras superficies no arboladas respecto al total quemado
proporcion_superficie_no_arbolada_otras <- (superficie_no_arbolada_otras_total / total_ba) * 100

# Clasificación por tamaño de incendio
incendios_mayores_10ha <- nrow(filtered_fires_1991[filtered_fires_1991$Total_BA > 10, ])
incendios_mayores_50ha <- nrow(filtered_fires_1991[filtered_fires_1991$Total_BA> 50, ])
incendios_mayores_100ha <- nrow(filtered_fires_1991[filtered_fires_1991$Total_BA > 100, ])
incendios_mayores_500ha <- nrow(filtered_fires_1991[filtered_fires_1991$Total_BA > 500, ])


# Mostrar los resultados
cat("Total de registros: ", total_registros, "\n")
cat("Superficie total quemada (ha): ", total_ba, "\n")
cat("Superficie de Murcia (ha): ", superficie_murcia, "\n")
cat("Proporción de superficie quemada respecto a Murcia (%): ", round(proporcion_ba_vs_murcia, 2), "\n")
cat("Superficie arbolada total quemada (ha): ", superficie_arbolada_total, "\n")
cat("Proporción de superficie arbolada respecto al total quemado (%): ", round(proporcion_superficie_arbolada, 2), "\n")
cat("Superficie no arbolada agrícola total quemada (ha): ", superficie_no_arbolada_agricola_total, "\n")
cat("Proporción de superficie no arbolada agrícola respecto al total quemado (%): ", round(proporcion_superficie_no_arbolada_agricola, 2), "\n")
cat("Superficie no arbolada total quemada (ha): ", superficie_no_arbolada_total, "\n")
cat("Proporción de superficie no arbolada total respecto al total quemado (%): ", round(proporcion_superficie_no_arbolada_total, 2), "\n")
cat("Otras superficies no arboladas quemadas (ha): ", superficie_no_arbolada_otras_total, "\n")
cat("Proporción de otras superficies no arboladas respecto al total quemado (%): ", round(proporcion_superficie_no_arbolada_otras, 2), "\n")

# Mostrar el número de incendios por clases de tamaño
cat("\nNúmero de incendios mayores a 10 ha: ", incendios_mayores_10ha, "\n")
cat("Número de incendios mayores a 50 ha: ", incendios_mayores_50ha, "\n")
cat("Número de incendios mayores a 100 ha: ", incendios_mayores_100ha, "\n")
cat("Número de incendios mayores a 500 ha: ", incendios_mayores_500ha, "\n")




