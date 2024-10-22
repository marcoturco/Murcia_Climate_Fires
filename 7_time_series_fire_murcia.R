# Limpiar el entorno
rm(list = ls())
graphics.off()
gc()

# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)
library(tidyr)

# Directorios
dir_fire <- '/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/data/'
dir_out <- '/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/docs/'

# Función para asignar la estación
get_season <- function(month) {
  if (month %in% c(12, 1, 2)) return("DJF")
  else if (month %in% 3:5) return("MAM")
  else if (month %in% 6:8) return("JJA")
  else return("SON")
}


# Define the file path
file_path <- "/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/data/fires_filtered_1980_2023.csv"
# Read the CSV file into a data frame
filtered_fires <- read.csv(file_path)
# View the first few rows to verify the data
head(filtered_fires)
plot(filtered_fires$Total_BA, log = 'y')

# Filter the dataset to include only fires from 1991 onwards
filtered_fires <- filtered_fires %>%
  filter(Incendio_Detectado >= as.Date("1980-01-01"))

filtered_fires$Incendio_Detectado <- as.Date(filtered_fires$Incendio_Detectado, format = "%Y-%m-%d")
filtered_fires$year <- as.numeric(format(filtered_fires$Incendio_Detectado, "%Y"))
filtered_fires$month <- as.numeric(format(filtered_fires$Incendio_Detectado, "%m"))

# Calcular el número de incendios (NF) y el área quemada total en el dataset viejo (annual_old)
annual_ba <- filtered_fires %>%
  group_by(year) %>%
  summarise(
    total_annual_ba = sum(Total_BA, na.rm = TRUE),  # Sumar el área quemada total
    NF = n()  # Contar el número de incendios (NF)
  )

# Agregar columna de estación
filtered_fires$season <- sapply(filtered_fires$month, get_season)

# Ajustar el año para DJF, donde diciembre debe contar como parte del año siguiente
filtered_fires <- filtered_fires %>%
  mutate(
    adjusted_year = ifelse(season == "DJF" & month == 12, year + 1, year)
  )


# Calcular el área quemada total y el número de incendios por estación y año
seasonal_ba <- filtered_fires %>%
  group_by(adjusted_year, season) %>%
  summarise(
    total_seasonal_ba = sum(Total_BA, na.rm = TRUE),  # Sumar el área quemada total por estación
    NF = n()  # Contar el número de incendios
  )

# Visualizar las primeras filas de la serie estacional
head(seasonal_ba)


# ----------- ANÁLISIS ANUAL -----------

# Ajustar para asegurarse de que no haya valores negativos o cero para log10

# Realizar la regresión lineal
annual_lm <- lm(log10(total_annual_ba) ~ year, data = annual_ba)
annual_summary <- summary(annual_lm)

# Extraer pendiente, R² y p-valor
slope_annual <- coef(annual_lm)[2]
r_squared_annual <- annual_summary$r.squared
p_value_annual <- annual_summary$coefficients[2, 4]

# Graficar la serie anual combinada con información de la regresión
p_annual <- ggplot(annual_ba, aes(x = year, y = total_annual_ba)) +
  geom_bar(stat = "identity", fill = "black", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_y_log10() +  # Escala logarítmica para mejor visualización
  labs(title = "Área Quemada Anual (Murcia)",
       x = "Año", y = "Área Quemada Total (ha)") +
  annotate("text", x = 2000, y = 0.8*1e+4, 
           label = paste0("Pendiente [ha/10·años]: ", round(slope_annual*10, 2), 
                          "\nR²: ", round(r_squared_annual, 2), 
                          "\np-valor: ", signif(p_value_annual, 2)),
           color = "blue", size = 5, hjust = 0) +  # Ajustar el tamaño del texto
  theme_minimal(base_size = 15)  # Establecer tamaño de texto base en 15

p_annual
# Guardar el gráfico anual
ggsave(paste0(dir_out, "combined_annual_burned_area_murcia.pdf"), plot = p_annual, width = 29.7, height = 29.7 / 1.618, units = "cm")


# ----------- ANÁLISIS ESTACIONAL -----------

# Crear una lista de estaciones
seasons <- c("DJF", "MAM", "JJA", "SON")

# Generar gráficos para cada estación
for (season_name in seasons) {
  print(season_name)
  
  # Filtrar los datos por la estación actual y asegurarse de que no haya valores <= 0
  seasonal_data <- seasonal_ba %>%
    filter(season == !!season_name & total_seasonal_ba > 0)  # Filtrar correctamente por temporada
  
  # Verificar si hay datos para la temporada actual
  if (nrow(seasonal_data) == 0) {
    print(season_name)
    next  # Si no hay datos, pasar a la siguiente temporada
  }
  
  # Realizar la regresión lineal
  lm_season <- lm(log10(total_seasonal_ba) ~ adjusted_year, data = seasonal_data)
  summary_season <- summary(lm_season)
  
  # Extraer pendiente, R² y p-valor
  slope_season <- coef(lm_season)[2]
  r_squared_season <- summary_season$r.squared
  p_value_season <- summary_season$coefficients[2, 4]
  
  # Graficar la serie estacional con información de la regresión
  p_season <- ggplot(seasonal_data, aes(x = adjusted_year, y = total_seasonal_ba)) +
    geom_bar(stat = "identity", fill = "black", alpha = 0.8) +  # Color de las barras negro
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    scale_y_log10() +  # Escala logarítmica para mejor visualización
    labs(title = paste("Área Quemada Estacional (", season_name, ")", sep = ""),
         x = "Año", y = "Área Quemada Total (ha)") +
    annotate("text", x = 2000, y = 0.8*max(seasonal_data$total_seasonal_ba, na.rm = TRUE),  # Cambiar de BA a total_seasonal_ba
             label = paste0("Pendiente [ha/10·años]: ", round(slope_season, 3),
                            "\nR²: ", round(r_squared_season, 2), 
                            "\np-valor: ", signif(p_value_season, 2)),
             color = "blue", size = 5, hjust = 0) +  # Ajustar el tamaño del texto
    theme_minimal(base_size = 15)  # Establecer tamaño de texto base en 15
  
  print(p_season)
  # Guardar los gráficos estacionales
  ggsave(paste0(dir_out, "combined_burned_area_", season_name, "_murcia.pdf"), plot = p_season, width = 29.7, height = 29.7 / 1.618, units = "cm")
  
}



# Calcular los promedios anuales de área quemada y número de incendios
annual_averages <- annual_ba %>%
  summarise(
    avg_annual_ba = mean(total_annual_ba, na.rm = TRUE),  # Promedio de área quemada anual
    avg_annual_nf = mean(NF, na.rm = TRUE)  # Promedio de número de incendios anuales
  )

# Mostrar los promedios anuales
print(annual_averages)



# Calcular los promedios estacionales de área quemada y número de incendios
seasonal_averages <- seasonal_ba %>%
  group_by(season) %>%
  summarise(
    avg_seasonal_ba = mean(total_seasonal_ba, na.rm = TRUE),  # Promedio de área quemada estacional
    avg_seasonal_nf = mean(NF, na.rm = TRUE)  # Promedio de número de incendios estacional
  )

# Mostrar los promedios estacionales
print(seasonal_averages)

