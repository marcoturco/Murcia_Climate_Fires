# Limpiar el entorno
rm(list = ls())
gc()

# Cargar las librerías necesarias
library(sf)
library(ggplot2)
library(scales)
library(dplyr)
library(lubridate)  # Necesario para usar year() y month()

# Definir la ruta al archivo shapefile
shapefile_path <- "/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/data/IIFF_Murcia_perimetros_90-23/IIFF_Murcia_perimetros_90-23.shp"
dir_out <- "/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/data/"
# Leer el shapefile
shapefile_data <- st_read(shapefile_path)

# Mostrar las primeras filas del shapefile para entender su estructura
head(shapefile_data)

# Asegurarnos de que la columna 'DIA' esté en formato Date (si no lo está ya)
shapefile_data$DIA <- as.Date(shapefile_data$DIA, format = "%Y-%m-%d")

# Verificar si 'DIA' tiene algún valor NA
summary(shapefile_data$DIA)

# Calcular el área de cada polígono en metros cuadrados utilizando la geometría del shapefile
shapefile_data$area_calculated_m2 <- st_area(shapefile_data)  # Calcular el área en metros cuadrados

# Convertir las áreas a numeric para facilitar el uso
shapefile_data$SUP_M2 <- as.numeric(shapefile_data$SUP_M2)  # Área original en metros cuadrados
shapefile_data$area_calculated_m2 <- as.numeric(shapefile_data$area_calculated_m2)  # Área calculada en metros cuadrados

# Convertir ambas áreas a hectáreas
shapefile_data$area_ha_original <- shapefile_data$SUP_M2 / 10000  # Convertir área original a hectáreas
shapefile_data$area_ha_calculated <- shapefile_data$area_calculated_m2 / 10000  # Convertir área calculada a hectáreas

# Verificar los nombres de las columnas antes de eliminar la geometría
colnames(shapefile_data)

# Eliminar la columna de geometría para evitar problemas con 'filter'
shapefile_data_no_geom <- st_drop_geometry(shapefile_data)

# Verificar los nombres de las columnas después de eliminar la geometría
colnames(shapefile_data_no_geom)

# Ahora puedes filtrar los incendios mayores a 1 ha desde 2005 en adelante
filtered_data <- shapefile_data_no_geom %>%
  filter(DIA >= as.Date("2005-01-01") & area_ha_calculated > 1)

# Revisar los datos filtrados
head(filtered_data)




# Extraer año y mes
filtered_data <- filtered_data %>%
  mutate(
    year = year(DIA),
    month = month(DIA),
    season = case_when(
      month %in% c(12, 1, 2) ~ "DJF",
      month %in% c(3, 4, 5) ~ "MAM",
      month %in% c(6, 7, 8) ~ "JJA",
      month %in% c(9, 10, 11) ~ "SON"
    )
  )

# Ajustar diciembre (D) para ser parte del DJF de la siguiente temporada (usar year - 1 para diciembre)
filtered_data <- filtered_data %>%
  mutate(year_adjusted = ifelse(month == 12, year + 1, year))

# Agrupar por año y estación para calcular el área quemada total (BA) y el número de incendios (NF)
seasonal_summary <- filtered_data %>%
  group_by(year_adjusted, season) %>%
  summarise(
    BA = sum(area_ha_calculated),
    NF = n()
  ) %>%
  ungroup()

# Agregar el resumen anual
annual_summary <- filtered_data %>%
  group_by(year_adjusted) %>%
  summarise(
    BA = sum(area_ha_calculated),
    NF = n()
  )



# Guardar los resultados en un archivo CSV
write.csv(seasonal_summary, paste0(dir_out,"seasonal_burned_area_fires_shapefile.csv"), row.names = FALSE)
write.csv(annual_summary, paste0(dir_out,"annual_burned_area_fires_shapefile.csv"), row.names = FALSE)

# Mostrar un mensaje de éxito
print("Los datos estacionales y anuales han sido guardados correctamente.")
