# Limpiar el entorno
rm(list = ls())
graphics.off()
gc()

# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Directorios
dir_fire <- '/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/data/'
dir_out <- '/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/docs/'

# Cargar el conjunto de datos filtrado (dataset viejo)
load(paste0(dir_fire, "murcia_incendios_filtered.RData"))

# Cargar el nuevo conjunto de datos desde 2005 (anual y estacional)
seasonal_new <- read.csv(paste0(dir_fire, "seasonal_burned_area_fires_shapefile.csv"))

# --------- COMPARACIÓN ESTACIONAL ---------

# Ajustar el año para DJF y calcular los totales estacionales en el dataset viejo
seasonal_totals_old <- murcia_incendios %>%
  mutate(
    season = case_when(
      month %in% c(12, 1, 2) ~ "DJF",  # Invierno (Dic-Ene-Feb)
      month %in% c(3, 4, 5) ~ "MAM",   # Primavera (Mar-Abr-May)
      month %in% c(6, 7, 8) ~ "JJA",   # Verano (Jun-Jul-Ago)
      month %in% c(9, 10, 11) ~ "SON"  # Otoño (Sep-Oct-Nov)
    ),
    adjusted_year = ifelse(month == 12, year + 1, year)  # Ajustar año para DJF
  ) %>%
  group_by(adjusted_year, season) %>%
  summarise(total_season_ba_old = sum(total_ba, na.rm = TRUE), .groups = "drop") %>%
  ungroup()

# Renombrar la columna 'year_adjusted' en 'seasonal_new' para que coincida con 'adjusted_year'
seasonal_new <- seasonal_new %>% rename(adjusted_year = year_adjusted)

# Hacer la unión de las dos tablas por 'adjusted_year' y 'season'
seasonal_comparison <- full_join(seasonal_new, seasonal_totals_old, by = c("adjusted_year", "season")) %>%
  rename(total_season_ba_new = BA, total_season_ba_old = total_season_ba_old)

# Filtrar los datos desde el año 2005
seasonal_comparison_filtered <- seasonal_comparison %>%
  filter(adjusted_year >= 2005)

# Crear una lista de estaciones
seasons <- c("DJF", "MAM", "JJA", "SON")

# Función para calcular la diferencia porcentual y la correlación para cada temporada
seasonal_plots <- lapply(seasons, function(current_season) {
  
  # Filtrar los datos por la temporada actual (usando current_season)
  season_data <- seasonal_comparison_filtered %>%
    filter(season == current_season)  # Filtrar explícitamente por la temporada actual
  
  # Imprimir un resumen para verificar si los datos están siendo filtrados correctamente
  cat("Filtrando datos para la temporada:", current_season, "\n")
  print(summary(season_data))
  
  # Verificar si la temporada tiene datos
  if(nrow(season_data) == 0) {
    cat("No hay datos para la temporada:", current_season, "\n")
    return(NULL)
  }
  
  # Calcular la correlación entre las dos series
  if (nrow(season_data) > 1) {
    correlation <- cor(season_data$total_season_ba_new, season_data$total_season_ba_old, use = "complete.obs")
  } else {
    correlation <- NA  # No calcular correlación si no hay suficientes datos
  }
  
  # Crear el gráfico con escala log10 en el eje Y
  p <- ggplot(season_data, aes(x = adjusted_year)) +
    geom_line(aes(y = total_season_ba_new, color = "IIFF - Murcia"), size = 1, na.rm = TRUE) +
    geom_line(aes(y = total_season_ba_old, color = "EGIF -ES"), size = 1, linetype = "dashed", na.rm = TRUE) +
    scale_y_log10() +  # Agregar escala logarítmica al eje Y
    labs(title = paste0("Comparación del Área Quemada - ", current_season),
         y = "Área Quemada (log10 ha)", x = "Año") +
    scale_color_manual(values = c("IIFF - Murcia" = "blue", "EGIF -ES" = "red")) +
    annotate("text", x = max(season_data$adjusted_year, na.rm = TRUE) - 2, 
             y = max(season_data$total_season_ba_new, na.rm = TRUE), 
             label = paste("Corr:", round(correlation, 2)), hjust = 1)
  
  return(p)
})

# Guardar gráficos (opcional)
for (i in 1:length(seasonal_plots)) {
  if (!is.null(seasonal_plots[[i]])) {
    ggsave(filename = paste0(dir_out, "seasonal_comparison_", seasons[i], ".png"),
           plot = seasonal_plots[[i]], width = 8, height = 6)
  }
}



# -------- COMBINAR LAS SERIES ESTACIONALES --------

# Función para combinar datos antiguos y nuevos para cada estación
combine_seasonal_data <- function(seasonal_totals_old, seasonal_new, season_name, year_old_limit, year_new_start) {
  seasonal_combined <- seasonal_totals_old %>%
    filter(season == season_name, adjusted_year <= year_old_limit) %>%
    bind_rows(
      seasonal_new %>%
        filter(season == season_name, adjusted_year >= year_new_start)
    ) %>%
    mutate(total_season_ba = coalesce(BA, total_season_ba_old)) %>%
    select(adjusted_year, season, total_season_ba, NF)
  
  return(seasonal_combined)
}

# Combinar para cada estación
djf_combined <- combine_seasonal_data(seasonal_totals_old, seasonal_new, "DJF", 2009, 2010)
mam_combined <- combine_seasonal_data(seasonal_totals_old, seasonal_new, "MAM", 2005, 2006)
jja_combined <- combine_seasonal_data(seasonal_totals_old, seasonal_new, "JJA", 2005, 2006)
son_combined <- combine_seasonal_data(seasonal_totals_old, seasonal_new, "SON", 2005, 2006)

# Guardar los resultados combinados
combined_seasons <- bind_rows(djf_combined, mam_combined, jja_combined, son_combined)

# Guardar los datos combinados como un archivo CSV
write.csv(combined_seasons, paste0(dir_fire, "combined_burned_area_series_seasonal.csv"), row.names = FALSE)
