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
annual_new <- read.csv(paste0(dir_fire, "annual_burned_area_fires_shapefile.csv"))
seasonal_new <- read.csv(paste0(dir_fire, "seasonal_burned_area_fires_shapefile.csv"))

# --------- COMPARACIÓN ANUAL ---------

# Calcular el número de incendios (NF) y el área quemada total en el dataset viejo (annual_old)
annual_old <- murcia_incendios %>%
  group_by(year) %>%
  summarise(
    total_annual_ba = sum(total_ba, na.rm = TRUE),  # Sumar el área quemada total
    NF = n()  # Contar el número de incendios (NF)
  )

# Renombrar la columna 'year_adjusted' a 'year' en el nuevo dataset
annual_new <- annual_new %>% rename(year = year_adjusted)

# Ahora podemos hacer la unión por 'year'
annual_comparison <- full_join(annual_new, annual_old, by = "year") %>%
  rename(total_annual_ba_new = BA, total_annual_ba_old = total_annual_ba) %>%
  mutate(
    log_area_new = log10(total_annual_ba_new), 
    log_area_old = log10(total_annual_ba_old)
  )

# Ver los primeros resultados para asegurarse de que la unión fue correcta
head(annual_comparison)


# Filtrar los datos desde el año 2005
annual_comparison_filtered <- annual_comparison %>%
  filter(year >= 2005)

# Graficar la comparación anual solo desde el 2005 con escala logarítmica en el eje Y
ggplot(annual_comparison_filtered, aes(x = year)) +
  geom_line(aes(y = total_annual_ba_new, color = "IIFF - Murcia"), size = 1) +
  geom_line(aes(y = total_annual_ba_old, color = "EGIF -ES"), size = 1, linetype = "dashed") +
  scale_y_log10() +  # Aplicar la escala logarítmica en el eje Y
  labs(title = "Comparación del Área Quemada Anual",
       y = "Área Quemada (ha)", x = "Año") +
  scale_color_manual(values = c("IIFF - Murcia" = "blue", "EGIF -ES" = "red")) +
  theme_minimal()

# Guardar gráfico anual
ggsave(paste0(dir_out, "comparison_annual_burned_area.pdf"), width = 29.7, height = 29.7 / 1.618, units = "cm")







# Eliminar filas donde 'total_annual_ba_old' es NA, ya que no podemos calcular la diferencia en esos casos
annual_comparison_filtered <- annual_comparison_filtered %>%
  filter(!is.na(total_annual_ba_old))

# Calcular la suma total del área quemada desde 2005 en ambos datasets (solo donde no hay NA)
total_ba_new <- sum(annual_comparison_filtered$total_annual_ba_new, na.rm = TRUE)
total_ba_old <- sum(annual_comparison_filtered$total_annual_ba_old, na.rm = TRUE)

# Calcular la diferencia porcentual total entre los dos conjuntos de datos
difference_percent_total <- ((total_ba_new - total_ba_old) / total_ba_new) * 100

# Calcular la correlación entre las dos series de áreas quemadas desde 2005 (solo donde no hay NA)
correlation <- cor(annual_comparison_filtered$total_annual_ba_new, 
                   annual_comparison_filtered$total_annual_ba_old, 
                   use = "complete.obs", method = "pearson")

# Mostrar los resultados
cat("Diferencia porcentual total:", round(difference_percent_total, 2), "%\n")
cat("Correlación entre las series:", round(correlation, 3), "\n")





# Combinar los datasets: usar el dataset viejo hasta 2005 y el nuevo desde 2005 en adelante
ba_combined <- annual_old %>%
  filter(year < 2005) %>%
  bind_rows(annual_new %>% filter(year >= 2005)) %>%
  # Combinar las columnas de 'BA' y 'total_annual_ba' en una sola
  mutate(total_annual_ba = coalesce(BA, total_annual_ba)) %>%
  select(year, total_annual_ba, NF)  # Mantener solo las columnas necesarias


# Guardar los datos combinados como un archivo CSV
write.csv(ba_combined, paste0(dir_fire, "combined_burned_area_series_annual.csv"), row.names = FALSE)
cc

##############. NF


annual_comparison_nf <- full_join(annual_new, annual_old, by = "year") %>%
  rename(
    NF_new = NF.x,  # Número de incendios del nuevo dataset
    NF_old = NF.y   # Número de incendios del viejo dataset
  )

# Filtrar los datos desde el año 2005
annual_comparison_nf_filtered <- annual_comparison_nf %>%
  filter(year >= 2005)

# Graficar la comparación del número de incendios solo desde el 2005
ggplot(annual_comparison_nf_filtered, aes(x = year)) +
  geom_line(aes(y = NF_new, color = "IIFF - Murcia"), size = 1) +
  geom_line(aes(y = NF_old, color = "EGIF -ES"), size = 1, linetype = "dashed") +
  labs(title = "Comparación del Número de Incendios Anual",
       y = "Número de Incendios (NF)", x = "Año") +
  scale_color_manual(values = c("IIFF - Murcia" = "blue", "EGIF -ES" = "red")) +
  theme_minimal()

# Guardar gráfico de NF anual
ggsave(paste0(dir_out, "comparison_annual_nf.pdf"), width = 29.7, height = 29.7 / 1.618, units = "cm")

# Eliminar filas donde 'NF_old' es NA
annual_comparison_nf_filtered <- annual_comparison_nf_filtered %>%
  filter(!is.na(NF_old))

# Calcular la suma total de NF desde 2005 en ambos datasets (solo donde no hay NA)
total_nf_new <- sum(annual_comparison_nf_filtered$NF_new, na.rm = TRUE)
total_nf_old <- sum(annual_comparison_nf_filtered$NF_old, na.rm = TRUE)

# Calcular la diferencia porcentual total de NF entre los dos conjuntos de datos
difference_percent_total_nf <- ((total_nf_new - total_nf_old) / total_nf_new) * 100

# Calcular la correlación entre las dos series de NF desde 2005 (solo donde no hay NA)
correlation_nf <- cor(annual_comparison_nf_filtered$NF_new, 
                      annual_comparison_nf_filtered$NF_old, 
                      use = "complete.obs", method = "pearson")


# Mostrar los resultados para NF
cat("Diferencia porcentual total de NF:", round(difference_percent_total_nf, 2), "%\n")
cat("Correlación entre las series de NF:", round(correlation_nf, 3), "\n")



