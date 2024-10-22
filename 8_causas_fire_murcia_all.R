# Clear the environment
rm(list = ls())
graphics.off()
gc()

# Load necessary libraries
library(Hmisc)
library(dplyr)
library(ggplot2)

# Directories for data
dir_fire <- '/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/data/'
dir_out <- '/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/docs/'

# Load the filtered data
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


# Crear una nueva columna para las clases de causas
filtered_fires$Causa_Clase <- case_when(
  filtered_fires$SubgrupoCausa == "Causa desconocida" | is.na(filtered_fires$SubgrupoCausa) ~ "Desconocida",
  filtered_fires$SubgrupoCausa == "Naturales" ~ "Natural",
  filtered_fires$SubgrupoCausa == "Incendio intencionado" ~ "Humana Intencionado",
  TRUE ~ "Humana No Intencionado"
)

# Calcular los porcentajes de cada clase
percentages <- filtered_fires %>%
  group_by(Causa_Clase) %>%
  summarise(
    Count = n(),
    Percentage = (n() / nrow(filtered_fires)) * 100
  )

# Mostrar los resultados
print(percentages)



# Calcular los porcentajes de cada causa en 'SubgrupoCausa'
percentages_by_cause <- filtered_fires %>%
  group_by(SubgrupoCausa) %>%
  summarise(
    Count = n(),
    Percentage = (n() / nrow(filtered_fires)) * 100
  )

# Mostrar los resultados
print(percentages_by_cause)


# Crear etiquetas para el gráfico de tarta
percentages$label <- paste0(round(percentages$Percentage, 1), "%")

# Crear el gráfico de tarta
grafico_tarta <- ggplot(percentages, aes(x = "", y = Percentage, fill = Causa_Clase)) +
  geom_bar(width = 1, stat = "identity", color = "black") +  # Barras con bordes negros para claridad
  coord_polar("y", start = 0) +  # Convertir a gráfico de tarta
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white", size = 5) +  # Añadir los porcentajes
  scale_fill_manual(values = c("Natural" = "lightblue", 
                               "Desconocida" = "gray",  
                               "Humana No Intencionado" = "lightcoral", 
                               "Humana Intencionado" = "darkred")) +  
  labs(
    title = "Distribución de Causas de Incendios",
    x = "",
    y = ""
  ) +
  theme_void() +  # Eliminar ejes para un gráfico limpio
  theme(
    axis.text = element_blank(),       # Eliminar texto de los ejes
    axis.ticks = element_blank(),      # Eliminar ticks de los ejes
    panel.grid = element_blank(),      # Eliminar líneas de la cuadrícula
    legend.title = element_blank(),    # Eliminar título de la leyenda
    legend.text = element_text(size = 15),  # Ajustar tamaño del texto de la leyenda
    legend.position = "right",         # Posicionar la leyenda a la derecha
    plot.title = element_text(hjust = 0.5, size = 15),  # Centrar y ajustar el tamaño del título
    text = element_text(size = 15)     # Ajustar tamaño del texto (porcentajes)
  )

# Mostrar el gráfico de tarta
grafico_tarta

# Definir proporciones armónicas (ancho mayor que alto, basado en relación áurea)
width_cm <- 29.7/2 # Ancho en cm (el largo de una hoja A4)
height_cm <- width_cm / 1.618  # Alto según la proporción áurea
# Guardar el gráfico como EPS
ggsave(paste0(dir_out,"causas_all.pdf"), plot = grafico_tarta, device = "pdf", width = width_cm, height = height_cm, units = "cm")



# Filtrar solo los incendios mayores a 50 hectáreas y desde 1991
filtered_fires_50ha <- filtered_fires %>%
  filter(Total_BA > 50) %>%
  filter(Incendio_Detectado >= as.Date("1980-01-01"))

# Crear una nueva columna para las clases de causas
filtered_fires_50ha$Causa_Clase <- case_when(
  filtered_fires_50ha$SubgrupoCausa == "Causa desconocida" | is.na(filtered_fires_50ha$SubgrupoCausa) ~ "Desconocida",
  filtered_fires_50ha$SubgrupoCausa == "Naturales" ~ "Natural",
  filtered_fires_50ha$SubgrupoCausa == "Incendio intencionado" ~ "Humana Intencionado",
  TRUE ~ "Humana No Intencionado"
)

# Calcular los porcentajes de cada clase
percentages_50ha <- filtered_fires_50ha %>%
  group_by(Causa_Clase) %>%
  summarise(
    Count = n(),
    Percentage = (n() / nrow(filtered_fires_50ha)) * 100
  )

# Mostrar los resultados
print(percentages_50ha)

# Crear etiquetas para el gráfico de tarta
percentages_50ha$label <- paste0(round(percentages_50ha$Percentage, 1), "%")

# Crear el gráfico de tarta
grafico_tarta_50ha <- ggplot(percentages_50ha, aes(x = "", y = Percentage, fill = Causa_Clase)) +
  geom_bar(width = 1, stat = "identity", color = "black") +  # Barras con bordes negros para claridad
  coord_polar("y", start = 0) +  # Convertir a gráfico de tarta
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white", size = 5) +  # Añadir los porcentajes
  scale_fill_manual(values = c("Natural" = "lightblue", 
                               "Desconocida" = "gray",  
                               "Humana No Intencionado" = "lightcoral", 
                               "Humana Intencionado" = "darkred")) +  
  labs(
    title = "Distribución de Causas de Incendios (> 50 ha)",
    x = "",
    y = ""
  ) +
  theme_void() +  # Eliminar ejes para un gráfico limpio
  theme(
    axis.text = element_blank(),       # Eliminar texto de los ejes
    axis.ticks = element_blank(),      # Eliminar ticks de los ejes
    panel.grid = element_blank(),      # Eliminar líneas de la cuadrícula
    legend.title = element_blank(),    # Eliminar título de la leyenda
    legend.text = element_text(size = 15),  # Ajustar tamaño del texto de la leyenda
    legend.position = "right",         # Posicionar la leyenda a la derecha
    plot.title = element_text(hjust = 0.5, size = 15),  # Centrar y ajustar el tamaño del título
    text = element_text(size = 15)     # Ajustar tamaño del texto (porcentajes)
  )

# Mostrar el gráfico de tarta
grafico_tarta_50ha

# Guardar el gráfico como PDF
ggsave(paste0(dir_out,"causas_incendios_mayores_50ha.pdf"), plot = grafico_tarta_50ha, device = "pdf", width = width_cm, height = height_cm, units = "cm")






# Definir los periodos
mid_year <- 2001  # Definimos 2007 como el último año de la primera mitad de la serie

# Filtrar los incendios desde 1991 y añadir una columna de periodo
filtered_fires_period <- filtered_fires %>%
  filter(Incendio_Detectado >= as.Date("1980-01-01")) %>%
  mutate(Periodo = ifelse(year <= mid_year, "1980-2001", "2002-2023"))

# Crear una nueva columna para las clases de causas (si no se ha hecho previamente)
filtered_fires_period$Causa_Clase <- case_when(
  filtered_fires_period$SubgrupoCausa == "Causa desconocida" | is.na(filtered_fires_period$SubgrupoCausa) ~ "Desconocida",
  filtered_fires_period$SubgrupoCausa == "Naturales" ~ "Natural",
  filtered_fires_period$SubgrupoCausa == "Incendio intencionado" ~ "Humana Intencionado",
  TRUE ~ "Humana No Intencionado"
)


# Calcular el total de incendios por periodo
total_fires_period <- filtered_fires_period %>%
  group_by(Periodo) %>%
  summarise(total_fires = n())

# Unir los totales con los datos agrupados por causa
percentages_period <- filtered_fires_period %>%
  group_by(Periodo, Causa_Clase) %>%
  summarise(Count = n()) %>%
  left_join(total_fires_period, by = "Periodo") %>%
  mutate(Percentage = (Count / total_fires) * 100)

# Mostrar los resultados corregidos
print(percentages_period)

# Crear etiquetas para el gráfico de tarta
percentages_period$label <- paste0(round(percentages_period$Percentage, 1), "%")


# Filtrar los datos para el periodo 1991-2007
percentages_1991_2007 <- percentages_period %>%
  filter(Periodo == "1980-2001")

# Filtrar los datos para el periodo 2008-2023
percentages_2008_2023 <- percentages_period %>%
  filter(Periodo == "2002-2023")

# Gráfico de tarta para el periodo 1991-2007
grafico_tarta_1991_2007 <- ggplot(percentages_1991_2007, aes(x = "", y = Percentage, fill = Causa_Clase)) +
  geom_bar(width = 1, stat = "identity", color = "black") +  # Barras con bordes negros
  coord_polar("y", start = 0) +  # Convertir a gráfico de tarta
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white", size = 5) +  # Añadir los porcentajes
  scale_fill_manual(values = c("Natural" = "lightblue", 
                               "Desconocida" = "gray",  
                               "Humana No Intencionado" = "lightcoral", 
                               "Humana Intencionado" = "darkred")) +  
  labs(
    title = "Distribución de Causas de Incendios (1980-2001)",
    x = "",
    y = ""
  ) +
  theme_void() +  # Eliminar ejes para un gráfico limpio
  theme(
    axis.text = element_blank(),       # Eliminar texto de los ejes
    axis.ticks = element_blank(),      # Eliminar ticks de los ejes
    panel.grid = element_blank(),      # Eliminar líneas de la cuadrícula
    legend.title = element_blank(),    # Eliminar título de la leyenda
    legend.text = element_text(size = 15),  # Ajustar tamaño del texto de la leyenda
    legend.position = "right",         # Posicionar la leyenda a la derecha
    plot.title = element_text(hjust = 0.5, size = 15),  # Centrar y ajustar el tamaño del título
    text = element_text(size = 15)     # Ajustar tamaño del texto (porcentajes)
  )

# Mostrar el gráfico de tarta 1991-2007
print(grafico_tarta_1991_2007)

# Guardar el gráfico como PDF para el periodo 1991-2007
ggsave(paste0(dir_out, "causas_incendios_1980_2001.pdf"), plot = grafico_tarta_1991_2007, device = "pdf", width = 29.7 / 2, height = (29.7 / 2) / 1.618, units = "cm")

# Gráfico de tarta para el periodo 2008-2023
grafico_tarta_2008_2023 <- ggplot(percentages_2008_2023, aes(x = "", y = Percentage, fill = Causa_Clase)) +
  geom_bar(width = 1, stat = "identity", color = "black") +  # Barras con bordes negros
  coord_polar("y", start = 0) +  # Convertir a gráfico de tarta
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white", size = 5) +  # Añadir los porcentajes
  scale_fill_manual(values = c("Natural" = "lightblue", 
                               "Desconocida" = "gray",  
                               "Humana No Intencionado" = "lightcoral", 
                               "Humana Intencionado" = "darkred")) +  
  labs(
    title = "Distribución de Causas de Incendios (2008-2023)",
    x = "",
    y = ""
  ) +
  theme_void() +  # Eliminar ejes para un gráfico limpio
  theme(
    axis.text = element_blank(),       # Eliminar texto de los ejes
    axis.ticks = element_blank(),      # Eliminar ticks de los ejes
    panel.grid = element_blank(),      # Eliminar líneas de la cuadrícula
    legend.title = element_blank(),    # Eliminar título de la leyenda
    legend.text = element_text(size = 15),  # Ajustar tamaño del texto de la leyenda
    legend.position = "right",         # Posicionar la leyenda a la derecha
    plot.title = element_text(hjust = 0.5, size = 15),  # Centrar y ajustar el tamaño del título
    text = element_text(size = 15)     # Ajustar tamaño del texto (porcentajes)
  )

# Mostrar el gráfico de tarta 2008-2023
print(grafico_tarta_2008_2023)

# Guardar el gráfico como PDF para el periodo 2008-2023
ggsave(paste0(dir_out, "causas_incendios_2002_2023.pdf"), plot = grafico_tarta_2008_2023, device = "pdf", width = 29.7 / 2, height = (29.7 / 2) / 1.618, units = "cm")

