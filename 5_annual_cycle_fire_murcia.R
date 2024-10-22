# Clear the environment
rm(list = ls())
graphics.off()
gc()

# Load necessary libraries
library(Hmisc)
library(dplyr)
library(ggplot2)

dir_fire <- '/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/data/'
dir_out <- '/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/docs/'

file_path <- "/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/data/fires_filtered_1980_2023.csv"
# Read the CSV file into a data frame
filtered_fires <- read.csv(file_path)
# View the first few rows to verify the data
head(filtered_fires)
plot(filtered_fires$Total_BA, log = 'y')

filtered_fires <- filtered_fires %>%
  filter(Incendio_Detectado >= as.Date("1980-01-01"))
# Extract day, month, and year from deteccion
filtered_fires$Incendio_Detectado <- as.Date(filtered_fires$Incendio_Detectado, format = "%Y-%m-%d")
filtered_fires$year <- as.numeric(format(filtered_fires$Incendio_Detectado, "%Y"))
filtered_fires$month <- as.numeric(format(filtered_fires$Incendio_Detectado, "%m"))
filtered_fires$day <- as.numeric(format(filtered_fires$Incendio_Detectado, "%d"))

# Create a new column that combines the month and day information
filtered_fires$month_day <- filtered_fires$month + (filtered_fires$day / 31)


# Bubble plot with progressively larger circles based on fire size
p<-ggplot(filtered_fires, aes(x = month_day, y = year, size = Total_BA)) +
  geom_hline(aes(yintercept = year), color = "gray30", linetype = "solid", size = 0.2) +  # Horizontal lines for each year
  geom_point(color = "lightgray", alpha = 0.6, shape = 21, fill = "orange", stroke = 0.5) +  # Orange circles with light gray contour
  scale_size_continuous(
    trans = "log10",  # Logarithmic scale to ensure progressive size increase
    range = c(2, 15),  # Smoother circle size growth
    name = "Burned Area (ha)",  # Legend label
    breaks = c(1, 100, 10000),  # Size breaks for better differentiation
    labels = function(x) format(round(x), big.mark = ","),  # Display values in hectares
    limits = c(1, max(filtered_fires$Total_BA, na.rm = TRUE))  # Ensure no upper limit, including largest fires
  ) +
  scale_x_continuous(
    breaks = 1:12,  # Month numbers from January to December
    labels = month.abb,  # Abbreviated month names
    name = "Month"
  ) +
  labs(x = "Month and Day", y = "Year") +  # X-axis as Month and Day
  theme_minimal(base_size = 15) +
  theme(
    plot.background = element_rect(fill = "black"), 
    panel.background = element_rect(fill = "black"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  )


p


# Filter out rows with NA in total_ba

# Categorize the data
filtered_fires$size_category <- cut(filtered_fires$Total_BA,
                                               breaks = c(0.99, 5, 50, 500, Inf),
                                               labels = c("1-5 ha", "5-50 ha", "50-500 ha", ">500 ha"))

# Define custom sizes for each category
size_values <- c("1-5 ha" = 2, "5-50 ha" = 4, "50-500 ha" = 10, ">500 ha" = 20)

# Define custom colors for each category
color_values <- c("1-5 ha" = "red",   # Dark Violet
                  "5-50 ha" = "#F46D43",  # Dark Orange
                  "50-500 ha" = "#FDAE61",# Bright Orange
                  ">500 ha" = "#FEE08B")  # Bright Yellow

# Plot with the filtered data
# Plot with the custom sizes and colors
p <- ggplot(filtered_fires, aes(x = month_day, y = year, size = size_category, fill = size_category)) +
  geom_hline(aes(yintercept = year), color = "gray30", linetype = "solid", size = 0.2) +  # Horizontal lines for each year
  geom_point(color = "lightgray", alpha = 0.6, shape = 21, stroke = 0.5) +  # Circles with different sizes and colors
  scale_size_manual(
    values = size_values,
    name = "Burned Area (ha)"
  ) +
  scale_fill_manual(
    values = color_values,
    name = "Burned Area (ha)"
  ) +
  scale_x_continuous(
    breaks = 1:12,  # Month numbers from January to December
    labels = month.abb,  # Abbreviated month names
    name = "Month"
  ) +
  labs(x = "Month and Day", y = "Year") +  # X-axis as Month and Day
  theme_minimal(base_size = 15) +
  theme(
    plot.background = element_rect(fill = "black"), 
    panel.background = element_rect(fill = "black"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  )



print(p)


# Definir proporciones armónicas (ancho mayor que alto, basado en relación áurea)
width_cm <- 29.7  # Ancho en cm (el largo de una hoja A4)
height_cm <- width_cm / 1.618  # Alto según la proporción áurea

p
# Guardar el gráfico como EPS
ggsave(paste0(dir_out,"annual_cycle_burned_area_murcia.pdf"), plot = p, device = "pdf", width = width_cm, height = height_cm, units = "cm")


# Define Spanish month abbreviations
months_es <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

# Updated plot with Spanish labels and elements
p <- ggplot(filtered_fires, aes(x = month_day, y = year, size = size_category, fill = size_category)) +
  geom_hline(aes(yintercept = year), color = "gray30", linetype = "solid", size = 0.2) +  # Líneas horizontales para cada año
  geom_point(color = "lightgray", alpha = 0.6, shape = 21, stroke = 0.5) +  # Círculos con diferentes tamaños y colores
  scale_size_manual(
    values = size_values,
    name = "Área Quemada (ha)"  # Legend title in Spanish
  ) +
  scale_fill_manual(
    values = color_values,
    name = "Área Quemada (ha)"  # Legend title in Spanish
  ) +
  scale_x_continuous(
    breaks = 1:12,  # Números de los meses de enero a diciembre
    labels = months_es,  # Nombres de los meses en español
    name = "Mes"  # Eje X como Mes
  ) +
  labs(x = "Mes y Día", y = "Año") +  # Etiquetas del eje X como Mes y Día, y el eje Y como Año
  theme_minimal(base_size = 15) +
  theme(
    plot.background = element_rect(fill = "black"), 
    panel.background = element_rect(fill = "black"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  )

print(p)

# Guardar el gráfico como EPS
ggsave(paste0(dir_out,"ciclo_anual_superficie_quemada_murcia.pdf"), plot = p, device = "pdf", width = width_cm, height = height_cm, units = "cm")


