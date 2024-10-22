rm(list = ls())
graphics.off()
gc()

library(ncdf4)
library(sf)
library(fields)
library(RColorBrewer)  # Para usar la paleta RdBu

dir_out='/home/marco/Dropbox/estcena/scripts/Murcia_FIRE/docs/'

# Read shapefile using sf
dir_shp <- '/home/marco/Dropbox/estcena/scripts/Murcia_FIRE/data/Murcia_Region/'
file_shp <- paste0(dir_shp, 'Murcia_Region.shp')
shp <- st_read(file_shp)
CRS.new <- "+proj=longlat +datum=WGS84 +no_defs"
shp <- st_transform(shp, CRS.new)

# Function to process NetCDF files
process_nc_file <- function(nc_file, shp) {
  # Open NetCDF file
  obs.nc <- nc_open(nc_file)
  print(paste("Processing file:", nc_file))
  
  # Get the variable (fwinx)
  obs <- ncvar_get(obs.nc, "fwinx")
  if (is.null(obs)) {
    print("fwinx variable not found!")
    return(NULL)
  }
  
  lon <- ncvar_get(obs.nc, "longitude")
  lat <- rev(ncvar_get(obs.nc, "latitude"))  # Reverse latitudes if necessary
  
  # Ensure obs data is in the correct orientation
  obs <- obs[, ncol(obs):1, ]  # Flip the data if necessary
  
  # Create spatial points for the grid
  points <- expand.grid(lon, lat)
  pts <- st_as_sf(points, coords = c("Var1", "Var2"), crs = CRS.new)
  
  # Check which points fall within the Murcia shapefile
  inout <- st_intersects(pts, shp, sparse = FALSE)
  if (is.null(inout)) {
    print("No intersection found with the shapefile!")
    return(NULL)
  }
  
  # Reshape inout to match obs dimensions
  inout[inout == FALSE] <- NA
  inout_matrix <- matrix(as.numeric(inout), nrow = length(lon), ncol = length(lat))  # Reshape
  
  # Plot the FWI mean over time
  fwi_mean <- apply(obs, c(1, 2), mean, na.rm = TRUE)
  image.plot(lon, lat, fwi_mean, main = "Mean FWI", xlab = "Longitude", ylab = "Latitude")
  plot(st_geometry(shp), add = TRUE, border = 'red')  # Add Murcia region as an overlay
  
  # Plot the inout mask
  image.plot(lon, lat, inout_matrix, main = "In/Out Mask", xlab = "Longitude", ylab = "Latitude")
  plot(st_geometry(shp), add = TRUE, border = 'red')  # Add Murcia region as an overlay
  
  # Calculate spatial mean for each time step
  sm_reg <- sapply(1:dim(obs)[3], function(itime) {
    obs_masked <- obs[, , itime] * inout_matrix  # Apply mask
    mean(obs_masked, na.rm = TRUE)  # Calculate the mean
  })
  
  # Close NetCDF file
  nc_close(obs.nc)
  
  return(sm_reg)
}


# Process all files and calculate the difference
fwi_files <- list.files(path = "/diskonfire/FWI_T_EU", pattern = "*_Murcia_seasonal.nc", full.names = TRUE)
fwi_files <- fwi_files[!grepl("ERA5", fwi_files)]

test_file <- fwi_files[1]
test_result <- process_nc_file(test_file, shp)
str(test_result)



fwi_data <- lapply(fwi_files, process_nc_file, shp = shp)
fwi_data_matrix <- do.call(cbind, fwi_data)

# Process ERA5 observed data
era5_file <- "/diskonfire/FWI_T_EU/ERA5_1981-2010-025-daily_Murcia_seasonal.nc"
era5_data <- process_nc_file(era5_file, shp)

# Calculate differences
fwi_differences <- sweep(fwi_data_matrix, 1, era5_data, "-")


# Exclude the first row (DJF 1981) and the last row (DJF 2010)
fwi_differences <- fwi_differences[2:120, ]

# Optionally, check the new dimensions to ensure rows were removed correctly
print(dim(fwi_differences))

library(fields)  # Para usar image.plot y generar el heatmap

# Paso 1: Extraer los valores numéricos de las perturbaciones de P y T
perturbation_T <- sub("FWI_(T[0-9.]+)_P[0-9.]+.*", "\\1", basename(fwi_files))
perturbation_P <- sub("FWI_T[0-9.]+_(P[0-9.]+).*", "\\1", basename(fwi_files))

# Convertir las perturbaciones de P a los cambios porcentuales numéricos
convert_precipitation <- function(p_label) {
  switch(p_label,
         "P0.6" = -40,
         "P0.8" = -20,
         "P1.0" = 0,
         "P1.2" = 20,
         "P1.4" = 40,
         "P1.6" = 60)
}

perturbation_P_labels <- sapply(perturbation_P, convert_precipitation)

# Extraer solo los números de las etiquetas de T
perturbation_T_labels <- gsub("T", "", perturbation_T)

# Asignar nombres a las columnas basados en las perturbaciones de T y P
colnames(fwi_data_matrix) <- paste(perturbation_T_labels, perturbation_P_labels, sep = " y ")

# Paso 2: Calcular el cambio porcentual respecto a ERA5
percentage_differences <- sweep(fwi_data_matrix, 1, era5_data, function(pert, era5) {
  100 * (pert - era5) / abs(era5)
})

# Excluir la primera y última fila (DJF 1981 y DJF 2010)
percentage_differences <- percentage_differences[2:120, ]
era5_data <- era5_data[2:120]
# Paso 3: Crear un vector de estaciones con nombres en español
seasons <- rep(c("Primavera", "Verano", "Otoño", "Invierno"), length.out = nrow(percentage_differences))

# Reordenar las estaciones para que empiece con "Invierno"
rownames(percentage_differences) <- paste0(seasons, " ", 1981:2010)
season_order <- c("Invierno", "Primavera", "Verano", "Otoño")

# Paso 4: Definir la paleta de colores personalizada
# custom_palette <- c("#80cdc1", "#c7eae5", "#f6e8c3", "#dfc27d", "#bf812d", "#8c510a", "#543005")
# 
# # Establecer los intervalos de cambio
# color_breaks <- c(-40, -20, 0, 20, 40, 60, 80, 100)

# Definir los intervalos de color ajustados a tus nuevos breakpoints
color_breaks <- c(-20, -10, 0, 10, 20, 30, 40, 50, 60, 100)
color_ticks <- color_breaks

# Definir manualmente los colores para cada intervalo en 'color_breaks'
# Tono de azules para valores negativos, y tonos de marrón para positivos
# custom_colors <- c("#80cdc1", "#c7eae5", "#f6e8c3", "#dfc27d","#bf812d", "#8c510a", "#3d2c02")
custom_colors <- c("#80cdc1", "#c7eae5","#f6e8c3", "#dfc27d", "#bf812d", "#8c510a", "#543005", "#3d2c02", "#2b1d01")
# Listar las estaciones para hacer un heatmap separado por cada una
season_list <- c("Invierno", "Primavera", "Verano", "Otoño")

# Crear el archivo PDF para guardar los gráficos
pdf(paste0(dir_out, "FWI_perturbaciones_temporadas.pdf"), width = 12, height = 8)

# Definir un layout para 2x2 gráficos en la misma página
par(mfrow = c(2, 2))

for (season in season_list) {
  # Filtrar los datos solo para la temporada específica
  season_data <- percentage_differences[grep(season, rownames(percentage_differences)), ]
  
  # Calcular la media del cambio porcentual por perturbación
  mean_changes_season <- colMeans(season_data, na.rm = TRUE)
  
  # Calcular la media observada del FWI para esta temporada
  mean_fwi_obs <- round(mean(era5_data[grep(season, rownames(percentage_differences))]), 2)
  
  # Convertir los datos en una matriz 6x6 (T x P)
  heatmap_data <- matrix(mean_changes_season, nrow = 6, ncol = 6, byrow = TRUE)
  
  # Crear el heatmap con los colores personalizados y los intervalos definidos
  image.plot(1:6, 1:6, heatmap_data, xlab = "Cambios de temperatura (°C)", ylab = "Cambios de precipitación (%)",
             main = paste(season, "(FWI = ", round(mean_fwi_obs), ")"),  # Título con el nombre de la estación y el FWI promedio
             axes = FALSE, col = custom_colors, breaks = color_breaks, zlim = range(color_breaks),
             # Personalizar la barra de colores con ticks manuales
             axis.args = list(at = color_ticks, labels = color_ticks, cex.axis = 1.2))  # Ajustar la escala y tamaño de texto)  # Ajustar la escala
  
  # Añadir ejes con los valores únicos de T y P
  axis(1, at = 1:6, labels = unique(perturbation_T_labels))
  axis(2, at = 1:6, labels = unique(perturbation_P_labels))
  
  # Dibujar las líneas verticales y horizontales para subrayar la cuadrícula
  abline(v = 0:6 + 0.5, col = "black", lwd = 1.5)  # Líneas verticales
  abline(h = 0:6 + 0.5, col = "black", lwd = 1.5)  # Líneas horizontales
}

# Cerrar el archivo PDF
dev.off()

