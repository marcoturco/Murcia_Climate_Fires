#===============================================================================
# Description: Code for predicting BA across all seasons without using 'months'
#===============================================================================

#===============================================================================
# 1). Preliminary -----
#===============================================================================

# Clean up
rm(list = ls())
graphics.off()
gc()

# Packages
wants <- c("verification", "dplyr", "tidyr", "ggplot2", "gridExtra", "cowplot", "pracma")
needs <- wants[!(wants %in% installed.packages()[, "Package"])]
if (length(needs))
  install.packages(needs)
lapply(wants, function(i)
  require(i, character.only = TRUE))
rm(needs, wants)

# Directories
dir <- list()

dir$data = '~/Dropbox/estcena/scripts/Murcia_FIRE/data/'
dir$fwi = dir$data  # Assuming FWI data is in the same directory
dir$out = '~/Dropbox/estcena/scripts/Murcia_FIRE/docs/'

# Fixed parameters
nsteps = 3  # Test if previous months (up to 3) play a role
ntimescale = 3  # FWI 3, 6, and 12 are considered
years = 1980:2023  # Updated to include the combined seasonal data
years_fwi = 1979:2023  # FWI data now goes up to 2023
seasons <- c("DJF", "MAM", "JJA", "SON")

#===============================================================================
# 2). Load data -----
#===============================================================================

## Load fire data
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

# Agregar columna de estación
filtered_fires$season <- sapply(filtered_fires$month, get_season)



# Ajustar el año para DJF, donde diciembre debe contar como parte del año siguiente
filtered_fires <- filtered_fires %>%
  mutate(
    adjusted_year = ifelse(season == "DJF" & month == 12, year + 1, year)
  )

# Filtrar para que el primer año con DJF comience en 1992 (porque DJF 1991 no tiene diciembre 1990)
# y evitar DJF para 2024
filtered_fires <- filtered_fires %>%
  filter(!(season == "DJF" & adjusted_year == 1980)) %>%  # Excluir DJF de 1991
  filter(!(season == "DJF" & adjusted_year > 2023))  # Excluir DJF de 2024

# Calcular el área quemada total y el número de incendios por estación y año, reemplazando por NA cuando no haya datos
seasonal_ba <- filtered_fires %>%
  group_by(adjusted_year, season) %>%
  summarise(
    total_seasonal_ba = ifelse(sum(Total_BA, na.rm = TRUE) == 0, NA, sum(Total_BA, na.rm = TRUE)),  # Sumar el área quemada total, NA si no hay datos
    NF = n()  # Contar el número de incendios (NF)
  ) %>%
  ungroup()

# Asegurarse de que todos los años tengan las 4 estaciones (DJF, MAM, JJA, SON)
seasonal_ba <- seasonal_ba %>%
  complete(adjusted_year = full_seq(adjusted_year, 1), season = seasons, fill = list(total_seasonal_ba = NA, NF = 0))

# Visualizar las primeras filas para verificar
head(seasonal_ba)

seasonal_totals=seasonal_ba
seasonal_totals$log_total_season_ba=log10(seasonal_ba$total_seasonal_ba)

## Load updated FWI data
# Positions in 'years_fwi' that correspond to 'years'
start_year_index = which(years_fwi == min(years))
end_year_index = which(years_fwi == max(years))

# Compute starting and ending month indices
start_month_index = (start_year_index - 1) * 12 + 1
end_month_index = end_year_index * 12

# Create 'jok_fwi' with the correct range of month indices
jok_fwi = start_month_index:end_month_index

# Load FWI data (up to 2023)
load(file.path(dir$fwi, "FWI3_ERA5_LAND_1979_2023.RData"))
load(file.path(dir$fwi, "FWI6_ERA5_LAND_1979_2023.RData"))
load(file.path(dir$fwi, "FWI12_ERA5_LAND_1979_2023.RData"))

# Prepare FWI data array
fwi_ave = array(NA, c(length(fwi3_ave), ntimescale))
fwi_ave[, 1] = fwi3_ave
fwi_ave[, 2] = fwi6_ave
fwi_ave[, 3] = fwi12_ave

#===============================================================================
# 3). Correlation analysis across all seasons -----
#===============================================================================

results_list <- list()

for (iseas in 1:length(seasons)) {
  season_name = seasons[iseas]
  seasonal_totals_temp <- seasonal_totals %>%
    filter(season == season_name)
  
  # Determine the last month of the season
  last_month <- switch(season_name, "DJF" = 2, "MAM" = 5, "JJA" = 8, "SON" = 11)
  
  rho = array(NA, c(ntimescale, nsteps))
  sig = array(NA, c(ntimescale, nsteps))
  
  for (isc in 1:ntimescale) {
    for (im in 1:nsteps) {
      if ((last_month - im + 1) < 1) {
        ind_fwi = jok_fwi[seq(last_month - im + 13, length(jok_fwi), 12)]
      } else {
        ind_fwi = jok_fwi[seq(last_month - im + 1, length(jok_fwi), 12)]
      }
      fwi_dum = fwi_ave[ind_fwi, isc]
      
      # Detrend BA and FWI
      ba_diff = detrend(seasonal_totals_temp$log_total_season_ba, tt = 'linear')
      fwi_diff = detrend(fwi_dum, tt = 'linear')
      
      # Perform correlation analysis
      if (length(ba_diff) > 0 && length(fwi_diff) > 0) {
        dum = cor.test(
          ba_diff,
          fwi_diff,
          use = "pairwise.complete.obs",
          alternative = "two.sided",
          method = "spearman"
        )
        rho[isc, im] = dum$estimate
        sig[isc, im] = dum$p.value
      } else {
        rho[isc, im] = NA
        sig[isc, im] = NA
      }
    }
  }
  
  # Filter non-significant correlations
  rho[sig > 0.05] = NA
  
  # Store results for the current season
  results_list[[season_name]] <- list(rho = rho, sig = sig)
  
  cat("\nSeason:", season_name, "\n")
  cat("rho:\n")
  print(rho)
  cat("sig:\n")
  print(sig)
}




#===============================================================================
# 4). Plot results and save -----
#===============================================================================
# Same plotting code as before for creating combined plots and saving them.





#===============================================================================
# Find Best Predictor and Plot Scatter Plots with Log-Scaled Y-Axis and Period Labels
#===============================================================================
# Determine the range of years across all seasons
all_years <- unique(seasonal_totals$adjusted_year)
min_year <- min(all_years, na.rm = TRUE)
max_year <- max(all_years, na.rm = TRUE)


# Cambiar los nombres de los meses a español
month_names_spanish <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

# Crear un vector para traducir los nombres de las estaciones
season_names_spanish <- c("DJF" = "Invierno", "MAM" = "Primavera", "JJA" = "Verano", "SON" = "Otoño")

# Initialize a list to store plots
plot_list <- list()

# Loop over each season
for (season_index in 1:length(seasons)) {
  season_name <- seasons[season_index]
  # Obtener el nombre de la estación en español
  season_name_spanish <- season_names_spanish[[season_name]]
  
  
  # Determine the last month of the season
  if (season_name == "DJF") {
    last_month <- 2  # February
  } else if (season_name == "MAM") {
    last_month <- 5  # May
  } else if (season_name == "JJA") {
    last_month <- 8  # August
  } else if (season_name == "SON") {
    last_month <- 11 # November
  }
  
  
  # Extract the correlation results for the season
  season_results <- results_list[[season_name]]
  rho_matrix <- season_results$rho
  sig_matrix <- season_results$sig
  
  # Find the indices of the maximum significant correlation
  # Exclude NAs and consider only significant correlations (sig <= 0.05)
  significant_rho <- rho_matrix
  significant_rho[is.na(significant_rho)] <- -Inf  # Set NAs to -Inf to exclude them
  significant_rho[sig_matrix > 0.05] <- -Inf  # Exclude non-significant correlations
  
  # Find the maximum significant rho and its indices
  max_rho <- max(significant_rho)
  if (max_rho == -Inf) {
    cat("\nSeason:",
        season_name,
        "- No significant correlations found.\n")
    next  # Skip to the next season if no significant correlations
  }
  
  # Get the indices of the maximum rho
  indices <- which(significant_rho == max_rho, arr.ind = TRUE)
  isc_best <- indices[1, 1]  # Timescale index
  im_best <- indices[1, 2]   # Lag index
  
  # Get the best timescale and lag
  best_timescale <- c(3, 6, 12)[isc_best]
  best_lag <- im_best  # Since im ranges from 1 to nsteps
  
  if ((last_month - im_best + 1) < 1) {
    ind_fwi = jok_fwi[seq(last_month - im_best + 13, length(jok_fwi), 12)]
  } else {
    ind_fwi = jok_fwi[seq(last_month - im_best + 1, length(jok_fwi), 12)]
  }
  fwi_dum = fwi_ave[ind_fwi, isc_best]
  
  # Create date sequence for BA data
  ba_dates <- as.Date(paste0(
    seasonal_totals_temp$adjusted_year,
    "-",
    sprintf("%02d", last_month),
    "-01"
  ))
  
  
  for (i in 1:length(ba_dates)) {
    # Extract year and month from ba_dates[i]
    year_i <- as.numeric(format(ba_dates[i], "%Y"))
    month_i <- as.numeric(format(ba_dates[i], "%m"))
    
    # Calculate the target month adjusted for lag
    target_month <- month_i - (best_lag - 1)
    target_year <- year_i
    while (target_month <= 0) {
      target_month <- target_month + 12
      target_year <- target_year - 1
    }
    
    # Calculate the months over which FWI is averaged
    fwi_months <- target_month - ((best_timescale - 1):0)
    fwi_years <- rep(target_year, best_timescale)
    for (j in 1:length(fwi_months)) {
      while (fwi_months[j] <= 0) {
        fwi_months[j] <- fwi_months[j] + 12
        fwi_years[j] <- fwi_years[j] - 1
      }
      while (fwi_months[j] > 12) {
        fwi_months[j] <- fwi_months[j] - 12
        fwi_years[j] <- fwi_years[j] + 1
      }
    }
    
    # Save the months for labeling (from the first iteration)
    if (i == 1) {
      period_months <- month.abb[fwi_months]  # Use abbreviated month names
    }
    
  }
  
  
  # Extract the rho and p-value for the best predictor
  cor_value <- rho_matrix[isc_best, im_best]
  p_value <- sig_matrix[isc_best, im_best]
  
  # Prepare the BA data for the season
  seasonal_totals_temp <- seasonal_totals %>%
    filter(season == season_name)
  
  
  # Prepare the BA data (original scale)
  ba_data <- detrend(seasonal_totals_temp$log_total_season_ba, tt = 'linear')
  fwi_dum = detrend(fwi_dum, tt = 'linear')
  
  # Remove NA values from both BA and FWI data
  valid_indices <- which(!is.na(ba_data) & !is.na(fwi_dum))
  
  ba_data_valid <- ba_data[valid_indices]
  fwi_dum_valid <- fwi_dum[valid_indices]
  years_valid <- seasonal_totals_temp$adjusted_year[valid_indices]
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    BA = (ba_data_valid),
    FWI = (fwi_dum_valid),
    Year = (years_valid)
  )
  
  
  # Use the Spanish month names
  for (i in 1:length(ba_dates)) {
    # Save the months for labeling
    if (i == 1) {
      period_months <- month_names_spanish[fwi_months]  # Use month names in Spanish
    }
  }
  
  # Construct the x-axis label with subscript notation
  period_str <- paste0("FWI['", paste(period_months, collapse = "-"), "']")
  
  # Determine the asterisk(s) to add to rho based on p-value
  if (p_value < 0.01) {
    asterisks <- "**"
  } else if (p_value < 0.05) {
    asterisks <- "*"
  } else {
    asterisks <- ""
  }
  
  # Create the annotation text
  annotation_text <- paste0(round(cor_value, 2), asterisks)
  
  
  # Create the plot with differences
  p <- ggplot(plot_data, aes(x = FWI, y = BA, color = Year)) +
    geom_point(size = 5) +
    geom_smooth(method = "lm", se = FALSE, color = "grey") +
    # scale_y_log10() +  # Log scale for BA difference
    # Use consistent color scale across all plots
    scale_color_gradientn(
      colors = c("yellow", "red"),
      limits = c(min_year, max_year),
      name = "Año"
    ) +
    labs(
      
      title = paste(season_name_spanish, "\nrho= ", annotation_text),
      x = parse(text = period_str),
      y = "BAI"
    ) +
    theme_minimal(base_size = 14) +  # Set the base font size
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right"  # Keep the legend position to extract it later
    ) +
    expand_limits(x = c(min(plot_data$FWI) - 0.1, max(plot_data$FWI) + 0.1), 
                  y = c(min(plot_data$BA) - 0.1, max(plot_data$BA) + 0.1))
  
  
  
  
  # Store the plot in the list
  plot_list[[season_index]] <- p
  
}

# Remove NULL elements from plot_list in case any seasons were skipped
plot_list <- plot_list[!sapply(plot_list, is.null)]

# Extract the legend from one of the plots
legend <- get_legend(plot_list[[1]])

# Remove legends from individual plots
plot_list_nolegend <- lapply(plot_list, function(p)
  p + theme(legend.position = "none"))

# Arrange the plots and add the shared legend
combined_plot <- plot_grid(
  plot_grid(
    plotlist = plot_list_nolegend,
    ncol = 2,
    nrow = 2
  ),
  legend,
  ncol = 2,
  rel_widths = c(1, 0.1)  # Adjust the relative width of the legend
)

# Display the combined plot
print(combined_plot)

# Increase the plot height to make the data points more visible
width_cm <- 29.7  # Ancho en cm (el largo de una hoja A4)
height_cm <- 15  # Increased height for better visibility

ggsave(
  paste0(dir$out, "FWI_vs_burned_area_murcia.pdf"),
  plot = combined_plot,
  device = "pdf",
  width = width_cm,
  height = height_cm,
  units = "cm"
)

results_list
