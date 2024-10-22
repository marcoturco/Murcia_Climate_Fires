# Clean up
rm(list = ls())
graphics.off()
gc()

# Load necessary libraries
library(dplyr)
library(ggplot2)


# Directories
dir <- list()
dir$data = '~/Dropbox/estcena/scripts/Murcia_FIRE/data/'
dir$out = '~/Dropbox/estcena/scripts/Murcia_FIRE/docs/'

years = 1980:2023  # Updated to include the combined seasonal data
# years = 1979:2023  # Updated to include the combined seasonal data
years_fwi = 1979:2023  # FWI data now goes up to 2023

# Load FWI data (up to 2023)
load(file.path(dir$data, "FWI3_ERA5_LAND_1979_2023.RData"))
load(file.path(dir$data, "FWI6_ERA5_LAND_1979_2023.RData"))

# Prepare FWI data array

start_year_index = which(years_fwi == min(years))
start_month_index = (start_year_index - 1) * 12 + 1

fwi3_ave = fwi3_ave[-(1:start_month_index-1)]  # Removing the first year (1979)
fwi6_ave = fwi6_ave[-(1:start_month_index-1)]  # Removing the first year (1979)
fwi_ave = array(NA, c(length(fwi3_ave), 2))
fwi_ave[, 1] = fwi3_ave
fwi_ave[, 2] = fwi6_ave

# Initialize a list to store time series plots
time_series_plot_list <- list()

# Loop over each season to extract FWI data and plot the time series with trend
for (season_name in c("DJF", "MAM", "JJA", "SON")) {
  
  # Determine the FWI data depending on the season
  last_month <- switch(season_name,
                       "DJF" = 2,
                       "MAM" = 5,
                       "JJA" = 8,
                       "SON" = 11)
  
  ts <- switch(season_name,
                       "DJF" = 2, #FWI 6 month
                       "MAM" = 1,
                       "JJA" = 1,
                       "SON" = 1)
  ind_fwi = seq(last_month, dim(fwi_ave)[1], 12)  # Use last month of season
  fwi_season = fwi_ave[ind_fwi,ts]
  
  # Associate years with FWI data for the season
  fwi_season_data <- data.frame(
    Year = years,  # Years (1980-2023)
    FWI = fwi_season  # Corresponding FWI values
  )
  
  # Linear regression for trend analysis
  fwi_lm <- lm(FWI ~ Year, data = fwi_season_data)
  slope <- coef(fwi_lm)[2]*10
  p_value <- summary(fwi_lm)$coefficients[2, 4]
  
  # Create the time series plot with points, trend line, and confidence bands
  p <- ggplot(fwi_season_data, aes(x = Year, y = FWI)) +
    geom_point(size = 3, color = "red") +
    geom_smooth(method = "lm", color = "orange", fill = "orange", se = TRUE) +
    labs(title = paste("FWI -", season_name),
         x = "Año",
         y = "FWI estandardizado") +
    theme_minimal(base_size = 15) +
    annotate("text", x = 1992, y = 0.8*max(fwi_season_data$FWI, na.rm = TRUE), 
             label = paste0("Pendiente [sigma/10·años]: ", round(slope, 2),
                            "\np-valor: ", signif(p_value, 2)),
             hjust = 0)
  
  # Store the plot
  time_series_plot_list[[season_name]] <- p
  
  # Save each individual plot as a PDF
  ggsave(filename = paste0(dir$out, "FWI_time_series_", season_name, ".pdf"), 
         plot = p, 
         width = 8, height = 6)
}
