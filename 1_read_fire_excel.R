# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Define file paths for all the Excel files
file_paths <- list.files("/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/data/fires_murcia/", 
                         pattern = "Xlsx_20241014_093744_\\d\\.xlsx", full.names = TRUE)

# Initialize an empty data frame to store the combined data
combined_fires <- data.frame()

# Loop through each file and process
for (file_path in file_paths) {
  # Read the 'PIFs' sheet from each Excel file
  df_pifs <- read_excel(file_path, sheet = "PIFs")
  
  # Extract the required columns and calculate the total burned area (BA)
  df_extracted <- df_pifs %>%
    select(Incendio_Detectado, ha_TOTAL_Sup_Arbolada, ha_TOTAL_Sup_NoArbolada, 
           ha_TOTAL_Sup_Agricola, ha_TOTAL_Otra_Superficies, SubgrupoCausa) %>%
    mutate(Total_BA = rowSums(select(., ha_TOTAL_Sup_Arbolada, ha_TOTAL_Sup_NoArbolada, 
                                     ha_TOTAL_Sup_Agricola, ha_TOTAL_Otra_Superficies), na.rm = TRUE),
           Incendio_Detectado = as.Date(Incendio_Detectado))
  
  # Append the data to the combined dataset
  combined_fires <- bind_rows(combined_fires, df_extracted)
}

# Filter the dataset for fires with Total_BA > 1 ha and between 1980 and 2023
filtered_fires <- combined_fires %>%
  filter(Total_BA > 1, Incendio_Detectado >= as.Date("1980-01-01") & Incendio_Detectado <= as.Date("2023-12-31"))

# Save the filtered dataset
write.csv(filtered_fires, "/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/data/fires_filtered_1980_2023.csv", row.names = FALSE)

# Extract the year from the Incendio_Detectado column
filtered_fires <- filtered_fires %>%
  mutate(Year = as.numeric(format(Incendio_Detectado, "%Y")),
         Month = as.numeric(format(Incendio_Detectado, "%m")))

# Function to determine season based on month
get_season <- function(month) {
  if (month %in% c(12, 1, 2)) return("DJF")
  else if (month %in% 3:5) return("MAM")
  else if (month %in% 6:8) return("JJA")
  else return("SON")
}

# Add a Season column
filtered_fires <- filtered_fires %>%
  mutate(Season = sapply(Month, get_season))

# Adjust DJF to handle December as part of the previous year
filtered_fires <- filtered_fires %>%
  mutate(Adjusted_Year = ifelse(Season == "DJF" & Month == 12, Year + 1, Year)) %>%
  filter(Adjusted_Year >= 1980 & Adjusted_Year <= 2023)

# Summarize annually
annual_sum <- filtered_fires %>%
  group_by(Adjusted_Year) %>%
  summarise(Annual_BA = sum(Total_BA, na.rm = TRUE))

# Save the annual totals
write.csv(annual_sum, "/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/data/annual_burned_area_1980_2023.csv", row.names = FALSE)

# Summarize seasonally
seasonal_sum <- filtered_fires %>%
  group_by(Adjusted_Year, Season) %>%
  summarise(Seasonal_BA = sum(Total_BA, na.rm = TRUE))

# Save the seasonal totals
write.csv(seasonal_sum, "/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/data/seasonal_burned_area_1980_2023.csv", row.names = FALSE)

# Plot annual sums
ggplot(annual_sum, aes(x = Adjusted_Year, y = Annual_BA)) +
  geom_line() +
  scale_y_log10() +
  labs(x = "Year", y = "Total Burned Area (log scale)", title = "Annual Total Burned Area (1980-2023)") +
  theme_minimal()

# Plot seasonal sums (faceted by season)
ggplot(seasonal_sum, aes(x = Adjusted_Year, y = Seasonal_BA)) +
  geom_line() +
  scale_y_log10() +
  facet_wrap(~ Season, scales = "free_y") +
  labs(x = "Year", y = "Total Burned Area", title = "Seasonal Burned Area (1980-2023)") +
  theme_minimal()




# Load the library
library(changepoint)

# Assuming df_annual_ba is your data frame with Annual_BA and Adjusted_Year
# Perform change point detection on the Annual_BA data
ba_cpt <- cpt.mean(log(annual_sum$Annual_BA), method = "PELT")

# Plot the results to visualize the detected change points
plot(ba_cpt, main = "Change Point Detection in Burned Area", xlab = "Year", ylab = "Log(Burned Area)")
points(annual_sum$Adjusted_Year, log(annual_sum$Annual_BA), type = "o", col = "blue")
# Get the change points
annual_sum$Adjusted_Year[cpts(ba_cpt)]



# Assuming you already have the seasonal_sum data frame from the previous processing
# We will perform change point detection for each season separately

# Create an empty list to store the results for each season
season_cpt_results <- list()

# Define the seasons to iterate through
seasons <- unique(seasonal_sum$Season)

# Loop over each season to perform change point detection
for (season in seasons) {
  
  # Filter the data for the current season
  season_data <- seasonal_sum %>%
    filter(Season == season)
  
  # Perform change point detection on the Seasonal_BA (log-transformed)
  season_cpt <- cpt.mean(log(season_data$Seasonal_BA), method = "PELT")
  
  # Save the results in the list
  season_cpt_results[[season]] <- list(
    "data" = season_data,
    "change_points" = cpts(season_cpt)
  )
  
  # Plot the results for each season
  plot(season_cpt, main = paste("Change Point Detection in", season, "Burned Area"), 
       xlab = "Year", ylab = paste("Log(Burned Area) in", season))
  points(season_data$Adjusted_Year, log(season_data$Seasonal_BA), type = "o", col = "blue")
}

# View change points for each season
season_cpt_results$JJA

