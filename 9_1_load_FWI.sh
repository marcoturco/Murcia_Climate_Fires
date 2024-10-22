#!/bin/bash

# Input file path
INPUT_FILE="/diskonfire/ERA5/FWIv4_1/europe/ERA5_1979-2023-025-daily-timefixed.nc"
OUTPUT_FILE="/diskonfire/ERA5/FWIv4_1/Murcia_FWI_1979_2023.nc"

# Define the longitude and latitude bounding box for Murcia
LON_MIN=-2.5
LON_MAX=-0.5
LAT_MIN=37.0
LAT_MAX=39.0

# Define the year range (1979 to 2023)
START_YEAR=1979
END_YEAR=2023

echo "Cutting data for the Murcia region from $INPUT_FILE for years $START_YEAR to $END_YEAR..."

# Use CDO to cut the data for the Murcia region and select the years 1979-2023
cdo -O sellonlatbox,$LON_MIN,$LON_MAX,$LAT_MIN,$LAT_MAX -selyear,$START_YEAR/$END_YEAR $INPUT_FILE $OUTPUT_FILE
cdo monmean $OUTPUT_FILE "Murcia_FWI_1979_2022_monthly.nc"

if [[ $? -eq 0 ]]; then
    echo "Cutting complete. Output saved to $OUTPUT_FILE"
else
    echo "Error during cutting process."
fi
