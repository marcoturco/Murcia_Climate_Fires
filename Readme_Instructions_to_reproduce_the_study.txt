#################################################################################################
#		
# Title:	Analysing fire and fire weather in the Murcia region
#				
# Author:    Marco Turco, University of Murcia (marco.turco@um.es)
#
#################################################################################################

#################################################################################################
# A. General instructions 
#################################################################################################

This project is designed to be executed with shell scripts and R codes. 
Execute script files in the order they are listed.

Data sources:

- Fire data source 1: Unidad de Defensa contra Incendios Forestales de la Región de Murcia. Excel

- Fire data source 2: Unidad de Defensa contra Incendios Forestales de la Región de Murcia. Shapefile

- Fire data (to contrast source 1):
https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/incendios-forestales/estad%C3%ADstica-iiff/Historico_Actualizado.zip

- Fire data (only annual totals):
https://econet.carm.es/inicio/-/crem/sicrem/PU_datosBasicos/sec11.html

- Fire Weather Index:
https://cds.climate.Copernicus.eu/cdsapp#!/dataset/cems-fire-historical-v1

If you have any questions or wish to express any comment to the authors, please 
contact Dr. Marco Turco at the emails indicated above.


#################################################################################################
# B. Description of script files
#################################################################################################

Scripts for data preparation

- 1_read_fire_excel.R
R script to read the excel tables and save R dataframe

- 2_read_fire_shapefiles.R
R script to read the shapefile with fire perimeter data

- 3_read_table_mdb_EGIF.R
R script to read the microsoft access data of fire from www.miteco.gob.es

- 4_1_comparisons.R & 4_2_comparisons_seasonal.R
R script to comapre the different fire data

- 5_annual_cycle_fire_murcia
R script to generate figure 1

- 6_stastistics_fire_murcia
R script to calculate basic statistics

- 7_time_series_fire_murcia.R
R script to produce figures 2 and 3

- 7_time_series_fire_murcia.R
R script to produce figures 2 and 3

- 8_causas_fire_murcia_all.R
R script to produce figure 4

- 9_1_load_FWI.sh, 9_2_load_FWI.R (it needs t2sti.R function that computes the Standardized FWI Index (STI) with monthly FWI data), 9_3_model_BA_FWI.R
Shell and R scripts to extract the Fire Weather Index (FWI) and produce figure 5

- 9_1_load_FWI.sh, 9_2_load_FWI.R (it needs t2sti.R function), 9_3_model_BA_FWI.R
R script to produce figure 6



