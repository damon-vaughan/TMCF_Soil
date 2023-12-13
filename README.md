# TMCF_Soil
Gropoint soil data from TMCF NUBES project

Folders:
Soil_data_raw: Raw data organized into folders sorted by import date. Data is in .txt format.
Soil_data_L1: Imported, formatted, and appended. Each file is one probe (2 per datalogger/tree)
Soil_data_supporting: Metadata files, mainly containing the key that links sensor serial numbers to their physical positions

Scripts:
Import_and_process_soil_data.R: Import script
App.R: Shiny application
Soil_functions.R: Some functions called by the import script

Units:
Moisture: Uncalibrated percent. L1 data needs to be calibrated either through a max/min adjustment or through soil moisture release curves.
Temperature: Degrees Celsius
