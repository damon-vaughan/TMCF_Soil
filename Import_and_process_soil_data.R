library(needs)
needs(tidyverse, lubridate, readxl)

source("Soil_functions.R")
options(readr.show_col_types = FALSE)

# Data should be uploaded into a folder named by date of upload.
import_date <- "2024-01-04"

# Import to L1 -----------------------------------------------------

filenames.full <- list.files(file.path("Soil_data_raw", import_date), 
                             full.names = T)

sensor.key <- read_excel(file.path("Soil_data_supporting", 
                              "Sensor_key_Install2.xlsx")) %>% 
  mutate(SensorID = as.character(SensorID))

col.names.raw <- c("Timestamp", "Sensor", "M1", "M2", "M3", "M4", "M5", "M6", 
                   "T1", "T2", "T3", "T4", "T5", "T6", "T7")

# If you get an error arising from missing data at the start of the file, open it and delete those rows manually
i <- filenames.full[14]
for(i in filenames.full){
  
  sensorkey.sub <- sensor.key %>% 
    filter(SensorID == str_sub(i, start = -11, end = -6))
  
  d <- read_gropoint2(i) %>% 
    distinct()
  
  new.data.starts <- min(d$Timestamp)
  new.data.ends <- max(d$Timestamp)
  
  filename.in.L1 <- str_c("Soil_data_L1/", 
                            unique(sensorkey.sub$TreeID), "_", 
                            unique(sensorkey.sub$Sensor), "_GP_L1", ".csv")
  
  # If the L1 file doesn't exist, need to start it
  if(file.exists(filename.in.L1) == F){
    out <- str_c(file.path("Soil_data_L1",
                      str_c(unique(d$TreeID), unique(d$Sensor), 
                            "GP_L1", sep = "_")))
    write_csv(d, str_c(out, ".csv"))
  } else{
    L1.data <- read_csv(filename.in.L1, show_col_types = F)
    data.needed.starting <- max(L1.data$Timestamp)
    
    if(new.data.starts <= (data.needed.starting + hours(2)) & 
       new.data.ends > data.needed.starting){
      d.append <- read_gropoint2(i) %>% 
        filter(Timestamp > as.POSIXct(data.needed.starting, tz = "CST"))
      new.L1 <- bind_rows(L1.data, d.append) %>% 
        distinct()
      out <- str_c(file.path("Soil_data_L1", 
                        str_c(unique(sensorkey.sub$TreeID), "_", 
                              unique(sensorkey.sub$Sensor), "_GP_L1")))
      write_csv(new.L1, str_c(out, ".csv"))
      print(str_c("Imported from: ", unique(sensorkey.sub$TreeID), "_", 
                  unique(sensorkey.sub$Sensor)))} 
    else if(new.data.starts <= data.needed.starting & 
            new.data.ends <= data.needed.starting) {
      print(str_c("No import needed from: ", unique(sensorkey.sub$TreeID), "_", 
                  unique(sensorkey.sub$Sensor)))} 
    else if(new.data.starts > data.needed.starting){
      print(str_c("Problem: No import due to data gap: ", 
                  unique(sensorkey.sub$TreeID), "_", 
                  unique(sensorkey.sub$Sensor)))
    }
  }
}

# L1 to L2 -------------------------------------
# Not updated for a very long time!!!

filenames.L1 <- list.files("Soil_data_L1", pattern = ".csv", full.names = T)
tree.vec <- full.tree.vec

sensor.key <- read_excel(file.path("Soil_data_supporting", 
                                   "Sensor_key_Install2.xlsx")) %>% 
  mutate(SensorID = as.character(SensorID))

i <- tree.vec[1]
for(i in tree.vec){
  filenames.sub <- filenames.L1[which(str_detect(filenames.L1, i) == T)]
  
  sensor.key2 <- sensor.key %>% 
    filter(TreeID == i) %>% 
    select(Sensor, Location)
  
  d <- lapply(filenames.sub, read_csv) %>% 
    bind_rows() %>% 
    distinct() %>% 
    left_join(sensor.key2, by = "Sensor") %>% 
    select(-Sensor) %>% 
    select(Tree = TreeID, Location, Timestamp, everything())
  
  d2 <- d %>% 
    pivot_longer(4:16, names_to = "Names", values_to = "Values") %>% 
    separate(Names, into = c("Type", "Depth"), sep = 1) %>% 
    pivot_wider(names_from = Type, values_from = Values) %>% 
    rename("Moisture" = "M", "Temperature" = "T")
  
  write_csv(d2, file.path("Soil_data_L2", str_c(i, "_GP_L2.csv")))
}


# Troubleshooting ---------------------------------------------------------

filenames <- list.files("Soil_data_L1", pattern = ".csv", full.names = T)

d <- read_csv(filenames[6])
which(duplicated(d) == T)
