
# Constants and vectors ---------------------------------------------------

full.tree.vec <- c("ET1", "ET2", "ET3", "ET4", "ET5", "ET6", "ET7", "ET8",
                   "FB1", "FB2", "FB3", "FB4", "FB5", "FB6", "FB7", "FB8",
                   "TV1", "TV2", "TV3", "TV4")

# Functions ---------------------------------------------------------------

# The original function. Doesn' work with the new method
# 2 warnings come up whenever there are errors in the data, but you can ignore them here
# read_gropoint <- function(x){
#   suppressWarnings(
#     read_csv(x, col_names = col.names, show_col_types = F) %>%  
#       mutate(across(all_of(3:15), as.numeric))
#   ) %>% 
#     mutate(TreeID = Tree) %>% 
#     select(TreeID, Sensor, everything())
# }

# New function
read_gropoint2 <- function(x){
  suppressWarnings(
    read_csv(x, col_names = col.names.raw, show_col_types = F) %>%  
      mutate(across(all_of(3:15), as.numeric))
  ) %>% 
    mutate(TreeID = unique(sensorkey.sub$TreeID),
           Sensor = unique(sensorkey.sub$SensorNum)) %>%
    select(TreeID, Sensor, everything())
}

format_GP_for_plotting <- function(x){
  x2 <- x %>% 
    pivot_longer(4:16, names_to = "key", values_to = "value") %>% 
    separate(key, into = c("M.or.T", "Position"), sep = 1) %>% 
    mutate(M.or.T = case_when(
      M.or.T == "M" ~ "Moisture",
      M.or.T == "T" ~ "Temperature"
    ))
  return(x2)
}

plot_GP_moisture <- function(x){
  ggplot(x %>% 
           filter(M.or.T == "Moisture"),
         aes(x = Timestamp, y = value)) +
    geom_point(size = 0.1) +
    geom_smooth(span = 0.1, linewidth = 0.1, se = F) +
    ggtitle(str_c(unique(x$TreeID), "_Sensor", unique(x$Sensor), "_", "Moisture")) +
    theme_bw() +
    scale_x_datetime(date_breaks="1 month", date_labels="%m-%Y") +
    labs(y = "Soil moisture (%)", color = "Position") +
    facet_wrap(~Position, ncol = 1, scales = "free_y") +
    theme(strip.text = element_blank(),
          axis.title.x = element_blank())
}

plot_GP_temp <- function(x){
  ggplot(x %>% 
           filter(M.or.T == "Temperature"),
         aes(x = Timestamp, y = value)) +
    geom_point(size = 0.1) +
    geom_smooth(span = 0.1, linewidth = 0.1, se = F) +
    ggtitle(str_c(unique(x$TreeID), "_Sensor", unique(x$Sensor), "_", "Temp")) +
    theme_bw() +
    scale_x_datetime(date_breaks="1 month", date_labels="%m-%Y") +
    labs(y = "Soil temperature (C)", color = "Position") +
    facet_wrap(~Position, ncol = 1, scales = "free_y") +
    theme(strip.text = element_blank(),
          axis.title.x = element_blank())
}



