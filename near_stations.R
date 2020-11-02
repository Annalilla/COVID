library(rnoaa)
library(tidyverse)

station_data <- ghcnd_stations()

near_stations <- NULL

for(i in 1:nrow(capitals)){
  station_df <- get_near_stations(capitals$Capital[i], capitals$Latitudes[i], capitals$Longitudes[i])
  near_stations <- c(near_stations, station_df$id[1])
}