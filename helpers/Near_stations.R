# Lists and selects weather station IDs near to capitals

library(rnoaa)
library(tidyverse)

station_data <- ghcnd_stations()

near_stations <- NULL

# In some cases selected manual
for(i in 1:nrow(capitals)){
  city <- data.frame(id = capitals$Capital[i], latitude = as.numeric(capitals$Latitudes[i]), longitude = as.numeric(capitals$Longitudes[i]))
  station_df <- meteo_nearby_stations(lat_lon_df = city, station_data = station_data, radius = 20, var = c("TAVG"), year_min = 2020, year_max = 2020)
  near_stations <- c(near_stations, station_df$id[1])
}
