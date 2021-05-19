# Downloads data from all sources, creates and saves two databases

source("data_collection/Save_data.R")
source("data_collection/Collect_data.R")
source("data_collection/Merge_data.R")

# Country_char: numerical to character
country_char <- as.data.frame(apply(country_char, 2, as.character))

# Save local
save_data(country_char, datname = "country_char", dirname = "data", archieve = TRUE)
save_data(tdata, datname = "tdata", dirname = "data", archieve = TRUE)

# Save online
save_data_online(country_char, datname = "country_char", archieve = TRUE)
save_data_online(tdata, datname = "tdata", archieve = TRUE)
