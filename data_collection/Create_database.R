source("data_collection/Save_data.R")
source("data_collection/Collect_data.R")
source("data_collection/Merge_data.R")

save_data(country_char, datname = "country_char", dirname = "data", archieve = TRUE)
save_data(tdata, datname = "tdata", dirname = "data", archieve = TRUE)
