# Downloads data from all sources, creates and saves two databases

source("data_collection/Save_data.R")
source("data_collection/Collect_data.R")
source("data_collection/Merge_data.R")

# Country_char: numerical to character **RUN THIS ROW ONLY if you would like to save the data online. If you would like to work with the country_char database right now, DON'T RUN
country_char <- as.data.frame(apply(country_char, 2, as.character))
                                    
# tdata only until maximal date set in prepare_run.R
tdata <- tdata[tdata$date <= maxdate,]

# Save local
save_data(country_char, datname = "country_char", dirname = "data", archieve = TRUE)
save_data(tdata, datname = "tdata", dirname = "data", archieve = TRUE)

# Save online
save_data_online(country_char, datname = "country_char", archieve = TRUE)
save_data_online(tdata, datname = "tdata", archieve = TRUE)
