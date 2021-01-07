library(compare)

old_tdata <- read.csv("data/tdata.csv")

source("data_collection/Save_data.R")
source("data_collection/Collect_data.R")
source("data_collection/Merge_data.R")
source("functions/Data_revision_functions.R")

to_compare <- tdata[which(tdata$date <= max(old_tdata$date)),]

differences <- list()
for(i in 1:ncol(old_tdata)){
  differences[[i]] <- c(which(old_tdata[,i] != to_compare[,i]))
  names(differences)[i] <- colnames(old_tdata)[i]
}
differences <- differences[lapply(differences,length) > 0]

show_number_of_differences()
show_first_differences(10)
show_last_differences(10)
show_all_differences_in_one_var("fb_data.dc_se_unw")
show_new_vars()

# Update variables
#old_tdata <- update_var("fb_data.percent_dc", records = c(1354, 2192, 2193))
#old_tdata <- update_var("tavg", records = "all")

# Add new variables
#old_tdata <- add_new_var("fb_data.dc_se_unw")

# Use only the new data
#old_tdata <- to_compare

# Save data
save_data(old_tdata, datname = "tdata", dirname = "data", archieve = TRUE)
