# Recollects and merges data with time variable from all data sources.
# Function are available to examine the differences between the old and the new data, and to update the old data with new values

library(compare)

source("functions/Get_data.R")
source("data_collection/Save_data.R")
source("data_collection/Collect_data.R")
source("data_collection/Merge_data.R")
source("functions/Data_revision_functions.R")

new_tdata <- tdata
tdata <- get_data("tdata")
# Changing variable types
source("helpers/Change_variable_types.R")
old_tdata <- as.data.frame(tdata)
tdata <- as.data.frame(new_tdata)

to_compare <- as.data.frame(tdata[which(tdata$date <= max(old_tdata$date)),])
# Check if records are for the same country and date
sum(old_tdata$date != to_compare$date) + sum(old_tdata$country != to_compare$country)

# to find differences between records occurring in both datasets
common_vars <- common_new()
old_tdata_common <- common_old()
# Match column numbers
colnum_old_tdata <- unlist(lapply(seq_along(common_vars), function(i){which(colnames(old_tdata_common) == colnames(common_vars)[i])}))

differences <- list()
for(i in 1:ncol(common_vars)){
  differences[[i]] <- c(which(old_tdata_common[,colnum_old_tdata[i]] != common_vars[,i]),
                        which(is.na(old_tdata_common[,colnum_old_tdata[i]]) & !is.na(common_vars[,i])))
  names(differences)[i] <- colnames(old_tdata_common)[colnum_old_tdata[i]]
}
differences <- differences[lapply(differences,length) > 0]

show_number_of_differences()
show_first_differences(10)
show_last_differences(10)
show_all_differences_in_one_var("tavg")
show_new_vars()
show_extinct_vars()

# Update variables
#old_tdata <- update_var("fb_data.dc_se_unw", records = c(3631:3635))
#old_tdata <- update_var("tavg", records = "all")

# Add new variables
#old_tdata <- add_new_var("fb_data.smoothed_dc")

# Use only the new data
#old_tdata <- to_compare

# Save data
#save_data(old_tdata, datname = "tdata", dirname = "data", archieve = TRUE)
#save_data_online(old_tdata, datname = "tdata", archieve = TRUE)
