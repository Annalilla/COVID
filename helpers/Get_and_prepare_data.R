# Loads the two (tdata and country_char) databases from Google Sheets
# Adds variable labels and changes the variable types if necessary

source("functions/Get_data.R")

tdata <- get_data("tdata", local = FALSE)
country_char <- get_data("country_char", local = FALSE)

source("helpers/Change_variable_types.R")
#source("helpers/Add_variable_labels.R")
