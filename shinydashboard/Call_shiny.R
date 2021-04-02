# Prepares the data for the dashboard and starts the application.

library(shinydashboard)
library(shiny)

source("helpers/Get_and_prepare_data.R")
source("shinydashboard/Shiny_data_prep.R")

runApp("shinydashboard")
