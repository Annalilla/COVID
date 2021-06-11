# Prepares the data for the dashboard and starts the application.

library(shinydashboard)
library(shiny)


source("helpers/Get_and_prepare_data.R")
source("random_forest/Random_forest.R")
source("cluster/Hierarchical_cluster.R")
source("random_forest/Cluster_RF.R")
source("random_forest/rank_corr.R")
source("shinydashboard/Shiny_data_prep.R")

runApp("shinydashboard")
