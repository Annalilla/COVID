## Create a time-varying dbase including clusters, then computes repeated variable importance for each cluster. 
#The result is the input for the rank correlation of the 'Country characteristics' tab.


library(caret)
library(ranger)
library(iml)
library(zoo)
library(randomForest)
library(Hmisc)
library(readr)
library(tidyverse)
library(stringr)
library(sjlabelled)


##Run RF on clusters

source("functions/RF_cluster_functions.R")


###preprocessing

##Selecting the variables, defining the period with meaningful amount of data at the end of the period
rf_max_date <- min(as.Date(do.call(c, lapply(unique(tdata$country), function(x){
  act_country <- tdata[tdata$country == x,]
  if(!all(is.na(act_country$fb_data.percent_mc)) & !all(is.na(act_country$fb_data.percent_dc))){
    max(act_country[!is.na(act_country$fb_data.percent_mc) & !is.na(act_country$fb_data.percent_dc), "date"])
  }
}))))

rf_dat_cl <- tdata_cl[(((tdata_cl$date >= "2020-02-28") & (tdata_cl$date <= rf_max_date))), 
                -which(colnames(tdata_cl) %in% c("year", "week", "country_code","testing_new_cases", "tests_done", "testing_population", 
                                              "testing_rate", "testing_positivity_rate", "fb_data.iso_code","fb_data.country", "fb_status",
                                              "fb_data.cli_se", "fb_data.percent_cli_unw","fb_data.cli_se_unw", "fb_data.sample_size_cli", 
                                              "fb_data.smoothed_cli", "fb_data.smoothed_cli_se", "fb_data.sample_size_smoothed_cli",
                                              "fb_data.mc_se", "fb_data.percent_mc_unw", "fb_data.mc_se_unw", "fb_data.sample_size_mc",
                                              "fb_data.smoothed_mc", "fb_data.smoothed_mc_se", "fb_data.sample_size_smoothed_mc",
                                              "fb_data.mc_se_dc", "fb_data.percent_dc_unw", "fb_data.dc_se_unw", "fb_data.sample_size_dc",               
                                              "fb_data.smoothed_dc", "fb_data.smoothed_dc_se", "fb_data.sample_size_smoothed_dc",      
                                              "iso_code", "deaths_new", "recovered_new"))]



##Outcome variable

# Number of cases proportionate to population size
rf_dat_cl$cases_new <- 100 * rf_dat_cl$cases_new/rf_dat_cl$`Population size`

# Lead to new cases
rf_dat$cases_new <- lead(rf_dat$cases_new, 14)

# Preprocess per cluster
# Split by country
rf_dat_cl_splitted <- split(rf_dat_cl, rf_dat_cl$country)
rf_dat_cl_splitted <- lapply(rf_dat_cl_splitted, function(x){
  # cumulative cases_new
  x$cases_new[which(is.na(x$cases_new))] <- 0
  x$cases_new_cum <- cumsum(x$cases_new)
  
  # smooth cases_new and cumulative cases_new with rolling average window = 7 days
  x$cases_new_cum <- rollmean(x$cases_new_cum, 7, fill = NA)
  x$cases_new<- rollmean(x$cases_new, 7, fill = NA)
  
  # Variable for number of cases on previous day and week
  x$last_day <- lag(x$cases_new, 15)
  x$last_week <- lag(x$cases_new, 21)
  
  # Smooth deaths, recovered, temperature, fb and vaccination variables with rolling average window = 7 days
  vars_to_smooth <- c("deaths_new", "recovered_new", "tavg", "fb_data.percent_cli", "fb_data.percent_mc", "fb_data.percent_dc")
  
  
  x[, which(colnames(x) %in% vars_to_smooth)] <-
    lapply(x[, which(colnames(x) %in% vars_to_smooth)], rollmean, 7, fill = NA)
  
  
  x
})

rf_dat_cl <- do.call("rbind", rf_dat_cl_splitted)

# Split and preprocess by cluster
rf_dat_cl_splitted <- split(rf_dat_cl, rf_dat_cl$groups)
rf_dat_cl_splitted <- lapply(rf_dat_cl_splitted, function(x){

  # Standardize
  x <- preproc_predict_cl(x)
  
  x$groups <- as.factor(x$groups)
  
  x
})


cl_rf_dat_fb <- rf_dat_cl_splitted

cluster_res_varimp <- lapply(cl_rf_dat_fb, function(x) rf_model_cl(x, wind = 28, hori = 5))

#Format data for the 'Country-char vs.varimp' tab input
cluster_res_varimp <- do.call(rbind, cluster_res_varimp)

cluster_res_varimp$groups <- rownames(cluster_res_varimp)

cluster_res_varimp$groups <- substr(cluster_res_varimp$groups, 1, 1)        # Extract first characters (cluster numbers)


#Save results and further input for the Shiny visualization

dir.create("shinydashboard", showWarnings = FALSE)
dir.create("shinydashboard/dat", showWarnings = FALSE)

saveRDS(cluster_res_varimp, "shinydashboard/dat/cluster_res_varimp.RDS")
saveRDS(cl_rf_dat_fb, "shinydashboard/dat/cl_rf_dat_fb.RDS")
