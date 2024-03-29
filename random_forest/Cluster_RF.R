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
#rf_max_date <- min(do.call(c, lapply(unique(tdata$country), function(x){
#  act_country <- tdata[tdata$country == x,]
#  if(!all(is.na(act_country$fb_data.percent_mc)) & !all(is.na(act_country$fb_data.percent_dc))){
#    max(act_country[!is.na(act_country$fb_data.percent_mc) & !is.na(act_country$fb_data.percent_dc), "date"])
#  }
#})))

rf_max_date <- min(do.call(c, lapply(unique(tdata$country), function(x){
  act_country <- tdata[tdata$country == x,]
  if(!all(is.na(act_country$fb_data.percent_mc))){
    max(act_country[!is.na(act_country$fb_data.percent_mc), "date"])
  }
})))

rf_dat_cl <- tdata_cl[(((tdata_cl$date >= "2020-02-28") & (tdata_cl$date <= rf_max_date))), 
                -which(colnames(tdata_cl) %in% c("year", "week", "country_code","testing_new_cases", "tests_done", "testing_population", 
                                                 "testing_rate", "testing_positivity_rate", "fb_data.iso_code","fb_data.country", "fb_status",
                                                 "fb_data.cli_se", "fb_data.percent_cli_unw","fb_data.cli_se_unw", "fb_data.sample_size_cli", 
                                                 "fb_data.smoothed_cli", "fb_data.smoothed_cli_se", "fb_data.sample_size_smoothed_cli",
                                                 "fb_data.mc_se", "fb_data.percent_mc_unw", "fb_data.mc_se_unw", "fb_data.sample_size_mc",
                                                 "fb_data.smoothed_mc", "fb_data.smoothed_mc_se", "fb_data.sample_size_smoothed_mc",
                                                 "fb_data.mc_se_dc", "fb_data.percent_dc_unw", "fb_data.dc_se_unw", "fb_data.sample_size_dc",               
                                                 "fb_data.smoothed_dc", "fb_data.smoothed_dc_se", "fb_data.sample_size_smoothed_dc",      
                                                 "iso_code",
                                                 "deaths_new", "recovered_new",
                                                 "fb_data.covid_se_cli" , "fb_data.pct_covid_unw_cli" , 
                                                 "fb_data.covid_se_unw_cli" , "fb_data.sample_size_cli" , "fb_data.smoothed_pct_covid_cli" , 
                                                 "fb_data.smoothed_covid_se_cli" , "fb_data.sample_size_smoothed_cli",
                                                 # From vaccination keep only "people_vaccinated_per_hundred" and new_people_vaccinated_smoothed_per_hundred
                                                 "total_vaccinations", "people_vaccinated", "people_fully_vaccinated", "new_vaccinations",
                                                 "new_vaccinations_smoothed", "total_vaccinations_per_hundred", "new_vaccinations_smoothed_per_million",
                                                 "new_people_vaccinated_smoothed", "people_fully_vaccinated_per_hundred",
                                                 # Remove direct contact because of missing values at the end
                                                 "fb_data.percent_dc"
                ))]


# Preprocess per cluster
# Split by country
rf_dat_cl_splitted <- split(rf_dat_cl, rf_dat_cl$country)

# In some countries only weekly reports about vaccination
# function to replace miccing vaccination values with the previous one
replace_na_with_prev <- function(country_dat){
  act_dat <- country_dat
  if(nrow(act_dat) > 0){
    for(i in 2:nrow(act_dat)){
      if(is.na(act_dat$people_vaccinated_per_hundred[i])){
        act_dat$people_vaccinated_per_hundred[i] <- act_dat$people_vaccinated_per_hundred[i-1]
      }
      #if(is.na(act_dat$people_fully_vaccinated_per_hundred[i])){
      #  act_dat$people_fully_vaccinated_per_hundred[i] <- act_dat$people_fully_vaccinated_per_hundred[i-1]
      #}
    }
  }
  return(act_dat)
}

rf_dat_cl_splitted <- lapply(rf_dat_cl_splitted, function(x) replace_na_with_prev(x))

rf_dat_cl_splitted <- lapply(rf_dat_cl_splitted, function(x){
  # cumulative cases_new
  x$cases_new[which(is.na(x$cases_new))] <- 0
  #x$cases_new_cum <- cumsum(x$cases_new)
  
  # Difference in new cases 14 days later and now
  #x$cases_new <- lead(x$cases_new, 14)/x$cases_new
  x$cases_new <- (x$cases_new/x$`Population size`) * 100
  x$cases_new <- lead(x$cases_new, 14) - x$cases_new
  
  # Giving more time for vaccination to show an effect (time for effect of vaccination and time for appear the case in the new_cases)
  x$people_vaccinated_per_hundred <- lag(x$people_vaccinated_per_hundred, 14)
  #x$people_fully_vaccinated_per_hundred <- lag(x$people_fully_vaccinated_per_hundred, 14)
  x$new_people_vaccinated_smoothed_per_hundred <- lag(x$new_people_vaccinated_smoothed_per_hundred, 14)
  
  # Data only from 2020.09.01
  #x <- x[which(x$date >= "2020-09-01"),]
  
  # If infinity (cases new is 0 or na) set to na
  x$cases_new[which(x$cases_new == Inf)] <- NA
  x$cases_new[which(is.infinite(x$cases_new))] <- NA
  
  # smooth cases_new and cumulative cases_new with rolling average window = 7 days
  #x$cases_new_cum <- rollapply(x$cases_new_cum, 7, mean, na.rm = TRUE, fill = NA)
  x$cases_new<- rollapply(x$cases_new, 7, mean, na.rm = TRUE, fill = NA)
  
  # Variable for number of cases on previous day and week
  #x$last_day <- lag(x$cases_new, 1)
  #x$last_week <- lag(x$cases_new, 21)
  
  # Smooth deaths, recovered, temperature, fb and vaccination variables with rolling average window = 7 days
  vars_to_smooth <- c("deaths_new", "recovered_new", "tavg", "fb_data.pct_covid_cli", "fb_data.percent_mc", "fb_data.percent_dc",
                      "people_vaccinated_per_hundred", "percent_variant.B.1.1.529",
                      "percent_variant.B.1.1.7", "percent_variant.B.1.617.2",
                      "percent_variant.Other", "percent_variant.P.1")
  
  
  x[, which(colnames(x) %in% vars_to_smooth)] <-
    lapply(x[, which(colnames(x) %in% vars_to_smooth)], function(y)  rollapply(y, 7, mean, na.rm = TRUE, fill = NA))
  
  
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
