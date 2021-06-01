## Create a time-varying dbase including clusters, then computes repeated variable importance for each cluster. 
#The result is the input for the rank correlation of the 'Country-char vs.varimp' tab.


library(caret)
library(ranger)
library(iml)
library(zoo)
library(randomForest)
library(Hmisc)
library(readr)
library(dplyr)
library(tidyverse)
library(stringr)
library(sjlabelled)


##Create a time-varying dbase with clusters

source("functions/Get_data.R")
tdata <- get_data("tdata", local = FALSE)
tdata$country_code<- as.factor(tdata$country_code)


clust_dat <- readRDS('shinydashboard/dat/clust_dat.rds')


row.names(clust_dat) <- c("AT", "BE", "BG", "CZ", "DE", "DK", "GR", "ES", "FI", "FR", "HR", "HU", "IE", "IT", "NL", "POL", "PT" ,
                          "RO", "SE", "SI", "SK")  
clust_dat$country_code <- rownames(clust_dat)

tdata <- as.data.frame(tdata)


#merge cluster membership and tdata

tdata_cl <- merge(tdata, clust_dat, by.x="country_code", by.y="country_code")

tdata_cl <- tdata_cl[order(tdata_cl$country, tdata_cl$date),]

#save for cluster_based_RF

saveRDS(tdata_cl, 'shinydashboard/dat/tdata_cl.RDS')



##Run RF on clusters

# Change variable types (to numerical, factor or date)

# tdata_cl
vars_to_numerical <- c("testing_rate", "testing_positivity_rate", "tavg", "total_vaccinations", "people_vaccinated",
                       "people_fully_vaccinated", "new_vaccinations", "new_vaccinations_smoothed", "total_vaccinations_per_hundred",
                       "people_vaccinated_per_hundred", "people_fully_vaccinated_per_hundred", "new_vaccinations_smoothed_per_million",
                       "AdaptationOfWorkplace", "AdaptationOfWorkplacePartial", "BanOnAllEvents", "BanOnAllEventsPartial",
                       "ClosDaycare", "ClosDaycarePartial", "ClosHigh", "ClosHighPartial", "ClosPrim", "ClosPrimPartial",
                       "ClosPubAny", "ClosPubAnyPartial", "ClosSec", "ClosSecPartial", "ClosureOfPublicTransport",
                       "ClosureOfPublicTransportPartial", "EntertainmentVenues", "EntertainmentVenuesPartial",
                       "GymsSportsCentres", "GymsSportsCentresPartial", "HotelsAccommodation", "HotelsAccommodationPartial",
                       "IndoorOver100", "IndoorOver1000", "IndoorOver1000Partial", "IndoorOver100Partial", "IndoorOver50",
                       "IndoorOver500", "IndoorOver500Partial", "IndoorOver50Partial", "MasksMandatoryAllSpaces",
                       "MasksMandatoryAllSpacesPartial", "MasksMandatoryClosedSpaces", "MasksMandatoryClosedSpacesPartial",
                       "MasksVoluntaryAllSpaces", "MasksVoluntaryAllSpacesPartial", "MasksVoluntaryClosedSpaces",
                       "MasksVoluntaryClosedSpacesPartial", "MassGatherAll", "MassGatherAllPartial", "NonEssentialShops",
                       "NonEssentialShopsPartial", "OutdoorOver100", "OutdoorOver1000", "OutdoorOver1000Partial",
                       "OutdoorOver100Partial", "OutdoorOver50", "OutdoorOver500", "OutdoorOver500Partial", "OutdoorOver50Partial",
                       "PlaceOfWorship", "PlaceOfWorshipPartial", "PrivateGatheringRestrictions",
                       "PrivateGatheringRestrictionsPartial", "RegionalStayHomeOrder", "RegionalStayHomeOrderPartial",
                       "RestaurantsCafes", "RestaurantsCafesPartial", "SocialCircle", "SocialCirclePartial", "StayHomeGen",
                       "StayHomeGenPartial", "StayHomeOrder", "StayHomeOrderPartial", "StayHomeRiskG", "StayHomeRiskGPartial",
                       "Teleworking", "TeleworkingPartial", "WorkplaceClosures", "WorkplaceClosuresPartial",
                       "cases_new", "deaths_new", "recovered_new",
                       "fb_data.percent_cli", "fb_data.cli_se", "fb_data.percent_cli_unw", "fb_data.cli_se_unw", "fb_data.sample_size_cli",
                       "fb_data.smoothed_cli", "fb_data.smoothed_cli_se", "fb_data.sample_size_smoothed_cli", "fb_data.percent_mc",
                       "fb_data.mc_se", "fb_data.percent_mc_unw", "fb_data.mc_se_unw", "fb_data.sample_size_mc", "fb_data.smoothed_mc", 
                       "fb_data.smoothed_mc_se", "fb_data.sample_size_mc_smoothed", "fb_data.percent_dc", "fb_data.mc_se_dc",
                       "fb_data.percent_dc_unw", "fb_data.dc_se_unw", "fb_data.sample_size_dc", "fb_data.smoothed_dc", "fb_data.smoothed_dc_se",
                       "fb_data.sample_size_dc_smoothed")


vars_to_factor <- c("country", "country_code", "iso_code", "fb_data.iso_code", "fb_data.country")

vars_to_date <- c("date")

# Convert variables to numerical
# Handling decimals
tdata_cl[, which(colnames(tdata_cl) %in% vars_to_numerical)] <-
  lapply(tdata_cl[, which(colnames(tdata_cl) %in% vars_to_numerical)], function(x) gsub(",", "\\.", x))
tdata_cl[, which(colnames(tdata_cl) %in% vars_to_numerical)] <-
  lapply(tdata_cl[, which(colnames(tdata_cl) %in% vars_to_numerical)], as.numeric)

# Convert variables to factor
tdata_cl[, which(colnames(tdata_cl) %in% vars_to_factor)] <-
  lapply(tdata_cl[, which(colnames(tdata_cl) %in% vars_to_factor)], as.factor)

# Convert variables to date
tdata_cl$date <- as.Date(tdata_cl$date)



source("functions/RF_cluster_functions.R")


###preprocessing

##Selecting the variables, defining the period with meaningful amount of data at the end of the period
rf_max_date <- min(as.Date(unlist(lapply(unique(tdata$country), function(x){
  act_country <- tdata[tdata$country == x,]
  if(!all(is.na(act_country$fb_data.percent_mc)) & !all(is.na(act_country$fb_data.percent_dc))){
    datevar <- act_country[!is.na(act_country$fb_data.percent_mc) & !is.na(act_country$fb_data.percent_dc), "date"]
    max(act_country[!is.na(act_country$fb_data.percent_mc) & !is.na(act_country$fb_data.percent_dc), "date"])
  }
}))))

rf_dat_cl <- tdata_cl[(((tdata_cl$date >= "2020-02-28") & (tdata_cl$date <= rf_max_date))), 
                -which(colnames(tdata) %in% c("year", "week", "country_code","testing_new_cases", "tests_done", "testing_population", 
                                              "testing_rate", "testing_positivity_rate", "fb_data.iso_code","fb_data.country", "fb_status",
                                              "fb_data.cli_se", "fb_data.percent_cli_unw","fb_data.cli_se_unw", "fb_data.sample_size_cli", 
                                              "fb_data.smoothed_cli", "fb_data.smoothed_cli_se", "fb_data.sample_size_smoothed_cli",
                                              "fb_data.mc_se", "fb_data.percent_mc_unw", "fb_data.mc_se_unw", "fb_data.sample_size_mc",
                                              "fb_data.smoothed_mc", "fb_data.smoothed_mc_se", "fb_data.sample_size_smoothed_mc",
                                              "fb_data.mc_se_dc", "fb_data.percent_dc_unw", "fb_data.dc_se_unw", "fb_data.sample_size_dc",               
                                              "fb_data.smoothed_dc", "fb_data.smoothed_dc_se", "fb_data.sample_size_smoothed_dc",      
                                              "new_vaccinations_smoothed", "iso_code", "people_vaccinated", "total_vaccinations", 
                                              "new_vaccinations_smoothed", "people_fully_vaccinated", "new_vaccinations_smoothed_per_million"))]



##Outcome variable

#cumulative cases_new
rf_dat_cl$cases_new_cum <- cumsum(rf_dat_cl$cases_new)

# Smooth cases_new and cumulative cases_new with rolling average window = 7 days
rf_dat_cl$cases_new_cum <- rollmean(rf_dat_cl$cases_new_cum, 7, fill = NA)
rf_dat_cl$cases_new<- rollmean(rf_dat_cl$cases_new, 7, fill = NA)


##Predictors

#vaccination data: change NAs to 0 after vaccination started

vacc <-c("new_vaccinations", "total_vaccinations_per_hundred", "people_vaccinated_per_hundred", "people_fully_vaccinated_per_hundred")

rf_dat_cl <- replace_na_after_first_vacc(rf_dat_cl) 



# Lead for new cases
rf_dat_cl$cases_new_lead <- lead(rf_dat_cl$cases_new, 14)

# Variable for number of cases on previous day and week
rf_dat_cl$last_day <- lag(rf_dat_cl$cases_new, 1)
rf_dat_cl$last_week <- lag(rf_dat_cl$cases_new, 7)

# Smooth deaths, recovered, temperature, fb and vaccination variables with rolling average window = 7 days

vars_to_smooth <- c("deaths_new", "recovered_new", "tavg", "fb_data.percent_cli", "fb_data.percent_mc", "fb_data.percent_dc", 
                    "new_vaccinations", "total_vaccinations_per_hundred", "people_vaccinated_per_hundred", 
                    "people_fully_vaccinated_per_hundred")


rf_dat_cl[, which(colnames(rf_dat_cl) %in% vars_to_smooth)] <-
  lapply(rf_dat_cl[, which(colnames(rf_dat_cl) %in% vars_to_smooth)], rollmean, 7, fill = NA)

rf_dat_cl$groups <- as.factor(rf_dat_cl$groups)

#
# Split by cluster 
cl_rf_dat <- split(rf_dat_cl, rf_dat_cl$groups)

#
#Standardize by clusters

rf_dat_cl <- lapply(cl_rf_dat, function(x) preproc_predict_cl(x))

#rf_dat_cl <- do.call(rbind, rf_dat_cl)

#find and remove highly correlated predictors

#make a correlation matrix of the numerical predictors

#cm <- cor(as.matrix(rf_dat_cl[, -which(colnames(rf_dat_cl) %in% c("cases_new", "cases_new_cum", "date", "year", "week",
#                                                            "country_code", "iso_code", "country", "groups"))]))
#summary(cm[upper.tri(cm)])

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-0.6500 -0.0728 -0.0124  0.0153  0.0866  0.8768    1200 

#highlyCorPred <-findCorrelation(cm, cutoff = 0.70)

#No high correlation, no need to remove any predictors

# Merging partially and not partially applied restrictions

#dat <- rf_dat_cl
#rf_dat_p <- merge_all_partial_rest(dat)

#rf_dat_cl$groups <- as.factor(rf_dat_cl$groups)


## Random Forest for all countries (with fb data) with timeslice, cumulative smoothed outcome, partial restrictions not merged, 
# with repeated variable importance

# Removing countries without fb data
#rf_dat_cl_fb <- rf_dat_cl[-which(rf_dat_cl$country %in% c("Cyprus", "Estonia", "Latvia", "Lithuania", "Luxembourg", "Malta")),]
# Refactor country
#rf_dat_cl_fb$country <- as.factor(as.character(rf_dat_cl_fb$country))

# Split by clusters
# cl_rf_dat_fb <-split(rf_dat_cl, rf_dat_cl$groups)

cl_rf_dat_fb <- rf_dat_cl

cluster_res_varimp <- lapply(cl_rf_dat_fb, function(x) rf_model_cl(x, iniwindow, hori))


#Format data for the 'Country-char vs.varimp' tab input
cluster_res_varimp <- do.call(rbind, cluster_res_varimp)

cluster_res_varimp$groups <- rownames(cluster_res_varimp)

cluster_res_varimp$groups <- substr(cluster_res_varimp$groups, 1, 1)        # Extract first characters (cluster numbers)


#Save results and further input for the Shiny visualization

dir.create("shinydashboard", showWarnings = FALSE)
dir.create("shinydashboard/dat", showWarnings = FALSE)

saveRDS(cluster_res_varimp, "shinydashboard/dat/cluster_res_varimp.RDS")
saveRDS(cl_rf_dat_fb, "shinydashboard/dat/cl_rf_dat_fb.RDS")
