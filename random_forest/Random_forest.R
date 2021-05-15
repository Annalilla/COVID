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


source("functions/Get_data.R")

tdata <- get_data("tdata", local = FALSE)

# Change variable types (to numerical, factor or date)

# tdata
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
tdata[, which(colnames(tdata) %in% vars_to_numerical)] <-
  lapply(tdata[, which(colnames(tdata) %in% vars_to_numerical)], function(x) gsub(",", "\\.", x))
tdata[, which(colnames(tdata) %in% vars_to_numerical)] <-
  lapply(tdata[, which(colnames(tdata) %in% vars_to_numerical)], as.numeric)

# Convert variables to factor
tdata[, which(colnames(tdata) %in% vars_to_factor)] <-
  lapply(tdata[, which(colnames(tdata) %in% vars_to_factor)], as.factor)

# Convert variables to date
tdata$date <- as.Date(tdata$date)



source("functions/RF_functions.R")


###preprocessing

##Selecting the variables, defining the period with meaningful amount of data at the end of the period

rf_dat <- tdata[(((tdata$date >= "2020-02-28") & (tdata$date <= "2021-03-28"))), 
                        -which(colnames(tdata) %in% c("year", "week", "country_code","testing_new_cases", "tests_done", "testing_population", 
                                              "testing_rate", "testing_positivity_rate", "fb_data.iso_code","fb_data.country", "fb_status",
                                               "fb_data.cli_se", "fb_data.percent_cli_unw","fb_data.cli_se_unw", "fb_data.sample_size_cli", 
                                              "fb_data.smoothed_cli", "fb_data.smoothed_cli_se", "fb_data.sample_size_smoothed_cli",
                                              "fb_data.mc_se", "fb_data.percent_mc_unw", "fb_data.mc_se_unw", "fb_data.sample_size_mc",
                                              "fb_data.smoothed_mc", "fb_data.smoothed_mc_se", "fb_data.sample_size_mc_smoothed",
                                              "fb_data.mc_se_dc", "fb_data.percent_dc_unw", "fb_data.dc_se_unw", "fb_data.sample_size_dc",               
                                              "fb_data.smoothed_dc", "fb_data.smoothed_dc_se", "fb_data.sample_size_dc_smoothed",      
                                              "new_vaccinations_smoothed", "iso_code", "people_vaccinated", "total_vaccinations", 
                                              "new_vaccinations_smoothed", "people_fully_vaccinated", "new_vaccinations_smoothed_per_million"))]
                


##Outcome variable

#cumulative cases_new
rf_dat$cases_new_cum <- cumsum(rf_dat$cases_new)

# Smooth cases_new and cumulative cases_new with rolling average window = 7 days
rf_dat$cases_new_cum <- rollmean(rf_dat$cases_new_cum, 7, fill = NA)
rf_dat$cases_new<- rollmean(rf_dat$cases_new, 7, fill = NA)


##Predictors

#vaccination data: change NAs to 0 after vaccination started

vacc <-c("new_vaccinations", "total_vaccinations_per_hundred", "people_vaccinated_per_hundred", "people_fully_vaccinated_per_hundred")

rf_dat <- replace_na_after_first_vacc(rf_dat) 



# Lead for new cases
rf_dat$cases_new_lead <- lead(rf_dat$cases_new, 14)

# Variable for number of cases on previous day and week
rf_dat$last_day <- lag(rf_dat$cases_new, 1)
rf_dat$last_week <- lag(rf_dat$cases_new, 7)

# Smooth deaths, recovered, temperature, fb and vaccination variables with rolling average window = 7 days

vars_to_smooth <- c("deaths_new", "recovered_new", "tavg", "fb_data.percent_cli", "fb_data.percent_mc", "fb_data.percent_dc", 
                    "new_vaccinations", "total_vaccinations_per_hundred", "people_vaccinated_per_hundred", 
                    "people_fully_vaccinated_per_hundred")


rf_dat[, which(colnames(rf_dat) %in% vars_to_smooth)] <-
  lapply(rf_dat[, which(colnames(rf_dat) %in% vars_to_smooth)], rollmean, 7, fill = NA)


#Standardize by countries

#IF data labelled: remove labels (preProcValues cannot handle labels, ruins factor and date variables)
#rf_dat <- remove_all_labels(rf_dat)

#rf_dat$country <- as.factor(rf_dat$country)
#rf_dat$date <-as.Date(rf_dat$date)

#Split by countries
c_rf_dat <- split(rf_dat, rf_dat$country)

#Standardize
rf_dat <- lapply(c_rf_dat, function(x) preproc_predict(x))
rf_dat <- do.call(rbind, rf_dat)

#find and remove highly correlated predictors

#make a correlation matrix of the numerical predictors

cm <- cor(as.matrix(rf_dat[, -which(colnames(rf_dat) %in% c("cases_new", "cases_new_cum", "date", "year", "week",
                                                                 "country_code", "iso_code", "country"))]))
summary(cm[upper.tri(cm)])

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-0.6416 -0.0562  0.0000  0.0217  0.0769  0.8517    3135 

#highlyCorPred <-findCorrelation(cm, cutoff = 0.70)

#No high correlation, no need to remove any predictors

# Merging partially and not partially applied restrictions

dat <- rf_dat
rf_dat_p <- merge_all_partial_rest(dat)



#### RF with timeslice, cumulative smoothed outcome, partial restrictions not merged

#data should be sorted by date (within country)!


# Only for Hungary
#rf_dat_t <- rf_dat[which(rf_dat$country == "Hungary"), ]
#rf_dat_t <- rf_dat_t[complete.cases(rf_dat_t),]

# Train and test set
#set.seed(9985)
#to_train <- createDataPartition(rf_dat_t$cases_new_cum,
#                                p = .8,
#                                list = FALSE,
#                                times = 1)

#rf_train <- rf_dat_t[to_train,]
#rf_test <- rf_dat_t[-to_train,]

# RF
#ctrl <- trainControl(method = "timeslice",
#                     initialWindow = 28,
#                     horizon = 5,
#                     fixedWindow = TRUE)

#grid <- expand.grid(mtry = c(round(sqrt(ncol(rf_train))),
#                             round(log(ncol(rf_train)))))

#rf <- train(as.numeric(cases_new_cum) ~ .,
#            data = rf_train[,-which(colnames(rf_train) %in% c("country", "cases_new", "date", "last_day", "last_week"))],
#            method = "rf",
#           trControl = ctrl,
#            tuneGrid = grid)


#rf1 <- rf$finalModel

# Interpretation
#varImpPlot(rf1)
#varImp(rf1)

#rf1

## All countries (with fb data)

# Removing countries without fb data
rf_dat_fb <- rf_dat[-which(rf_dat$country %in% c("Cyprus", "Estonia", "Latvia", "Lithuania", "Luxembourg", "Malta")),]
# Refactor country
rf_dat_fb$country <- as.factor(as.character(rf_dat_fb$country))

# Split by countries
c_rf_dat_fb <- c_rf_dat <- split(rf_dat_fb, rf_dat_fb$country)

country_res <- lapply(c_rf_dat_fb, function(x) rf_model(x))

dir.create("shinydashboard", showWarnings = FALSE)
dir.create("shinydashboard/dat", showWarnings = FALSE)
saveRDS(country_res, "shinydashboard/dat/country_res.RDS")
saveRDS(c_rf_dat_fb, "shinydashboard/dat/c_rf_dat_fb.RDS")


#p <- varImpPlot(country_res[[8]])
#p + title(names(country_res)[8])
