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

source("functions/RF_functions.R")


###preprocessing

##Selecting the variables, defining the period with meaningful amount of data at the end of the period

tdata <- as.data.frame(tdata)

# Maximum date: fb direct contact and mask coverage available in all countries
rf_max_date <- min(do.call(c, lapply(unique(tdata$country), function(x){
  act_country <- tdata[tdata$country == x,]
  if(!all(is.na(act_country$fb_data.percent_mc)) & !all(is.na(act_country$fb_data.percent_dc))){
    max(act_country[!is.na(act_country$fb_data.percent_mc) & !is.na(act_country$fb_data.percent_dc), "date"])
  }
})))

rf_dat <- tdata[(((tdata$date >= "2020-02-28") & (tdata$date <= rf_max_date))), 
                        -which(colnames(tdata) %in% c("year", "week", "country_code","testing_new_cases", "tests_done", "testing_population", 
                                                      "testing_rate", "testing_positivity_rate", "fb_data.iso_code","fb_data.country", "fb_status",
                                                      "fb_data.cli_se", "fb_data.percent_cli_unw","fb_data.cli_se_unw", "fb_data.sample_size_cli", 
                                                      "fb_data.smoothed_cli", "fb_data.smoothed_cli_se", "fb_data.sample_size_smoothed_cli",
                                                      "fb_data.mc_se", "fb_data.percent_mc_unw", "fb_data.mc_se_unw", "fb_data.sample_size_mc",
                                                      "fb_data.smoothed_mc", "fb_data.smoothed_mc_se", "fb_data.sample_size_smoothed_mc",
                                                      "fb_data.mc_se_dc", "fb_data.percent_dc_unw", "fb_data.dc_se_unw", "fb_data.sample_size_dc",               
                                                      "fb_data.smoothed_dc", "fb_data.smoothed_dc_se", "fb_data.sample_size_smoothed_dc",      
                                                      "new_vaccinations_smoothed", "iso_code", "people_vaccinated", "total_vaccinations", 
                                                      "new_vaccinations_smoothed", "people_fully_vaccinated", "new_vaccinations_smoothed_per_million",
                                                      "deaths_new", "recovered_new"))]


##Outcome variable

# Number of cases proportionate to population size
rf_dat$cases_new <- 100 * rf_dat$cases_new/rf_dat$`Population size`

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

#dat <- rf_dat
#rf_dat_p <- merge_all_partial_rest(dat)



### RF with timeslice, cumulative smoothed outcome, partial restrictions not merged, with REPEATED variable importance
#for more robust result.

#data should be sorted by date (within country)!


# Only for Hungary
#rf_dat_r <- rf_dat[which(rf_dat$country == "Hungary"), ]
#rf_dat_r <- rf_dat_r[complete.cases(rf_dat_r),]


#set.seed(9985)

# RF
#ctrl <- trainControl(method = "timeslice",
#                     initialWindow = 28,
#                     horizon = 5,
#                     fixedWindow = TRUE)

#grid <- expand.grid(mtry = c(round(sqrt(ncol(rf_dat_r))),
#                             round(log(ncol(rf_dat_r)))))

#rf_r <- train(as.numeric(cases_new_cum) ~ .,
#            data = rf_dat_r[,-which(colnames(rf_dat_r) %in% c("country", "cases_new", "date", "last_day", "last_week"))],
#            method = "rf",
#           trControl = ctrl,
#            tuneGrid = grid)

## create iml predictor

#create features data (without outcome cases_new_cum)
#feat <- rf_dat_r[,-which(colnames(rf_dat_r) %in% c("cases_new_cum", "country", "cases_new", "date", "last_day", "last_week"))]
#predictor <- Predictor$new(model = rf_r, data = feat, y = as.numeric(rf_dat_r$cases_new_cum))
# calculate feature importance
#permimp <- FeatureImp$new(predictor, loss = "mae", compare = "ratio", n.repetitions = 5)


#plot(permimp)
#permimp$results$
#permimp$results$importance
#permimp$results[, c("feature", "importance")]

#rf2 <- rf_r$finalModel


# Interpretation
#varImpPlot(rf2)
#varImp(rf2)

#rf2

## All countries (with fb data)

# Removing countries without fb data
rf_dat_fb <- rf_dat[-which(rf_dat$country %in% c("Cyprus", "Estonia", "Latvia", "Lithuania", "Luxembourg", "Malta")),]
# Refactor country
rf_dat_fb$country <- as.factor(as.character(rf_dat_fb$country))

# Split by countries
c_rf_dat_fb <- c_rf_dat <- split(rf_dat_fb, rf_dat_fb$country)

# Create Random Forest input for PDP tab
country_res <- lapply(c_rf_dat_fb, function(x) rf_model_pdp(x))

#Save results and further input for the Shiny visualization

dir.create("shinydashboard", showWarnings = FALSE)
dir.create("shinydashboard/dat", showWarnings = FALSE)

#saveRDS(country_res, "shinydashboard/dat/country_res.RDS")
saveRDS(c_rf_dat_fb, "shinydashboard/dat/c_rf_dat_fb.RDS")


#p <- varImpPlot(country_res[[8]])
#p + title(names(country_res)[8])


# Create Random Forest input (repeated varimp) for 'Bump Chart' and 'Country characteristics' tab
country_res_varimp <- lapply(c_rf_dat_fb, function(x) rf_model_varimp(x))

#Save results and further input for the Shiny visualization


saveRDS(country_res_varimp, "shinydashboard/dat/country_res_varimp.RDS")
