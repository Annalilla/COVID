# Functions to edit and process the data for RF modelling within clusters, gives RF estaimates for Partial Dependence Plots and calculates repeated feature importance for countries


## Merging restrictions (partials with not partials)
# Function to change value of selected restriction to 1, is the same restriction but as partial was applied
merge_partial_rest <- function(rest){
  part <- paste(rest, "Partial", sep = "")
  partial_rows <- which(dat[,which(colnames(dat) == part)] == 1)
  dat[partial_rows, which(colnames(dat) == rest)] <- 1
  return(dat[,which(colnames(dat) == rest)])
}

# Merge all restriction in the dataset
merge_all_partial_rest <- function(dat){
  part_vars <- colnames(dat)[grepl("Partial", colnames(dat))]
  
  #Pair of these restrictions
  part_pairs <- str_replace_all(part_vars, "Partial", "")
  
  dat <- as.data.frame(dat)
  
  dat[, part_pairs] <- do.call(cbind, lapply(part_pairs, function(x) merge_partial_rest(x)))
  dat <- dat[, -which(colnames(dat) %in% part_vars)]
  return(dat)
}


###Vaccination data: change NAs to 0 after vaccination started
replace_na_after_first_vacc <- function(dat){
  first_date <- dat %>%
    group_by(country) %>%
    dplyr::summarise(first = min(date))
  dat <- merge(dat, first_date, by = "country", all.x = TRUE)
  dat[which(dat$date > dat$first),which(colnames(dat)%in% vacc)] <- dat[which(dat$date > dat$first), which(colnames(dat)%in% vacc)] %>% 
    replace(is.na(.), 0)
  dat <- subset(dat, select = -first)
  return(dat)
}
  
## Standardize predictors

preproc_predict <- function(rf_country_dat){
  
  preProcValues <- preProcess(rf_country_dat[, -which(colnames(rf_country_dat) %in% c("cases_new", "cases_new_cum","date", "year", "week"))],
                              method = c("center", "scale"))
  
  act_rf_dat <- predict(preProcValues, newdata = rf_country_dat)
  return(act_rf_dat)
}

# Function to cut interval for countries to be dividable with horizon+window
cut_time_interval_country <- function(country_dat, wind, hori){
  # Cut interval to be dividable with horizon+window
  interv <- wind + hori
  int_l <- nrow(country_dat)
  to_cut <- int_l %% interv
  country_dat <- country_dat[-c(1:to_cut),]

  return(country_dat)
}
                                             
## Random Forest for PDP input

rf_model_pdp <- function(country_dat){
  country <- country_dat$country[1]
  rf_dat_t <- country_dat[complete.cases(country_dat),]
  rf_dat_t <- cut_time_interval_country(rf_dat_t, 28, 5)
  
  # Train and test set
  set.seed(9985)
  #For timeslices no need to split data into train/test set before the 'train' function.
  #to_train <- createDataPartition(rf_dat_t$cases_new_cum,
  #                               p = .8,
  #                                list = FALSE,
  #                                times = 1)
  
  #rf_train <- rf_dat_t[to_train,]
  #rf_test <- rf_dat_t[-to_train,]
  
  ctrl <- trainControl(method = "timeslice",
                       initialWindow = 28,
                       horizon = 5,
                       fixedWindow = TRUE)
  
  grid <- expand.grid(mtry = c(round(sqrt(ncol(rf_dat_t))),
                               round(log(ncol(rf_dat_t)))))
  
  rf <- caret::train(as.numeric(cases_new_cum) ~ .,
              data = rf_dat_t[,-which(colnames(rf_dat_t) %in% c("country", "country_code", "cases_new", "date", "last_day", "last_week"))],
              method = "rf",
              trControl = ctrl,
              tuneGrid = grid)
  
  
  ## create iml predictor for repeated variable importance to get more robust results
  
  #create features data (without outcome cases_new_cum)
  #feat <- rf_dat_t[,-which(colnames(rf_dat_t) %in% c("cases_new_cum", "country", "country_code", "cases_new", "date", "last_day", "last_week"))]
  #predictor <- iml::Predictor$new(model = rf, data = feat, y = as.numeric(rf_dat_t$cases_new_cum))
  # calculate feature importance with repetitions
  #permimp <- iml::FeatureImp$new(predictor, loss = "mae", compare = "ratio", n.repetitions = 5)
  
  #rep_res <- permimp$results[, c("feature", "importance")]
  
  rf1 <- rf$finalModel
  
  return(rf1)
}

## Random Forest resulting with repeated variable importance

rf_model_varimp <- function(country_dat){
  country <- country_dat$country[1]
  rf_dat_t <- country_dat[complete.cases(country_dat),]
  rf_dat_t <- cut_time_interval_country(rf_dat_t, 28, 5)
  
  # Train and test set
  set.seed(9985)
  #For timeslices no need to split data into train/test set before the 'train' function.
  #to_train <- createDataPartition(rf_dat_t$cases_new_cum,
  #                               p = .8,
  #                                list = FALSE,
  #                                times = 1)
  
  #rf_train <- rf_dat_t[to_train,]
  #rf_test <- rf_dat_t[-to_train,]
  
  ctrl <- trainControl(method = "timeslice",
                       initialWindow = 28,
                       horizon = 5,
                       fixedWindow = TRUE)
  
  grid <- expand.grid(mtry = c(round(sqrt(ncol(rf_dat_t))),
                               round(log(ncol(rf_dat_t)))))
  
  rf <- caret::train(as.numeric(cases_new_cum) ~ .,
                     data = rf_dat_t[,-which(colnames(rf_dat_t) %in% c("country", "country_code", "cases_new", "date", "last_day", "last_week"))],
                     method = "rf",
                     trControl = ctrl,
                     tuneGrid = grid)
  
  
  ## create iml predictor for repeated variable importance to get more robust results
  
  #create features data (without outcome cases_new_cum)
  feat <- rf_dat_t[,-which(colnames(rf_dat_t) %in% c("cases_new_cum", "country", "country_code", "cases_new", "date", "last_day", "last_week"))]
  predictor <- iml::Predictor$new(model = rf, data = feat, y = as.numeric(rf_dat_t$cases_new_cum))
  # calculate feature importance with repetitions
  permimp <- iml::FeatureImp$new(predictor, loss = "mae", compare = "ratio", n.repetitions = 5)
  
  rep_res <- permimp$results[, c("feature", "importance")]
  
  #rf1 <- rf$finalModel
  
  return(rep_res)
}
