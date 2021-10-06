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

preproc_predict_cl <- function(rf_cluster_dat){
  
  preProcValues <- preProcess(rf_cluster_dat[, -which(colnames(rf_cluster_dat) %in% c("cases_new", "cases_new_cum","date", "year", "week", "groups"))],
                              method = c("center", "scale"))
  
  act_rf_dat <- predict(preProcValues, newdata = rf_cluster_dat)
  return(act_rf_dat)
}

## Random Forest

rf_model_cl <- function(cluster_dat){
  cluster <- cluster_dat$cluster[1]
  rf_dat_t <- cluster_dat[complete.cases(cluster_dat),]
  
  # Train and test set
  set.seed(9985)
  ctrl <- trainControl(method = "timeslice",
                       initialWindow = 28,
                       horizon = 5,
                       fixedWindow = TRUE)
  
  grid <- expand.grid(mtry = c(round(sqrt(ncol(rf_dat_t))),
                               round(log(ncol(rf_dat_t)))))
  
  rf <- caret::train(as.numeric(cases_new) ~ .,
                     data = rf_dat_t[,-which(colnames(rf_dat_t) %in% c("country", "country_code", "groups", "cases_new_cum", "date",
                                                                       "Population size", "Males", "Health Expenditures", "Cultural participation",
                                                                       "Under 20", "20-39", "40-59", "60-79", "Above 80"    
                                                                       ))],
                     method = "rf",
                     trControl = ctrl,
                     tuneGrid = grid)
  
  
  ## create iml predictor for repeated variable importance to get more robust results
  
  #create features data (without outcome cases_new_cum)
  feat <- rf_dat_t[,-which(colnames(rf_dat_t) %in% c("cases_new_cum", "country", "country_code", "groups", "cases_new", "date",
                                                     "Population size", "Males", "Health Expenditures", "Cultural participation",
                                                     "Under 20", "20-39", "40-59", "60-79", "Above 80"    
                                                     ))]
  predictor <- iml::Predictor$new(model = rf, data = feat, y = as.numeric(rf_dat_t$cases_new))
  # calculate feature importance with repetitions
  permimp <- iml::FeatureImp$new(predictor, loss = "mae", compare = "ratio", n.repetitions = 5)
  
  rep_res_cl <- permimp$results[, c("feature", "importance")]
  
  #rf1 <- rf$finalModel
  
  return(rep_res_cl)
}

## Random Forest
# Function to cut interval for countries to be dividable with horizon+window
cut_time_interval <- function(cluster_dat, wind, hori){
  #cluster_dat <- cl_rf_dat_fb[[1]]
  # Length of intervals for countries within cluster
  cluster_dat <- split(cluster_dat, as.character(cluster_dat$country))
  #
  # Cut interval to be dividable with horizon+window
  interv <- wind + hori
  cluster_dat <- lapply(cluster_dat, function(x){
    int_l <- nrow(x)
    to_cut <- int_l %% interv
    x <- x[-c(1:to_cut),]
  })
  
  cluster_dat <- do.call(rbind, cluster_dat)
  return(cluster_dat)
}

rf_model_cl <- function(cluster_dat, wind, hori){
  cluster <- cluster_dat$groups[1]
  cluster_dat <- cluster_dat[complete.cases(cluster_dat),]
  rf_dat_t <- cut_time_interval(cluster_dat, wind, hori)
  # Train and test set
  set.seed(9985)
  ctrl <- trainControl(method = "timeslice",
                       initialWindow = wind,
                       horizon =hori,
                       fixedWindow = TRUE)
  
  grid <- expand.grid(mtry = c(round(sqrt(ncol(rf_dat_t))),
                               round(log(ncol(rf_dat_t)))))
  
  rf <- caret::train(as.numeric(cases_new) ~ .,
                     data = rf_dat_t[,-which(colnames(rf_dat_t) %in% c("country", "country_code", "groups", "cases_new_cum", "date",
                                                                       "Population size", "Males", "Health Expenditures", "Cultural participation",
                                                                       "Under 20", "20-39", "40-59", "60-79", "Above 80"    
                                                                       ))],
                     method = "rf",
                     trControl = ctrl,
                     tuneGrid = grid)
  
  
  ## create iml predictor for repeated variable importance to get more robust results
  
  #create features data (without outcome cases_new_cum)
  feat <- rf_dat_t[,-which(colnames(rf_dat_t) %in% c("cases_new_cum", "country", "country_code", "groups", "cases_new", "date",
                                                     "Population size", "Males", "Health Expenditures", "Cultural participation",
                                                     "Under 20", "20-39", "40-59", "60-79", "Above 80"    
                                                     ))]
  predictor <- iml::Predictor$new(model = rf, data = feat, y = as.numeric(rf_dat_t$cases_new))
  # calculate feature importance with repetitions
  permimp <- iml::FeatureImp$new(predictor, loss = "mae", compare = "ratio", n.repetitions = 5)
  
  rep_res_cl <- permimp$results[, c("feature", "importance")]
  
  #rf1 <- rf$finalModel
  
  return(rep_res_cl)
}
