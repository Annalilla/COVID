## Creates the train object per countries, then plots pdps for countries

pdp_plots <- function(country_dat){
  country <- country_dat$country[1]
  rf_dat_t <- country_dat[complete.cases(country_dat),]
  
  # Train and test set
  set.seed(9985)
  to_train <- createDataPartition(rf_dat_t$cases_new_cum,
                                  p = .8,
                                  list = FALSE,
                                  times = 1)
  
  rf_train <- rf_dat_t[to_train,]
  rf_test <- rf_dat_t[-to_train,]
  
  # RF
  ctrl <- trainControl(method = "timeslice",
                       initialWindow = 28,
                       horizon = 5,
                       fixedWindow = TRUE)
  
  grid <- expand.grid(mtry = c(round(sqrt(ncol(rf_train))),
                               round(log(ncol(rf_train)))))
  
  rf <- train(as.numeric(cases_new_cum) ~ .,
              data = rf_train[,-which(colnames(rf_train) %in% c( "cases_new", "date", "last_day", "last_week"))],
              method = "rf",
              trControl = ctrl,
              tuneGrid = grid)
  
  pdp_plots <- pdp::partial(rf, pred.var = "tavg", plot=TRUE, rug=TRUE)
  
  return(pdp_plots)
}




