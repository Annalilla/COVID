#Creates pdp plots for all countries (with FB data), for one predictor

plot_pdp <-function(country_train) {
  train <- country_train$country[1]
  pdp_plots <- pdp::partial(rf, pred.var = "tavg", plot=TRUE)
  
  return(pdp_plots)
}
