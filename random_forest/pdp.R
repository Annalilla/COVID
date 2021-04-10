#Creates Partial Dependence Plots of the Random Forest train object for all countries (with FB data)

library(pdp)

#Creates the RF train object
country_train <- lapply(c_rf_dat_fb, function(x) input_pdp(x))

#Creates pdp plots for all countries
country_pdp_plots <- lapply(country_train, function(x) plot_pdp(x))
