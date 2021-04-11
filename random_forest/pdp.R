#Creates Partial Dependence Plots of the Random Forest train object for all countries (with FB data)

library(pdp)


#Creates the RF train objects and plots pdps per countries
pdp_plots <- lapply(c_rf_dat_fb, function(x) pdp_plots(x))

pdp_plots


