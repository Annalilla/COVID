library(caret)
library(ranger)
library(iml)
library(zoo)
library(reshape2)
library(pcdpca)

# Some countries are missing from fb data
mis_c <- c("Cyprus", "Estonia", "Latvia", "Lithuania", "Luxembourg", "Malta")
rf_dat <- tdata[(((tdata$date >= "2020-05-01") & (tdata$date <= "2021-02-28")) & tdata$country %nin% mis_c), -which(colnames(tdata) %in% c("testing_new_cases", "tests_done", "testing_population",
                                                                                                                                           "testing_rate", "testing_positivity_rate", "fb_data.cli_se", "fb_data.percent_cli_unw",
                                                                                                                                           "fb_data.cli_se_unw", "fb_data.sample_size_cli", "fb_data.smoothed_cli_se",
                                                                                                                                           "fb_data.sample_size_smoothed_cli", "fb_data.percent_mc", "fb_data.mc_se",
                                                                                                                                           "fb_data.percent_mc_unw", "fb_data.mc_se_unw", "fb_data.sample_size_mc",               
                                                                                                                                           "fb_data.smoothed_mc_se", "fb_data.sample_size_mc_smoothed", "fb_data.percent_dc",
                                                                                                                                           "fb_data.mc_se_dc", "fb_data.percent_dc_unw", "fb_data.dc_se_unw", "fb_data.sample_size_dc",
                                                                                                                                           "fb_data.smoothed_dc_se", "fb_data.sample_size_dc_smoothed", "fb_data.iso_code",                     
                                                                                                                                           "fb_data.country", "fb_status", "fb_data.percent_cli", "week", "year"))]
# Test *******
# First only for Austria
rf_dat <- rf_dat[which(rf_dat$country == "Austria"),]
rf_dat <- rf_dat[complete.cases(rf_dat),]

# Only numerical
num_cols <- unlist(lapply(rf_dat, is.numeric))
pca_dat <- rf_dat[,num_cols]

# Remove explained variables
pca_dat <- pca_dat[, -(which(colnames(pca_dat) %in% c("cases_new", "deaths_new", "recovered_new")))]

# Normalize
pca_dat <- as.data.frame(scale(pca_dat))

# Remove not apllied restrictions
pca_dat <- pca_dat[, -which(is.na(colSums(pca_dat)))]

# Plot
to_plot <- cbind("id" = 1:nrow(pca_dat), pca_dat)
to_plot <- melt(to_plot, id = "id")
before <- to_plot %>%
  ggplot(aes(x = id, y = value, colour=variable)) +
  geom_line() +
  labs(title = "Before PCA")

pca <- prcomp(pca_dat)
pca_timeseries <- data.frame(as.matrix(pca_dat) %*% pca$rotation) 

# Plot
n_eigenv <- 20
to_plot <- cbind("id" = 1:nrow(pca_timeseries), pca_timeseries[,1:n_eigenv])
to_plot <- melt(to_plot, id = "id")
after <- to_plot %>%
  ggplot(aes(x = id, y = value, colour=variable)) +
  geom_line() +
  labs(title = "After PCA")
before
after
