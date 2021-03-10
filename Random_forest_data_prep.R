library(caret)
library(ranger)
library(iml)
library(zoo)

# Some countries are missing from fb data
mis_c <- c("Cyprus", "Estonia", "Latvia", "Lithuania", "Luxembourg", "Malta")
rf_dat <- tdata[(((tdata$date >= "2020-05-01") & (tdata$date <= "2021-02-28")) & tdata$country %nin% mis_c), -which(colnames(tdata) %in% c("testing_new_cases", "tests_done", "testing_population",
                                                                                                                                           "testing_rate", "testing_positivity_rate", "fb_data.iso_code",
                                                                                                                                           "fb_data.country", "fb_status"))]
# Test *******
# First only with a few variables and for Austria
rf_dat <- rf_dat[which(rf_dat$country == "Austria"), c("date", "cases_new", "ClosDaycare", "ClosPubAny", "StayHomeOrder",
                                                       "fb_data.percent_mc", "fb_data.percent_dc", "tavg")]
rf_dat <- rf_dat[complete.cases(rf_dat),]

# visualize
rf_dat %>% 
  ggplot(aes(date, cases_new)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Number of new COVID infections in Austria", x = "date", y = "number of cases")

# Train and test set
set.seed(9985)
to_train <- createDataPartition(rf_dat$cases_new,
                               p = .8,
                               list = FALSE,
                               times = 1)

rf_train <- rf_dat[to_train,]
rf_test <- rf_dat[-to_train,]

# RF
ctrl <- trainControl(method = "cv",
                     number = 10,
                     verboseIter = TRUE)

grid <- expand.grid(mtry = c(round(sqrt(ncol(rf_train))),
                             round(log(ncol(rf_train)))))

rf <- train(as.numeric(cases_new) ~ .,
            data = rf_train[,-which(colnames(rf_train) == "date")],
            method = "rf",
            trControl = ctrl,
            tuneGrid = grid)

rf1 <- rf$finalModel

# Interpretation
varImpPlot(rf1)
importance(rf1)
varImp(rf1)

rf1

# RF with timeslice
ctrl <- trainControl(method = "timeslice",
                     initialWindow = 28,
                     horizon = 1,
                     fixedWindow = TRUE)

rf <- train(as.numeric(cases_new) ~ .,
            data = rf_train[,-which(colnames(rf_train) == "date")],
            method = "rf",
            trControl = ctrl)

rf2 <- rf$finalModel

# Interpretation
varImpPlot(rf2)
importance(rf2)
varImp(rf2)

rf2

###########################
# With modified data

# Lead for new cases
rf_dat$cases_new <- lead(rf_dat$cases_new, 14)

# Smooth cases with rolling average window = 7 days
rf_dat$cases_new <- rollmean(rf_dat$cases_new, 7, fill = NA)

# Variable for number of cases on previous day and week
rf_dat$last_day <- lag(rf_dat$cases_new, 1)
rf_dat$last_week <- lag(rf_dat$cases_new, 7)

# Smooth temperature and fb variables with rolling average window = 7 days
rf_dat$tavg <- rollmean(rf_dat$tavg, 7, fill = NA)
rf_dat$fb_data.percent_mc <- rollmean(rf_dat$fb_data.percent_mc, 7, fill = NA)
rf_dat$fb_data.percent_dc <- rollmean(rf_dat$fb_data.percent_dc, 7, fill = NA)

# Missing values 
rf_dat <- rf_dat[complete.cases(rf_dat),]

######
# Train and test set
set.seed(9985)
to_train <- createDataPartition(rf_dat$cases_new,
                                p = .8,
                                list = FALSE,
                                times = 1)

rf_train <- rf_dat[to_train,]
rf_test <- rf_dat[-to_train,]

#RF
ctrl <- trainControl(method = "cv",
                     number = 10,
                     verboseIter = TRUE)

grid <- expand.grid(mtry = c(round(sqrt(ncol(rf_train))),
                             round(log(ncol(rf_train)))))

rf <- train(as.numeric(cases_new) ~ .,
            data = rf_train[,-which(colnames(rf_train) == "date")],
            method = "rf",
            trControl = ctrl,
            tuneGrid = grid)

rf3 <- rf$finalModel

# Interpretation
varImpPlot(rf3)
importance(rf3)
varImp(rf3)
rf3

# RF with timeslice and modified data
ctrl <- trainControl(method = "timeslice",
                     initialWindow = 28,
                     horizon = 1,
                     fixedWindow = TRUE)

rf <- train(as.numeric(cases_new) ~ .,
            data = rf_train[,-which(colnames(rf_train) == "date")],
            method = "rf",
            trControl = ctrl)

rf4 <- rf$finalModel

# Interpretation
varImpPlot(rf4)
importance(rf4)
varImp(rf4)

rf4
