library(caret)
library(ranger)
library(iml)
library(zoo)
library(randomForest)
library(Hmisc)
library(readr)
library(dplyr)
library(stringr)
library(sjlabelled)



setwd("C:/Users/balan/Documents/IPSDS/MDM/Master_Thesis/Model")
tdata<- read_rds("C:/Users/balan/Documents/IPSDS/MDM/Master_Thesis/Data/tdata_13_02.rds")

#source("functions/Data_process_functions.R")

###preprocessing


##Selecting the variables

# Some countries are missing from fb data
mis_c <- c("Cyprus", "Estonia", "Latvia", "Lithuania", "Luxembourg", "Malta")
rf_dat <- tdata[(((tdata$date >= "2020-05-01") & (tdata$date <= "2021-02-28")) & tdata$country %nin% mis_c), 
                -which(colnames(tdata) %in% c("testing_new_cases", "tests_done", "testing_population", 
                                              "testing_rate", "testing_positivity_rate", "fb_data.iso_code","fb_data.country", "fb_status"))]



##Outcome variable

#cumulative cases_new

rf_dat$cum_cases_new <- cumsum(rf_dat$cases_new)

label(rf_dat$cum_cases_new) <- "Cumulative confirmed daily new cases" 

length(which(rf_dat$cases_new <0)) #12

# Smooth cumulative cases_new with rolling average window = 7 days
rf_dat$cumra_cases_new <- rollmean(rf_dat$cum_cases_new, 7, fill = NA)

label(rf_dat$cumra_cases_new) <- "7-day rolling average of cumulative confirmed daily new cases" 

str(rf_dat$cumra_cases_new)
summary(rf_dat$cum_cases_new)

##Predictors

#Rescaling to [0,1]

#remove labels (preProcValues cannot handle labels, ruins factor and date variables)
pprf_dat <- remove_all_labels(rf_dat)
str(pprf_dat)

pprf_dat$country <- as.factor(pprf_dat$country)
pprf_dat$iso_code <-as.factor(pprf_dat$iso_code)
pprf_dat$date <-as.Date(pprf_dat$date)


preProcValues <- preProcess(pprf_dat[, -which(colnames(pprf_dat) %in% c("cases_new", "cum_cases_new", "cumra_cases_new", "date", "year", "week",
                                                                        "WorkplaceClosuresPartial", "country_code"))], method = "range")
preProcValues

transformed <- predict(preProcValues, newdata = pprf_dat )
head(transformed)


#find and remove highly correlated predictors

#make a correlation matrix of the numerical predictors

cm <- cor(as.matrix(transformed[, -which(colnames(transformed) %in% c("cases_new", "cum_cases_new", "cumra_cases_new", "date", "year", "week",
                                                                "WorkplaceClosuresPartial", "country_code", "iso_code", "country"))]))

str(cm)
summary(cm[upper.tri(cm)])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-0.6898 -0.0786 -0.0194  0.0174  0.1006  0.6698    2646 

#No high correlation, no need to remove any predictors
#highlyCorPred <-findCorrelation(cm, cutoff = 0.9)
#filtered_trans <- filtered_trans[,-highlyCorPred]

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

# rf1

#Call:
#  randomForest(x = x, y = y, mtry = param$mtry) 
#Type of random forest: regression
#Number of trees: 500
#No. of variables tried at each split: 2

#Mean of squared residuals: 1747538
#% Var explained: 52.47



# RF
ctrl <- trainControl(method = "cv",
                     number = 10,
                     verboseIter = TRUE)

grid <- expand.grid(mtry = c(round(sqrt(ncol(rf_train))),
                             round(log(ncol(rf_train)))))



rf <- train(as.numeric(cum_cases_new) ~ .,
            data = rf_train[,-which(colnames(rf_train) %in% c("date", "cases_new"))],
            method = "rf",
            trControl = ctrl,
            tuneGrid = grid)

rf1.1 <- rf$finalModel

# Interpretation
varImpPlot(rf1.1)
importance(rf1.1)
varImp(rf1.1)

rf1.1

#> rf1.1

#Call:
#  randomForest(x = x, y = y, mtry = param$mtry) 
#Type of random forest: regression
#Number of trees: 500
#No. of variables tried at each split: 3

#Mean of squared residuals: 2272740939
#% Var explained: 89.19

# RF with timeslice



myTimeControl <- trainControl(method = "timeslice", initialWindow = 36, horizon = 12, fixedWindow = TRUE)

plsFitTime <- train(unemploy ~ pce + pop + psavert, data = economics, method = "pls",
                    preProc = c("center", "scale"), trControl = myTimeControl)

A better example can be found here: https://r-norberg.blogspot.com/2016/08/data-splitting-time-slices-with.html. It include a function for irregular timeslices!
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

rf_dat <- tdata[(((tdata$date >= "2020-05-01") & (tdata$date <= "2021-02-28")) & tdata$country %nin% mis_c), -which(colnames(tdata) %in% c("testing_new_cases", "tests_done", "testing_population",
                                                                                                                                           "testing_rate", "testing_positivity_rate", "fb_data.iso_code",
                                                                                                                                           "fb_data.country", "fb_status"))]
rf_dat <- rf_dat[which(rf_dat$country == "Austria"), c("date", "cases_new", "ClosDaycare", "ClosPubAny", "StayHomeOrder",
                                                       "ClosDaycarePartial", "ClosPubAnyPartial", "StayHomeOrderPartial",
                                                       "fb_data.percent_mc", "fb_data.percent_dc", "tavg")]

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

# Merging partially and not partially applied restrictions
rf_dat <- merge_all_partial_rest(rf_dat)

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