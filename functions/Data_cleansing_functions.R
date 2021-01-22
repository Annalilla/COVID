library(Hmisc)

clean_reponse <- function(resp_dat){
  source("helpers/Selected_response_measures.R")
 
  # Variable types
  resp_dat <- as.data.frame(resp_dat)
  resp_dat$date <- as.Date(resp_dat$date)
  resp_dat$Country <- as.factor(resp_dat$Country)
  resp_dat[, -which(colnames(resp_dat) %in% c("date", "Country"))] <-
   lapply(resp_dat[, -which(colnames(resp_dat) %in% c("date", "Country"))], as.numeric)
 
  # Removing columns and adding labels
  resp_dat <- resp_dat[, which(colnames(resp_dat) %in% c("Country", "date", "year", "week", selected_measures))]
  label(resp_dat) <- as.list(response_labels[match(names(resp_dat), names(response_labels))])
 
  return(resp_dat)
}

clean_testing <- function(test_dat)
{
  #Removing columns
  test_dat <- subset(test_dat, select = c(country, country_code, testing_new_cases, tests_done, testing_population, testing_rate,
                                          testing_positivity_rate, year, week))
  
  # Variable types
  test_dat <- as.data.frame(test_dat)
  test_dat[, which(colnames(test_dat) %in% c("country", "country_code"))] <-
    lapply(test_dat[, which(colnames(test_dat) %in% c("country", "country_code"))], as.factor)
  test_dat[, which(colnames(test_dat) %in% c("year", "week"))] <-
    lapply(test_dat[, which(colnames(test_dat) %in% c("year", "week"))], as.numeric)
  
  # Labels
  testing_lab <- c(testing_new_cases = "Number of new confirmed cases", tests_done = "Number of tests done",
                   testing_rate = "Testing rate per 100 000 population", testing_positivity_rate = "Weekly test positivity (%)")
  label(test_dat) <- as.list(testing_lab[match(names(test_dat), names(testing_lab))])
  
  return(test_dat)
}

clean_weather <- function(weath_data)
{
  # Temperature is in tenths of degrees of Celsius -> calculating to Celsius
  weath_data$tavg <- weath_data$tavg/10
  label(weath_data$tavg) <- "Average temperature"
  
  # Variable types
  weath_data$country_code <- as.factor(weath_data$country_code)
  weath_data$date <- as.Date(weath_data$date)
  
  return(weath_data)
}

clean_vaccination <- function(vacc_dat){
  
  # Variable types
  vacc_dat <- as.data.frame(vacc_dat)
  vacc_dat[, which(colnames(vacc_dat) %in% c("iso_code", "country"))] <-
    lapply(vacc_dat[, which(colnames(vacc_dat) %in% c("iso_code", "country"))], as.factor)
  vacc_dat$date <- as.Date(vacc_dat$date)
  
  # Labels
  vacc_lab <- c(total_vaccinations = "total number of doses administered", new_vaccinations = "daily change in the total number of doses administered",
                new_vaccinations_smoothed = "new doses administered per day (7-day smoothed)",
                total_vaccinations_per_hundred = "people vaccinated per 100 people in the total population of the country",
                new_vaccinations_smoothed_per_million = "daily vaccinations per 1,000,000 people in the total population of the country")
  label(vacc_dat) <- as.list(vacc_lab[match(names(vacc_dat), names(vacc_lab))])
  
  return(vacc_dat)
}
