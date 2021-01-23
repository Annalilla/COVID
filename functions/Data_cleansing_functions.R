library(Hmisc)

clean_testing <- function(test_dat)
{
  #Removing columns
  test_dat <- subset(test_dat, select = c(country, country_code, testing_new_cases, tests_done, testing_population, testing_rate,
                                          testing_positivity_rate, year, week))
  
  return(test_dat)
}

clean_weather <- function(weath_data)
{
  # Temperature is in tenths of degrees of Celsius -> calculating to Celsius
  weath_data$tavg <- weath_data$tavg/10
  
  return(weath_data)
}
