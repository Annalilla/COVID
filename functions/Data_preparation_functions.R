#Response measurements
# Get active days for response measurements
days_in_interval <- function(start, end){
  start_date <- mdy(format(start, format = "%m-%d-%Y"))
  end_date <- mdy(format(end, format = "%m-%d-%Y"))
  x<-seq(start_date, end_date, by = "days")
  paste(x, collapse = ", ")
}

# Response measurements - data preparation
prepare_response <- function(resp_dat, rangefrom = NA){
  resp_dat[which(is.na(resp_dat$date_end)), "date_end"] <- maxdate
  resp_dat <- resp_dat[with(resp_dat, order(Country, Response_measure)),]
  
  days <- mapply(days_in_interval, resp_dat$date_start, resp_dat$date_end)
  resp_dat <- cbind(resp_dat, days)
  
  # Measurements occurring more than once
  dupl <- sort(which(duplicated(resp_dat[,c("Country", "Response_measure")]) |
                       duplicated(resp_dat[,c("Country", "Response_measure")], fromLast = TRUE)), decreasing = TRUE)
  to_keep <- NULL
  
  for(i in 2:length(dupl)){
    if(!(resp_dat[dupl[i]]$Country == resp_dat[dupl[i-1]]$Country &
         resp_dat[dupl[i]]$Response_measure == resp_dat[dupl[i-1]]$Response_measure)){
      to_keep <- c(to_keep, dupl[i-1])
    }else{
      resp_dat$days[dupl[i]] <- paste(resp_dat$days[dupl[i]], resp_dat$days[dupl[i-1]], sep = ", ")
    }
  }
  to_keep <- c(to_keep, dupl[i])
  
  to_drop <- dupl[-which(dupl %in% to_keep)]
  resp_dat <- resp_dat[-to_drop,]
  
  # Creating variables for days in the whole time interval
  days_start_end <- unlist(strsplit(days_in_interval(min(resp_dat$date_start), maxdate), ", "))
  m_days <- matrix(0L, ncol = length(days_start_end), nrow = nrow(resp_dat))
  colnames(m_days) <- days_start_end
  
  resp_dat <- cbind(resp_dat, m_days)
  
  # Mark active days with 1 for response measurements
  get_cols_with_date <- function(d){
    which(sapply(colnames(resp_dat), grepl, d))
  }
  
  for(i in 1:nrow(resp_dat)){
    resp_dat[i, get_cols_with_date(resp_dat$days[i])] <- 1
  }
  
  # Subset response
  resp_dat <- subset(resp_dat, select = -c(change, date_start, date_end, days))
  resp_dat <- split(resp_dat[, -1], f = resp_dat$Country)
  resp_dat <- sapply(resp_dat, t)
  resp_dat <- lapply(resp_dat, function(x) tail("colnames<-"(x, x[1, ]), -1))
  resp_dat <- Map(cbind, resp_dat, date = lapply(resp_dat, rownames))
  resp_dat <- rbindlist(lapply(resp_dat, as.data.table), use.names = TRUE, fill = TRUE, idcol = "Country")
  resp_dat[is.na(resp_dat)] <- 0
  
  # Adding year and week number (to merge later with the other data)
  resp_dat$year <- str_extract(resp_dat$date, "^\\d+")
  resp_dat$week <- strftime(resp_dat$date, format = "%V")
  
  # Filtering out rows from start date if is not requires
  if(!(is.na(rangefrom))){
    resp_dat <- filter(resp_dat, date >= rangefrom)
  }
   
  # Filtering out non-eu countries
  resp_dat <- resp_dat[which(resp_dat$Country %in% capitals$country),]
  
  return(resp_dat)
}

#
# Testing
prepare_testing <- function(test_dat, rangefrom = NA){
  test_dat$year <- str_extract(test_dat$year_week, "^\\d+")
  test_dat$week <- str_extract(test_dat$year_week, "\\d+$")
  test_dat <- subset(test_dat, select = -c(year_week))
  # Some variables are marked with "testing_" (to be identifiable after merging with other datasources)
  vars_to_mark <- colnames(test_dat)[-which(colnames(test_dat) %in%
                                             c("country", "country_code", "tests_done", "testing_rate",
                                               "testing_data_source", "year", "week"))] 
  colnames(test_dat)[which(colnames(test_dat) %in% vars_to_mark)] <-
    paste("testing",colnames(test_dat)[which(colnames(test_dat) %in% vars_to_mark)], sep = "_")
  
  # Filtering out rows from start date if is not requires
  if(!(is.na(rangefrom))){
    test_dat <- filter(test_dat, week >= rangefrom)
  }
  
  # Filtering out non-eu countries
  test_dat <- test_dat[which(test_dat$country %in% capitals$country),]
  
  return(test_dat)
}

#
# Weather
prepare_weather <- function(weat_dat){
  if(length(weat_dat) > 1 & nrow(weat_dat[[1]] > 0)){
      act_weat <- map_df(weat_dat, ~as.data.frame(.x), .id="geo")
      act_weat <- subset(act_weat, select = c("geo", "date", "tavg"))
      colnames(act_weat)[1] <- c("country_code")
      act_weat$date <- as.character(act_weat$date)
      return(act_weat)
  }
}

#
# Covid
prepare_covid <- function(cov_dat){
  cov_dat <- spread(cov_dat, key = data_type, value = cases)
  colnames(cov_dat)[1] <- "country"
  cov_dat$date <- as.character(cov_dat$date)
  return(cov_dat)
}

#
# fb
# Duplicates:        
#head(fb[,duplicated(t(fb)) |
#  duplicated(t(fb), fromLast = TRUE)])
prepare_fb <- function(fb_dat){
  fb_dat <- fb_dat[,-which(duplicated(t(fb_dat)))]
  
  # Mark with "fb_" (to be identifiable after merging with other datasources)
  colnames(fb_dat) <- paste("fb",colnames(fb_dat), sep = "_")
  
  # Add Country code in other form than iso2 and format date to prepare for merging with other data
  fb_dat <- merge(fb_dat, capitals[,c("country_code_iso", "country_code")], by.x = "fb_data.iso_code", by.y = "country_code_iso", all.x = TRUE)
  
  fb_dat$fb_data.survey_date <- paste(str_sub(fb_dat$fb_data.survey_date, 1, 4), str_sub(fb_dat$fb_data.survey_date, 5, 6),
                                  str_sub(fb_dat$fb_data.survey_date, 7, 8), sep = "-")
  
  colnames(fb_dat)[which(colnames(fb_dat) == "fb_data.survey_date")] <- "date"
  return(fb_dat)
}

