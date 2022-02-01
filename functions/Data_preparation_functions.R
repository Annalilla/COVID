# Contains functions used during the preparation and merge of the data

#Response measurements
# Get active days for response measurements
days_in_interval <- function(start, end){
  #start_date <- mdy(format(start, format = "%m-%d-%Y"))
  #end_date <- mdy(format(end, format = "%m-%d-%Y"))
  x <- seq(as.Date(start), as.Date(end), by = "days")
  paste(x, collapse = ", ")
}

# Response measurements - data preparation
# Response measurements - data preparation
prepare_response <- function(resp_dat, rangefrom = NA){
  resp_dat$date_end <- as.character(resp_dat$date_end)
  resp_dat[which(is.na(resp_dat$date_end)), "date_end"] <- as.character(maxdate)
  resp_dat <- resp_dat[with(resp_dat, order(Country, Response_measure)),]
  
  # Replace some values for consistency
  resp_dat$Response_measure[which(resp_dat$Response_measure %in% c("OutdoorOver5", "IndoorOver5"))] <- "PrivateGatheringRestrictions"
  resp_dat$Response_measure[which(resp_dat$Response_measure  == "HotelsOtherAccommodation")] <- "HotelsAccommodation"
  resp_dat$Response_measure[which(resp_dat$Response_measure  == "HotelsOtherAccommodationPartial")] <- "HotelsAccommodationPartial"
  resp_dat <- unique(resp_dat)
  
  days <- mapply(days_in_interval, resp_dat$date_start, resp_dat$date_end)
  resp_dat <- cbind(resp_dat, days)
  
  # Measurements occurring more than once
  resp_dat <- resp_dat[order(resp_dat[,c("Response_measure", "Country")])]
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
  resp_dat <- subset(resp_dat, select = -c(date_start, date_end, days))
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
  
  # Filtering out non-eu countries and subnational records
  test_dat <- test_dat[which((test_dat$country %in% capitals$country) & (test_dat$testing_level == "national")),]
  
  return(test_dat)
}

#
# Dominant variant
prepare_variant <- function(variant_dat){
  
  #select source of data
  
  source_GISAID <- c('Austria','Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czechia', 'Denmark', 'Estonia', 'Finland', 'France',
                     'Germany', 'Greece',
                     'Ireland', 'Italy', "Latvia", "Lithuania", "Luxembourg", "Malta", 'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia', 
                     'Spain', 'Sweden')
  source_TESSy <- c('Hungary')
  
  variant_gisaid <- variant_dat[variant_dat$country %in% source_GISAID & variant_dat$source == 'GISAID',]
  variant_tessy <-  variant_dat[variant_dat$country %in% source_TESSy & variant_dat$source == 'TESSy',]
  
  variant_dat <- rbind(variant_gisaid, variant_tessy)
  
  # Keep only variants that were dominant for at least 5 weeks in countries involved in the random forest
  rf_count<- c("Austria", "Belgium", "Bulgaria", "Croatia", "Czechia", "Denmark", "Finland",
               "France","Germany", "Greece",  "Hungary", "Ireland", "Italy", "Netherlands",
               "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
  common_vari <- variant_dat[variant_dat$country %in% rf_count] %>%
    group_by(country, year_week) %>%
    top_n(1, percent_variant) %>%
    group_by(variant) %>%
    dplyr::summarise(n = n())
  common_vari <- common_vari$variant[common_vari$n > 4]
  most_common <- paste("percent_variant.", common_vari, sep = "")
  
  variant_list <- split(variant_dat, variant_dat$country)
  
  format_variant <- function(var_dat){
    act_country <- var_dat$country[1]
    vari <- var_dat[,c("year_week", "variant", "percent_variant")]
    vari <- reshape(vari, idvar = "year_week", timevar = "variant", direction = "wide")
    vari$country = act_country
    year_week <- colsplit(vari$year_week, '-', names =  c('year','week'))
    vari <- cbind(vari, year_week)  
    vari$year <- as.character(vari$year)
    vari$week <- as.character(vari$week)
    vari <- as.data.frame(vari)
    vari <- vari[,-which(colnames(vari) == "year_week")]
    return(vari)
  }
  
  all_variants <- lapply(variant_list, function(x) format_variant(x))
  
  # Get most frequent variants for each country
  #common_vari <- lapply(all_variants, function(x){
  #  act_x <- as.data.frame(sort(colSums(x[,1:(ncol(x) - 3)], na.rm = TRUE), decreasing = TRUE)[1:10])
  #  return(rownames(act_x))
  #  })
  #intersect(common_va)
  #Reduce(intersect, common_vari)
  
  #common_vari <- do.call("c", common_vari)
  #sort(table(common_vari), decreasing = TRUE)
  
  # 10 most common variants are:
  # "percent_variant.B.1.1.7", "percent_variant.B.1.351", "percent_variant.Other", "percent_variant.B.1.1.529",
  #"percent_variant.B.1.617.2", "percent_variant.AY.4.2", " percent_variant.P.1", "percent_variant.B.1.525",
  #"percent_variant.B.1.1.7+E484K", "percent_variant.SGTF"
  
  # Keep only 10 most common variants
  #most_common <- c("percent_variant.B.1.1.7", "percent_variant.B.1.351", "percent_variant.Other", "percent_variant.B.1.1.529",
  #                 "percent_variant.B.1.617.2", "percent_variant.AY.4.2", "percent_variant.P.1", "percent_variant.B.1.525",
  #                 "percent_variant.B.1.1.7+E484K", "percent_variant.SGTF")
  
  
  y <- do.call("rbind", lapply(all_variants, function(x){
    x[,most_common[which(most_common %nin% colnames(x))]] <- 0
    x <- x[,c(most_common, "country", "year", "week")]
    return(x)
    }))
  
  # Replace na with 0
  y <- y %>% mutate_all(~replace_na(., 0))
  
  # Change variable name 'percent_variant.B.1.1.7+E484K' because '+' symbol can cause problems in the analysis
  #colnames(y)[which(colnames(y) == 'percent_variant.B.1.1.7+E484K')] <- "percent_variant.B.1.1.7_E484K"
  
  return(y)
}



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
                     
#
# Vaccination
# Replace NAs before the first vaccination in the country with 0
replace_na_before_first_vacc <- function(dat){
  nonmissing <- dat[which(rowSums(dat[, -c(1:3)], na.rm = TRUE) > 0),]
  first_date <- nonmissing %>%
    group_by(iso_code) %>%
    dplyr::summarise(first = min(date))
  dat <- merge(dat, first_date, by = "iso_code", all.x = TRUE)
  dat[which(dat$date < dat$first),] <- dat[which(dat$date < dat$first),] %>%
    replace(is.na(.), 0)
  dat <- subset(dat, select = -first)
  return(dat)
}

# Prepare vaccination
prepare_vaccination <- function(vaccine_dat){
  vaccine_dat <- vaccine_dat[which(vaccine_dat$iso_code %in% capitals$country_code_iso)]
  vaccine_cols <- which(grepl("vaccin", colnames(vaccine_dat)))
  vaccine_dat <- vaccine_dat %>%
    subset(select = c(iso_code, location, date, vaccine_cols))
  vaccine_dat <- replace_na_before_first_vacc(vaccine_dat)
  colnames(vaccine_dat)[which(colnames(vaccine_dat) == "location")] <- "country"
  vaccine_dat$date <- as.character(vaccine_dat$date)
  return(vaccine_dat)
} 


