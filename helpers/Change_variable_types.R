# Change variable types (to numerical, factor or date)

# tdata
vars_to_numerical <- c("testing_rate", "testing_positivity_rate", "tavg", "total_vaccinations", "people_vaccinated",
                       "people_fully_vaccinated", "new_vaccinations", "new_vaccinations_smoothed", "total_vaccinations_per_hundred",
                       "people_vaccinated_per_hundred", "people_fully_vaccinated_per_hundred", "new_vaccinations_smoothed_per_million",
                       "AdaptationOfWorkplace", "AdaptationOfWorkplacePartial", "BanOnAllEvents", "BanOnAllEventsPartial",
                       "ClosDaycare", "ClosDaycarePartial", "ClosHigh", "ClosHighPartial", "ClosPrim", "ClosPrimPartial",
                       "ClosPubAny", "ClosPubAnyPartial", "ClosSec", "ClosSecPartial", "ClosureOfPublicTransport",
                       "ClosureOfPublicTransportPartial", "EntertainmentVenues", "EntertainmentVenuesPartial",
                       "GymsSportsCentres", "GymsSportsCentresPartial", "HotelsAccommodation", "HotelsAccommodationPartial",
                       "IndoorOver100", "IndoorOver1000", "IndoorOver1000Partial", "IndoorOver100Partial", "IndoorOver50",
                       "IndoorOver500", "IndoorOver500Partial", "IndoorOver50Partial", "MasksMandatoryAllSpaces",
                       "MasksMandatoryAllSpacesPartial", "MasksMandatoryClosedSpaces", "MasksMandatoryClosedSpacesPartial",
                       "MasksVoluntaryAllSpaces", "MasksVoluntaryAllSpacesPartial", "MasksVoluntaryClosedSpaces",
                       "MasksVoluntaryClosedSpacesPartial", "MassGatherAll", "MassGatherAllPartial", "NonEssentialShops",
                       "NonEssentialShopsPartial", "OutdoorOver100", "OutdoorOver1000", "OutdoorOver1000Partial",
                       "OutdoorOver100Partial", "OutdoorOver50", "OutdoorOver500", "OutdoorOver500Partial", "OutdoorOver50Partial",
                       "PlaceOfWorship", "PlaceOfWorshipPartial", "PrivateGatheringRestrictions",
                       "PrivateGatheringRestrictionsPartial", "RegionalStayHomeOrder", "RegionalStayHomeOrderPartial",
                       "RestaurantsCafes", "RestaurantsCafesPartial", "SocialCircle", "SocialCirclePartial", "StayHomeGen",
                       "StayHomeGenPartial", "StayHomeOrder", "StayHomeOrderPartial", "StayHomeRiskG", "StayHomeRiskGPartial",
                       "Teleworking", "TeleworkingPartial", "WorkplaceClosures", "WorkplaceClosuresPartial",
                       "cases_new", "deaths_new", "recovered_new",
                       "fb_data.percent_cli", "fb_data.cli_se", "fb_data.percent_cli_unw", "fb_data.cli_se_unw", "fb_data.sample_size_cli",
                       "fb_data.smoothed_cli", "fb_data.smoothed_cli_se", "fb_data.sample_size_smoothed_cli", "fb_data.percent_mc",
                       "fb_data.mc_se", "fb_data.percent_mc_unw", "fb_data.mc_se_unw", "fb_data.sample_size_mc", "fb_data.smoothed_mc", 
                       "fb_data.smoothed_mc_se", "fb_data.sample_size_mc_smoothed", "fb_data.percent_dc", "fb_data.mc_se_dc",
                       "fb_data.percent_dc_unw", "fb_data.dc_se_unw", "fb_data.sample_size_dc", "fb_data.smoothed_dc", "fb_data.smoothed_dc_se",
                       "fb_data.sample_size_dc_smoothed")
                       

vars_to_factor <- c("country", "country_code", "iso_code", "fb_data.iso_code", "fb_data.country")

vars_to_date <- c("date")

# Convert variables to numerical
# Handling decimals
tdata[, which(colnames(tdata) %in% vars_to_numerical)] <-
  lapply(tdata[, which(colnames(tdata) %in% vars_to_numerical)], function(x) gsub(",", "\\.", x))
tdata[, which(colnames(tdata) %in% vars_to_numerical)] <-
  lapply(tdata[, which(colnames(tdata) %in% vars_to_numerical)], as.numeric)

# Convert variables to factor
tdata[, which(colnames(tdata) %in% vars_to_factor)] <-
  lapply(tdata[, which(colnames(tdata) %in% vars_to_factor)], as.factor)

# Convert variables to date
tdata$date <- as.Date(tdata$date)
         
#
# country_char

colnames(country_char) <- gsub("\\.", "-", colnames(country_char))

vars_to_numerical_cc <- c("health_expenditures", "Total", "Y_LT5", "Y5-9", "Y10-14", "Y15-19", "Y20-24", "Y25-29", "Y30-34", "Y35-39", "Y40-44",
                       "Y45-49", "Y50-54", "Y55-59", "Y60-64", "Y65-69", "Y70-74", "Y75-79", "Y_GE75",
                       "Y80-84", "Y_GE80", "Y_GE85", "T", "M", "F",
                      
                       "health_expenditure",
                       
                       "cult_Y_GE16", "cult_Y_GE75", "cult_Y16-24", "cult_Y16-29", "cult_Y25-34", "cult_Y25-64",
                       "cult_Y35-49", "cult_Y50-64", "cult_Y65-74")

vars_to_factor_cc <- c("geo")

# Convert variables to numerical
# Handling decimals
country_char[, which(colnames(country_char) %in% vars_to_numerical_cc)] <-
  lapply(country_char[, which(colnames(country_char) %in% vars_to_numerical_cc)], function(x) gsub(",", "\\.", x))
country_char[, which(colnames(country_char) %in% vars_to_numerical_cc)] <-
  lapply(country_char[, which(colnames(country_char) %in% vars_to_numerical_cc)], as.numeric)

# Convert variables to factor
country_char[, which(colnames(country_char) %in% vars_to_factor_cc)] <-
  as.factor(country_char[, which(colnames(country_char) %in% vars_to_factor_cc)])
         
    
