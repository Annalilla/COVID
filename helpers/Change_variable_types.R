vars_to_numerical <- c("testing_rate", "testing_positivity_rate", "tavg", "total_vaccinations_per_hundred",
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
                       "fb_data.percent_cli", "fb_data.cli_se", "fb_data.percent_cli_unw", "fb_data.cli_se_unw","fb_data.sample_size",
                       "fb_data.percent_mc", "fb_data.mc_se.x", "fb_data.percent_mc_unw", "fb_data.mc_se_unw", "fb_data.sample_size_mc", 
                       "fb_data.percent_dc", "fb_data.mc_se.y","fb_data.percent_dc_unw", "fb_data.dc_se_unw", "fb_data.sample_size_dc")

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
