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
                       "Teleworking", "TeleworkingPartial", "WorkplaceClosures", "WorkplaceClosuresPartial")
vars_to_factor <- c("country", "country_code", "iso_code")
vars_to_date <- c("date")

# Convert variables to numerical
tdata[, which(colnames(tdata) %in% vars_to_numerical)] <-
  lapply(tdata[, which(colnames(tdata) %in% vars_to_numerical)], as.numeric)

# Convert variables to factor
tdata[, which(colnames(tdata) %in% vars_to_factor)] <-
  lapply(tdata[, which(colnames(tdata) %in% vars_to_factor)], as.factor)

# Convert variables to date
tdata$date <- as.Date(tdata$date)
