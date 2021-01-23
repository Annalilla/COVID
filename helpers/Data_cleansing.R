# Response measures

# Labels
# Response labels: https://www.ecdc.europa.eu/sites/default/files/documents/Variable_Dictionary_and_Disclaimer_non-pharmacautical_measures_v3.pdf
# Download: 01.20.2021
resp_labels <- "StayHomeOrder= Stay-at-home orders for the general population (these are enforced and also referred to as 'lockdown')RegionalStayHomeOrder = Regional stay-at-home orders for the general populationat least in one region(these are enforced and also referred to as 'lockdown')StayHomeGen= Stay-at-home recommendations for the general population (which are voluntary or not enforced)StayHomeRiskG= Stay-at-home recommendations for risk groups or vulnerable populations (such as the elderly, people with underlying health conditions, physically disabled people, etc.)SocialCircle = Social circle/bubble to limit social contacts e.g. to limited number of householdsPrivateGatheringRestrictions= Restrictions on private gatheringsClosDaycare= Closure of educational institutions: daycare or nursery.ClosPrim= Closure of educational institutions: primary schools.ClosSec= Closure of educational institutions: secondary schools.ClosHigh= Closure of educational institutions: higher education.
MassGatherAll= Interventions are in place to limit mass/public gatherings (any interventions on mass gatherings up to 1000 participants included)BanOnAllEvents = Interventions are in place to limit all indoor/outdoor mass/public gatheringsIndoorOver50= Interventions are in place to limit indoor mass/public gatherings of over 50participantsIndoorOver100= Interventions are in place to limit indoor mass/public gatherings of over 100participantsIndoorOver500= Interventions are in place to limit indoor mass/public gatherings of over 500participantsIndoorOver1000= Interventions are in place to limit indoor mass/public gatherings of over 1000participantsOutdoorOver50 = Interventions are in place to limit outdoor mass/public gatherings of over 50participantsOutdoorOver100 = Interventions are in place to limit outdoor mass/public gatherings of over 100participantsOutdoorOver500 = Interventions are in place to limit outdoormass/public gatherings of over 500participantsOutdoorOver1000 = Interventions are in place to limit outdoor mass/public gatherings of over 1000participantsClosPubAny= Closure of public spaces of any kind (including restaurants, entertainment venues, non-essential shops, partial or full closure of public transport, gyms and sport centers, etc).EntertainmentVenues = Closure of entertainment venuesClosureOfPublicTransport = Closure of public transportGymsSportsCentres = Closure of gyms/sports centresHotelsAccommodation = Closure of hotels/accommodation servicesNonEssentialShops = Closures of non-essential shopsPlaceOfWorship = Closure of places of worshipRestaurantsCafes = Closure of restaurants and cafes/barsMasksVoluntaryAllSpaces= Protective mask use in all public spaceson voluntary basis (general recommendation not enforced)MasksVoluntaryClosedSpaces= Protective mask use in closed public spaces/transport on voluntary basis (general recommendation not enforced)MasksMandatoryAllSpaces= Protective mask use in all public spaces on mandatory basis (enforced by law)MasksMandatoryClosedSpaces= Protective mask use in closedpublic spaces/transport on mandatory basis (enforced by law)Teleworking= Teleworking recommendationAdaptationOfWorkplace = Adaptationof workplaces(e.g. to reduce risk of transmission)WorkplaceClosures = Closures of workplacesStayHomeOrderPartial= Stay-at-home orders for the general population (these are enforced and also referred to as 'lockdown') -partially relaxed measure
RegionalStayHomeOrderPartial= Regional stay-at-home orders for the general population at least in one region (these are enforced and also referred to as 'lockdown')-partially relaxed measureStayHomeGenPartial= Stay-at-home recommendations for the general population (which are voluntary or not enforced) -partially relaxed measureStayHomeRiskGPartial= Stay-at-home recommendations for risk groups or vulnerable populations (such as the elderly, people with underlying health conditions, physically disabled people, etc.) -partially relaxed measureSocialCirclePartial= Social circle/bubble to limit social contacts e.g. to limited number of households-partially relaxed measurePrivateGatheringRestrictionsPartial= Restrictions on private gatherings-partially relaxed measureClosDaycarePartial= Closure of educational institutions: daycare or nursery -partially relaxed measureClosPrimPartial= Closure of educational institutions: primary schools -partially relaxed measureClosSecPartial= Closure of educational institutions: secondary schools -partially relaxed measureClosHighPartial= Closure of educational institutions: higher education -partially relaxed measureMassGatherAllPartial = Interventions are in place to limit mass/public gatherings (any interventions on mass gatherings up to 1000 participants included)-partially relaxed measureBanOnAllEventsPartial= Interventions are in place to limit all indoor/outdoor mass/public gatherings-partially relaxed measureIndoorOver50Partial= Interventions are in place to limit indoor mass/public gatherings of over 50participants-partially relaxed measureIndoorOver100Partial= Interventions are in place to limit indoor mass/public gatherings of over 100participants-partially relaxed measureIndoorOver500Partial= Interventions are in place to limit indoor mass/public gatherings of over 500participants-partially relaxed measureIndoorOver1000Partial= Interventions are in place to limit indoor mass/public gatherings of over 1000participants-partially relaxed measureOutdoorOver50Partial= Interventions are in place to limit outdoor mass/public gatherings of over 50participants-partially relaxed measureOutdoorOver100Partial= Interventions are in place to limit outdoor mass/public gatherings of over 100participants-partially relaxed measureOutdoorOver500Partial= Interventions are in place to limit outdoor mass/public gatherings of over 500participants-partially relaxed measureOutdoorOver1000Partial= Interventions are in place to limit outdoor mass/public gatherings of over 1000participants-partially relaxed measureClosPubAnyPartial= Closure of public spaces of any kind (including restaurants, entertainment venues, non-essential shops, partial orfull closure of public transport, gyms and sport centers etc) -partially relaxed measureEntertainmentVenuesPartial= Closure of entertainment venues-partially relaxed measureClosureOfPublicTransportPartial= Closure of public transport-partially relaxed measure
GymsSportsCentresPartial= Closure of gyms/sports centres-partially relaxed measureHotelsAccommodationPartial= Closure of hotels/accommodation services-partially relaxed measureNonEssentialShopsPartial= Closures of non-essential shops -partially relaxed measurePlaceOfWorshipPartial= Closure of places of worship-partially relaxed measureRestaurantsCafesPartial= Closure of restaurants and cafes/bars-partially relaxed measureMasksVoluntaryAllSpacesPartial= Protective mask use in all public spaces on voluntary basis (general recommendation not enforced)-partially relaxed measureMasksVoluntaryClosedSpacesPartial= Protective mask use in closed public spaces/transport on voluntary basis (general recommendation not enforced)-partially relaxed measureMasksMandatoryAllSpacesPartial= Protective mask use in all public spaces on mandatory basis (enforced by law)-partially relaxed measureMasksMandatoryClosedSpacesPartial= Protective mask use in closed public spaces/transport on mandatory basis (enforced by law)-partially relaxed measureTeleworkingPartial= Teleworking recommendation or workplace closures -partially relaxed measureAdaptationOfWorkplacePartial= Adaptation of workplaces (e.g. to reduce risk of transmission)-partially relaxed measureWorkplaceClosuresPartial= Closures of workplaces-partially relaxed measure"

#resp_vars <- unlist(str_extract_all(resp_labels, "[A-Z]{1}[a-zA]+[A-Z]{1}[a-zA-Z0-9]+"))
resp_vars_1 <- unlist(str_extract_all(resp_labels, "[A-Z]{1}[a-zA-Z0-9 ]+="))
resp_vars_1 <- unlist(str_replace_all(resp_vars_1, "[= ]", ""))
resp_vars_2 <- unlist(str_extract_all(resp_labels, "[A-Z]{1}[a-zA]+[A-Z]{1}[a-zA-Z0-9]+"))
resp_vars_2 <- unlist(str_replace_all(resp_vars_2, "[ ]", ""))
resp_vars <- unique(c(resp_vars_1, resp_vars_2))

for(i in 1:length(resp_vars))
{
  resp_labels <- str_replace(resp_labels, resp_vars[i], paste("---", resp_vars[i], sep = ""))
}
rows <- unlist(str_split(resp_labels, "---"))
rows <- str_split(rows, "= ")
rows <- rows[-1]
dicti <- as.data.frame(do.call(rbind, rows))
colnames(dicti) <- c("Variable", "Definition")
dicti$Variable <- str_replace_all(dicti$Variable, " ", "")

# Controll is all response variables are in the dictionary (Output: "Country" "date"    "year"    "week")
colnames(response)[which(!(colnames(response)) %in% dicti$Variable)]

dicti <- dicti[order(dicti$Variable),]

# Save dictionary for documentation
write.csv2(dicti, "data/dictionary_response_measures.csv", quote = FALSE, row.names = FALSE)

#
# Temperature

# Missing values
length(which(is.na(tempavg$tavg)))
tempavg[which(is.na(tempavg$tavg)),]

#
# Vaccination
vaccination[rowSums(is.na(vaccination)) > 0,]
vaccination[which(is.na(vaccination$new_vaccinations_smoothed)),]
