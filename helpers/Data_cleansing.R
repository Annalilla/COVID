# Explores the data to discover the necessary data cleansing steps

# country_char variables

#
# Population

# Whole population
which(is.na(total$values))
which(!(capitals$country_code_iso2 %in% total$geo))
summary(total[total$geo %in% capitals$country_code_iso2, "values"])

# Population by sex
which(is.na(sex$values))
which(!(capitals$country_code_iso2 %in% sex$geo))
summary(sex[sex$geo %in% capitals$country_code_iso2, "values"])

# Population by age
which(is.na(age$values))
which(!(capitals$country_code_iso2 %in% age$geo))
summary(age[age$geo %in% capitals$country_code_iso2, "values"])

# Population by age
which(is.na(age$values))
which(!(capitals$country_code_iso2 %in% age$geo))
summary(age[age$geo %in% capitals$country_code_iso2, "values"])



# Physicians

physicians$geo #data available for only 6 countries: "IE" "IS" "IT" "LI" "NO" "UK", so physicians will not be used.
#Inactivated in collect_data.R and Merge_data.R

# Health expenditures

str(health_expenditures_eurostat)

which(is.na(health_exp$geo))
which(is.na(health_exp$values))
which(is.na(health_exp$variable))

summary(values[health_exp$geo %in% capitals$country_code_iso2, "values"])

health_expenditures_eurostat$icha11_hc <-as.factor(health_expenditures_eurostat$icha11_hc)
levels(health_expenditures_eurostat$icha11_hc)

# Are levels additive, i.e. mutually exclusive? What is "Current health care expenditure (CHE)" ?

# https://ec.europa.eu/eurostat/cache/metadata/en/hlth_sha11_esms.htm :
#health care by function (ICHA-HC)
#Health care functions

#Healthcare functions relate to the type of need that current expenditure on healthcare aims to satisfy or the kind of objective pursued. The following main items are defined:
  
#  curative care, which means the healthcare services during which the principal intent is to relieve symptoms or to reduce the severity of an illness or injury, or to protect against its exacerbation or complication that could threaten life or normal function;
#rehabilitative care, which means the services to stabilise, improve or restore impaired body functions and structures, compensate for the absence or loss of body functions and structures, improve activities and participation and prevent impairments, medical complications and risks;
#inpatient care, which means the treatment and/or care provided in a healthcare facility to patients formally admitted and requiring an overnight stay;
#outpatient care, which means the medical and ancillary services delivered in a healthcare facility to a patient who is not formally admitted and does not stay overnight;
#day care, which means the planned medical and paramedical services delivered in a healthcare facility to patients who have been formally admitted for diagnosis, treatment or other types of healthcare and are discharged on the same day;
#long-term care (health), which means a range of medical and personal care services that are consumed with the primary goal of alleviating pain and suffering and reducing or managing the deterioration in health status in patients with a degree of long-term dependency. Main results and findings from a questionnaire submitted to countries on sources and methodology for long-term care spending can be found as an annex.
#home-based care, which means the medical, ancillary and nursing services that are consumed by patients at their home and involve the providers' physical presence;
#   ancillary services (non-specified by function), which means the healthcare or long-term care related services non-specified by function and non-specified by mode of provision, which the patient consumes directly, in particular during an independent contact with the health system and that are not integral part of a care service package, such as laboratory or imaging services or patient transportation and emergency rescue;
#   pharmaceuticals and other medical non-durable goods (non-specified by function), which means pharmaceutical products and non-durable medical goods intended for use in the diagnosis, cure, mitigation or treatment of disease, including prescribed medicines and over-the-counter drugs, where the function and mode of provision are not specified;
#   therapeutic appliances and other medical goods (non-specified by function), which means medical durable goods including orthotic devices that support or correct deformities and/or abnormalities of the human body, orthopaedic appliances, prostheses or artificial extensions that replace a missing body part, and other prosthetic devices including implants which replace or supplement the functionality of a missing biological structure and medico-technical devices, where the function and the mode of provision are not specified;
#   preventive care, which means any measure that aims to avoid or reduce the number or the severity of injuries and diseases, their sequelae and complications; Preventive care includes interventions for both individual and collective consumption
#   governance, and health system and financing administration, which means services that focus on the health system rather than direct healthcare, direct and support health system functioning, and are considered to be collective, as they are not allocated to specific individuals but benefit all health system users.

# !Finally, current expenditure on healthcare means the final consumption expenditure of resident units on healthcare goods and services, including the healthcare goods and services provided directly to individual persons as well as collective healthcare services.

# Eurostat answer is pending.


str(health_exp)

which(!(capitals$country_code_iso2 %in% health_exp$geo))

summary(health_exp$values)


#Cultural participation

#Percentage of 16 years and older, under 30, above 75 who didn't attend any cultural event in the last 12 months

str(cult_part)

which(is.na(cult_part$geo))
which(is.na(cult_part$values))
which(is.na(cult_part$variable))

which(!(capitals$country_code_iso2 %in% cult_part$geo))

cult_part$variable <- as.factor(cult_part$variable)
levels(cult_part$variable)

summary(cult_part$values)
#percentages are between 0 and 100


#
# tdata variables

#
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

# Only 0 and 1 values (yes)
responses <- select(response, -c(Country, date, year, week))
imp_resp <- unlist(lapply(lapply(responses, function(x) x %nin% c("0", "1")), sum))
imp_resp[which(imp_resp > 0)]

#
# Temperature

# Missing values
length(which(is.na(tempavg$tavg)))
tempavg[which(is.na(tempavg$tavg)),]
# implausible values
summary(tempavg)
tempavg[which(tempavg$tavg == max(tempavg$tavg, na.rm = TRUE)),]
tempavg[which(tempavg$tavg == min(tempavg$tavg, na.rm = TRUE)),]

#
# Vaccination
vaccination[rowSums(is.na(vaccination)) > 0,]
vaccination[which(is.na(vaccination$new_vaccinations_smoothed)),]
# Negativ cases (none)
which(select(vaccination, select = -c("iso_code", "country", "date")) < 0, arr = TRUE)
                                 
#
#Coronavirus, df:covid

# Missing values
covid[rowSums(is.na(covid)) > 0,] #no NA


#Negative new cases

length(which(covid$cases_new <0)) #20
covid[covid$cases_new < 0,]

length(which(covid$deaths_new <0)) #51
covid[covid$deaths_new < 0,]

length(which(covid$recovered_new <0)) #58
covid[covid$recovered_new < 0,]

#https://github.com/RamiKrispin/coronavirus/issues/55 24.01.2021:
#The coronavirus dataset calculates the daily new cases (confirmed, recovered, death) by taking the delta of each day with the previous one. 
#Therefore, negative cases could occur when there is a drop in the cumulative number of cases with respect to the previous day. 
#This so far occurred when:
#-There is a change in the counting methodology or data resource (for example this issue and this one)
#-Updating new cases (or removing false-positive cases) not on the day that they were count
#-Errors in the raw data


#
#FB, UMD; df: fb

#Missing values

fb[rowSums(is.na(fb)) > 0,] 

# Check for implausible values, outside of [0;1]

fb <-as_tibble(fb)
which(fb < 0, arr.ind=TRUE) #none

impvar_fb <- fb %>% select(fb_data.percent_cli: fb_data.dc_se_unw, -contains("sample_size"))
which(impvar_fb > 1, arr.ind=TRUE) #none


