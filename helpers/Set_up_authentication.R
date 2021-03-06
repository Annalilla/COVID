# Don’t run this. The purpose of this script is only to demonstrate, how the authentication file was created
# Generates the token to access the private Google Sheets where the data are stored

library(googlesheets4)

# Setting up authentication
options(gargle_oauth_cache = ".secrets")

#Interactive authentication to generate the token
googlesheets4::sheets_auth()

# Trying out
gs4_deauth()
# Reauthentication with the generated token and the email address
gs4_auth(
  cache = ".secrets",
  email = "covid.data.storage@gmail.com"
)
gs4_has_token() # Should be TRUE
