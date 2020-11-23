# TEMPLATE file to authenticate a google sheets user
# GO to https://console.cloud.google.com/apis/credentials and follow instructions

library(googlesheets4)

# see and store the current user-configured OAuth app (probaby `NULL`)
(original_app <- gs4_oauth_app())
# NULL

# see and store the current user-configured API key (probaby `NULL`)
(original_api_key <- gs4_api_key())
# NULL



# METHOD 1 ----------------------------------------------------------------

  # bring your own app via client id (aka key) and secret
  google_app <- httr::oauth_app(
    "", # NAME
    key = "", # KEY
    secret = "" # SECRET
  )
  google_key <- "" # GOOGLE-KEY
  gs4_auth_configure(app = google_app, api_key = google_key)

  # confirm the changes
  # gs4_oauth_app()
  # gs4_api_key()

  
# METHOD 2 ----------------------------------------------------------------
  
#   # bring your own app via JSON downloaded from Google Developers Console
  # this file has the same structure as the JSON from Google
  app_path <- system.file(
    "extdata", "[FULL PATH TO FILE HERE]", # json FILE HERE (ends in .apps.googleusercontent.com.json)
    package = "googlesheets4"
  )
  gs4_auth_configure(path = app_path)

  # confirm the changes
  # sheets_oauth_app()