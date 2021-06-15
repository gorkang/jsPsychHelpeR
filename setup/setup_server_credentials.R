# Secure credentials ------------------------------------------------------

  # 1) Using template, ADD your server credentials
  rstudioapi::navigateToFile(".vault/.credentials_TEMPLATE")
  
  # 2) Rename file to ".vault/.credentials"
  file.rename(from = ".vault/.credentials_TEMPLATE", to = ".vault/.credentials")
  
  # 3) Secure credentials file
  Sys.chmod(".vault/.credentials", mode = "0400")
