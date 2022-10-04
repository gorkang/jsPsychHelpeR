# Sync and zip data folder of active protocols

### THIS DOCUMENT RUNS DAILY: /etc/cron.daily/gorka-sync_data_jspsychHelpeR ###

# CHECKS active folders: https://docs.google.com/spreadsheets/d/13xX8aGhKmfz8zVvNuCMDnS6L6Dy6T1NlA6nTOqZuErA/edit#gid=0
# ZIPS to jsPsychR/SHARED-data/
# Individual project folders will be shared with the IP for each project


# Sources and parameters --------------------------------------------------

  source(here::here("R/helper_functions.R"))
  source(here::here("R/sync_server_local.R"))
  

  google_sheet_ID = "1eE0xrk_DGIOShhWjqKaKhk0NIeb-7k0eoFG93822-Ho" 
  cli::cli_h1("Reading https://docs.google.com/spreadsheets/d/{google_sheet_ID}/edit#gid=0")
  googlesheets4::gs4_auth("gorkang@gmail.com")
  googlesheets4::local_gs4_quiet() # No googlesheets4::read_sheet messages



# GET google sheet --------------------------------------------------------

  DF_resumen_ALL = googlesheets4::read_sheet(google_sheet_ID, sheet = 1, skip = 0) 
  
  DF_resumen_clean = 
    DF_resumen_ALL |> 
    # dplyr::filter(STATUS == "Running") |>
    dplyr::filter(grepl("Running", STATUS)) |> 
    dplyr::select(ID, contacto) |> 
    dplyr::mutate(ID = unlist(ID))
  
  PIDs = 
    DF_resumen_clean |> 
    dplyr::pull(ID) 


# Download and zip --------------------------------------------------------

  1:length(PIDs) |> 
    purrr::walk( ~ {
      
      # .x = 1
      cli::cli_h1("Downloading data for project {PIDs[.x]}")
      
      get_zip(pid = PIDs[.x], what = "data")
      
      })

  

# PERMISOS ----------------------------------------------------------------
  
  # TODO: FOR NOW, do this manually. Make sure all emails are OK
  
  # DF_resumen_clean
  
  # 1:length(PIDs) |>
  #   purrr::walk( ~ {
  # 
  #     DF_permisos = DF_resumen_clean |> dplyr::filter(ID == PIDs[.x])
  # 
  #     IS_EMAIL_regex = "^[[:alnum:].-_]+@[[:alnum:].-]+$"
  # 
  #     # CHECK if contacto looks like a single well formed email
  #     if (grepl(pattern = IS_EMAIL_regex, DF_permisos$contacto)) {
  #       # Set permissions, only if not ADMIN and does not already have permissions
  #       set_permissions_google_drive(pid = DF_permisos$ID, email_IP = DF_permisos$contacto)
  #     } else {
  #       cli::cli_alert_danger("{DF_permisos$contacto} does NOT look like a proper email")
  #     }
  # 
  #   })
