# Sync and zip data folder of active protocols

  # CHECKS active folders: https://docs.google.com/spreadsheets/d/13xX8aGhKmfz8zVvNuCMDnS6L6Dy6T1NlA6nTOqZuErA/edit#gid=0
  # ZIPS to jsPsychR/SHARED-data/
  # Processes and zips DF_raw and DF_clean
  # Individual project folders will be shared with the IP for each project



### THIS DOCUMENT RUNS DAILY via anacron ###

# FILE: /etc/cron.daily/gorka-sync_data_jspsychHelpeR -----------------------

  # #!/bin/sh
  # 
  # # If started as root, then re-start as user "emrys":
  # if [ "$(id -u)" -eq 0 ]; then
  # exec sudo -H -u emrys $0 "$@"
  # echo "This is never reached.";
  # fi
  # 
  # cd /home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/jsPsychHelpeR/admin/ && Rscript sync_data_active_protocols.R
  # 
  # exit 0;

# END FILE /etc/cron.daily/gorka-sync_data_jspsychHelpeR --------------------


# TO TEST THIS WILL WORK WHEN RUN DAILY: 
# system("cd /home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/jsPsychHelpeR/admin/ && Rscript sync_data_active_protocols.R")


# Sources and parameters --------------------------------------------------
  
  suppressPackageStartupMessages(source(here::here("_targets_packages.R")))
  invisible(lapply(list.files(here::here("./R"), full.names = TRUE, pattern = ".R$"), source))
  
  # source(here::here("R/helper_functions.R"))
  # source(here::here("R/sync_server_local.R"))

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
      
      cli::cli_h1("Project {PIDs[.x]}")

      # Clean outputs/data/
      cli::cli_h2("Clean up outputs/data")
      FILES = list.files(here::here("outputs/data/"), full.names = TRUE, pattern = "DF_clean|DF_raw")
      file.remove(FILES)
      Sys.sleep(5) # Give some time to sync changes to google drive
            
      # Download data
      cli::cli_h2("Downloading data for project {PIDs[.x]}")
      get_zip(pid = PIDs[.x], what = "data")
      
      # Read ZIP and process data
      zip_name = here::here(paste0("../SHARED-data/", PIDs[.x], "/", PIDs[.x], ".zip"))
      
      
      if (file.exists(zip_name)) {
      
        # Process data (DF_raw and DF_clean)
        cli::cli_h2("Processing data for project {PIDs[.x]}")
        
        read_data_safely = purrr::safely(read_data)
        
        DF_raw = read_data_safely(input_files = zip_name)
        
        if (is.null(DF_raw$error)) {
            
          DF_clean = create_clean_data(DF_raw$result)
          
          # Copy processed files to destination folder
          cli::cli_h2("Copy output processed files for project {PIDs[.x]}")
          FILES_processed = list.files(here::here("outputs/data/"), full.names = TRUE, pattern = "DF_clean|DF_raw")
          destination = paste0(dirname(zip_name), "/processed/")
          if (!dir.exists(destination)) dir.create(destination)
          file.copy(from = FILES_processed, to = paste0(destination, basename(FILES_processed)), overwrite = TRUE)
          
          # Create zip with process data
          zip_files(folder_files = destination, 
                    zip_name = paste0(destination, "", "processed.zip"), 
                    remove_files = TRUE)
        
        }
      }
      
    })

  

# PERMISOS ----------------------------------------------------------------
  
  # TODO: FOR NOW, do this manually. Make sure all emails are OK
  
  # DF_resumen_clean
  # EMAIL = "name-surname@university.edu.com"
  # grepl(pattern = "\\b[-A-Za-z0-9_.%]+\\@[-A-Za-z0-9_.%]+\\.[A-Za-z]+", EMAIL)
  # grepl(pattern = "^[[:alnum:].-_]+@[[:alnum:].-]+$", EMAIL)
  
  
  1:length(PIDs) |>
    purrr::walk( ~ {

      DF_permisos = DF_resumen_clean |> dplyr::filter(ID == PIDs[.x])

      IS_EMAIL_regex = "\\b[-A-Za-z0-9_.%]+\\@[-A-Za-z0-9_.%]+\\.[A-Za-z]+"
      
      # CHECK if contacto looks like a single well formed email
      if (grepl(pattern = IS_EMAIL_regex, DF_permisos$contacto)) {
        
        # Set permissions, only if not ADMIN and does not already have permissions
        set_permissions_google_drive(pid = DF_permisos$ID, email_IP = DF_permisos$contacto)
        
        cli::cli_alert_success("Asignados permisos sobre pid: {DF_permisos$ID} a {DF_permisos$contacto}")
        
        
      } else {
        cli::cli_alert_danger("{DF_permisos$contacto} does NOT look like a proper email")
      }

    })
