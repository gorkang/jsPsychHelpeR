

DELETE_data_server <- function(pid = NULL) {
  
  # CHECKS  ------------------------
  if (is.null(pid)) cli::cli_abort("pid needs a value")
  credentials_exist = file.exists(".vault/.credentials")
  SSHPASS = Sys.which("sshpass") # Check if sshpass is installed

  if (credentials_exist) {
    # sshpass installed
    if (SSHPASS != "") { 
      # cli::cli_text(cli::col_green("{cli::symbol$tick} "), "`sshpass` and credentials exist")
    } else {
      cli::cli_abort("'sshpass' not installed")
    }
  } else {
    cli::cli_abort("Can't find server credentials in '.vault/.credentials'")
  }
  

  # DELETE ------------------------------------------------------------------

  list_credentials = source(".vault/.credentials") # Get server credentials
  folder_to_delete = paste0(pid, '/.data/')
  
  cli::cli_alert_info("Checking files in {.pkg https://cscn.uai.cl/lab/public/instruments/protocols/{folder_to_delete}}. This can take a while...")
  
  FILES_in_folder = 
    suppressWarnings(
      system(paste0('sshpass -p ', list_credentials$value$password, ' ssh ', list_credentials$value$user, '@', list_credentials$value$IP, ' ls ', list_credentials$value$main_FOLDER, folder_to_delete), intern = TRUE)
    )
  
  if (length(FILES_in_folder) == 0) {
    
    cli::cli_alert_info("NO files found in `{.pkg {folder_to_delete}}`")
    
  } else {
    
    cli::cli_par()
    cli::cli_h1("PROCEED (?)")
    cli::cli_end()
    
    response_prompt = menu(choices = c("Yes", "NO"), 
                           title = 
                             cli::cli(
                               {
                                 cli::cli_par()
                                 cli::cli_alert_info("{cli::style_bold((cli::col_red('DELETE')))} {length(FILES_in_folder)} files in {.pkg https://cscn.uai.cl/lab/public/instruments/protocols/{folder_to_delete}}? This CAN NOT be undone.")
                                 cli::cli_end()
                                 cli::cli_text("Files found: {.pkg {FILES_in_folder}}")
                                 }
                               
                             )
                           )

    
    if (response_prompt == 1) {
      suppressWarnings(
        system(paste0('sshpass -p ', list_credentials$value$password, ' ssh ', list_credentials$value$user, '@', list_credentials$value$IP, ' rm ', list_credentials$value$main_FOLDER, folder_to_delete, '*'))
      )
      cli::cli_alert_success("{length(FILES_in_folder)} files in `{.pkg {folder_to_delete}}` DELETED")
    } else {
      cli::cli_alert_info("Nothing done")
    }
    
  }
  
}
