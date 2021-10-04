# Sync server with local Maker
sync_server_local <- function(server_folder, local_folder, direction) {
  
  # DEBUG
  # server_folder = "test/999/"
  # local_folder = "canonical_protocol_DEV/"
  # direction = "server_to_local"
  
  
  local_folder = normalizePath(here::here(local_folder))
  
  # CHECKS we have credentials and necessary software ------------------------
  
  credentials_exist = file.exists(".vault/.credentials")
  SSHPASS = Sys.which("sshpass") # Check if sshpass is installed
  RSYNC = Sys.which("rsync") # Check if rsync is installed
  
  if (credentials_exist) {
    # sshpass and rsync installed (?)
    if (SSHPASS != "" & RSYNC != "") { 
      # cli::cli_text(cli::col_green("{cli::symbol$tick} "), "All is well.")
    } else {
      cli::cli_text(cli::col_red("{cli::symbol$cross} "), "'sshpass' or 'rsync' not installed. Can't use `sync_server_local()`")
    }
  } else {
    cli::cli_text(cli::col_red("{cli::symbol$cross} "), "Can find server credentials in '.vault/.credentials'")
  }
  
  
  
  # CHECK -------------------------------------------------------------------
  
  if (direction == "server_to_local") {
    message_text = paste0(cli::col_yellow("Will sync: "), cli::col_silver("cscn.uai.cl/", server_folder, " -->> ", local_folder))
  } else if (direction == "local_to_server") {
    message_text = paste0(cli::col_yellow("Will sync: "), cli::col_silver(local_folder, " -->> ", "cscn.uai.cl/", server_folder))
  } else {
    cli::cli_text(cli::col_red("{cli::symbol$cross} "), "direction should be either 'server_to_local' or 'local_to_server'")
    stop()
  }
  out <- utils::menu(c("yes", "no"), title = cat(message_text))
  
  
  # SYNC --------------------------------------------------------------------
  
  # Get server credentials
  list_credentials = source(".vault/.credentials")
  
  
  if (out == 1) {
    
    if (direction == "server_to_local") {
      
      # DOWNLOAD from server to local folder
      system(
        paste0('sshpass -p ', list_credentials$value$password, ' rsync -av --rsh=ssh ', 
               list_credentials$value$user, "@", list_credentials$value$IP, ":", list_credentials$value$main_FOLDER, server_folder, '/ ',
               here::here(local_folder), '/ '
        )
      )
      
      
    } else if (direction == "local_to_server") {
      
      # UPLOAD from local folder to server
      system(
        paste0('sshpass -p ', list_credentials$value$password, ' rsync -av --rsh=ssh ', 
               here::here(local_folder), '/ ',
               list_credentials$value$user, "@", list_credentials$value$IP, ":", list_credentials$value$main_FOLDER, server_folder, '/ '
        )
      )
      
    }
    
  } else {
    cat(crayon::green("Not doing anything..."))
  }
  
}
# sync_server_local(server_folder = "test/999/", local_folder = "canonical_protocol_DEV/", direction = "server_to_local")
