#' sync_server_local
#' Standalone function to Download/Upload files from server to local folder or vice-versa
#'
#' @param server_folder 
#' @param local_folder 
#' @param direction should be either 'server_to_local' or 'local_to_server'
#' @param only_test TRUE/FALSE
#' @param exclude_csv DO NOT INCLUDE DATA
#' @param delete_nonexistent  Delete files localy if they are NOT in server anymore
#' @param dont_ask For the initial setup, do not ask and proceed! DANGER
#'
#' @return
#' @export
#'
#' @examples
sync_server_local <-
  function(server_folder,
           local_folder,
           direction,
           only_test = TRUE,
           exclude_csv = FALSE,
           delete_nonexistent = FALSE,
           dont_ask = FALSE,
           all_messages = TRUE) {
    
  # DEBUG
    # pid = "999"
    # project_folder = getwd()
    # TEMP_DIR = tempdir(check = TRUE)
    # server_folder = pid
    # local_folder = TEMP_DIR
    # direction = "server_to_local"
    # only_test = FALSE
    # exclude_csv = TRUE
    # delete_nonexistent = TRUE
    # dont_ask = TRUE
    # all_messages = TRUE
    
    

  # Parameters --------------------------------------------------------------

  if (only_test == TRUE) {
    extra_message = paste0(cli::col_red("THIS IS A dry-run"))
    dry_run = " --dry-run "
  } else {
    extra_message = ""
    dry_run = ""
  }
  
  
  if (exclude_csv) {
    exclude_files = " --exclude='*.csv' "
  } else {
    exclude_files = ""
  }
  
  if (delete_nonexistent) {
    delete_nonexistent_files = " --delete "
  } else {
    delete_nonexistent_files = ""
  }
  
  # Check and prepare local folder and path
  if (!file.exists(local_folder)) dir.create(local_folder)
  local_folder = normalizePath(here::here(local_folder))
  local_folder_terminal = gsub(" ", "\\\\ ", local_folder)
  
  # CHECKS we have credentials and necessary software ------------------------
  
  credentials_exist = file.exists(here::here(".vault/.credentials"))
  SSHPASS = Sys.which("sshpass") # Check if sshpass is installed
  RSYNC = Sys.which("rsync") # Check if rsync is installed
  
  if (credentials_exist) {
    # sshpass and rsync installed (?)
    if (SSHPASS != "" & RSYNC != "") { 
      # cli::cli_text(cli::col_green("{cli::symbol$tick} "), "rsync installed and credentials exist")
    } else {
      cli::cli_abort("'sshpass' or 'rsync' not installed. Can't use `sync_server_local()`")
    }
  } else {
    cli::cli_abort("Can't find server credentials in '.vault/.credentials'")
  }
  
  
  
  # CHECK -------------------------------------------------------------------
  
  if (direction == "server_to_local") {
    message_text = paste0(cli::col_yellow("Will sync: "), cli::col_silver("cscn.uai.cl/", server_folder, " -->> ", local_folder), "\n", extra_message)
  } else if (direction == "local_to_server") {
    message_text = paste0(cli::col_yellow("Will sync: "), cli::col_silver(local_folder, " -->> ", "cscn.uai.cl/", server_folder), "\n", extra_message)
  } else {
    cli::cli_abort("`direction` should be either 'server_to_local' or 'local_to_server'")
  }
  
  if (all_messages == TRUE) cli::cli_h1("sync {direction} | delete_nonexistent {delete_nonexistent}")
  
  if (dont_ask == TRUE) {
    out = 1
    
    if (all_messages == TRUE) {
      cli::cli_par()
      cli::cli_end()
      # cli::cli_text(paste0(cli::col_yellow("BACKUP of full protocol WITHOUT data: "), cli::col_silver("cscn.uai.cl/", server_folder, " -->> data/protocol_", local_folder), "\n", extra_message))
      cli::cli_text(paste0(cli::col_yellow("Will sync: "), cli::col_silver("cscn.uai.cl/", server_folder, " -->> ", local_folder), "\n", extra_message))
    }
    
  } else {
    out <- utils::menu(c("yes", "no"), title = cat(message_text))  
  }
  
  
  
  # SYNC --------------------------------------------------------------------
  
  # Get server credentials
  list_credentials = source(here::here(".vault/.credentials"))
  
  
  if (out == 1) {
    
    if (direction == "server_to_local") {
      
      # DOWNLOAD server to local
      OUT = 
        suppressWarnings(
          system(
            paste0('sshpass -p ', list_credentials$value$password, ' rsync -av ', dry_run, ' --rsh=ssh ', 
                  delete_nonexistent_files, # Delete files from destination if they are NOT in origin anymore
                  exclude_files, # exclude CSV files
                  list_credentials$value$user, "@", list_credentials$value$IP, ":", list_credentials$value$main_FOLDER, server_folder, '/ ',
                  here::here(local_folder_terminal), '/ '
                  ), intern = TRUE
            )
        )
      
      
    } else if (direction == "local_to_server") {
      
      # UPLOAD local to server
      OUT = 
        suppressWarnings(
          system(
            paste0('sshpass -p ', list_credentials$value$password, ' rsync -av ', dry_run, ' --rsh=ssh ', 
                   here::here(local_folder_terminal), '/ ',
                   list_credentials$value$user, "@", list_credentials$value$IP, ":", list_credentials$value$main_FOLDER, server_folder, '/ '
          ), intern = TRUE)
      )
      
    }
    
  } else {
    cli::cli_alert_info("Not doing anything...")
  }
  
}
# sync_server_local(server_folder = "test/FONDECYT2021/", local_folder = "canonical_protocol_DEV/", direction = "server_to_local")
