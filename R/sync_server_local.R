#' sync_server_local
#' Standalone function to Download/Upload files from server to local folder or vice-versa
#'
#' @param server_folder server folder
#' @param local_folder local folder
#' @param direction should be either 'server_to_local' or 'local_to_server'
#' @param only_test TRUE/FALSE
#' @param exclude_csv DO NOT INCLUDE DATA
#' @param delete_nonexistent  Delete files locally if they are NOT in server anymore
#' @param ignore_existing If TRUE, does not overwrite existing files even if they are newer. Good for .data/, Bad for rest
#' @param dont_ask For the initial setup, do not ask and proceed! DANGER
#' @param all_messages show all messages
#' @param list_credentials where are the credentials
#'
#' @return Sync folders
#' @export
sync_server_local <-
  function(server_folder,
           local_folder,
           direction,
           only_test = TRUE,
           exclude_csv = FALSE,
           delete_nonexistent = FALSE,
           ignore_existing = FALSE,
           dont_ask = FALSE,
           all_messages = TRUE,
           list_credentials = NULL) {
    
    # DEBUG
    # jsPsychAdmin::get_parameters_of_function("jsPsychHelpeR::sync_server_local()")
    
    # TEMP_DIR = tempdir(check = TRUE)
    # server_folder = pid
    # local_folder = TEMP_DIR
    # direction = "server_to_local"
    
    
    # Parameters --------------------------------------------------------------
    
    if (only_test == TRUE) {
      extra_message = paste0("||| THIS IS A dry-run |||")
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
      delete_nonexistent_files = " --delete-during " # trying delete-during to check if works with ignore-existing #delete
    } else {
      delete_nonexistent_files = ""
    }
    
    if (ignore_existing) {
      ignore_existing_text = " --ignore-existing " # Ignore existing does not overwrite existing files (ok for data, bad for rest)
    } else {
      ignore_existing_text = ""
    }
    
    
    # Check and prepare local folder and path
    if (!file.exists(local_folder)) dir.create(local_folder)
    local_folder = normalizePath(here::here(local_folder))
    local_folder_terminal = gsub(" ", "\\\\ ", local_folder)
    FILES_in_local_folder = list.files(local_folder, recursive = TRUE)
    
    
    # CHECKS we have credentials and necessary software ------------------------
    
    # Get server credentials
    if (is.null(list_credentials)) {
      list_credentials = source(here::here(".vault/.credentials")) 
      credentials_exist = file.exists(here::here(".vault/.credentials"))
    } else {
      credentials_exist = TRUE
    }
    
    
    # credentials_exist = file.exists(here::here(".vault/.credentials"))
    SSHPASS = Sys.which("sshpass") # Check if sshpass is installed
    RSYNC = Sys.which("rsync") # Check if rsync is installed
    
    if (credentials_exist) {
      # sshpass and rsync installed (?)
      if (SSHPASS != "" & RSYNC != "") { 
        if (all_messages == TRUE) cli::cli_alert_success("rsync installed and credentials exist")
      } else {
        cli::cli_abort("'sshpass' or 'rsync' not installed. Can't use `sync_server_local()`")
      }
    } else {
      cli::cli_abort("Can't find server credentials in '.vault/.credentials'")
    }
    
    
    
    # CHECK -------------------------------------------------------------------
    
    if (direction == "server_to_local") {
      message_text = paste0("\n", extra_message, "\n", cli::col_yellow("Will sync: "), cli::col_silver("cscn.uai.cl/lab/protocols/", server_folder, " -->> ", local_folder))
      message_text_simple = paste0("\n", extra_message, "\n cscn.uai.cl/lab/protocols/", server_folder, " -->> ", local_folder)
    } else if (direction == "local_to_server") {
      message_text = paste0("\n", extra_message, "\n", cli::col_yellow("Will sync: "), cli::col_silver(local_folder, " -->> ", "cscn.uai.cl/lab/protocols/", server_folder))
      message_text_simple = paste0("\n", extra_message, "\n", local_folder, " -->> cscn.uai.cl/lab/protocols/", server_folder)
    } else {
      cli::cli_abort("`direction` should be either 'server_to_local' or 'local_to_server'")
    }
    
    if (all_messages == TRUE) cli::cli_h1("sync {direction} | delete_nonexistent {delete_nonexistent}")
    
    if (dont_ask == TRUE) {
      out = 1
      
      if (all_messages == TRUE) {
        cli::cli_par()
        cli::cli_end()
        cat(message_text)
        # cli::cli_text(paste0(cli::col_yellow("BACKUP of full protocol WITHOUT data: "), cli::col_silver("cscn.uai.cl/", server_folder, " -->> data/protocol_", local_folder), "\n", extra_message))
        # cli::cli_text(paste0(cli::col_yellow("Will sync: "), cli::col_silver("cscn.uai.cl/lab/protocols/", server_folder, " -->> ", local_folder), "\n", extra_message))
      }
      
    } else {
      out <- utils::menu(c("yes", "no"), title = cat(message_text))  
    }
    
    
    
    # SYNC --------------------------------------------------------------------
    
    if (out == 1) {
      
      # Delete initial /
      server_folder = gsub("^/", "", server_folder)
      
      # Full path in server
      FOLDER_server = paste0(list_credentials$value$main_FOLDER, server_folder)
      
      # Public folder, only to show a simple message
      FOLDER_URL = gsub("/srv/users/user-cscn/apps/uai-cscn/public/", "", list_credentials$value$main_FOLDER)
      
      # If it's a dev protocol delete protocol/ as protocols_DEV comes in pid
      if (grepl("protocols_DEV", FOLDER_server)) {
        FOLDER_URL = gsub("protocols/", "", FOLDER_URL)
        FOLDER_server = gsub("protocols/", "", FOLDER_server)
      }
      
      cli::cli_alert_info("Checking files in {.pkg https://cscn.uai.cl/{FOLDER_URL}{server_folder}}. This can take a while...")
      
      
      
      if (direction == "server_to_local") {
        
        # DOWNLOAD server to local
        OUT = 
          suppressWarnings(
            system(
              paste0('sshpass -p ', list_credentials$value$password, ' rsync -av ', dry_run, ' --rsh=ssh ', 
                     delete_nonexistent_files, # Delete files from destination if they are NOT in origin anymore
                     ignore_existing_text, # Ignore existing files
                     exclude_files, # exclude CSV files
                     # list_credentials$value$user, "@", list_credentials$value$IP, ":", list_credentials$value$main_FOLDER, server_folder, '/ ',
                     list_credentials$value$user, "@", list_credentials$value$IP, ":", FOLDER_server, '/ ',
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
                     # list_credentials$value$user, "@", list_credentials$value$IP, ":", list_credentials$value$main_FOLDER, server_folder, '/ '
                     list_credentials$value$user, "@", list_credentials$value$IP, ":", FOLDER_server, '/ '
                     
              ), intern = TRUE)
          )
        
      }
      
    } else {
      cli::cli_alert_info("Not doing anything...")
    }
    
    if(is.null(OUT)) OUT = "Nothing done"
    
    
    # Clean OUT so only filenames remain
    OUT_clean = OUT[!grepl("^deleting|^receiving incremental file list|^sent|\\.\\/|^total|sending incremental file list", OUT)]
    
    # Show messages and create output
    if (all_messages == TRUE) {

      cli::cli_bullets_raw(OUT_clean)
      cli::cli_alert_info("{length(OUT_clean) - 1} NEW files SYNCED {message_text_simple}")
      cli::cli_alert_info("{length(FILES_in_local_folder)} files initially in {local_folder_terminal}")
      
      OUT_messages = ""
      
    } else {
      
      OUT_messages = paste0(length(OUT_clean) - 1, " NEW files SYNCED ", message_text_simple)
      
    }
    
    return(OUT_messages)
    
  }
