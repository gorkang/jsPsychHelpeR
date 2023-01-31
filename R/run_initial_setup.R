#' run_initial_setup
#' Run initial setup for a specific project
#'
#' @param pid project id
#' @param download_files should download the data files? (requires server credentials) [TRUE/FALSE] 
#' @param download_task_script should download the task scripts? (requires server credentials) [TRUE/FALSE]
#' @param dont_ask answer YES to all questions
#'
#' @return
#' @export
#'
#' @examples
run_initial_setup <- function(pid, download_files = FALSE, download_task_script = FALSE, dont_ask = FALSE) {
  
  # DEBUG
  # pid = "999"
  
  # Load all functions
  invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source))
  
  # CHECKS
  files_pid = list.files(paste0("data/", pid))
  credentials_exist = file.exists(".vault/.credentials")
  
  # CHECK if NO files in project's folder & NO credentials to download
  if (length(files_pid) == 0 & credentials_exist == FALSE) {
    
    cli_message(var_used = pid, 
                h1_title = "ERROR", 
                danger = "Can't access .csv files for protocol `{pid}`:\n- No files in `data/{pid}`\n- `.vault/credentials` file not present",
                info = "You can either:\n- manually download files to `data/{pid}`\n- edit `.vault/credentials_TEMPLATE` and rename it to `.vault/credentials`")
    
    cli::cli_abort("No way to get the protocol's .csv files")
    
  }
  

  if (dont_ask == FALSE) {
    
    response_prompt = menu(choices = c("Yes", "No"), 
                           title = 
                             cli_message(h1_title = "Initial SETUP", 
                                         info = "Do you want to run the {.pkg initial setup}?",
                                         details = "This will {cli::style_bold((cli::col_red('DELETE')))} the _targets/ folder, 
                                                   {cli::style_bold((cli::col_green('install')))} necessary packages, 
                                                   {cli::style_bold((cli::col_green('copy')))} configuration files, 
                                                   {cli::style_bold((cli::col_yellow('replace')))} the _targets.R, etc."
                             )
    )
    
    
  } else {
    response_prompt = 1
  }
  
  
  if (response_prompt == 1) {
  
    # 1) Run to make sure you have all the necessary packages and folders -------
    
    cli_message(h1_title = "Setup")
    run_setup(dont_ask = dont_ask)
    
    
    
    # 2) Make sure the folder "data/[YOUR_PROJECT_ID]/" exists -----------------
    
    if (!dir.exists(paste0("data/", pid, "/"))) dir.create(paste0("data/", pid, "/"), recursive = TRUE)
    
    
    # 3) **Manually** copy .csv files to data/[YOUR_PROJECT_ID]/  --------------
      # or DOWNLOAD from server (needs a .vault/credentials file. 
      # Rename and edit .vault/credentials_TEMPLATE)
    
    if (credentials_exist) {
      
      if (download_files == TRUE) {
 
        cli_message(h1_title = "Download files")
        update_data(id_protocol = pid) 
          
      } else if (download_files == FALSE) {
        
        cli::cli_alert_info("Will NOT download files. {length(files_pid)} files found in `{paste0('data/', pid, '/')}`")

      }
      
      
      if (download_task_script == TRUE) {
        
        cli_message(h1_title = "Download task script")
        
        # Get protocol without data and zip it in data/protocol_PID.zip
        get_zip(pid, what = "protocol", dont_ask = dont_ask)
          
        
      } else if (download_task_script == FALSE) {
        
        cli::cli_alert_info("Will NOT download task script")
        
      }
      
    } else {
      
      cli::cli_alert_danger("Can't find server credentials in '.vault/.credentials'")
      
    }
    
    # 4) Create a _targets.R file for your data -------------------------------
    
    cli_message(h1_title = "Create _targets.R file")
    create_targets_file(pid = pid, folder_data = paste0("data/", pid, "/"), dont_ask = dont_ask)
    
    # Open _targets.R and run.R
    if (Sys.getenv("RSTUDIO") == "1") {
      cli_message(info = "Opening `_targets.R` and `run.R`")
      invisible(rstudioapi::navigateToFile("_targets.R"))
      invisible(rstudioapi::navigateToFile("run.R"))
    }
    
    
  } else {
    
    cli::cli_alert_warning("OK, nothing done")
    
  }
  
}
