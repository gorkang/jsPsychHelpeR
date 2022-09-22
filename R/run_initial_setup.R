run_initial_setup <- function(pid, download_files = FALSE, download_task_script = FALSE) {
  
  # DEBUG
  # pid = "999"
  
  files_pid = list.files(paste0("data/", pid))
  credentials_exist = file.exists(".vault/.credentials")
  # SSHPASS = Sys.which("sshpass") # Check if sshpass is installed
  # RSYNC = Sys.which("rsync") # Check if rsync is installed
  
  # CHECK if NO files in project's folder & NO credentials to download
  if (length(files_pid) == 0 & credentials_exist == FALSE) {
    cat(cli::col_red(
    paste0("Can't access protocol csv files:
    - No files in data/", pid), " 
    - .vault/credentials file not present\n"),
    cli::col_silver("You can either:
    - manually download files to", paste0("data/", pid), "
    - edit .vault/credentials_TEMPLATE and rename it to .vault/credentials"))
  }
  
  # Load all functions
  invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source))
  
  
  
  response_prompt = menu(choices = c("Yes", "No"), 
                         title = 
                           cli::cli(
                             {
                               cli::cli_par()
                               cli::cli_alert_info("Do you want to run the {.pkg initial setup}?")
                               cli::cli_end()
                               cli::cli_text("This will {cli::style_bold((cli::col_red('DELETE')))} the _targets/ folder, 
                                             {cli::style_bold((cli::col_green('install')))} necessary packages, 
                                             {cli::style_bold((cli::col_green('copy')))} configuration files, 
                                             {cli::style_bold((cli::col_yellow('replace')))} the _targets.R, etc.")
                             }
                           )
  )
  
  if (response_prompt == 1) {
  
    # 1) Run to make sure you have all the necessary packages and folders -------
    
    cli::cli_par()
    cli::cli_h1("Setup")
    cli::cli_end()
    source("setup/setup.R")
    
    
    # 2) Make sure the folder "data/[YOUR_PROJECT_ID]/" exists -----------------
    
    if (!dir.exists(paste0("data/", pid, "/"))) dir.create(paste0("data/", pid, "/"), recursive = TRUE)
    
    
    # 3) **Manually** copy .csv files to data/[YOUR_PROJECT_ID]/  --------------
      # or DOWNLOAD from server (needs a .vault/credentials file. 
      # Rename and edit .vault/credentials_TEMPLATE)
    
    if (credentials_exist) {
      
      if (download_files == TRUE) {
 
          cli::cli_par()
          cli::cli_h1("Download files")
          cli::cli_end()
          
          update_data(id_protocol = pid) 
          
        } else if (download_files == FALSE) {
          
          cli::cli_alert_danger("Will NOT download files")
          cli::cli_alert_info("You may need to manually download the files to '{paste0('data/', pid, '/')}'")
          
        }
      
      
      if (download_task_script == TRUE) {
        
        cli::cli_par()
        cli::cli_h1("Download task script")
        cli::cli_end()
        
        # Get protocol without data and zip it in data/protocol_PID.zip
        get_zip_protocol(pid)
          
        
      } else if (download_task_script == FALSE) {
        
        cli::cli_alert_danger("Will NOT download task script")
        # cli::cli_alert_info("You may need to manually download the files to '{paste0('data/', pid, '/')}'")
        
      }
      
    } else {
      
      cli::cli_alert_danger("Can't find server credentials in '.vault/.credentials'")
      
    }
    
    # 4) Create a _targets.R file for your data -------------------------------
    
    cli::cli_par()
    cli::cli_h1("Create _targets.R file")
    cli::cli_end()
    
    create_targets_file(pid_protocol = pid, folder_data = paste0("data/", pid, "/"))
    
    # Open _targets.R and run.R
    rstudioapi::navigateToFile("_targets.R")
    rstudioapi::navigateToFile("run.R")
    
  } else {
    
    cli::cli_alert_warning("OK, nothing done")
    
  }
  
}
