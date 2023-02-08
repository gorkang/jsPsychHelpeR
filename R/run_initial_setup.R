#' run_initial_setup
#' Run initial setup for a specific project
#'
#' @param pid project id
#' @param download_files should download the data files? (requires server credentials) TRUE/FALSE 
#' @param data_location local folder where the raw data for the project is
#' @param download_task_script should download the task scripts? (requires server credentials) TRUE/FALSE
#' @param dont_ask answer YES to all questions
#' @param folder location for the project
#' @param sensitive_tasks short names of the sensitive tasks in the protocol, if any
#'
#' @return
#' @export
#'
#' @examples
run_initial_setup <- function(pid, download_files = FALSE, data_location = NULL, download_task_script = FALSE, folder =  "~/Downloads/jsPsychHelpeRtest", sensitive_tasks = c(""), dont_ask = FALSE) {
  
  # DEBUG
  # pid = "999"
  # data_location = "/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/jsPsychHelpeR/data/999/"
  
  # CHECKS
  
  if (download_files == FALSE & is.null(data_location)) cli::cli_abort("Either `download_files` or `data_location` need to be set. Otherwise, I don't have access to the project's data!")
  if (download_files == TRUE & !is.null(data_location)) cli::cli_abort("Only one of `download_files` or `data_location` must be set.")
  
  if (dont_ask == TRUE) response_prompt = 1
  folder_data = paste0(folder, "/data/", pid, "/")
  
  credentials_exist = file.exists(".vault/.credentials") # TODO: location of credentials for other users. If not in jsPsychHelpeR folder, won't be able to Download
  
  # CHECK if NO files in project's folder & NO credentials to download
  if (is.null(data_location) & credentials_exist == FALSE) {
    
    cli_message(var_used = pid, 
                h1_title = "ERROR", 
                danger = "Can't access .csv files for protocol `{pid}`:\n- No files in `data/{pid}`\n- `data_location` parameter is empty \n- `.vault/credentials` file not present",
                info = "You can either:\n- manually download files to `data/{pid}`\n- edit `.vault/credentials_TEMPLATE` and rename it to `.vault/credentials`")
    
    cli::cli_abort("No way to get the protocol's .csv files")
    
  }
  
  
  # ASK FOR USER PERMISSION
  if (dont_ask == FALSE)  response_prompt = menu(choices = c("Yes", "No"), 
                                                 title = 
                                                   cli_message(h1_title = "Initial SETUP", 
                                                               info = "Do you want to run the {.pkg initial setup}?",
                                                               details = "This will {cli::style_bold((cli::col_red('DELETE')))} the _targets/ folder, 
                                                                         {cli::style_bold((cli::col_green('install')))} necessary packages, 
                                                                         {cli::style_bold((cli::col_green('copy')))} configuration files, 
                                                                         {cli::style_bold((cli::col_yellow('replace')))} the _targets.R, etc."))
      
    
  
  if (response_prompt == 1) {
  
    # 1) Run to make sure you have all the necessary packages and folders -------
    
    cli_message(h1_title = "Setup")
    setup_folders(pid = pid, folder = folder, extract_zip = TRUE)
    
    
    # 2) **Manually** copy .csv files to data/[YOUR_PROJECT_ID]/  --------------
      # or DOWNLOAD from server (needs a .vault/credentials file. 
      # Rename and edit .vault/credentials_TEMPLATE)
    
    cli_message(h1_title = "Get data files and task script")
    if (download_files == TRUE) {
      
      if (!credentials_exist) cli::cli_abort("Can't find server credentials in '.vault/.credentials'")
      
      update_data(pid = pid, folder = folder) 
        
    } else if (download_files == FALSE) {
      
      # We will check if there is only a zip file or multiple csv's in create_targets_file(). No need to do it here too
    
      cli::cli_alert_info("Will copy files from {.code {data_location}}")
      
      # Copy files from data_location to folder_data
      files_raw = list.files(path = data_location, pattern = "*.csv|*.zip", full.names = TRUE)
      file.copy(from = files_raw, to = paste0(folder_data,  basename(files_raw)))
      
      # Files present in destination (after copying)
      files_destination = list.files(folder_data, pattern = "*.csv|*.zip", full.names = FALSE)
      cli::cli_alert_info("{length(files_destination)} files in 'data/{pid}'")
      
    }
    
    # Make sure sensitive tasks are in .vault
    move_sensitive_tasks_to_vault(pid = pid, folder = folder, sensitive_tasks = sensitive_tasks)
    
    
    # Files present in destination (after copying)
    files_pid = list.files(folder_data, pattern = "*.csv|*.zip", full.names = FALSE)
    
    # cli::cli_alert_info("Will NOT download data files. {length(files_pid)} files found in `{paste0('data/', pid, '/')}`")
    
      
      if (download_task_script == TRUE) {
        
        cli_message(h1_title = "Download task script")
        
        # Get protocol without data and zip it in data/protocol_PID.zip
        # TODO: download protocol to 'folder'. Now will download to the wd
        get_zip(pid, what = "protocol", dont_ask = dont_ask)
          
        
      } else if (download_task_script == FALSE) {
        
        cli::cli_alert_info("Will NOT download task script")
        
      }
      
    
    # 3) Create a _targets.R file for your data -------------------------------
    
    cli_message(h1_title = "Create _targets.R file")
    create_targets_file(pid = pid, folder = folder, dont_ask = dont_ask)
    
    # Open _targets.R and run.R
    if (Sys.getenv("RSTUDIO") == "1") {
      cli_message(info = "Opening new RStudio project. Open `_targets.R` and `run.R` there to start")
      
      rstudioapi::openProject(folder, newSession = TRUE)
      # invisible(rstudioapi::navigateToFile("_targets.R"))
      # invisible(rstudioapi::navigateToFile("run.R"))
    }
    
    
  } else {
    
    cli::cli_alert_warning("OK, nothing done")
    
  }
  
}
