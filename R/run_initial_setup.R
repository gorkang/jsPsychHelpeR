#' Create a jsPsychHelpeR project for your data
#'
#' run_initial_setup() will read your data and create a jsPsychHelpeR project 
#' tailoring the _targets.R file to the tasks included in the data.
#'
#' @param pid project id
#' @param download_files Download the data files? FALSE / TRUE
#' - If TRUE, requires sFTP server credentials to be located in `credentials_file`,
#' by default: `.vault/credentials`
#' - See `.vault/credentials_TEMPLATE` for more details
#' @param data_location Local folder where the raw data for the project is located
#' @param download_task_script should download the task scripts? (requires server credentials) FALSE / TRUE
#' @param dont_ask answer YES to all questions so the process runs uninterrupted. This will: 
#' @param folder location for the project
#' @param credentials_file Path to .credentials file. By default: `.vault/.credentials`
#' @param sensitive_tasks short names of the sensitive tasks in the protocol, if any
#' @param open_rstudio Open RStudio with the new project TRUE / FALSE
#'
#' @return Opens a new RStudio project
#' @export
#' @examples 
#' 
#' run_initial_setup(pid = 999, download_files = FALSE,
#'                   data_location = system.file("extdata", package = "jsPsychHelpeR"),
#'                   download_task_script = FALSE, 
#'                   folder = tempdir(),
#'                   sensitive_tasks = c(""), 
#'                   credentials_file = ".vault/.credentials",
#'                   dont_ask = TRUE, 
#'                   open_rstudio = FALSE)
 
run_initial_setup <- function(pid, download_files = FALSE, data_location = NULL, download_task_script = FALSE, folder =  "~/Downloads/jsPsychHelpeRtest", sensitive_tasks = c(""), credentials_file = ".vault/.credentials", dont_ask = FALSE, open_rstudio = TRUE) {
  
  # CHECKS
  if (download_files == FALSE & is.null(data_location)) cli::cli_abort("Either `download_files` or `data_location` need to be set. Otherwise, I don't have access to the project's data!")
  if (download_files == TRUE & !is.null(data_location)) cli::cli_abort("Only one of `download_files` or `data_location` must be set.")
  credentials_exist = file.exists(credentials_file)
  
  
  if (dont_ask == TRUE) response_prompt = 1
  
  # Folder where the jsPsychHelper will store the data
  pid_data_folder = gsub("/", "_", pid)
  folder_data_helper = paste0(folder, "/data/", pid_data_folder, "/") # With the gsub we avoid sub-folders

  
  # CHECK if NO files in project's folder & NO credentials to download
  if (is.null(data_location) & credentials_exist == FALSE) {
    
    cli_message(var_used = pid, 
                h1_title = "ERROR", 
                danger = "Can't access .csv files for protocol `{pid}`:\n- No files in `data/{gsub('/', '_', pid)}/`\n- `data_location` parameter is empty \n- `{credentials_file}` file not present",
                info = "You can either:\n- manually download files to `data/{pid}`\n- edit `.vault/credentials_TEMPLATE` and rename it to `{credentials_file}`")
    
    cli::cli_abort("No way to get the protocol's .csv files")
    
  }
  
  
  # ASK FOR USER PERMISSION
  if (dont_ask == FALSE)  response_prompt = menu(choices = c("Yes", "No"), 
                                                 title = 
                                                   cli_message(h1_title = "Initial SETUP", 
                                                               var_used = folder,
                                                               info = "Do you want to run the {.pkg initial setup}?",
                                                               details = "This will {cli::style_bold((cli::col_red('DELETE')))} the _targets/ folder of {.code {folder}}, 
                                                                         {cli::style_bold((cli::col_green('install')))} necessary packages, 
                                                                         {cli::style_bold((cli::col_green('copy')))} configuration files, 
                                                                         {cli::style_bold((cli::col_yellow('replace')))} the _targets.R, etc."))
      
    
  
  if (response_prompt == 1) {
  
    # 1) Run to make sure you have all the necessary packages and folders -------
    
    cli_message(h1_title = "Setup")
    setup_folders(pid_data_folder = pid_data_folder, folder = folder, extract_zip = TRUE)
    
    
  
    # 2) Copy .csv/.zip files to data/[YOUR_PROJECT_ID]/  ------------
      # DOWNLOAD from server (needs a .vault/credentials file) (rename and edit .vault/credentials_TEMPLATE)
      # OR Copy from data_location
    
    cli_message(h1_title = "Get data files and task script")
    if (download_files == TRUE) {
      
      if (!credentials_exist) cli::cli_abort("Can't find server credentials in '.vault/.credentials'")
      
      # update_data(pid = pid, folder = folder)
      get_zip(pid = pid, what = "data", where = paste0(folder, "/data/"), dont_ask = TRUE, credentials_file = credentials_file)
        
    } else if (download_files == FALSE) {
      
      cli::cli_h1("download_files: {download_files}")
      # CHECK
      if (grepl("\\.zip", data_location)) cli::cli_abort("`data_location` should be a folder ")
      
      # We will check if there is only a zip file or multiple csv's in create_targets_file(). No need to do it here too
    
      cli::cli_alert_info("Will copy files from {.code {data_location}}")
      
      # Copy files from data_location to folder_data_helper
      files_raw = list.files(path = data_location, pattern = "*.csv|*.zip", full.names = TRUE)
      
      if (length(files_raw) == 0) {
        files_raw_extensive_search = list.files(path = data_location, pattern = "*.csv|*.zip", full.names = TRUE, recursive = TRUE, all.files = TRUE)
        
        if (length(files_raw_extensive_search) != 0) {
          
          folders_files_found = paste0(data_location, "/", unique(basename(dirname(files_raw_extensive_search))))
          
          cli::cli_alert_info("We found {length(files_raw_extensive_search)} .csv or .zip files in {.code {folders_files_found}}. \n\n Maybe you meant `data_location = {folders_files_found}`")
        }
        
      } else {
        file.copy(from = files_raw, to = paste0(folder_data_helper,  basename(files_raw)))  
      }
      
      
      # Files present in destination (after copying)
      files_destination = list.files(folder_data_helper, pattern = "*.csv|*.zip", full.names = FALSE)
      cli::cli_alert_info("{length(files_destination)} files in 'data/{pid}'")
      
    }
    
    # Make sure sensitive tasks are in .vault
    move_sensitive_tasks_to_vault(pid = pid, folder = folder, sensitive_tasks = sensitive_tasks)
    
    # Files present in destination (after copying)
    files_pid = list.files(folder_data_helper, pattern = "*.csv|*.zip", full.names = FALSE)
      
      if (download_task_script == TRUE) {
        
        cli_message(h1_title = "Download task script")
        
        # Get protocol without data and zip it in data/protocol_PID.zip
        # TODO: download protocol to 'folder'. Now will download to the wd
        get_zip(pid, what = "protocol", dont_ask = dont_ask)
          
        
      } else if (download_task_script == FALSE) {
        
        cli::cli_alert_info("Will NOT download task script")
        
      }
      
    
    # 3) Create a _targets.R file for your data -------------------------------
    
    cli_message(var_used = folder, h1_title = "Create _targets.R file in {.code {folder}}")
    create_targets_file(pid = pid, folder = folder, dont_ask = dont_ask)
    
    # Copy tests to tests/testthat/
    tests_templates_origin = list.files(paste0(folder, "/inst/templates/tests"), full.names = TRUE, recursive = TRUE)
    tests_templates_destination = gsub("inst/templates/", "", tests_templates_origin)
    folder_destination_snaps = paste0(folder, "/tests/testthat/_snaps/snapshots/") # Create needed folders
    if(!dir.exists(folder_destination_snaps)) dir.create(folder_destination_snaps, recursive = TRUE)
    file.copy(tests_templates_origin, tests_templates_destination, overwrite = TRUE)
    
    # If the pid is not 999 (is not the canonical protocol), delete the snaps folder
      # The snaps folder contains tests for jsPsychHelpeR development
    if (pid != 999) unlink(folder_destination_snaps, recursive = TRUE)
    
    
    cli_message(var_used = folder, h1_title = "Initial setup successful", 
                success = "The new RStudio project is in {.code {folder}}",
                info = "Open `run.R` there to start",
                  
                details = cli::col_grey("Use the following commands to start the data preparation:"),
                list = c("Visualize pipeline: {.code targets::tar_visnetwork()}",
                         "Delete cache: {.code targets::tar_destroy()}", 
                         "Start data preparation: {.code targets::tar_make()}")
    )
    
    # Open _targets.R and run.R
    if (Sys.getenv("RSTUDIO") == "1" & open_rstudio == TRUE) {
      cli_message(info = "Opening new RStudio project")
      
      rstudioapi::openProject(folder, newSession = TRUE)

    } else {
      
      cli_message(var_used = folder, info = "Your RStudio project is in  {.code {folder}}")
      
    }
    
    
  } else {
    
    cli::cli_alert_warning("OK, nothing done")
    
  }
}
