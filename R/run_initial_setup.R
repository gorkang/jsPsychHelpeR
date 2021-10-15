run_initial_setup <- function(pid, download_files = TRUE) {
  
  # DEBUG
  # pid = "0"
  
  files_pid = list.files(paste0("data/", pid))
  credentials_exist = file.exists(".vault/.credentials")
  SSHPASS = Sys.which("sshpass") # Check if sshpass is installed
  RSYNC = Sys.which("rsync") # Check if rsync is installed
  
  if (length(files_pid) == 0 & credentials_exist == FALSE) {
    cat(crayon::red(
    paste0("Can't access protocol csv files:
    - No files in data/", pid), " 
    - .vault/credentials file not present\n"),
    crayon::silver("You can either:
    - manually download files to", paste0("data/", pid), "
    - edit .vault/credentials_TEMPLATE and rename it to .vault/credentials"))
  }
  
  invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source))
  
  # 1) Run to make sure you have all the necessary packages and folders
  cli::cli_h1("Setup")
  source("setup/setup.R")
  
  # 2) Make sure the folder "data/[YOUR_PROJECT_ID]/" exists
  if (!dir.exists(paste0("data/", pid, "/"))) dir.create(paste0("data/", pid, "/"))
  
  # 3) **Manually** copy .csv files to data/[YOUR_PROJECT_ID]/ or DOWNLOAD from server (needs a .vault/credentials file. Rename and edit .vault/credentials_TEMPLATE)
  if (credentials_exist & download_files == TRUE) {
    
    # sshpass and rsync installed (?)
    if (SSHPASS != "" & RSYNC != "") { 
      cli::cli_h1("Download files")
      update_data(id_protocol = pid) 
    } else {
      cli::cli_text(cli::col_red("{cli::symbol$cross} "), "sshpass or rsync not installed. Can't use `update_data()`")
      cli::cli_text(cli::col_silver("- You need to manually download the files to '", paste0("data/", pid), "'"))
    }
  } else {
    cli::cli_text(cli::col_red("{cli::symbol$cross} "), "Can find server credentials in '.vault/.credentials'")
  }
  
  # 4) Create a _targets.R file for your data
  cli::cli_h1("Create _targets.R file")
  create_targets_file(pid_protocol = pid, folder_data = paste0("data/", pid, "/"))

}
