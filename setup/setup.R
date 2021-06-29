# Run this script to install all the dependencies needed.

  # You might need to run it twice and restart the RStudio session afterwards: Control + Shift + F10


# Install targets and tarchetypes ------------------------------------------

  if (!require('targets')) install.packages('targets'); library('targets')
  if (!require('tarchetypes')) install.packages('tarchetypes'); library('tarchetypes')
  

# Make sure all packages are present --------------------------------------

  if (!require('rmarkdown')) install.packages('rmarkdown'); library('rmarkdown')
  
  source("_targets.R")
  missing_packages = packages_to_load[!packages_to_load %in% installed.packages()[,1]]
  
  if (length(missing_packages) > 0) {
    cat("The following packages are missing and will be installed: ", packages_to_load[!packages_to_load %in% installed.packages()[,1]])
    install.packages(packages_to_load[!packages_to_load %in% installed.packages()[,1]])
  } else {
    cat(crayon::green("All the necessary packages are present\n"))
  }

  # If you have issues with DT::datables()
  # if (webshot::is_phantomjs_installed() == FALSE) webshot::install_phantomjs()
  

# Make sure all the necessary folders exist -----------------------------
  
  necessary_folders = c("data", "outputs/data", "outputs/plots", "outputs/reliability", "outputs/reports", "outputs/tables", "outputs/tests_outputs", 
                        ".vault", ".vault/docs", ".vault/outputs/data", ".vault/outputs/reports")
  if (all(necessary_folders %in% dir(recursive = TRUE, include.dirs = TRUE, all.files = TRUE))) {
    cat(crayon::green("All the necessary folders are present\n"))
  } else {
    cat(crayon::yellow("Creating necessary folders: "), paste(necessary_folders, collapse = ", "), "\n")
    invisible(purrr::map(necessary_folders, dir.create, recursive = TRUE, showWarnings = FALSE))
    system("chmod 700 -R .vault/")
    
  }


# Server credentials ------------------------------------------------------

  # FOLLOW 1-2-3 here:
    # rstudioapi::navigateToFile("setup/setup_server_credentials.R")
  
  # Will need to install this one time
  # remotes::install_github("skgrange/threadr")
  
  # On Ubuntu linux, install sshpass via Terminal
  # sudo apt install sshpass
  
  
  
# Configure shourcuts ---------------------------------------------------
  
  # Will install the shrtcts package from Github and copy the config files to your home folder
  
  # Control+P: load all packages and functions
  # Control+M: targets::tar_make()
  # Control+L: targets::rstudio_addin_tar_load()
  source("setup/setup_shortcuts.R"); setup_shortcuts()
  
