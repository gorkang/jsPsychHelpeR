# Run this script to install all the dependencies needed.

  # You might need to run it twice and restart the RStudio session afterwards: Control + Shift + F10


# Make sure all packages are present --------------------------------------

  if (!require('rlang')) install.packages('rlang'); library('rlang')
  if (!require('targets')) install.packages('targets'); library('targets')
  if (!require('tarchetypes')) install.packages('tarchetypes'); library('tarchetypes')
  if (!require('rmarkdown')) install.packages('rmarkdown'); library('rmarkdown')


# REVIEW: This solves the renv error without needing to tar_destroy()?
# if (file.exists("_targets_packages.R")) {
# } else {
#   cli::cli_abort("RUN `targets::tar_renv()` in the console before proceeding")
# }

  # Create _targets_packages and read all dependencies
  targets::tar_destroy(ask = FALSE) # To avoid tar_renv() error
  targets::tar_renv()
  packages_renv = gsub("library\\(|\\)", "", readLines("_targets_packages.R")[-1])
  
  # Asks user before installing all packages missing (if any)
  rlang::check_installed(packages_renv, reason = "for jsPsychHelpeR to work") 
  
  # If you have issues with DT::datables()
  if (webshot::is_phantomjs_installed() == FALSE) webshot::install_phantomjs()


# Clean up ----------------------------------------------------------------

  # Clean up _targets folder  
  cli::cli_alert_warning("Destroying OLD _targets/* files\n")
  targets::tar_destroy(ask = FALSE)
  
  # Delete content of outputs
  invisible(file.remove(list.files("outputs", pattern = "*", full.names = TRUE, recursive = TRUE)))


# Make sure all the necessary folders exist -----------------------------
  
  necessary_folders = c("data/manual_correction", "outputs/backup", "outputs/data", "outputs/plots", "outputs/reliability", "outputs/reports", "outputs/tables", "outputs/tests_outputs", 
                        ".vault/data_vault", ".vault/Rmd", ".vault/outputs/data", ".vault/outputs/reports")
  
  if (all(necessary_folders %in% dir(recursive = TRUE, include.dirs = TRUE, all.files = TRUE))) {
    
    cli::cli_alert_success("All the necessary folders are present\n")
    
  } else {
    
    cli::cli_alert_warning("Creating necessary folders: \n")
    cli::cli_li(paste(necessary_folders, collapse = ", "), "\n")
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
  
  if (Sys.info()["sysname"] %in% c("Linux")) {
    cli::cli_alert_warning("Setting up shortcuts [only Linux]\n")
    source("setup/setup_shortcuts.R")
    setup_shortcuts(overwrite = FALSE)
  }
  
