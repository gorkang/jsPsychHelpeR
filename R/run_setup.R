# DEPRECATED: by jsPsychHelpeR package.

# Run this script to install all the dependencies needed.

  # You might need to run it twice and restart the RStudio session afterwards: Control + Shift + F10

# run_setup <- function(dont_ask = FALSE) {
#   
#   
#   # Make sure all packages are present --------------------------------------
#   
#     # if (!suppressMessages(require('rlang'))) utils::install.packages('rlang'); suppressPackageStartupMessages(suppressWarnings(library('rlang')))
#     rlang::check_installed(c("targets", "tarchetypes", "rmarkdown"), reason = "to run the initial {jsPsychHelpeR} setup")
#   
#     # Create _targets_packages and read all dependencies
#     targets::tar_destroy(ask = FALSE) # To avoid tar_renv() error
#     # targets::tar_renv()
#     targets::tar_renv(extras = c("clustermq", "future", "gt", "markdown", "pingr", "rstudioapi",
#                                  "shiny", "shinybusy", "visNetwork"))
#     
#     packages_renv = gsub("library\\(|\\)", "", readLines("_targets_packages.R")[-1])
#     
#     if (dont_ask == FALSE) {
#       # Asks user before installing all packages missing (if any)
#       rlang::check_installed(packages_renv, reason = "for {jsPsychHelpeR} to work") 
#     } else {
#       info <- rlang:::pkg_version_info(packages_renv)
#       needs_install <- !rlang:::detect_installed(info)
#       missing_pkgs = packages_renv[needs_install]
#       
#       if (length(missing_pkgs) != 0) {
#         cli::cli_alert_info("Will install {.pkg {missing_pkgs}}. Use `dont_ask = FALSE` if you don't want automatic installation of dependencies")
#         utils::install.packages(missing_pkgs, dependencies = FALSE)
#       }
#     }
#     
#     
#     # If you have issues with DT::datables()
#     
#     # if (webshot::is_phantomjs_installed() == FALSE) webshot::install_phantomjs()
#   
#     cli::cli_alert_success("All the necessary packages are present\n")
#     
#   
#   # Clean up ----------------------------------------------------------------
#   
#     # Clean up _targets folder  
#     targets::tar_destroy(ask = FALSE)
#     cli::cli_alert_success("OLD `_targets/*` files deleted\n")
#     
#     # Delete content of outputs
#     invisible(file.remove(list.files("outputs", pattern = "*", full.names = TRUE, recursive = TRUE)))
#   
#   
#   # Make sure all the necessary folders exist -------------------------------
#     # DONE in setup_folders()
#     
#     necessary_folders = c("data/manual_correction", "outputs/backup", "outputs/data", "outputs/plots", "outputs/reliability", "outputs/reports", "outputs/tables", "outputs/tests_outputs", 
#                           ".vault/data_vault", ".vault/Rmd", ".vault/outputs/data", ".vault/outputs/reports")
#     
#     if (all(necessary_folders %in% dir(recursive = TRUE, include.dirs = TRUE, all.files = TRUE))) {
#       
#       cli::cli_alert_success("All the necessary folders are present\n")
#       
#     } else {
#       
#       invisible(purrr::map(necessary_folders, dir.create, recursive = TRUE, showWarnings = FALSE))
#       system("chmod 700 -R .vault/")
#       cli::cli_alert_success("Created necessary folders: {.pkg {necessary_folders}}\n")
# 
#     }
#   
#   
#   # Server credentials ------------------------------------------------------
#   
#     # FOLLOW 1-2-3 here:
#       # rstudioapi::navigateToFile("setup/setup_server_credentials.R")
#     
#     # Will need to install this one time
#     # remotes::install_github("skgrange/threadr")
#     
#     # On Ubuntu linux, install sshpass via Terminal
#     # sudo apt install sshpass
#     
#     
#     
#   # Configure shourcuts ---------------------------------------------------
#     
#     # Will install the shrtcts package from Github and copy the config files to your home folder
#     
#     # Control+P: load all packages and functions
#     # Control+M: targets::tar_make()
#     # Control+L: targets::rstudio_addin_tar_load()
#     
#     if (Sys.info()["sysname"] %in% c("Linux")) {
#       source("setup/setup_shortcuts.R")
#       setup_shortcuts(overwrite = FALSE)
#       cli::cli_alert_success("Shortcuts [only Linux] ready\n")
#     }
#     
# 
# }
