# Initial setup -----------------------------------------------------------

  # Do it only once #


  # 1) Run to make sure you have all the necessary packages and folders
    source("setup/setup.R")

  # 2) Make sure you create a "data/0/" folder
    if (!dir.exists("data/0/")) dir.create("data/0/")
  
  # 3) **Manually** copy .csv files to data/0/

  # 4) Run to create a _targets.R file for your data
    invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source))
    create_targets_file(pid_protocol = 0, folder_data = "data/0/")
   

# RUN ---------------------------------------------------------------------

  # Visualize targets tree
  targets::tar_visnetwork(targets_only = TRUE, label = "time")

  # First time, and if needed, clean up old targets
  targets::tar_destroy()
  
  # Run project
  targets::tar_make()

  # List available objects
  targets::tar_objects()
  
  # Load DF_analysis file
  targets::tar_load(DF_analysis)
  DF_analysis
  

# Task --------------------------------------------------------------------

  # Open report_analysis.Rmd and edit
  rstudioapi::navigateToFile("doc/report_analysis.Rmd")
  
  # After editing it:
  targets::tar_make()