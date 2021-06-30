# Uncomment and run the necessary bits

# Initial setup -----------------------------------------------------------

  # Run the fist time or when you have an error
    targets::tar_renv()
    source("setup/setup.R")


# Automatically creating _targets from data folder ------------------------

  # Make sure you create a folder called "0" in "data/": data/0/
    if (!dir.exists("data/0/")) dir.create("data/0/")
  
  # Manually copy .csv files to data/0/

  # Run once if you want to create a _targets.R file using the data folder
    lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source)
    create_targets_file(folder_data = "data/0/")
   
  # Afterwards: 
    # 1) DELETE _targets.R
      # file.remove("_targets.R")
    # 2) RENAME _targets_automatic_file.R as _targets.R
      # file.rename(from = "_targets_automatic_file.R", to = "_targets.R")
    
  # Finally:
    # Go to _targets.R and change pid_target = 999 for pid_target = 0
    rstudioapi::navigateToFile("_targets.R")



# RUN ---------------------------------------------------------------------

  # Visualize targets tree
  targets::tar_visnetwork(targets_only = TRUE, label = "time")

  # First time, and if needed, clean up old targets
  targets::tar_destroy()
  
  # Run project
  targets::tar_make()

  # List available objects
  targets::tar_objects()
  
  # See DF_joined file
  targets::tar_load(DF_analysis)
  DF_analysis
  
  