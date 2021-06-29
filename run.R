# Uncomment and run the necessary bits

# Initial setup -----------------------------------------------------------

  # Run the fist time or when you have an error
    # targets::tar_renv()
    # source("setup/setup.R")


# Automatically creating _targets from data folder ------------------------

  # Run once if you want to create a _targets.R file using the data folder
    # create_targets_file(folder_data = "data/")
   
  # Afterwards manually: 
  # 1) DELETE _targets.R
  # 2) RENAME _targets_automatic_file.R as _targets.R



# RUN ---------------------------------------------------------------------

  # Visualize targets tree
  targets::tar_visnetwork(targets_only = TRUE, label = "time")

  # First time, and if needed, clean up old targets
  targets::tar_destroy()
  
  # Run project
  targets::tar_make()

  # See DF_joined file
  targets::tar_load(DF_joined)
  DF_joined
  