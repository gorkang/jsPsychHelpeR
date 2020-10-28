
# Install packages ---------------------------------------------------------

  # remotes::install_github("wlandau/targets")
  # remotes::install_github("wlandau/tarchetypes")
  # remotes::install_github("gadenbuie/shrtcts")


# Run project --------------------------------------------------------------

  targets::tar_make()

  
# Visualize targets networks -----------------------------------------------

  targets::tar_visnetwork(targets_only = TRUE) #label = "time"
  # targets::tar_glimpse()

# Destroy cache (_targets folder) -----------------------------------------

  targets::tar_destroy()



# Invalidate a specific target (to rerun it) -----------------------------
  
  targets::tar_invalidate(matches("TESTS"))


# See warnings in all functions ------------------------------------------
  targets::tar_meta(fields = warnings) %>% tidyr::drop_na(warnings)# See warnings



# Errors ------------------------------------------------------------------

  # If we get an error, load workspace of errored state
  targets::tar_workspace(TESTS)
  targets::tar_undebug() # Delete all the debugging stuff
  
  
  
# Data frame of targets info
  targets::tar_manifest(fields = "command")



# Load individual object
  # targets::tar_load(model_PPV)

 
 # targets::tar_outdated()
 