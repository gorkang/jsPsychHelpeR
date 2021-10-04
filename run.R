# Initial setup -----------------------------------------------------------

  # Do it only once #
  invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source))
  run_initial_setup(pid = 999) # REPLACE pid = 0 with your project ID




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
  
  # CHECK warnings
  targets::tar_meta() %>% select(name, warnings) %>% drop_na()

# Task --------------------------------------------------------------------

  # Open report_analysis.Rmd and edit
  rstudioapi::navigateToFile("doc/report_analysis.Rmd")
  
  # After editing it:
  targets::tar_make()