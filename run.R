# Initial setup -----------------------------------------------------------

  # **Run only once**

  # 1) FULLY AUTOMATIC (works on linux computers with FTP credentials)
    # REPLACE 'pid = 999' with your project ID
    invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source))
    run_initial_setup(pid = "999", download_files = TRUE, download_task_script = TRUE)

    
  # 2) SEMI AUTOMATIC
    # Will need to manually place data in 'data/pid' 
    # REPLACE 'pid' with your project ID
    invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source))
    run_initial_setup(pid = 999, download_files = FALSE, download_task_script = FALSE)
    

  
# RUN pipeline -------------------------------------------------------------

  # Visualize targets tree
  targets::tar_visnetwork(targets_only = TRUE, label = "time")

  # First time, and if needed, clean up old targets (deletes _targets/)
  # targets::tar_destroy()
  
  # Run data preparation 
  targets::tar_make()

  
  
# Edit report ---------------------------------------------------------------

  # Open report_analysis.Rmd and edit
  rstudioapi::navigateToFile("Rmd/report_analysis.Rmd")
  
  # After editing it:
  targets::tar_make()

  

# Other commands ----------------------------------------------------------

  # _targets.R contains the full pipeline
  rstudioapi::navigateToFile("_targets.R") # Open _targets.R file
  
  
  # After running the pipeline with targets::tar_make()
  
  # List available objects
  targets::tar_objects()
  
  # Load DF_analysis file
  targets::tar_load(DF_analysis)
  DF_analysis
  
  # CHECK warnings
  targets::tar_meta() %>% select(name, warnings) %>% drop_na()
  
  
  