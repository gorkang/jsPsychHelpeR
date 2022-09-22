# Initial setup -----------------------------------------------------------

  # **Run only once**
  # REPLACE 'pid' below with your project ID
  # If you have the FTP credentials in .vault/.credentials, run 1)
  # If you manually copied the .csv files to data/pid, run 2)

  # 1) FULLY AUTOMATIC: Will download data automatically
    invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source))
    run_initial_setup(pid = "999", download_files = TRUE, download_task_script = TRUE)
    
  # 2) SEMI AUTOMATIC: Manually place data in 'data/pid' 
    invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source))
    run_initial_setup(pid = "999", download_files = FALSE, download_task_script = FALSE)
    

  
# RUN pipeline -------------------------------------------------------------

  # Visualize targets tree
  targets::tar_visnetwork(targets_only = TRUE, label = "time")

  # First time, and if needed, clean up old targets (deletes _targets/)
  targets::tar_destroy()
  
  # Run data preparation 
  targets::tar_make()


# Check results -----------------------------------------------------------
  
  # After running the pipeline with targets::tar_make()
  
  # List available objects
  targets::tar_objects()
  
  # Load DF_analysis file
  targets::tar_load(DF_analysis)
  
  # See DF_analysis dataframe
  DF_analysis
  
  
  
# Edit report ---------------------------------------------------------------

  # Open report_analysis.Rmd and edit
  rstudioapi::navigateToFile("Rmd/report_analysis.Rmd")
  
  # After editing it:
  targets::tar_make()


  
  
# Other commands ----------------------------------------------------------
  
  # _targets.R contains the full pipeline
  rstudioapi::navigateToFile("_targets.R") # Open _targets.R file
  
  # CHECK warnings
  targets::tar_meta() %>% select(name, warnings) %>% drop_na()
  
  
  