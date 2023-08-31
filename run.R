# RUN pipeline -------------------------------------------------------------

  # Your full pipeline is in the file _targets.R. You can open the file with:
  # rstudioapi::navigateToFile("_targets.R")
  
  # Run data preparation 
  targets::tar_make()

  # Visualize targets tree
  targets::tar_visnetwork(targets_only = TRUE, label = "time")

  # In case of an error --- 
    # See: https://books.ropensci.org/targets/debugging.html#workspaces
    # tar_workspaces() # Lists the available workspaces (e.g. DF_clean)
    # tar_workspace(DF_clean) # Loads the errored workspace
  

# Check results -----------------------------------------------------------
  
  # After running the pipeline with targets::tar_make()
  
  # List available objects
  targets::tar_objects()
  
  # Load one of the objects (e.g. DF_analysis)
  targets::tar_load(DF_analysis)
  
  # See DF_analysis dataframe
  DF_analysis$DF_analysis |> View()
  
  
# Edit report ---------------------------------------------------------------

  # Open report_analysis.Rmd and adapt the code:
  rstudioapi::navigateToFile("Rmd/report_analysis.Rmd")
  
  # After editing it you need to open _targets.R and uncomment the
    # Analysis report tar_render() lines there:
  rstudioapi::navigateToFile("_targets.R")
  
  # And finally run the full pipeline:
  targets::tar_make()

  
# Other commands ----------------------------------------------------------
  
  # Destroy pipeline
  # targets::tar_destroy() # This will force the full pipeline to start from 0
  
  # CHECK warnings
  targets::tar_meta() %>% dplyr::select(name, warnings) %>%tidyr::drop_na()


# Create docker container -------------------------------------------------

  # system("docker builder prune --all -f") # Clean all docker builder cache
  
  PID = 999
  jsPsychHelpeR::create_docker_container(PID = PID)
  file.remove(list.files(paste0("~/Downloads/jsPsychHelpeR", PID, "/outputs"), recursive = TRUE, full.names = TRUE))
  system(paste0("docker run --rm -d --name pid", PID, " -v ~/Downloads/jsPsychHelpeR", PID, "/outputs:/home/project/jsPsychHelpeR/outputs:rw gorkang/jspsychhelper:pid", PID))

