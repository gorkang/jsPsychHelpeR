# Initial setup -----------------------------------------------------------

  # Install necessary libraries for data preparation
  if (!require('pak')) utils::install.packages('pak')
  pak::pkg_install("gorkang/jsPsychHelpeR")


# Run pipeline -------------------------------------------------------------

  # Run data preparation 
  targets::tar_make()
  
  # The full pipeline is in the file _targets.R. Open it with:
  # rstudioapi::navigateToFile("_targets.R")

  # Visualize targets tree
  targets::tar_visnetwork(targets_only = TRUE, label = "time")

  
  # In case of an error --- 
  
    # You can clean up everything so targets will run from scratch
    # targets::tar_destroy()
  
    # See: https://books.ropensci.org/targets/debugging.html#workspaces
    # tar_workspaces() # Lists the available workspaces (e.g. DF_clean)
    # tar_workspace(DF_clean) # Loads the errored workspace
  
  

# Check results -----------------------------------------------------------
  
  # After running the pipeline with targets::tar_make()
  
  # Load all libraries
  targets::tar_load_globals()
  
  # Load all targets (e.g. DF_joined, DF_analysis, etc.)
  targets::tar_load_everything()
  
  # Or load a specific DF
  # targets::tar_load(DF_joined)
  
  # List available objects
  targets::tar_objects()
  
  # See DF_analysis dataframe
  DF_analysis$DF_analysis |> View()
  

# Join data from DF_joined to DF_analysis ---------------------------------

  # Select columns from DF_joined
  DF_final = DF_joined |> 
    select(id, 
           matches("DEMOGR46"),
           ends_with("DIRd"), ends_with("DIRt") 
           )
  
  # Save to a csv
  write_csv(DF_final, "outputs/data/DF_final.csv")
  write_csv2(DF_final, "outputs/data/DF_final_sp.csv")
  
  
  
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



# Admin -------------------------------------------------------------------

  # Create new task
  jsPsychHelpeR::create_new_task("NEW_TASK", get_info_googledoc = TRUE)
  
  # Get data
  jsPsychHelpeR::get_zip(
    pid = "protocols_DEV/999/", what = "data", where = "data/",
    credentials_file = "~/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/jsPsychAdmin/.vault/.credentials"
  )
  
  