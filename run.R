# RUN pipeline -------------------------------------------------------------

  # Your pipeline is in the file _targets.R
  
  # Run data preparation 
  targets::tar_make()

  # Visualize targets tree
  targets::tar_visnetwork(targets_only = TRUE, label = "time")


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
  targets::tar_meta() %>% dplyr::select(name, warnings) %>%tidyr::drop_na()


# Create docker container -------------------------------------------------
  
  # Install jsPsychHelpeR package:
  if (!require('remotes')) utils::install.packages('remotes'); remotes::install_github('gorkang/jsPsychHelpeR')
  # system("docker builder prune --all -f") # Clean all docker builder cache
  
  PID = 999
  jsPsychHelpeR::create_docker_container(PID = PID)
  file.remove(list.files(paste0("~/Downloads/jsPsychHelpeR", PID, "/outputs"), recursive = TRUE, full.names = TRUE))
  system(paste0("docker run --rm -d --name pid", PID, " -v ~/Downloads/jsPsychHelpeR", PID, "/outputs:/home/project/jsPsychHelpeR/outputs:rw gorkang/jspsychhelper:pid", PID))

