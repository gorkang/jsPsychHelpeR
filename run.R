
# Initial setup (run once) ------------------------------------------------

# Install jsPsychHelpeR package:

  if (!require('remotes')) utils::install.packages('remotes'); remotes::install_github('gorkang/jsPsychHelpeR')
  if (!require('renv')) utils::install.packages('renv'); renv::install("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/jsPsychHelpeR_0.2.0.tar.gz")

# Create and configure RStudio project with data preparation (use the folder parameter to set a destination)

  # REPLACE 'pid' below with your project ID

  # 1) FULLY AUTOMATIC: If you have the FTP credentials in .vault/.credentials
  jsPsychHelpeR::run_initial_setup(pid = '999', download_files = TRUE, download_task_script = FALSE, dont_ask = TRUE)
    
  # 2) SEMI AUTOMATIC: If you have the raw data somewhere in your computer
  jsPsychHelpeR::run_initial_setup(pid = '999', data_location = '~/Downloads/JSPSYCH/24/', dont_ask = TRUE)
    

  
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
  targets::tar_meta() %>% dplyr::select(name, warnings) %>%tidyr::drop_na()
  
  
  

# Create docker container -------------------------------------------------
  
  PID = 999
  jsPsychHelpeR::create_docker_container(PID = PID)
  file.remove(list.files(paste0("~/Downloads/jsPsychHelpeR", PID, "/outputs"), recursive = TRUE, full.names = TRUE))
  system(paste0("docker run --rm -d --name pid", PID, " -v ~/Downloads/jsPsychHelpeR", PID, "/outputs:/home/project/jsPsychHelpeR/outputs:rw gorkang/jspsychhelper:pid", PID))
  
  