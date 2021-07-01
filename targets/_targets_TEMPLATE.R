# jsPsychHelpeR with {targets}

# Parameters --------------------------------------------------------------

  pid_target = 999


# Libraries ---------------------------------------------------------------

  library(targets) 
  library(tarchetypes) 


# Set options, load packages -----------------------------------------------
  
  # Source all /R files
  lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source)
  lapply(list.files("./R_tasks/", full.names = TRUE, pattern = ".R$"), source)
  options(pillar.sigfig = 5)
  
  # Packages to load
  main_packages = c("cli", "crayon", "furrr", "patchwork", "renv", "tarchetypes", "targets", "testthat")
  data_preparation_packages = c("dplyr", "forcats", "here", "janitor", "purrr", "readr", "stringr", "tibble", "tidyr") #"safer", 
  data_analysis_packages = c("broom", "broom.mixed", "emmeans", "gmodels", "gt", "gtsummary", "irr", "lme4", "parameters", "performance", "psych", "sjPlot") #"report"
  data_visualization_packages = c("ggalluvial", "ggridges")
  non_declared_dependencies = c("qs", "visNetwork", "webshot", "performance", "shinyWidgets")
  extra_packages = c("shrtcts")
  packages_to_load = c(main_packages, data_preparation_packages, data_analysis_packages, data_visualization_packages, non_declared_dependencies, extra_packages)
  
  # target options (packages, errors...)
  tar_option_set(packages = packages_to_load, # Load packages for all targets
                 error = "workspace") # Needed to load workspace on error to debug
  

  # Make sure tests run always
  if (file.exists("_targets/objects/TESTS") == TRUE ) targets::tar_invalidate(matches("TESTS"))

  

# Define targets -------------------------------------------------------------
  
targets <- list(
  
  ## Read files --------------------------------------------------------------
  
  # RAW data
  tar_target(input_files, list.files(path = paste0("data/", pid_target), pattern="*.csv", full.names = TRUE), format = "file"),
  tar_target(DF_raw, read_data(input_files, anonymize = FALSE)),
  
  # Cleaned data
  tar_target(DF_clean, create_clean_data(DF_raw)),
  
  # Diccionary of tasks
  tar_target(DICCIONARY_tasks, create_diccionary_tasks(DF_clean), priority = 1),

  
  ## Prepare tasks -----------------------------------------------------------
  
  # Use R/prepare_template.R to create new preparation_scripts

TARGETS_HERE
  
  ## Join tasks --------------------------------------------------------------
  
  tar_target(DF_joined, 
             create_joined(
JOINS_HERE
             )),
  

  
  ## Analysis ----------------------------------------------------------------- 
  
  # Prepare a DF ready for the analysis
  tar_target(DF_analysis, create_DF_analysis(DF_joined)),
  
  # [TODO] Descriptive Table 1
  # Important: Should we compare DF_analysis with the final data used in each model? 
  tar_render(descriptives_table1, "doc/descriptives_table1.Rmd", deployment = "main"),

  # Analisys report
  tar_render(report_analysis, "doc/report_analysis.Rmd",
             output_file = paste0("../outputs/reports/report_analysis.html")),


  # Models
  # tar_target(model_E1, analysis_model_E1(DF_analysis)),


  # Tables and plots use the model (e.g. model_XXX) as input.  
  # Most model objects in R include the data used to fit the model

  # Tables
  # tar_target(table1_model_E1, analysis_model_E1_table(model_E1)),

  # Plots
  tar_target(plots_descriptive, analysis_descriptive_plots(DF_joined, DF_raw)),
  # tar_target(plot1_model_E1, analysis_model_E1_plot(model_E1)),
  
  
  ## Tests -------------------------------------------------------------------
  
  # [REMEMBER]: Have to manually put every target we have a test for here (except the automatic tests: 'input_files_automatic_tests_str' takes care of that)
  # tar_target(input_files_automatic_tests_str, list.files(path = "_targets/objects/", pattern="df_*", full.names = FALSE, ignore.case = FALSE)),

  tar_target(TESTS, test_testhat(#input_files_automatic_tests_str = input_files_automatic_tests_str,
                                 input_files,
                                 DF_raw,
                                 DF_clean,
                                 DICCIONARY_tasks,
                                 DF_joined
                                 )
  ),
  
  # Reports ------------------------------------------------------------------

  # Automatic report
  tar_render(report_DF_clean, "doc/report_DF_clean.Rmd", 
             params = list(last_task = "Goodbye",
                           pid_report = pid_target),
             output_file = paste0("../outputs/reports/report_DF_clean.html")),
  
  # Progress report
  tar_render(report_PROGRESS, path = "doc/report_PROGRESS.Rmd", 
             params = list(input_files_vector = input_files, 
                           pid_report = pid_target, 
                           last_task = "Goodbye", 
                           goal = 500),
             output_file = paste0("../outputs/reports/report_PROGRESS_", pid_target , ".html"))


)


# Declare pipeline --------------------------------------------------------

  targets
