# FONDECYT 2020 Huepe project working with {targets}


# Parameters --------------------------------------------------------------

options(pillar.sigfig = 5)


# Libraries ---------------------------------------------------------------

library(targets) 
library(tarchetypes) 


# Set options, load packages -----------------------------------------------
  
  # Source all /R files
  lapply(list.files("./R", full.names = TRUE, pattern = ".R"), source)
  
  # Packages to load
  main_packages = c("broom", "broom.mixed", "crayon", "dplyr", "emmeans", "forcats", "ggalluvial", "ggridges", "gmodels", "gt", "gtsummary", "irr", "janitor", "lme4", "parameters", "patchwork", "performance", "psych", "purrr", "readr", "report", "sjPlot", "shrtcts", "stringr", "parameters", "performance", "renv", "tarchetypes", "targets", "testthat", "tidyr")
  non_declared_dependencies = c("qs", "visNetwork", "webshot")
  packages_to_load = c(main_packages, non_declared_dependencies)
  
  # target options (packages, errors...)
  tar_option_set(packages = packages_to_load, # Load packages for all targets
                 error = "save") # Needed to load workspace on error to debug
  
  # Recreates _packages.R with the above packages (so renv founds them). Should be launched after tar_option_set()
  targets::tar_renv(ask = FALSE) # Need to run renv::init() if anything changes
  
  # Make sure tests are run always
  if (file.exists("_targets/objects/TESTS") == TRUE ) targets::tar_invalidate(matches("TESTS"))



# Define targets -------------------------------------------------------------
  
targets <- list(
  
  # _Read files --------------------------------------------------------------
  
    tar_target(input_files, list.files(path = "data", pattern="*.csv", full.names = TRUE), format = "file"),
    tar_target(DF, read_data(input_files)),
    tar_target(DF_tasks, prepare_list_tasks(DF)),


  # _Process tasks -----------------------------------------------------------
  
    # [TODO]: Each of the individual tasks should have TESTS!
    tar_target(df_SBS, prepare_SBS(DF)),
    tar_target(df_CRT7, prepare_CRT7(DF)),
    

  # _Join tasks --------------------------------------------------------------
  
    # [REMEMBER]: Have to manually put every test we prepare here
    tar_target(df_joined, prepare_joined(df_SBS,
                                         df_CRT7)),
    
  
  # _Analysis ----------------------------------------------------------------- 
  
    # Prepare a DF ready for the analysis
    tar_target(df_analysis, prepare_df_analysis(df_joined)),
  
    # Models
    tar_target(model_XXX, analysis_model_XXX(df_analysis)),
    
  
    # Tables
    tar_target(table1_model_XXX, analysis_model_XXX_table(model_XXX)),
    
    
    # Plots
    tar_target(plo1_model_XXX, analysis_model_XXX_plot(model_XXX)),
  
  
  # _Tests -------------------------------------------------------------------
  
    # [REMEMBER]: Have to manually put every target we have a test for here
    tar_target(TESTS, test_testhat(input_files,
                                   DF, 
                                   df_SBS,
                                   df_CRT7))
    
  
)

tar_pipeline(targets)