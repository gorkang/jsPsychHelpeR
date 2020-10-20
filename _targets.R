# jsPsychHelpeR with {targets}

# Parameters --------------------------------------------------------------

  options(pillar.sigfig = 5)


# Libraries ---------------------------------------------------------------

  library(targets) 
  library(tarchetypes) 


# Set options, load packages -----------------------------------------------
  
  # Source all /R files
  lapply(list.files("./R", full.names = TRUE, pattern = ".R"), source)
  
  # Packages to load
  main_packages = c("cli", "crayon", "patchwork", "renv", "tarchetypes", "targets", "testthat")
  data_preparation_packages = c("dplyr", "forcats", "janitor", "purrr", "readr", "safer", "stringr", "tidyr")
  data_analysis_packages = c("broom", "broom.mixed", "emmeans", "gmodels", "gt", "gtsummary", "irr", "lme4", "parameters", "performance", "psych", "report", "sjPlot")
  data_visualization_packages = c("ggalluvial", "ggridges")
  non_declared_dependencies = c("qs", "visNetwork", "webshot", "performance")
  extra_packages = c("shrtcts")
  packages_to_load = c(main_packages, data_preparation_packages, data_analysis_packages, data_visualization_packages, non_declared_dependencies, extra_packages)
  
  # target options (packages, errors...)
  tar_option_set(packages = packages_to_load, # Load packages for all targets
                 error = "save") # Needed to load workspace on error to debug
  
  # Recreates _packages.R with the above packages (so renv founds them). Should be launched after tar_option_set()
  targets::tar_renv(ask = FALSE) # Need to run renv::init() if anything changes
  
  # Make sure tests run always
  if (file.exists("_targets/objects/TESTS") == TRUE ) targets::tar_invalidate(matches("TESTS"))

  

# Define targets -------------------------------------------------------------
  
targets <- list(
  
  # _Read files --------------------------------------------------------------

    # RAW data
    tar_target(input_files, list.files(path = "data", pattern="*.csv", full.names = TRUE)), #, format = "file" (IF files in vault/ first run fails)
    tar_target(DF_raw, read_data(input_files, anonymize = TRUE)),

  
    # Cleaned data
    tar_target(DF_clean, create_clean_data(DF_raw)),
  
    # Diccionary of tasks
    tar_target(DICCIONARY_tasks, create_diccionary_tasks(DF_clean)),


  # _Prepare tasks -----------------------------------------------------------
  
    # Use R/prepare_template.R to create new preparation_scripts
  
    # [TODO]: Each of the individual tasks should have specific hardcoded TESTS!
    # [REMEMBER]: the target name needs to be ==  df_[short_name_scale_str]
    tar_target(df_CRT7, prepare_CRT7(DF_clean, short_name_scale_str = "CRT_7")),
    tar_target(df_GHQ12, prepare_GHQ12(DF_clean, short_name_scale_str = "Goldberg")),
    tar_target(df_MIS, prepare_MIS(DF_clean, short_name_scale_str = "Magical_Ideation")),
    tar_target(df_bRCOPE, prepare_bRCOPE(DF_clean, short_name_scale_str = "Religious_Coping")),
  

  # _Join tasks --------------------------------------------------------------
  
    # [REMEMBER]: Have to manually put every test we prepare here
      # rlang::sym("s") para convertir en simbolos caracteres. 
      # Si usamos estandar para el output de prepared_TASKS() (por ejemplo, df_XXX, vs DF_XXX), podemos hacer que se joineen aqui automaticamente!!!
    tar_target(DF_joined, create_joined(df_CRT7,
                                        df_MIS,
                                        df_bRCOPE)),
    
  
  # _Analysis ----------------------------------------------------------------- 
  
    # Prepare a DF ready for the analysis
    tar_target(DF_analysis, create_DF_analysis(DF_joined)),
  
  
    # [TODO] Descriptive Table 1
    # Important: Should we compare DF_analysis with the final data used in each model? 
    tar_render(descriptives_table1, "doc/descriptives_table1.Rmd"),
  
    
    # Models
    tar_target(model_XXX, analysis_model_XXX(DF_analysis)),


    # Tables and plots use the model (e.g. model_XXX) as input.  
    # Most model objects in R include the data used to fit the model
  
      # Tables
      tar_target(table1_model_XXX, analysis_model_XXX_table(model_XXX)),
  
  
      # Plots
      tar_target(plot1_model_XXX, analysis_model_XXX_plot(model_XXX)),
    
  
  # _Tests -------------------------------------------------------------------
  
    # [REMEMBER]: Have to manually put every target we have a test for here (except the automatic tests: 'input_files_automatic_tests_str' takes care of that)
    tar_target(input_files_automatic_tests_str, list.files(path = "_targets/objects/", pattern="df_*", full.names = FALSE, ignore.case = FALSE)),
  
    tar_target(TESTS, test_testhat(input_files_automatic_tests_str = input_files_automatic_tests_str,
                                   input_files,
                                   DF_clean, 
                                   DICCIONARY_tasks,
                                   DF_joined
                                   )
               ),
  

  # Report ------------------------------------------------------------------

     # Automatic report
    tar_render(report_DF_clean, "doc/report_DF_clean.Rmd")
  

)


# Declare pipeline --------------------------------------------------------

  tar_pipeline(targets)
