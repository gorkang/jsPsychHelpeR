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
  main_packages = c("crayon", "dplyr", "forcats", "gt", "gtsummary", "janitor", "patchwork", "purrr", "readr", "report", "shrtcts", "stringr", "renv", "tarchetypes", "targets", "testthat", "tidyr")
  data_analysis_dependencies = c("broom", "broom.mixed", "emmeans", "gmodels", "irr", "lme4", "parameters", "performance", "psych", "sjPlot")
  data_visualization_dependencies = c("ggalluvial", "ggridges")
  non_declared_dependencies = c("qs", "visNetwork", "webshot", "performance")
  packages_to_load = c(main_packages, data_analysis_dependencies, data_visualization_dependencies, non_declared_dependencies)
  
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

    # RAW data con anonimizacion
  
      # These should go in a single function. It runs once and then is commented out
        # tar_target(input_files, list.files(path = "data", pattern="*.csv", full.names = TRUE), format = "file"),
        # tar_target(DF_raw_dirty, read_data(input_files)),
        # tar_target(DF_raw_clean, anonymize_data(DF_raw_dirty)),
  
      # This stays uncommented after running anonimization once
        # tar_target(DF_raw, read_data_anonimized(DF_raw_clean)), 
  
  
    # RAW data sin anonimizacion
    tar_target(input_files, list.files(path = "data", pattern="*.csv", full.names = TRUE), format = "file"),
    tar_target(DF_raw, read_data(input_files)),

  
    # Cleaned data
    tar_target(DF_clean, create_clean_data(DF_raw)),
  
    # Diccionary of tasks
    tar_target(DICCIONARY_tasks, create_diccionary_tasks(DF_clean)),


  # _Process tasks -----------------------------------------------------------
  
    # Use R/prepare_template.R to create new preparation_scripts
  
    # [TODO]: Each of the individual tasks should have specific hardcoded TESTS!
    tar_target(df_SBS, prepare_SBS(DF_clean,  name_scale_str = "Supernatural_Belief_Scale", short_name_scale_str = "Supernatural")),
    tar_target(df_CRT7, prepare_CRT7(DF_clean, name_scale_str = "CRT_7", short_name_scale_str = "CRT_7")),
    tar_target(df_Goldberg, prepare_Goldberg(DF_clean, name_scale_str = "Goldberg_Questionnaire", short_name_scale_str = "Goldberg")),
    tar_target(df_MagicalIdeation, prepare_Magical_Ideation(DF_clean, name_scale_str = "Magical_Ideation", short_name_scale_str = "Magical_Ideation")),
  

  # _Join tasks --------------------------------------------------------------
  
    # [REMEMBER]: Have to manually put every test we prepare here
      # rlang::sym("s") para convertir en simbolos caracteres. 
      # Si usamos estandar para el output de prepared_TASKS() (por ejemplo, df_XXX, vs DF_XXX), podemos hacer que se joineen aqui automaticamente!!!
    tar_target(DF_joined, create_joined(df_SBS,
                                        df_CRT7,
                                        df_MagicalIdeation)),
    
  
  # _Analysis ----------------------------------------------------------------- 
  
    # Prepare a DF ready for the analysis
    tar_target(DF_analysis, create_DF_analysis(DF_joined)),

  
    # [TODO] Descriptive Table 1
    # tar_target(table_descriptive, analysis_descriptive(df_analysis)),
    # Importante: Comparar los datos de df_analysis con los datos finalmente usados en el modelo? Al menos reportar numero de missings y similares
    
    # Models
    tar_target(model_XXX, analysis_model_XXX(DF_analysis)),


    # Tables
    tar_target(table1_model_XXX, analysis_model_XXX_table(model_XXX)),


    # Plots
    tar_target(plot1_model_XXX, analysis_model_XXX_plot(model_XXX)),
  
  
  # _Tests -------------------------------------------------------------------
  
    # [REMEMBER]: Have to manually put every target we have a test for here
    # rlang::sym("s") para convertir en simbolos caracteres. 
    # Si usamos estandar para el output de prepared_TASKS() (por ejemplo, df_XXX, vs DF_XXX), podemos hacer que se joineen aqui automaticamente!!!
    tar_target(TESTS, test_testhat(input_files,
                                   DF_clean, 
                                   DF_joined,
                                   df_SBS,
                                   df_CRT7
                                   # df_MagicalIdeation,
                                   )),
  

  # Report ------------------------------------------------------------------

     # Automatic report
    tar_render(report, "doc/report.Rmd")
  

)


# Declare pipeline --------------------------------------------------------

  tar_pipeline(targets)
