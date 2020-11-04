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
  main_packages = c("cli", "crayon", "furrr", "patchwork", "renv", "tarchetypes", "targets", "testthat")
  data_preparation_packages = c("dplyr", "forcats", "here", "janitor", "purrr", "readr", "stringr", "tidyr") #"safer", 
  data_analysis_packages = c("broom", "broom.mixed", "emmeans", "gmodels", "gt", "gtsummary", "irr", "lme4", "parameters", "performance", "psych", "sjPlot") #"report"
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
    tar_target(input_files, list.files(path = "data", pattern="*.csv", full.names = TRUE), format = "file"), #, format = "file" (IF files in vault/ first run fails)
    tar_target(DF_raw, read_data(input_files, anonymize = FALSE)),

  
    # Cleaned data
    tar_target(DF_clean, create_clean_data(DF_raw)),
  
    # Diccionary of tasks
    tar_target(DICCIONARY_tasks, create_diccionary_tasks(DF_clean)),


  # _Prepare tasks -----------------------------------------------------------
  
    # Use R/prepare_template.R to create new preparation_scripts
  
    # [TODO]: Each of the individual tasks should have specific hardcoded TESTS!
    # [TODO]: Will change short_name_scale_str for the final names once we have the corrected names!
  
    # [REMEMBER]: the target name needs to be ==  df_[short_name_scale_str]
    tar_target(df_AIM, prepare_AIM(DF_clean, short_name_scale_str = "AIM")),
  
    tar_target(df_bRCOPE, prepare_bRCOPE(DF_clean, short_name_scale_str = "bRCOPE")),
    tar_target(df_Cov19Q, prepare_Cov19Q(DF_clean, short_name_scale_str = "Cov19Q")),
    tar_target(df_CRS, prepare_CRS(DF_clean, short_name_scale_str = "CRS")),
    tar_target(df_CRT7, prepare_CRT7(DF_clean, short_name_scale_str = "CRT7")),
    tar_target(df_CRTv, prepare_CRTv(DF_clean, short_name_scale_str = "CRTv")),
    tar_target(df_ERQ, prepare_ERQ(DF_clean, short_name_scale_str = "ERQ")),
    tar_target(df_FDMQ, prepare_FDMQ(DF_clean, short_name_scale_str = "FDMQ")),
  
    # tar_target(df_GHQ12, prepare_GHQ12(DF_clean, short_name_scale_str = "GHQ12")),
    tar_target(df_IDQ, prepare_IDQ(DF_clean, short_name_scale_str = "IDQ")),
    tar_target(df_IEC, prepare_IEC(DF_clean, short_name_scale_str = "IEC")),
    tar_target(df_IRI, prepare_IRI(DF_clean, short_name_scale_str = "IRI")),
    tar_target(df_IRS, prepare_IRS(DF_clean, short_name_scale_str = "IRS")),
    tar_target(df_MIS, prepare_MIS(DF_clean, short_name_scale_str = "MIS")),
    tar_target(df_PBS, prepare_PBS(DF_clean, short_name_scale_str = "PBS")),
    tar_target(df_PSS, prepare_PSS(DF_clean, short_name_scale_str = "PSS")),
    tar_target(df_REI40, prepare_REI40(DF_clean, short_name_scale_str = "REI40")),
    tar_target(df_RSS, prepare_RSS(DF_clean, short_name_scale_str = "RSS")),
    tar_target(df_RTS, prepare_RTS(DF_clean, short_name_scale_str = "RTS")),
    tar_target(df_SASS, prepare_SASS(DF_clean, short_name_scale_str = "SASS")),
    tar_target(df_SBS, prepare_SBS(DF_clean, short_name_scale_str = "SBS")),
    tar_target(df_SCSORF, prepare_SCSORF(DF_clean, short_name_scale_str = "SCSORF")),
    # tar_target(df_SDG, prepare_SDG(DF_clean, short_name_scale_str = "SDG")),
    tar_target(df_SRA, prepare_SRA(DF_clean, short_name_scale_str = "SRA")),
    tar_target(df_SRSav, prepare_SRSav(DF_clean, short_name_scale_str = "SRSav")),
    tar_target(df_SWBQ, prepare_SWBQ(DF_clean, short_name_scale_str = "SWBQ")),
    tar_target(df_WEBEXEC, prepare_WEBEXEC(DF_clean, short_name_scale_str = "WEBEXEC")),
  
  
  

  # _Join tasks --------------------------------------------------------------
  
    # [REMEMBER]: Have to manually put every test we prepare here
      # rlang::sym("s") para convertir en simbolos caracteres. 
      # Si usamos estandar para el output de prepared_TASKS() (por ejemplo, df_XXX, vs DF_XXX), podemos hacer que se joineen aqui automaticamente!!!
    tar_target(DF_joined, create_joined(df_AIM,
                                        df_bRCOPE,
                                        df_Cov19Q,
                                        df_CRS,
                                        df_CRT7,
                                        df_CRTv,
                                        df_ERQ,
                                        df_FDMQ,
                                        # df_GHQ12,
                                        df_IDQ,
                                        df_IEC,
                                        df_IRI,
                                        df_IRS,
                                        df_MIS,
                                        df_PBS,
                                        df_PSS,
                                        df_REI40,
                                        df_RSS,
                                        df_RTS,
                                        df_SASS,
                                        df_SBS,
                                        df_SCSORF,
                                        # df_SDG,
                                        df_SRA,
                                        df_SRSav,
                                        df_SWBQ,
                                        df_WEBEXEC)),
    
  
  # _Analysis ----------------------------------------------------------------- 
  
    # Prepare a DF ready for the analysis
    tar_target(DF_analysis, create_DF_analysis(DF_joined)),
  
  
    # [TODO] Descriptive Table 1
    # Important: Should we compare DF_analysis with the final data used in each model? 
    tar_render(descriptives_table1, "doc/descriptives_table1.Rmd"),
  
    
    # Models
    tar_target(model_E1, analysis_model_E1(DF_analysis)),


    # Tables and plots use the model (e.g. model_XXX) as input.  
    # Most model objects in R include the data used to fit the model
  
      # Tables
      tar_target(table1_model_E1, analysis_model_E1_table(model_E1)),
  
  
      # Plots
      tar_target(plot1_model_E1, analysis_model_E1_plot(model_E1)),
    
  
  # _Tests -------------------------------------------------------------------
  
    # [REMEMBER]: Have to manually put every target we have a test for here (except the automatic tests: 'input_files_automatic_tests_str' takes care of that)
    tar_target(input_files_automatic_tests_str, list.files(path = "_targets/objects/", pattern="df_*", full.names = FALSE, ignore.case = FALSE)),
  
    tar_target(TESTS, test_testhat(input_files_automatic_tests_str = input_files_automatic_tests_str,
                                   input_files,
                                   DF_raw,
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
