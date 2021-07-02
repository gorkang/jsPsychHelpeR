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

   tar_target(df_ESM, prepare_ESM(DF_clean, short_name_scale_str = 'ESM')),
   tar_target(df_AIM, prepare_AIM(DF_clean, short_name_scale_str = 'AIM')),
   tar_target(df_BNT, prepare_BNT(DF_clean, short_name_scale_str = 'BNT')),
   tar_target(df_bRCOPE, prepare_bRCOPE(DF_clean, short_name_scale_str = 'bRCOPE')),
   tar_target(df_Consent, prepare_Consent(DF_clean, short_name_scale_str = 'Consent')),
   tar_target(df_Cov19Q, prepare_Cov19Q(DF_clean, short_name_scale_str = 'Cov19Q')),
   tar_target(df_COVIDCONTROL, prepare_COVIDCONTROL(DF_clean, short_name_scale_str = 'COVIDCONTROL')),
   tar_target(df_CRS, prepare_CRS(DF_clean, short_name_scale_str = 'CRS')),
   tar_target(df_CRT7, prepare_CRT7(DF_clean, short_name_scale_str = 'CRT7')),
   tar_target(df_CRTMCQ4, prepare_CRTMCQ4(DF_clean, short_name_scale_str = 'CRTMCQ4')),
   tar_target(df_CRTv, prepare_CRTv(DF_clean, short_name_scale_str = 'CRTv')),
   tar_target(df_DEBRIEF, prepare_DEBRIEF(DF_clean, short_name_scale_str = 'DEBRIEF')),
   tar_target(df_DEMOGR, prepare_DEMOGR(DF_clean, short_name_scale_str = 'DEMOGR')),
   tar_target(df_EAR, prepare_EAR(DF_clean, short_name_scale_str = 'EAR')),
   tar_target(df_EmpaTom, prepare_EmpaTom(DF_clean, short_name_scale_str = 'EmpaTom')),
   tar_target(df_ERQ, prepare_ERQ(DF_clean, short_name_scale_str = 'ERQ')),
   tar_target(df_FDMQ, prepare_FDMQ(DF_clean, short_name_scale_str = 'FDMQ')),
   tar_target(df_GHQ12, prepare_GHQ12(DF_clean, short_name_scale_str = 'GHQ12')),
   tar_target(df_Goodbye, prepare_Goodbye(DF_clean, short_name_scale_str = 'Goodbye')),
   tar_target(df_HRPVB, prepare_HRPVB(DF_clean, short_name_scale_str = 'HRPVB')),
   tar_target(df_HRPVBpost, prepare_HRPVBpost(DF_clean, short_name_scale_str = 'HRPVBpost')),
   tar_target(df_IDQ, prepare_IDQ(DF_clean, short_name_scale_str = 'IDQ')),
   tar_target(df_IEC, prepare_IEC(DF_clean, short_name_scale_str = 'IEC')),
   tar_target(df_INFCONS, prepare_INFCONS(DF_clean, short_name_scale_str = 'INFCONS')),
   tar_target(df_IRI, prepare_IRI(DF_clean, short_name_scale_str = 'IRI')),
   tar_target(df_IRS, prepare_IRS(DF_clean, short_name_scale_str = 'IRS')),
   # tar_target(df_ITC, prepare_ITC(DF_clean, short_name_scale_str = 'ITC')),
   tar_target(df_MIS, prepare_MIS(DF_clean, short_name_scale_str = 'MIS')),
   tar_target(df_OBJNUM, prepare_OBJNUM(DF_clean, short_name_scale_str = 'OBJNUM')),
   tar_target(df_OTRASRELIG, prepare_OTRASRELIG(DF_clean, short_name_scale_str = 'OTRASRELIG')),
   tar_target(df_PBS, prepare_PBS(DF_clean, short_name_scale_str = 'PBS')),
   tar_target(df_PRFBM, prepare_PRFBM(DF_clean, short_name_scale_str = 'PRFBM')),
   tar_target(df_PRFBMpost, prepare_PRFBMpost(DF_clean, short_name_scale_str = 'PRFBMpost')),
   tar_target(df_PSETPP, prepare_PSETPP(DF_clean, short_name_scale_str = 'PSETPP')),
   tar_target(df_PSPPC, prepare_PSPPC(DF_clean, short_name_scale_str = 'PSPPC')),
   tar_target(df_PSS, prepare_PSS(DF_clean, short_name_scale_str = 'PSS')),
   tar_target(df_PWb, prepare_PWb(DF_clean, short_name_scale_str = 'PWb')),
   tar_target(df_REI40, prepare_REI40(DF_clean, short_name_scale_str = 'REI40')),
   tar_target(df_RSS, prepare_RSS(DF_clean, short_name_scale_str = 'RSS')),
   tar_target(df_RTS, prepare_RTS(DF_clean, short_name_scale_str = 'RTS')),
   tar_target(df_SASS, prepare_SASS(DF_clean, short_name_scale_str = 'SASS')),
   tar_target(df_SBS, prepare_SBS(DF_clean, short_name_scale_str = 'SBS')),
   tar_target(df_SCSORF, prepare_SCSORF(DF_clean, short_name_scale_str = 'SCSORF')),
   tar_target(df_SDG, prepare_SDG(DF_clean, short_name_scale_str = 'SDG')),
   tar_target(df_SRA, prepare_SRA(DF_clean, short_name_scale_str = 'SRA')),
   tar_target(df_SRSav, prepare_SRSav(DF_clean, short_name_scale_str = 'SRSav')),
   tar_target(df_SWBQ, prepare_SWBQ(DF_clean, short_name_scale_str = 'SWBQ')),
   tar_target(df_WEBEXEC, prepare_WEBEXEC(DF_clean, short_name_scale_str = 'WEBEXEC')),

  
  ## Join tasks --------------------------------------------------------------
  
  tar_target(DF_joined, 
             create_joined(
							 df_ESM,
							 df_AIM,
							 df_BNT,
							 df_bRCOPE,
							 df_Consent,
							 df_Cov19Q,
							 df_COVIDCONTROL,
							 df_CRS,
							 df_CRT7,
							 df_CRTMCQ4,
							 df_CRTv,
							 df_DEBRIEF,
							 df_DEMOGR,
							 df_DEMOGR3,
							 df_EAR,
							 df_EmpaTom,
							 df_ERQ,
							 df_FDMQ,
							 df_GHQ12,
							 df_Goodbye,
							 df_HRPVB,
							 df_HRPVBpost,
							 df_IDQ,
							 df_IEC,
							 df_INFCONS,
							 df_IRI,
							 df_IRS,
							 df_ITC,
							 df_MIS,
							 df_OBJNUM,
							 df_OTRASRELIG,
							 df_PBS,
							 df_PRFBM,
							 df_PRFBMpost,
							 df_PSETPP,
							 df_PSPPC,
							 df_PSS,
							 df_PWb,
							 df_REI40,
							 df_RSS,
							 df_RTS,
							 df_SASS,
							 df_SBS,
							 df_SCSORF,
							 df_SDG,
							 df_SRA,
							 df_SRSav,
							 df_SWBQ,
							 df_WEBEXEC
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
