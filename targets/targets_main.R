targets <- list(
  
  
  # _Read files --------------------------------------------------------------
  
  # RAW data
  # tar_files(input_files, list.files(path = "data", pattern = "csv", full.names = TRUE)),
  tar_target(input_files, list.files(path = "data", pattern = "csv", full.names = TRUE)),
  tar_target(DF_raw, read_data(input_files, anonymize = FALSE, save_output = TRUE, workers = 4)),
  tar_target(tests_DFraw, tests_DF_raw(DF_raw), priority = 1),
  
  # Cleaned data
  tar_target(DF_clean, create_clean_data(DF_raw)),
  
  # Diccionary of tasks
  tar_target(DICCIONARY_tasks, create_diccionary_tasks(DF_clean)),
  
  
  # _Prepare tasks -----------------------------------------------------------
  
  # tar_target(df_SDG, prepare_SDG(DF_clean, short_name_scale_str = "SDG"), priority = .9),
  
  # Sensitive tasks  
  # tar_files(input_files_sensitive, list.files(path = ".vault/data_vault_5", pattern = "csv", full.names = TRUE)),
  # tar_target(input_files_sensitive, list.files(path = ".vault/data_vault_5", pattern = "csv", full.names = TRUE)),
  # tar_target(df_AIM, run_sensitive_data(input_files_sensitive, df_SDG, DF_clean)),
  
  # Non sensitive tasks
  tar_target(df_AIM, prepare_AIM(DF_clean, short_name_scale_str = "AIM")),
  tar_target(df_Cov19Q, prepare_Cov19Q(DF_clean, short_name_scale_str = "Cov19Q")),
  tar_target(df_COVIDCONTROL, prepare_COVIDCONTROL(DF_clean, short_name_scale_str = "COVIDCONTROL")),
  tar_target(df_CRS, prepare_CRS(DF_clean, short_name_scale_str = "CRS")),
  tar_target(df_CRTMCQ4, prepare_CRTMCQ4(DF_clean, short_name_scale_str = "CRTMCQ4")),
  tar_target(df_EAR, prepare_EAR(DF_clean, short_name_scale_str = "EAR")),
  tar_target(df_ERQ, prepare_ERQ(DF_clean, short_name_scale_str = "ERQ")),
  tar_target(df_FDMQ, prepare_FDMQ(DF_clean, short_name_scale_str = "FDMQ")),
  tar_target(df_GHQ12, prepare_GHQ12(DF_clean, short_name_scale_str = "GHQ12")),
  tar_target(df_IEC, prepare_IEC(DF_clean, short_name_scale_str = "IEC")),
  tar_target(df_IRI, prepare_IRI(DF_clean, short_name_scale_str = "IRI")),
  tar_target(df_IRS, prepare_IRS(DF_clean, short_name_scale_str = "IRS")),
  tar_target(df_OTRASRELIG, prepare_OTRASRELIG(DF_clean, short_name_scale_str = "OTRASRELIG")),
  tar_target(df_MIS, prepare_MIS(DF_clean, short_name_scale_str = "MIS")),
  tar_target(df_PBS, prepare_PBS(DF_clean, short_name_scale_str = "PBS")),
  tar_target(df_PSS, prepare_PSS(DF_clean, short_name_scale_str = "PSS")),
  tar_target(df_PWb, prepare_PWb(DF_clean, short_name_scale_str = "PWb")),
  tar_target(df_REI40, prepare_REI40(DF_clean, short_name_scale_str = "REI40")),
  tar_target(df_RSS, prepare_RSS(DF_clean, short_name_scale_str = "RSS")),
  tar_target(df_RTS, prepare_RTS(DF_clean, short_name_scale_str = "RTS")),
  tar_target(df_SASS, prepare_SASS(DF_clean, short_name_scale_str = "SASS")),
  tar_target(df_SBS, prepare_SBS(DF_clean, short_name_scale_str = "SBS")),
  tar_target(df_SCSORF, prepare_SCSORF(DF_clean, short_name_scale_str = "SCSORF")),
  tar_target(df_SDG, prepare_SDG(DF_clean, short_name_scale_str = "SDG")),
  tar_target(df_SRA, prepare_SRA(DF_clean, short_name_scale_str = "SRA")),
  tar_target(df_SRSav, prepare_SRSav(DF_clean, short_name_scale_str = "SRSav")),
  tar_target(df_SWBQ, prepare_SWBQ(DF_clean, short_name_scale_str = "SWBQ")),
  tar_target(df_WEBEXEC, prepare_WEBEXEC(DF_clean, short_name_scale_str = "WEBEXEC")),
  
  
  # _Join tasks --------------------------------------------------------------
  
  # [REMEMBER]: Have to manually put every test we prepare here
  tar_target(DF_joined, create_joined(df_AIM, # Prepared in .vault
                                      df_Cov19Q,
                                      df_COVIDCONTROL,
                                      df_CRS,
                                      df_CRTMCQ4,
                                      df_EAR,
                                      df_ERQ,
                                      df_FDMQ,
                                      df_GHQ12,
                                      df_IEC,
                                      df_IRI,
                                      df_IRS,
                                      df_MIS,
                                      df_OTRASRELIG,
                                      df_PBS,
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
                                      df_WEBEXEC)),
  
  
  
  # _Analysis ----------------------------------------------------------------- 
  
  # Prepare a DF ready for the analysis
  tar_target(DF_analysis, create_DF_analysis(DF_joined)),
  
  # [TODO] Descriptive Table 1
  # Important: Should we compare DF_analysis with the final data used in each model? 
  # tar_render(descriptives_table1, "doc/descriptives_table1.Rmd",
        # output_file = paste0("../outputs/reports/descriptives_table1.html")),
  
  
  # Models
  tar_target(model_E1, analysis_model_E1(DF_analysis)),
  
  
  # Tables and plots use the model (e.g. model_XXX) as input.  
  # Most model objects in R include the data used to fit the model
  
  # Tables
  tar_target(table1_model_E1, analysis_model_E1_table(model_E1)),
  
  # Plots
  tar_target(plots_descriptive, analysis_descriptive_plots(DF_joined, DF_raw)),
  tar_target(plot1_model_E1, analysis_model_E1_plot(model_E1)),
  
  
  # _Tests -------------------------------------------------------------------
  
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
  
  # Report ------------------------------------------------------------------
  
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
