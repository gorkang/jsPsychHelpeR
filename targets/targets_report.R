targets <- list(
  
  
  # _Read files --------------------------------------------------------------
  
  # RAW data
  # tar_files(input_files, list.files(path = "data", pattern = "csv", full.names = TRUE)),
  tar_target(input_files, list.files(path = "data", pattern = "csv", full.names = TRUE)),
  tar_target(DF_raw, read_data(input_files, anonymize = FALSE, save_output = TRUE, workers = 4)),
  # tar_target(tests_DFraw, tests_DF_raw(DF_raw), priority = 1),
  
  # Cleaned data
  tar_target(DF_clean, create_clean_data(DF_raw)),
  

  # _Prepare tasks -----------------------------------------------------------
  
  # tar_target(df_SDG, prepare_SDG(DF_clean, short_name_scale_str = "SDG"), priority = 1),
  
  # Sensitive tasks  
  # tar_files(input_files_sensitive, list.files(path = ".vault/data_vault_5", pattern = "csv", full.names = TRUE)),
  # tar_target(input_files_sensitive, list.files(path = ".vault/data_vault_5", pattern = "csv", full.names = TRUE)),
  # tar_target(df_AIM, run_sensitive_data(input_files_sensitive, df_SDG, DF_clean)),
  
  
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
