list_input_files <- function(pid_target) {
  
  # DEBUG
  # pid_target = 9999
  # pid_target = 999
  # list.files(path = paste0("data/", pid_target), full.names = TRUE)
  
  pid_folder = paste0("data/", pid_target)
  
  files_raw = list.files(path = pid_folder, pattern = "*.csv|*.zip", full.names = TRUE)
  

  # CHECKS ------------------------------------------------------------------
  all_csvs = all(grepl("\\.csv", files_raw))
  length_files = length(files_raw)
  all_zips = all(grepl("\\.zip", files_raw))
  
  

  # Message -----------------------------------------------------------------

  if (all_csvs) {
    
    cli::cli_alert_info("All files are .csv")
    
  } else if (length_files == 1 & all_zips) {
    
    cli::cli_alert_info(".zip file detected")
    
  } else {
    
    # Other issues
    if (length_files == 0) cli::cli_abort("NO files in '{pid_folder}'")
    if (length_files != 1 & all_zips) cli::cli_abort("Multiple ZIP files detected in '{pid_folder}'")
    if (length_files != 0 & !all_zips & !all_csvs) cli::cli_abort("Multiple types of files detected in '{pid_folder}'")
    
  }
  
  
  return(files_raw)
  
}