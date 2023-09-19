#' list_input_files
#' Lists all input csv files or single zip file in the data folder. 
#'
#' @param pid_target Project id
#'
#' @return list of files
#' @export
#'
list_input_files <- function(pid_target) {
  
  # List csv and zip files in folder data/[pid]
  input_folder = here::here(paste0("data/", pid_target))
  files_raw = list.files(path = input_folder, pattern = "*.csv|*.zip", full.names = TRUE)
  length_files = length(files_raw)
  
  if (length_files == 0) cli::cli_abort("No files found in {.code {input_folder}}")
  
  # CHECKS ------------------------------------------------------------------
  all_csvs = all(grepl("\\.csv", files_raw))
  all_zips = all(grepl("\\.zip", files_raw))
  
  # Message -----------------------------------------------------------------

  if (all_csvs) {
    
    cli::cli_alert_info("All files are .csv")
    
  } else if (length_files == 1 & all_zips) {
    
    cli::cli_alert_info(".zip file detected")
    
  } else {
    
    # Other issues
    if (length_files == 0) cli::cli_abort("NO files in '{input_folder}'")
    if (length_files != 1 & all_zips) cli::cli_abort("Multiple ZIP files detected in '{input_folder}'")
    if (length_files != 0 & !all_zips & !all_csvs) cli::cli_abort("Multiple types of files detected in '{input_folder}'. Make sure either a single zip or multiple csv files are present.")
    
  }
  
  
  return(files_raw)
  
}