#' read_data
#' 
#' Read raw data and prepare a global DF
#'
#' @param input_files A character vector of files to read
#' @param is_sensitive If the data contains sensitive information, select TRUE to save in .vault. TRUE / FALSE
#' @param save_output TRUE / FALSE
#' @param workers number of threads for data.table::fread
#'
#' @return A DF 
#' @export
read_data <- function(input_files, is_sensitive = FALSE, save_output = TRUE, workers = 1) {
  
  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(read_data)
  
  # We accept either a single zip file or multiple csv files
  DF_raw_read = read_csv_or_zip(input_files, workers = workers)
  
  # In some old jsPsych plugins, we have a responses column instead of response
  if (!"response" %in% names(DF_raw_read) & "responses" %in% names(DF_raw_read)) DF_raw_read = DF_raw_read %>% dplyr::rename(response = responses)
  
  # Extract information from filename
  DF_raw =
    DF_raw_read %>% 
    parse_filename() |> 
    dplyr::mutate(time_elapsed = as.integer(time_elapsed))  
  

  # Wide version ------------------------------------------------------------
  DF_raw_wide = 
    DF_raw %>% 
    dplyr::filter(!grepl("repeated", id)) %>% # Delete users that have "repeated" in name
    dplyr::filter(!grepl("[Ii]nstructions", trialid)) %>%
    dplyr::filter(trialid != "") %>% 
    dplyr::rename(RAW = response) %>%
    dplyr::select(id, trialid, RAW) %>%
    tidyr::pivot_wider(
      names_from = trialid, 
      values_from = c(RAW),
      names_glue = "{trialid}_{.value}") 
  
  
  
  # Save files --------------------------------------------------------------
  if (save_output == TRUE) save_files(DF_raw, short_name_scale = "raw", is_scale = FALSE, is_sensitive = is_sensitive)
  if (save_output == TRUE) save_files(DF_raw_wide, short_name_scale = "raw_wide", is_scale = FALSE, is_sensitive = is_sensitive)
  
  
  # Output of function ---------------------------------------------------------
  return(DF_raw)
  
}
