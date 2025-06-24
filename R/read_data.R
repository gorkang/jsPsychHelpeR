#' read_data
#' 
#' Read raw data and prepare a global DF
#'
#' @param input_files A character vector of files to read
#' @param is_sensitive If the data contains sensitive information, select TRUE to save in .vault. TRUE / FALSE
#' @param save_output TRUE / FALSE
#' @param workers number of threads for data.table::fread
#' @param location_duplicates If there are duplicate trialid's, a csv with details will be saved here
#'
#' @return A DF 
#' @export
read_data <- function(input_files, is_sensitive = FALSE, save_output = TRUE, workers = 1, location_duplicates = NULL) {
  
  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(read_data)
  
  # We accept either a single zip file or multiple csv files
  DF_raw_read = read_csv_or_zip(input_files, workers = workers)
  
  # In some old jsPsych plugins, we have a responses column instead of response
  if (!"response" %in% names(DF_raw_read) & "responses" %in% names(DF_raw_read)) DF_raw_read = DF_raw_read %>% dplyr::rename(response = responses)
  
  # Extract information from filename
  DF_raw =
    DF_raw_read |>  
    parse_filename() |> 
    dplyr::mutate(time_elapsed = as.integer(time_elapsed))  
  

  # Wide version ------------------------------------------------------------
  # DF_raw_wide = 
  #   DF_raw |> 
  #   dplyr::filter(!grepl("repeated", id))  |> # Delete users that have "repeated" in name
  #   dplyr::filter(!grepl("[Ii]nstructions", trialid)) |> 
  #   dplyr::filter(trialid != "") |> 
  #   dplyr::rename(RAW = response) |> 
  #   dplyr::select(id, trialid, RAW)  |> 
  #   tidyr::pivot_wider(
  #     names_from = trialid, 
  #     values_from = c(RAW),
  #     names_glue = "{trialid}_{.value}") 
  
  # We do this through a function to use purrr::quietly to capture the warning and 
  # save the specific issue
  RAW_to_wide <- function(DF_raw) {
    
      DF_raw |> 
      dplyr::filter(!grepl("repeated", id))  |> # Delete users that have "repeated" in name
      dplyr::filter(!grepl("[Ii]nstructions", trialid)) |> 
      dplyr::filter(trialid != "") |> 
      dplyr::rename(RAW = response) |> 
      dplyr::select(id, trialid, RAW)  |> 
      tidyr::pivot_wider(
        names_from = trialid, 
        values_from = c(RAW),
        names_glue = "{trialid}_{.value}") 
    
  }
  
  RAW_to_wide_safely = purrr::quietly(RAW_to_wide)
  DF_raw_wide_quiet = RAW_to_wide_safely(DF_raw)
  
  if (length(DF_raw_wide_quiet$warnings) > 0) {
    pid = unique(DF_raw$project)
    OUT = DF_raw|>
      dplyr::filter(!grepl("repeated", id))  |> # Delete users that have "repeated" in name
      dplyr::filter(!grepl("[Ii]nstructions", trialid)) |> 
      dplyr::filter(trialid != "") |> 
      dplyr::summarise(n = dplyr::n(), .by = c(id, trialid)) |>
      dplyr::filter(n > 1L)

    # Write dups csv file
    if (!is.null(location_duplicates)) readr::write_csv(OUT, paste0(location_duplicates, "/", pid[1], "_DUPLICATED_warning.csv"))
    
    # Abort
    if (length(pid) > 1) cli::cli_abort("Multiple pid's in protocol: {pid}") 
    cli::cli_abort("Duplicated trialids in pid {pid}:\n\n {names(OUT)}\n\n{OUT}") 
    
  } else {
    DF_raw_wide = DF_raw_wide_quiet$result
  }
  
  # Save files --------------------------------------------------------------
  if (save_output == TRUE) save_files(DF_raw, short_name_scale = "raw", is_scale = FALSE, is_sensitive = is_sensitive)
  if (save_output == TRUE) save_files(DF_raw_wide, short_name_scale = "raw_wide", is_scale = FALSE, is_sensitive = is_sensitive)
  
  
  # Output of function ---------------------------------------------------------
  return(DF_raw)
  
}
