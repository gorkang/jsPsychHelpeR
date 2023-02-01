##' Read raw data and prepare a global DF
##'
##'
##' @title read_data()
##'
##' @param anonymize TODO
##' @param save_output should save output?
##' @param workers number of threads for data.table::fread
##' @param input_files files to read
##'
##' @return
##' @author gorkang
##' @export
read_data <- function(input_files, anonymize = FALSE, save_output = TRUE, workers = 1) {
  
  # DEBUG
  # debug_function(read_data)
  
  # We accept either a single zip file or multiple csv files
  DF_raw_read = read_csv_or_zip(input_files, workers = workers)
  
  
  # In some old jsPsych plugins, we have a responses column instead of response
  if (!"response" %in% names(DF_raw_read) & "responses" %in% names(DF_raw_read)) DF_raw_read = DF_raw_read %>% rename(response = responses)
  
  
  # Extract information from filename
  DF_raw =
    DF_raw_read %>% 
    parse_filename() |> 
    mutate(time_elapsed = as.integer(time_elapsed))  
  

  # Wide version ------------------------------------------------------------

  DF_raw_wide = 
    DF_raw %>% 
    filter(!grepl("Instructions", trialid)) %>%
    filter(trialid != "") %>% 
    rename(RAW = response) %>%
    select(id, trialid, RAW) %>%
    pivot_wider(
      names_from = trialid, 
      values_from = c(RAW),
      names_glue = "{trialid}_{.value}") 
  
  
  
  # Save files --------------------------------------------------------------
  if (save_output == TRUE) save_files(DF_raw, short_name_scale = "raw", is_scale = FALSE)
  if (save_output == TRUE) save_files(DF_raw_wide, short_name_scale = "raw_wide", is_scale = FALSE)
  
  
  # Output of function ---------------------------------------------------------
  return(DF_raw)
  
}
