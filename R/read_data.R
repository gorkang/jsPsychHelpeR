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
read_data <- function(input_files, anonymize = FALSE, save_output = FALSE, workers = 1) {
  
  # DEBUG
  # debug_function(read_data)
  # workers = 1
  
  
  # Read all files
  DF_raw_read = purrr::map_dfr(input_files %>% set_names(basename(.)), data.table::fread, .id = "filename", colClasses = 'character', encoding = 'UTF-8', nThread = as.numeric(workers)) %>% as_tibble()
  # colClasses = c(response = "character")
  
  if (!"response" %in% names(DF_raw_read)) DF_raw_read = DF_raw_read %>% rename(response = responses)
  
  
  # Extract information from filename
  DF_raw =
    DF_raw_read %>% 
    separate(col = filename, 
             into = c("project", "experimento", "version", "datetime", "id"), 
             sep = c("_"), remove = FALSE) %>% 
    mutate(time_elapsed = as.integer(time_elapsed),
           id = gsub("(*.)\\.csv", "\\1", id)) # userID
  
  
  # Save files --------------------------------------------------------------
  if (save_output == TRUE) save_files(DF_raw, short_name_scale = "raw", is_scale = FALSE)
  
  
  # Output of function ---------------------------------------------------------
  return(DF_raw)
  
}
