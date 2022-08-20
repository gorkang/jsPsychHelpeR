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
  # workers = 1
  
  # We accept either a single zip file or multiple csv files
    # The function list_input_files() checks that is the case, so input_files here SHOULD be one of these two
  all_csvs = all(grepl("\\.csv", input_files))
  all_zips = all(grepl("\\.zip", input_files))
  
  
  # Read file/s
  if (all_csvs) {
    
    # TEST and remove empty files (size < 100 bytes)
    empty_files = file.info(input_files) %>% as_tibble(rownames = "files") %>% filter(size < 100)
    if (nrow(empty_files) > 0) cli::cli_alert_warning("There are {length(empty_files)} empty input files (size < 100 bytes)")
    input_files = input_files[!input_files %in% empty_files$files]
    
    DF_raw_read = purrr::map_dfr(input_files %>% set_names(basename(.)), data.table::fread, .id = "filename", colClasses = 'character', encoding = 'UTF-8', nThread = as.numeric(workers)) %>% as_tibble()
    # colClasses = c(response = "character")
    
  } else if (all_zips) {
    
    # Unzips to temp folder, reads files and deletes temp folder
    DF_raw_read = read_zips(input_files)  
    
  } else {
    cli::cli_abort("Something wrong in read_data(). Are input files all csv files or a single zip file?")
  }
  
  
  # In some old jsPsych plugins, we have a responses column instead of response
  if (!"response" %in% names(DF_raw_read) & "responses" %in% names(DF_raw_read)) DF_raw_read = DF_raw_read %>% rename(response = responses)
  
  
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
