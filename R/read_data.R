##' Read raw data and prepare a global DF
##'
##' 
##'
##' @title
##' @param input_files
##' @return
##' @author gorkang
##' @export
read_data <- function(input_files, anonymize = FALSE, save_output = FALSE, workers = 1) {
  
  # DEBUG
  # debug_function(read_data)
  
  # Read all files
  DF_raw_read = purrr::map_dfr(input_files %>% set_names(basename(.)), data.table::fread, .id = "filename", encoding = 'UTF-8', nThread = as.numeric(workers)) %>% as_tibble()
  
  
  # Extract information from filename
  DF_raw =
    DF_raw_read %>% 
    separate(col = filename, 
             into = c("project", "experimento", "version", "datetime", "id"), 
             sep = c("_"), remove = FALSE) %>% 
    mutate(
      id = gsub("(*.)\\.csv", "\\1", id), # userID
      stimulus = gsub('\\{""Q0"":""|""\\}', '', stimulus), # Clean stimulus
      responses = gsub('\\{""Q0"":""|""\\}', '', responses), # Clean responses [REMEMBER: Only works with one response per screen]
      responses = gsub('&nbsp;|\u00A0', '', responses), # Remove non-breaking space (tools::showNonASCII(DF_raw$responses))
      
      # This takes care of one type of multiple responses:  COVIDCONTROL ["response1", "response2"]
      responses = gsub('\\{""Q0"":\\[""|""\\]\\}', '', responses), 
      responses = gsub('("","")', ', ', responses)
    ) 
  # THIS plus the rowwise and mutate below for multiple answers. ADDS a lot of time        
  # responses = gsub('"Q[0-9]"|&nbsp;|\u00A0', '', responses)
  # ) %>% 
  # rowwise() %>% 
  # mutate(responses = lapply(regmatches(responses, gregexpr('(\").*?(\")', responses, perl = TRUE)), function(y) gsub("^\"|\"$", "", y)) %>% unlist() %>% paste(., collapse = "; ")) # Should deal with multiple responses (?)
  
  
  # Save files --------------------------------------------------------------
  if (save_output == TRUE) save_files(DF_raw, short_name_scale = "raw", is_scale = FALSE)
  
  
  # Output of function ---------------------------------------------------------
  return(DF_raw)
  
}
