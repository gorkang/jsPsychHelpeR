##' Read raw data and prepare a global DF
##'
##' 
##'
##' @title
##' @param input_files
##' @return
##' @author gorkang
##' @export
read_data <- function(input_files, anonymize = FALSE) {
  
  plan(multisession, workers = 2)
  
  # Read all files
    DF_raw = furrr::future_map_dfr(input_files %>% set_names(basename(.)), readr::read_csv, .id = "filename",
    # DF_raw = purrr::map_dfr(input_files %>% set_names(basename(.)), readr::read_csv, .id = "filename",
                         col_types = 
                           cols(
                             .default = col_character(),
                             success = col_logical(),
                             trial_type = col_character(),
                             trial_index = col_double(),
                             time_elapsed = col_double(),
                             internal_node_id = col_character(),
                             view_history = col_character(),
                             rt = col_double(),
                             trialid = col_character(),
                             stimulus = col_character(),
                             responses = col_character()
                           )
                         )  
  
    
    DF_raw =
      DF_raw %>% 
      mutate(
        # [REVIEW]: experimento and ID should be in the DF_raw?
        # projectCode_shortName_version_(fecha)_userID.csv
        id = gsub(".*_([0-9]{1,99}).csv", "\\1", filename), # userID
        project = gsub("^([0-9]{1,99})_.*.csv", "\\1", filename), # projectCode
        experimento = gsub("^[0-9]{1,99}_([a-zA-Z0-9]{1,99})_.*.csv", "\\1", filename), # shortname
        version = gsub("^[0-9]{1,99}_[a-zA-Z0-9]{1,99}_([a-zA-Z0-9]{1,99})_.*.csv", "\\1", filename), # version
        datetime = gsub("^[0-9]{1,99}_[a-zA-Z0-9]{1,99}_[a-zA-Z0-9]{1,99}_([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2})_.*.csv", "\\1", filename), # fecha
        
        stimulus = gsub('\\{"Q0":"|"\\}', '', stimulus), # Clean stimulus
        responses = gsub('"Q[0-9]"|&nbsp;|\u00A0', '', responses)
        # responses = gsub('\\{"Q0":"|"\\}', '', responses), # Clean responses [REMEMBER: Only works with one response per screen]
        # responses = gsub('\u00A0', '', responses) # Remove non-breaking space (tools::showNonASCII(DF_raw$responses))
      ) %>% 
      rowwise() %>% 
      mutate(responses = lapply(regmatches(responses, gregexpr('(\").*?(\")', responses, perl = TRUE)), function(y) gsub("^\"|\"$", "", y)) %>% unlist() %>% paste(., collapse = "; ")) # Should deal with multiple responses (?)
    
  
 
    
  # CHECK -------------------------------------------------------------------
  
    DF_duplicates = suppressMessages(DF_raw %>% janitor::get_dupes(-c(filename)))
    # suppressMessages(DF_raw %>% janitor::get_dupes(c(id, trialid))) %>% filter(trialid !="Instructions") %>% distinct(id) %>% pull(id)
    # DF_raw%>% distinct(id) %>% pull(id)
    # DF_raw %>% 
    #   filter(trialid == "SDG_00") %>% 
    #   janitor::get_dupes(c(id, project, experimento)) %>%
    #   group_by(id) %>% 
    #   sample_n(1) %>% 
    #   pull(filename) %>% 
    #   file.remove(paste0("data/", .))
    # 
    
    if (nrow(DF_duplicates) > 0) {
      input_files_duplicates = DF_duplicates %>% filter(success == TRUE) %>% distinct(filename) %>% pull(filename)
      stop("[ERROR]: There are duplicates in the '/data' input files: ", paste(input_files_duplicates, collapse = ", "))
    }
  
  # Output of function ---------------------------------------------------------
    
    return(DF_raw)
  
}
