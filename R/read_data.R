##' Read raw data and prepare a global DF
##'
##' 
##'
##' @title
##' @param input_files
##' @return
##' @author gorkang
##' @export
read_data <- function(input_files) {
  
  # [TODO]: response_X sera response cuando en input files solo exista responses -----------------------------
  # [TODO]: HARDCODE vars of input files once Herman fixes the issues -----------------------------
  
  
  DF_raw = purrr::map_df(input_files %>% set_names(basename(.)), readr::read_csv, .id = "filename", 
                         col_types = 
                           cols(
                             .default = col_character(),
                             success = col_logical(),
                             trial_type = col_character(),
                             trial_index = col_double(),
                             time_elapsed = col_double(),
                             internal_node_id = col_character(),
                             # view_history = col_character(),
                             rt = col_double()
                             # trialid = col_character()
                             # `question text` = col_character(),
                             # responses = col_character()
                           )
                         )
  
  # Output of function ---------------------------------------------------------
  return(DF_raw)
  
}
