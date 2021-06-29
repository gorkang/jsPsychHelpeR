##' Create DF_clean
##'
##' .. content for \details{} ..
##'
##' @title
##' @param DF_raw
##' @return
##' @author gorkang
##' @export
create_clean_data <- function(DF_raw) {
  
  DF_clean_raw =
    DF_raw %>% 
    janitor::clean_names() %>% 
    filter(trial_type != "fullscreen") %>% # Empty line
    filter(!trialid %in% c("Screen_WM", "Instructions")) %>%  # Delete instructions [TODO]: use regexp to clean instrucciones_NOMBRETEST
    filter(!grepl("Instructions", trialid, ignore.case = TRUE)) %>% 
    mutate(response = gsub('&nbsp;|\u00A0', '', response), # HTML space and invisible character
           response = gsub("\\[|\\]", "", response)) # In multi-select, output is ["response1", "response2"]
  
  # DEBUG
  # DF_clean_raw = DF_clean_raw %>% filter(experimento == "DEMOGR")
  
  DF_clean = separate_responses(DF_clean_raw)  
  
  # Output of function ---------------------------------------------------------
  return(DF_clean)
  
}
