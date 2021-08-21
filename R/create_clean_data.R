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
           response = gsub(pattern = '\\[(.*?)"",""(.*?)\\]', replacement = "\\1; \\2", x = response, perl = TRUE), # In multi-select, output is: {\"\"Q0\"\":\"\"response1"",""response2\"\"} -> {\"\"Q0\"\":\"\"response1; response2\"\"}
           response = gsub("\\[|\\]", "", response)) # Clean up remaining "[]"
  
  # DEBUG
  # DF_clean_raw = DF_clean_raw %>% filter(experimento == "DEMOGR")
  
  # Creates one line per response for screens with multiple responses (e.g. Q0, Q1)
  DF_clean = separate_responses(DF_clean_raw)  
  
  # Output of function ---------------------------------------------------------
  return(DF_clean)
  
}
