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
  
  DF_clean =
    DF_raw %>% 
    janitor::clean_names() %>% 
    filter(trial_type != "fullscreen") %>% # Empty line
    filter(!trialid %in% c("Screen_WM", "Instructions")) %>%  # Delete instructions [TODO]: use regexp to clean instrucciones_NOMBRETEST
    filter(!grepl("Instructions", trialid, ignore.case = TRUE))
  
  # Output of function ---------------------------------------------------------
  return(DF_clean)

}
