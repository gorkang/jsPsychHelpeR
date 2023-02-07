#' create_diccionary_tasks
#' 
#' Create DICCIONARY with long and short names for the tasks in the protocol
#'
#' @param DF_clean .
#'
#' @return
#' @export
#'
#' @examples
create_diccionary_tasks <- function(DF_clean) {
  
  DICCIONARY_tasks = 
    DF_clean %>% 
    dplyr::mutate(`short_name: from trialid` = 
            dplyr::case_when(
               grepl("_[0-9]{1,3}_[[:alnum:]]{1,30}", trialid) ~ gsub("^(.*)_[0-9]{1,3}_[[:alnum:]]{1,30}", "\\1", trialid), # NAME_001_ANYTHING [for questions depending on previous key, BART, etc.]
               TRUE ~ gsub("^(.*)_[0-9]{1,3}", "\\1", trialid)
             )
    ) %>% 
    #dplyr::select(trialid, `short_name: from trialid`)
    dplyr::distinct(experiment, `short_name: from trialid`) %>% 
   tidyr::drop_na()
  
  readr::write_csv(DICCIONARY_tasks, "outputs/data/DICCIONARY_tasks.csv")
  
}
