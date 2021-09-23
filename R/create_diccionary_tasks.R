##' Create DICCIONARY with long and short names for the tasks in the protocol
##'
##' .. content for \details{} ..
##'
##' @title
##' @param DF
##' @return
##' @author gorkang
##' @export
create_diccionary_tasks <- function(DF_clean) {
  
  DICCIONARY_tasks = 
    DF_clean %>% 
    mutate(`short_name: from trialid` = 
             case_when(
               grepl("_[0-9]{1,3}_[[:alnum:]]{1,30}", trialid) ~ gsub("^(.*)_[0-9]{1,3}_[[:alnum:]]{1,30}", "\\1", trialid), # NAME_001_ANYTHING [for questions depending on previous key, BART, etc.]
               TRUE ~ gsub("^(.*)_[0-9]{1,3}", "\\1", trialid)
             )
    ) %>% 
    # select(trialid, `short_name: from trialid`)
    distinct(experimento, `short_name: from trialid`) %>% 
    drop_na()
  
  write_csv(DICCIONARY_tasks, "outputs/data/DICCIONARY_tasks.csv")
  
}
