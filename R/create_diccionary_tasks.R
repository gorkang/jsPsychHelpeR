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

  DICCIONARY_tasks = DF_clean %>% 
    mutate(`short_name: from trialid` = gsub("(.*)_[0-9]{1,3}", "\\1", trialid)) %>% 
    # select(trialid, short_name)
    distinct(experimento, `short_name: from trialid`) %>% 
    drop_na()
  
  write_csv(DICCIONARY_tasks, "outputs/data/DICCIONARY_tasks.csv")

}
