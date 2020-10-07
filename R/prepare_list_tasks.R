##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param DF
##' @return
##' @author gorkang
##' @export
prepare_list_tasks <- function(DF) {

  DF_list_tasks = DF %>% 
    mutate(`short_name: from trialid` = gsub("(.*)_[0-9]{1,3}", "\\1", trialid)) %>% 
    # select(trialid, short_name)
    distinct(experimento, `short_name: from trialid`) %>% 
    drop_na()
  
  write_csv(DF_list_tasks, "output/data/DF_list_tasks.csv")

}
