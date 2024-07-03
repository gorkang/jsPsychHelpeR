#' create_dictionary_tasks
#' 
#' Create DICTIONARY with long and short names for the tasks in the protocol
#'
#' @param DF_clean DF_clean
#'
#' @return Writes outputs/data/DICTIONARY_tasks.csv
create_dictionary_tasks <- function(DF_clean) {
  
  DICTIONARY_tasks = 
    DF_clean |>  
    dplyr::mutate(`short_name: from trialid` = 
            dplyr::case_when(
               grepl("_[0-9]{1,3}_[[:alnum:]]{1,30}", trialid) ~ gsub("^(.*)_[0-9]{1,3}_[[:alnum:]]{1,30}", "\\1", trialid), # NAME_001_ANYTHING [for questions depending on previous key, BART, etc.]
               TRUE ~ gsub("^(.*)_[0-9]{1,3}", "\\1", trialid)
             )
    ) |> 
    #dplyr::select(trialid, `short_name: from trialid`)
    dplyr::distinct(experiment, `short_name: from trialid`) |> 
   tidyr::drop_na()
  
  data.table::fwrite(DICTIONARY_tasks, "outputs/data/DICTIONARY_tasks.csv")

  return(DICTIONARY_tasks)
}
