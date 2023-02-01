##' tests_DF_raw
##'
##' .. content for \details{} ..
##'
##' @title

##' @return
##' @author gorkang
##' @export
tests_DF_raw <- function(DF_raw) {

  # # Load targets objects used in tests --------------------------------------
  # 
  # argnames <- sys.call()
  # arguments = lapply(argnames[-1], as.character) %>% unlist()

  # Load DF_clean
  targets::tar_load("DF_raw")
  
  
  # Only one project
  test_projects = 
    DF_raw %>% 
    count(project)
  
  if (nrow(test_projects) > 1) cat(cli::col_red(paste0("\n\n[WARNING]: More than 1 project\n")))
  
  
  # All tasks same version!
  test_version_tasks = 
    DF_raw %>% 
    count(experiment, version) %>% 
    count(experiment) %>% 
    arrange(desc(n)) %>% 
    filter(n > 1)
  
  if (nrow(test_version_tasks) > 0) cat(cli::col_red(paste0("\n\n[WARNING]: Some of the tasks have data from different versions\n")))
  
    
  # No repeated id's per experiment!
  repeated_id = 
    DF_raw %>% 
    count(id, experiment, filename) %>% 
    count(id, experiment) %>% 
    arrange(desc(n)) %>% 
    filter(n > 1)
  
  
  if (nrow(repeated_id) > 0) {
    cat(cli::col_red(paste0("\n\n[WARNING]: We have repeated id's in: ")), paste(repeated_id$experiment, collapse = ", "), "\n")
    cat(cli::col_red(paste0("\t\t      Offending IDs: ")), paste(repeated_id %>% distinct(id) %>% pull(id), collapse = ", "), "\n")
    stop("FIX this error before proceeding")
    
  }
  
  
  # No repeated trialid per id ----------------
  
  # DF_clean %>% 
  # count(id, trialid) %>% 
  # arrange(desc(n)) %>% 
  # filter(n > 1) %>% 
  # select(-n)

}
