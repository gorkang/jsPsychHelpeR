#' tests_DF_raw
#'
#' @param DF_raw .
#'
#' @return
#' @export
#'
#' @examples
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
    dplyr::count(project)
  
  if (nrow(test_projects) > 1) cat(cli::col_red(paste0("\n\n[WARNING]: More than 1 project\n")))
  
  
  # All tasks same version!
  test_version_tasks = 
    DF_raw %>% 
    dplyr::count(experiment, version) %>% 
    dplyr::count(experiment) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::filter(n > 1)
  
  if (nrow(test_version_tasks) > 0) cat(cli::col_red(paste0("\n\n[WARNING]: Some of the tasks have data from different versions\n")))
  
    
  # No repeated id's per experiment!
  repeated_id = 
    DF_raw %>% 
    dplyr::count(id, experiment, filename) %>% 
    dplyr::count(id, experiment) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::filter(n > 1)
  
  
  if (nrow(repeated_id) > 0) {
    cat(cli::col_red(paste0("\n\n[WARNING]: We have repeated id's in: ")), paste(repeated_id$experiment, collapse = ", "), "\n")
    cat(cli::col_red(paste0("\t\t      Offending IDs: ")), paste(repeated_id %>% dplyr::distinct(id) %>% dplyr::pull(id), collapse = ", "), "\n")
    stop("FIX this error before proceeding")
    
  }
  
  
  # No repeated trialid per id ----------------
  
  # DF_clean %>% 
  # dplyr::count(id, trialid) %>% 
  # dplyr::arrange(desc(n)) %>% 
  # dplyr::filter(n > 1) %>% 
  #dplyr::select(-n)

}
