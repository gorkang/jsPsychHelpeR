##' Checks if the NAs of the RAW calculation are the same as the ones from the PROC calculation
##'
##' Important to catch errors when transforming data from RAW to PROCESSED
##'
##' @title check_NAs
##' @param DF
##' @return
##' @author gorkang
##' @export
check_NAs <- function(DF) {
  
  DF_CHECK_NA = DF %>% 
    select(ends_with("_NA")) 
  
  if (ncol(DF_CHECK_NA) == 2) {
  
    # Check we have the same number of NAs in RAW and PROC DFs
    if (!identical(DF_CHECK_NA[[1]], DF_CHECK_NA[[2]])) stop("Missing data when processing RAW responses")
    
    # [REWIEW]: Other ways to check equality
    # all(DF_CHECK_NA[1] == DF_CHECK_NA[2])
    # all.equal(DF_CHECK_NA[[1]], DF_CHECK_NA[[2]])
    
  } else {
    
    cat(crayon::blue("\n  - Can't perform NA check, DF does not have RAW_NA and PROC_NA columns\n"))
    
  }
  
}




##' Create long DF for a specific task (name_scale)
##'
##' 
##'
##' @title create_raw_long
##' @param DF
##' @return
##' @author gorkang
##' @export
create_raw_long <- function(DF, name_scale, numeric_responses = FALSE) {
  
  # DEBUG
  # name_scale = "CRT_7"
  

  DF %>% 
    filter(experimento == name_scale) %>% 
    
    # [TODO]: DELETE THIS ONCE input files are OK ---------------------
    mutate(response = 
             case_when(
               is.na(response_x) ~ response,
               is.na(response) ~ response_x)
           ) %>% 
    
    select(id, experimento, rt, trialid, question_text, response) %>% 
    
    mutate(response = 
             if(numeric_responses == TRUE) {
               as.numeric(response) 
              } else {
                as.character(response) 
              }
           ) %>% 
    drop_na(trialid)
}




##' Create wide DF for a specific task (short_name_scale)
##'
##' 
##'
##' @title create_raw_wide
##' @param DF
##' @return
##' @author gorkang
##' @export
create_raw_wide <- function(DF_long, short_name_scale) {

  # DEBUG  
  # short_name_scale = "Supernatural"
  
  name_RAW_NA = paste0(short_name_scale, "_RAW_NA")

  DF_long %>% 
    select(id, trialid, response) %>% 
    mutate(trialid = paste0(trialid, "_RAW")) %>% 
    pivot_wider(names_from = trialid, values_from = response) %>% 
    mutate(!!name_RAW_NA := rowSums(is.na(select(., matches(short_name_scale)))))
  
}


##' Save files
##'
##' 
##'
##' @title create_raw_wide
##' @param DF
##' @return
##' @author gorkang
##' @export
save_files <- function(DF, short_name_scale) {
  
  write_csv(DF, here::here(paste0("output/data/df_", short_name_scale , ".csv")))
  write_rds(DF, here::here(paste0("output/data/df_", short_name_scale , ".rds")))
  
}
