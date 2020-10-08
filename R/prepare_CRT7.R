##' Prepare CRT7
##'
##' .. content for \details{} ..
##'
##' @title prepare_CRT7
##' @param DF
##' @return
##' @author gorkang
##' @export
prepare_CRT7 <- function(DF) {

  # Parameters --------------------------------------------------------------

  name_scale_str = "CRT_7"
  short_name_scale_str = "CRT_7"
  

    
  # Create long -------------------------------------------------------------
  DF_long = create_raw_long(DF, name_scale = name_scale_str, numeric_responses = FALSE)
  
  # Create wide -------------------------------------------------------------
  DF_wide = create_raw_wide(DF_long, short_name_scale = short_name_scale_str)
  
  
  
  
  # [ADAPT] Wide: processed responses --------------------------------------------------
  
  DF_wide_processed =
    DF_long %>% 
    select(id, trialid, response) %>% 

    # Process data
    mutate(response =
             case_when(
               trialid == "CRT_7_1" & response == "50" ~ 1,
               trialid == "CRT_7_2" & response == "5" ~ 1,
               trialid == "CRT_7_3" & response == "47" ~ 1,
               trialid == "CRT_7_4" & response == "4" ~ 1,
               trialid == "CRT_7_5" & response == "29" ~ 1,
               trialid == "CRT_7_6" & response == "20" ~ 1,
               trialid == "CRT_7_7" & response == "Ha perdido dinero" ~ 1,
               TRUE ~ 0)) %>% 
    
    mutate(trialid = paste0(trialid, "_PROC")) %>%    
    pivot_wider(names_from = trialid, values_from = response) %>% 
    mutate(CRT7_PROC = rowSums(select(., matches(short_name_scale_str)), na.rm = TRUE),
           CRT7_PROC_NA = rowSums(is.na(select(., matches(short_name_scale_str)))))
  

  
  # Join all ------------------------------------------------------------------------------------
  DF_joined = DF_wide %>% left_join(DF_wide_processed, by = c("id"))
  
  # CHECK -------------------------------------------------------------------
  check_NAs(DF_joined)
  
  # Save files --------------------------------------------------------------
  save_files(DF_joined, short_name_scale = short_name_scale_str)
  
  # Output function ---------------------------------------------------------
  return(DF_joined) 
 
}
