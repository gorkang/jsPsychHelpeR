##' Prepare TEMPLATE
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: prepare_TEMPLATE -> prepare_[NAME_OF TEST] 
##'
##' @title prepare_TEMPLATE
##' @param DF
##' @return
##' @author gorkang
##' @export
prepare_TEMPLATE <- function(DF_clean, name_scale_str, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_TEMPLATE)
  

  # Standardized names ------------------------------------------------------
  standardized_names(short_name_scale = short_name_scale_str, help_names = TRUE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long = create_raw_long(DF_clean, name_scale = name_scale_str, numeric_responses = FALSE)
  
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
    
  # [USE STANDARD NAMES FOR Scales and dimensions] ***************************
  # Check with: standardized_names(short_name_scale = short_name_scale_str,help_names = TRUE)
    
  # Use Standardized names for the dimensions and scale variables, etc.
  mutate(trialid = paste0(trialid, sufix_DIR)) %>%
    pivot_wider(names_from = trialid, values_from = response) %>% 
    
    mutate(!!name_DIRt := rowSums(select(., matches(short_name_scale_str)), na.rm = TRUE),
           !!name_DIRt_NA := rowSums(is.na(select(., matches(short_name_scale_str)))))
  
  # **************************************************************************
  

  
  # Join all ------------------------------------------------------------------------------------
  DF_joined = DF_wide %>% left_join(DF_wide_processed, by = c("id"))
  
  # CHECK -------------------------------------------------------------------
  check_NAs(DF_joined)
  
  # Save files --------------------------------------------------------------
  save_files(DF_joined, short_name_scale = short_name_scale_str, is_scale = TRUE)
  
  # Output of function ---------------------------------------------------------
  return(DF_joined) 
 
}
