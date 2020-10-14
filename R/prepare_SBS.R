##' Prepare Supernatural Belief Scale
##'
##' .. content for \details{} ..
##'
##' @title prepare_SBS
##' @param DF
##' @return
##' @author gorkang
##' @export
prepare_SBS <- function(DF_clean, name_scale_str, short_name_scale_str) {
  
  # DEBUG
  # debug_function(prepare_SBS)
  
  
  # Standardized names ------------------------------------------------------
  standardized_names(short_name_scale = short_name_scale_str)
    
  # Create long -------------------------------------------------------------
  DF_long = create_raw_long(DF_clean, name_scale = name_scale_str, numeric_responses = TRUE)
  
  # Create wide -------------------------------------------------------------
  DF_wide = create_raw_wide(DF_long, short_name_scale = short_name_scale_str)
  
  
  
  
  # [ADAPT] Wide: processed responses --------------------------------------------------
  
  # [REMEMBER] : THIS IS BULLSHIT NOW!!!!! JUST AN EXAMPLE TO HAVE A REASONABLE TEMPLATE
  
  DF_wide_processed =
    DF_long %>% 
    select(id, trialid, response) %>% 
    
    # Process data
    mutate(response = 
             case_when(
               response < 0 ~ response^2,
               response > 0 ~ response^3,
               TRUE ~ response)) %>% 

    # [USE STANDARD NAMES FOR Scales and dimensions] ***************************
     # Check with: standardized_names(short_name_scale = short_name_scale_str, help_names = TRUE)

    # Use Standardized names for the dimensions and scale variables, etc.
    mutate(trialid = paste0(trialid, sufix_DIR)) %>%
    pivot_wider(names_from = trialid, values_from = response) %>% 
    
    # All this variables have to be !!name_[XXX] :=
    mutate(!!name_DIRt := rowSums(select(., matches(short_name_scale_str)), na.rm = TRUE),
           !!name_DIRt_NA := rowSums(is.na(select(., matches(short_name_scale_str)))))
  
  # **************************************************************************

  
  
  # Join all ------------------------------------------------------------------------------------
  DF_joined = DF_wide %>% left_join(DF_wide_processed, by = c("id"))
  
  # CHECK -------------------------------------------------------------------
  check_NAs(DF_joined)
  
  # Save files --------------------------------------------------------------
  save_files(DF_joined, short_name_scale = short_name_scale_str)
  
  # Output of function ---------------------------------------------------------
  return(DF_joined)
  
}
