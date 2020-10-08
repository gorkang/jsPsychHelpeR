##' Prepare Supernatural Belief Scale
##'
##' .. content for \details{} ..
##'
##' @title prepare_SBS
##' @param DF
##' @return
##' @author gorkang
##' @export
prepare_SBS <- function(DF) {

  # Parameters --------------------------------------------------------------
  
  name_scale_str = "Supernatural_Belief_Scale"
  short_name_scale_str = "Supernatural"
  

    
  # Create long -------------------------------------------------------------
  DF_long = create_raw_long(DF, name_scale = name_scale_str, numeric_responses = TRUE)
  
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

    mutate(trialid = paste0(trialid, "_PROC")) %>%
    pivot_wider(names_from = trialid, values_from = response) %>% 
    mutate(SBS_PROC = rowSums(select(., matches(short_name_scale_str)), na.rm = TRUE),
           SBS_PROC_NA = rowSums(is.na(select(., matches(short_name_scale_str)))))
  

  
  # Join all ------------------------------------------------------------------------------------
  DF_joined = DF_wide %>% left_join(DF_wide_processed, by = c("id"))
  
  # CHECK -------------------------------------------------------------------
  check_NAs(DF_joined)
  
  # Save files --------------------------------------------------------------
  save_files(DF_joined, short_name_scale = short_name_scale_str)
  
  # Output function ---------------------------------------------------------
  return(DF_joined)
  
}
