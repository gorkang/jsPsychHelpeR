##' Prepare FORM6
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_FORM6 -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_FORM6
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_FORM6 <- function(files_status, DF_DICTIONARY_id, short_name_scale_str) {

  # DEBUG
  # DF_DICTIONARY_id
  # short_name_scale_str = "FORM6"


  # Read and process files --------------------------------------------------

  DF_status = read_data(files_status, is_sensitive = FALSE)
  DF_clean_status = create_clean_data(DF_status)
  
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     # dimensions = c("NameDimension1", "NameDimension2"), # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  
  DF_long_RAW = 
    DF_clean_status %>% 
    # dplyr::filter(experiment == name_scale) %>% 
    dplyr::filter(grepl(paste0(short_name_scale_str, "_[0-9]"), trialid)) %>% 
   dplyr::select(id, datetime, trialid,response) %>% 
   tidyr::drop_na(trialid) %>% 
    dplyr::rename(RAW = response) %>% 
    dplyr::arrange(trialid, id)
  
  

  # Create DF_wide_RAW_DIR -----------------------------------------------------
  DF_wide_RAW_DIR =
    DF_long_RAW %>%
      tidyr::pivot_wider(
        names_from = trialid, 
        values_from = c(RAW),
        names_glue = "{trialid}_{.value}")
    
  
  ## GET protocol id ---------------
  DF_wide_RAW_DIR = 
    DF_wide_RAW_DIR %>% 
    dplyr::rename(RUT = id) %>% 
    # dplyr::mutate(rut = FORM6_01_RAW) %>% 
    dplyr::left_join(DF_DICTIONARY_id, by = "RUT") %>% 
   dplyr::select(id, RUT, dplyr::everything())
  
  

  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE, is_sensitive = TRUE)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
