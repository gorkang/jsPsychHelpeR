##' Prepare Bank
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_Bank -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_Bank
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_Bank <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_Bank)

  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     # dimensions = c("NameDimension1", "NameDimension2"), # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE, help_prepare = FALSE)
  
  
  # Create long DIR ------------------------------------------------------------
  
  # [ADAPT]: Items to ignore and reverse ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("00|00") # Ignore the following items: If nothing to ignore, keep "00|00"
  items_to_reverse = c("00|00") # Reverse the following items: If nothing to ignore, keep "00|00"
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  DF_long_DIR = 
    DF_long_RAW |> 
   dplyr::select(id, trialid, RAW) |>
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
    # Transformations
    dplyr::mutate(
      DIR = RAW
        #dplyr::case_when(
        #   RAW == "Nunca" ~ 1,
        #   RAW == "Poco" ~ 2,
        #   RAW == "Medianamente" ~ 3,
        #   RAW == "Bastante" ~ 4,
        #   RAW == "Mucho" ~ 5,
        #   is.na(RAW) ~ NA_real_,
        #   trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_real_, # OR NA_character_
        #   TRUE ~ 9999
        # )
    )  
    
    # # Invert items
    # dplyr::mutate(
    #   DIR = 
    #    dplyr::case_when(
    #       DIR == 9999 ~ DIR, # To keep the missing values unchanged
    #       trialid %in% paste0(short_name_scale_str, "_", items_to_reverse) ~ (6 - DIR),
    #       TRUE ~ DIR
    #     )
    # )
    # 
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
    

  # Create DF_wide_RAW_DIR -----------------------------------------------------
  DF_wide_RAW =
    DF_long_DIR |> 
    tidyr::pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR),
      names_glue = "{trialid}_{.value}") |> 
    
    # NAs for RAW and DIR items
    dplyr::mutate(!!names_list$name_RAW_NA := rowSums(is.na(across((-matches(paste0(short_name_scale_str, "_", items_to_ignore, "_RAW")) & matches("_RAW$"))))),
                  !!names_list$name_DIR_NA := rowSums(is.na(across((-matches(paste0(short_name_scale_str, "_", items_to_ignore, "_DIR")) & matches("_DIR$"))))))
  

  
  # [ADAPT 3/3]: Scales and dimensions calculations ----------------------------
  # ****************************************************************************
  
  DF_wide_RAW_DIR =
    DF_wide_RAW  |>  
    dplyr::mutate(

      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!names_list$name_DIRd[1] := rowSums(select(., matches("02|04|05") & matches("_DIR$")), na.rm = TRUE), 
      # !!names_list$name_DIRd[2] := rowSums(select(., matches("01|03|08") & matches("_DIR$")), na.rm = TRUE), 
      
      # Score Scale
      # !!names_list$name_DIRt := rowSums(across(all_of(matches("_DIR$"))), na.rm = TRUE)
      
    )
    
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************


  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE, is_sensitive = TRUE)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
