##' Prepare IEC
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_IEC -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_IEC
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_IEC <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_IEC)
  
  
  # [ADAPT]: Items to ignore, reverse and dimensions ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("000")
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("000")
  
  items_dimensions = list(
    Azar = c("002", "006", "007", "010", "012", "014", "016", "024"), 
    OtrosPoderosos = c("003", "008", "011", "013", "015", "017", "020", "022"), 
    Internalidad = c("001", "004", "005", "009", "018", "019", "021", "023")
  )
  

  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  

  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = names(items_dimensions),
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE, help_prepare = FALSE)
  
  
  # Create long DIR ------------------------------------------------------------
  DF_long_DIR = 
    DF_long_RAW |> 
   dplyr::select(id, trialid, RAW) |>
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
    dplyr::mutate(
      DIR =
       dplyr::case_when(
          RAW == "Completamente en desacuerdo." ~  1,
          RAW == "Moderadamente en desacuerdo." ~ 2,
          RAW == "Ligeramente en desacuerdo." ~ 3,
          RAW == "Ligeramente de acuerdo." ~ 4,
          RAW == "Moderadamente de acuerdo." ~ 5,
          RAW == "Completamente de acuerdo." ~ 6,
          TRUE ~ 9999
        )
    ) |> 
    
    # Invert items
    dplyr::mutate(
      DIR = 
       dplyr::case_when(
          DIR == 9999 ~ DIR, # To keep the missing values unchanged
          trialid %in% paste0(short_name_scale_str, "_", items_to_reverse) ~ (6 - DIR),
          TRUE ~ DIR
        )
    )
    
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
        
  # Reliability -------------------------------------------------------------
  
  REL1 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_dimensions[[1]])
  REL2 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_dimensions[[2]])
  REL3 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_dimensions[[3]])
  
  items_RELd1 = REL1$item_selection_string
  items_RELd2 = REL2$item_selection_string
  items_RELd3 = REL3$item_selection_string

  
  
    
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)

  DF_wide_RAW_DIR = 
    DF_wide_RAW |> 
    dplyr::mutate(

      # Score Dimensions (use 3 digit item numbers)
      !!names_list$name_DIRd[1] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR"))), na.rm = TRUE), 
      !!names_list$name_DIRd[2] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[3] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR"))), na.rm = TRUE),
      
      # Reliability Dimensions (use 3 digit item numbers)
      !!names_list$name_RELd[1] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_RELd1, "_DIR"))), na.rm = TRUE), 
      !!names_list$name_RELd[2] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_RELd2, "_DIR"))), na.rm = TRUE),
      !!names_list$name_RELd[3] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_RELd3, "_DIR"))), na.rm = TRUE),
      
    )
    
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************


  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE, output_formats = output_formats)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
