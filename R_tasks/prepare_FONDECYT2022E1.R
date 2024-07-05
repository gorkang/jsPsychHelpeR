##' Prepare FONDECYT2022E1
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_FONDECYT2022E1 -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_FONDECYT2022E1
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_FONDECYT2022E1 <- function(DF_clean, short_name_scale_str, output_formats) {
  
  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_FONDECYT2022E1)
  
  # TIENEN dos copias [es la caratula: prueba 1/4]
  # FONDECYT2022E1_01_0
  # FONDECYT2022E1_07_0
  
  
  
  
  
  # [ADAPT]: Items to ignore and reverse ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("00") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("00") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  names_dimensions = c("pictorialAid") # If no dimensions, keep names_dimensions = c("")
  
  # 02, 03, 04
  # screening1
  # diagnostic1
  
  # 05, 06
  # diagnostic2
  # intervention2
  
  ## lowQuality
  ## highQuality
  ## Stroke
  ## Cancer
  
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = names(items_dimensions), # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = 
    create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE, is_experiment = TRUE, help_prepare = FALSE) |> 
    
    # SHOULD DO THIS INSIDE create_raw_long is_experiment????
    dplyr::mutate(trialid = gsub("_[1-5]$", "", trialid),
           trialid = paste0(trialid, "_", condition_within)) |> 
   dplyr::select(-condition_within)
  
  
  # Create long DIR ------------------------------------------------------------
  
  DF_long_DIR = 
    DF_long_RAW |> 
   dplyr::select(id, trialid, RAW, condition_between) |>
    
    
    # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
  # Transformations
  dplyr::mutate(
    DIR =
     dplyr::case_when(
        RAW == "Si" ~ "1",
        RAW == "No" ~ "0",
        is.na(RAW) ~ NA_character_,
        trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_real_,
        TRUE ~ RAW
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
  
  # REL1 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_dimensions[[1]])
  # items_RELd1 = REL1$item_selection_string
  
  
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
  # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)
  
  DF_wide_RAW_DIR =
    DF_wide_RAW |> 
    dplyr::mutate(
      
      # Make sure to use the correct formula: rowMeans() / rowSums()
      
      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      !!names_list$name_DIRd[1] := paste0(condition_between),
      # !!names_list$name_DIRd[2] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR"))), na.rm = TRUE),
      
      # Reliability Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!names_list$name_RELd[1] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_RELd1, "_DIR"))), na.rm = TRUE), 
      
      # Score Scale
      # !!names_list$name_DIRt := rowSums(across(all_of(matches("_DIR$"))), na.rm = TRUE)
      
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
