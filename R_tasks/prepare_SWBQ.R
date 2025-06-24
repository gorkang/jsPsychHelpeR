##' Prepare SWBQ
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_SWBQ -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_SWBQ
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_SWBQ <- function(DF_clean, short_name_scale_str, output_formats) {

  # REMEMBER:
    # 6 extra items. ONLY use the ones that end up in the dimensions for the total score.
  
  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_SWBQ)

  
  # [ADAPT]: Items to ignore, reverse and dimensions ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("000")
  items_to_reverse = c("005", "006", "008", "009", "010", "011", "012", "013", "017", "018", "019", "022", "023", "025", "026", "028", "029", "030", "031", "032") # Reverse these items: If nothing to reverse, keep as is
  
  items_dimensions = list(
    PersonalEspiritual = c("005", "009", "014", "016", "018"), 
    EspiritualComunal = c("001", "008", "019", "021", "026"), 
    EspiritualEntorno = c("004", "010", "020", "022", "024"), 
    EspiritualTrascendental = c("002", "006", "011", "013", "015")
  )
  

  all_items = c(items_dimensions[[1]], items_dimensions[[2]], items_dimensions[[3]], items_dimensions[[4]])
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                                  dimensions = names(items_dimensions),
                                  help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = TRUE, help_prepare = FALSE)
  
  
  # Create long DIR ------------------------------------------------------------
  DF_long_DIR = 
    DF_long_RAW |> 
   dplyr::select(id, trialid, RAW) |>
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
    dplyr::mutate(
      DIR = RAW
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
        
    
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: names_list$name_DIRd[1], names_list$name_DIRt, etc.] Check with: standardized_names(help_names = TRUE)
  
  DF_wide_RAW_DIR =
    DF_wide_RAW |> 
    dplyr::mutate(

      # Score Dimensions (use 3 digit item numbers)
      !!names_list$name_DIRd[1] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR"))), na.rm = TRUE), 
      !!names_list$name_DIRd[2] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[3] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR"))), na.rm = TRUE), 
      !!names_list$name_DIRd[4] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[4]], "_DIR"))), na.rm = TRUE),
      
      
      # Score Scale
      !!names_list$name_DIRt := rowSums(across(all_of(paste0(short_name_scale_str, "_", all_items, "_DIR"))), na.rm = TRUE)
      
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
