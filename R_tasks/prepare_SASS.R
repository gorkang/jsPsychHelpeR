##' Prepare SASS
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_SASS -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_SASS
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_SASS <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_SASS)

  # [ADAPT]: Items to ignore, reverse and dimensions ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("01") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("18", "19", "21") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  # [REMEMBER]: REVISAR https://github.com/HeRm4nV/CSCN_Maker/issues/27 
  # REMEMBER ITEMS 2 and 3 both are two instances of the "same" item (work/home)
  
  items_dimensions = list(
    NameDimension1 = c("000")
  )
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = names(items_dimensions), # Use names of dimensions, "" or comment out line
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
          RAW == "Moderadamente" & trialid == "SASS_10" ~ 1, # Moderadamente is the third option except for item SASS_10
          grepl("Nada|Nada de entusiasmo|Insatisfactoria|Insatisfactorio|Nunca|Nadie|Pasivamente|Ningún valor|Para nada", RAW) ~ 0,
          grepl("Poco|Poco entusiasmo|Justa|Justo|Raramente|Pocas personas|Poco valor|Levemente|No mucho|A veces", RAW) ~ 1,
          grepl("Medianamente|Algo de gozo|Algo de entusiasmo|Buena|Bueno|Frecuentemente|Algunas personas|Activamente|A menudo|La mayor parte del tiempo|Algún valor|Moderadamente", RAW) ~ 2,
          grepl("Mucho|Mucho entusiasmo|Muy buena|Muy bueno|Muy frecuente|Muchas personas|Muy activamente|Gran valor|Muy a menudo|Siempre|Completamente|Muchísimo", RAW) ~ 3,
          is.na(RAW) ~ NA_real_,
          trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_real_,
          TRUE ~ 9999
        )
    ) |> 
    
    # Invert items
    dplyr::mutate(
      DIR = 
       dplyr::case_when(
          DIR == 9999 ~ DIR, # To keep the missing values unchanged
          trialid %in% paste0(short_name_scale_str, "_", items_to_reverse) ~ (3 - DIR),
          TRUE ~ DIR
        )
    )
    
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
    

  # Create DF_wide_RAW -----------------------------------------------------
  
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
  
  RELt = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str)
  
  # With only a couple participants, it can happen that we only have one of the two
  included_key_items = c("SASS_02_DIR", "SASS_03_DIR")[c("SASS_02_DIR", "SASS_03_DIR") %in% names(DF_wide_RAW)]
  # items_RELt = c("02", "03", RELt$item_selection_string)
  items_RELt = RELt$item_selection_string
  # REVIEW: EN ESTE CASO, los items 02 y 03 NO ENTRAN EN alphadrop_me() pq tienen NA's, pero SI los incluimos aqui (???) ------
  
  
  
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)
  
  DF_wide_RAW_DIR =
    DF_wide_RAW |> 
    
    dplyr::mutate(
      
      # Score Scale
      !!names_list$name_DIRt := rowSums(across(all_of(matches("_DIR$"))), na.rm = TRUE),
      
      # Reliability Scale 
      !!names_list$name_RELt := rowSums(across(all_of(c(included_key_items, paste0(short_name_scale_str, "_", items_RELt, "_DIR")))), na.rm = TRUE)

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
