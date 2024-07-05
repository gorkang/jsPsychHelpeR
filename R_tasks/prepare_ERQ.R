##' Prepare ERQ
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change:
##'   - Name of function: prepare_ERQ -> prepare_[value of short_name_scale_str]
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_ERQ
##'
##' @param short_name_scale_str
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_ERQ <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_ERQ)

  
  # [ADAPT]: Items to ignore, reverse and dimensions ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("00") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("00") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  items_dimensions = list(
    ReEvaluacionCognitiva = c("001", "003", "005", "007", "008", "010"), 
    SupresionEmocional = c("002", "004", "006", "009")
  )
  
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str,
                                  dimensions = names(items_dimensions),
                                  help_names = FALSE) # help_names = FALSE once the script is ready

  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(
    DF_clean,
    short_name_scale = short_name_scale_str,
    numeric_responses = TRUE,
    help_prepare = FALSE
  )


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
    

  # Reliability -------------------------------------------------------------

  # REL1 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_dimensions[[1]])
  # REL2 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_dimensions[[2]])
  
  # items_RELd1 = REL1$item_selection_string
  # items_RELd2 = REL2$item_selection_string

  # RELt = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str)
  # items_RELt = RELt$item_selection_string
  
  
  
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)
  DF_wide_RAW_DIR = 
    DF_wide_RAW |> 
    dplyr::mutate(

      # Score Dimensions (use 3 digit item numbers)
      !!names_list$name_DIRd[1] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR"))), na.rm = TRUE), 
      !!names_list$name_DIRd[2] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR"))), na.rm = TRUE),

      # Score Scale
      !!names_list$name_DIRt := rowSums(across(all_of(matches("_DIR$"))), na.rm = TRUE)
      
      # Reliability Dimensions (use 3 digit item numbers)
      # !!names_list$name_RELd[1] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_RELd1, "_DIR"))), na.rm = TRUE), 
      # !!names_list$name_RELd[2] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_RELd2, "_DIR"))), na.rm = TRUE),
      
      # Reliability Scale 
      # !!names_list$name_RELt := rowSums(select(., paste0(short_name_scale_str, "_", items_RELt, "_DIR")), na.rm = TRUE)
      
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
