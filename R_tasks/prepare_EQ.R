##' Prepare EQ
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_EQ -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_EQ
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_EQ <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # debug_function(prepare_EQ)

  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("02", "03", "05", "07", "09", "13", "16", "17", "20", "23", "24", "30", "31", "33", "40", "45", "47", "51", "53", "56") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  ## NameDimension1, NameDimension2 should be the names of the dimensions
  ## Inside each c() create a vector of the item numbers for the dimension
  ## Add lines as needed. If there are no dimensions, keep as is
  items_dimensions = list(
    CoeficienteEmpatia = c("01", "06", "19", "22", "25", "26", "35", "36", "37", "38", "41", "42", "43", "44", "52", "54", "55", "57", "58", "59", "60", "04", "08", "10", "11", "12", "14", "15", "18", "21", "27", "28", "29", "32", "34", "39", "46", "48", "49", "50")
  )
  
  # [END ADAPT 1/3]: ***********************************************************
  # ****************************************************************************
  
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                                  dimensions = names(items_dimensions),
                                  help_names = FALSE) # [KEEP as FALSE]
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, 
                                short_name_scale = short_name_scale_str, 
                                numeric_responses = FALSE, # [TRUE or FALSE]
                                is_experiment = FALSE, 
                                keep_time = FALSE, # Keep timestamp for each response
                                help_prepare = FALSE) # Show n of items, responses,... [CHANGE to FALSE] 
  
  
  # Create long DIR ------------------------------------------------------------
  DF_long_DIR = 
    DF_long_RAW |> 
    # If using keep_time = TRUE above, use this and add timestamp to the select() call
    # dplyr::mutate(timestamp = as.POSIXlt(datetime, format = "%Y-%m-%dT%H%M%S")) |> 
    dplyr::select(id, trialid, RAW) |>
    
    
    
  # [ADAPT 2/3]: RAW to DIR for individual items -------------------------------
  # ****************************************************************************
  
    # Transformations
    dplyr::mutate(
      DIR =
       dplyr::case_when(
         trialid %in% c("EQ_01", "EQ_06", "EQ_19", "EQ_22", "EQ_25", "EQ_26", "EQ_35", "EQ_36", "EQ_37", "EQ_38", "EQ_41", "EQ_42", "EQ_43", "EQ_44", "EQ_52", "EQ_54", "EQ_55", "EQ_57", "EQ_58", "EQ_59", "EQ_60") & RAW %in% c("Totalmente de acuerdo", "Totalmentede acuerdo") ~ 2,
         trialid %in% c("EQ_01", "EQ_06", "EQ_19", "EQ_22", "EQ_25", "EQ_26", "EQ_35", "EQ_36", "EQ_37", "EQ_38", "EQ_41", "EQ_42", "EQ_43", "EQ_44", "EQ_52", "EQ_54", "EQ_55", "EQ_57", "EQ_58", "EQ_59", "EQ_60") & RAW %in% c("Un poco de acuerdo", "Un pocode acuerdo") ~ 1,
         trialid %in% c("EQ_01", "EQ_06", "EQ_19", "EQ_22", "EQ_25", "EQ_26", "EQ_35", "EQ_36", "EQ_37", "EQ_38", "EQ_41", "EQ_42", "EQ_43", "EQ_44", "EQ_52", "EQ_54", "EQ_55", "EQ_57", "EQ_58", "EQ_59", "EQ_60") & RAW %in% c("Un poco de desacuerdo", "Un pocode desacuerdo") ~ 0,
         trialid %in% c("EQ_01", "EQ_06", "EQ_19", "EQ_22", "EQ_25", "EQ_26", "EQ_35", "EQ_36", "EQ_37", "EQ_38", "EQ_41", "EQ_42", "EQ_43", "EQ_44", "EQ_52", "EQ_54", "EQ_55", "EQ_57", "EQ_58", "EQ_59", "EQ_60") & RAW %in% c( "Totalmente desacuerdo", "Totalmentedesacuerdo") ~ 0,
         
         trialid %in% c("EQ_04", "EQ_08", "EQ_10", "EQ_11", "EQ_12", "EQ_14", "EQ_15", "EQ_18", "EQ_21", "EQ_27", "EQ_28", "EQ_29", "EQ_32", "EQ_34", "EQ_39", "EQ_46", "EQ_48", "EQ_49", "EQ_50") & RAW %in% c("Totalmente de acuerdo", "Totalmentede acuerdo") ~ 0,
         trialid %in% c("EQ_04", "EQ_08", "EQ_10", "EQ_11", "EQ_12", "EQ_14", "EQ_15", "EQ_18", "EQ_21", "EQ_27", "EQ_28", "EQ_29", "EQ_32", "EQ_34", "EQ_39", "EQ_46", "EQ_48", "EQ_49", "EQ_50") & RAW %in% c("Un poco de acuerdo", "Un pocode acuerdo") ~ 0,
         trialid %in% c("EQ_04", "EQ_08", "EQ_10", "EQ_11", "EQ_12", "EQ_14", "EQ_15", "EQ_18", "EQ_21", "EQ_27", "EQ_28", "EQ_29", "EQ_32", "EQ_34", "EQ_39", "EQ_46", "EQ_48", "EQ_49", "EQ_50") & RAW %in% c("Un poco de desacuerdo", "Un pocode desacuerdo") ~ 1,
         trialid %in% c("EQ_04", "EQ_08", "EQ_10", "EQ_11", "EQ_12", "EQ_14", "EQ_15", "EQ_18", "EQ_21", "EQ_27", "EQ_28", "EQ_29", "EQ_32", "EQ_34", "EQ_39", "EQ_46", "EQ_48", "EQ_49", "EQ_50") & RAW %in% c( "Totalmente desacuerdo", "Totalmentedesacuerdo") ~ 2,
         
          is.na(RAW) ~ NA_real_, # OR NA_character_,
          trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_real_, # OR NA_character_,
          TRUE ~ 9999 # OR "9999"
        )
    ) |> 
    
    # Invert items [CAN BE DELETED IF NOT USED or DIR is non-numeric]
    dplyr::mutate(
      DIR = 
       dplyr::case_when(
          DIR == 9999 ~ DIR, # To keep the missing values unchanged
          trialid %in% paste0(short_name_scale_str, "_", items_to_reverse) ~ (6 - DIR), # REVIEW and replace 6 by MAX + 1
          TRUE ~ DIR
        )
    )
    
  # [END ADAPT 2/3]: ***********************************************************
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
  
  # Reliability -------------------------------------------------------------
  # REL1 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_dimensions[[1]])
  # items_RELd1 = REL1$item_selection_string
    
  
  # [USE STANDARD NAMES FOR Scales and dimensions: names_list$name_DIRd[1], names_list$name_DIRt,...] 
  # CHECK with: create_formulas(type = "dimensions_DIR", functions = "sum", names(items_dimensions))
  DF_wide_RAW_DIR =
    DF_wide_RAW |> 
    dplyr::mutate(

      # [CHECK] Using correct formula? rowMeans() / rowSums()
      
      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      !!names_list$name_DIRd[1] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR"))), na.rm = TRUE)
    )
    
  # [END ADAPT 3/3]: ***********************************************************
  # ****************************************************************************


  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE, output_formats = output_formats)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
