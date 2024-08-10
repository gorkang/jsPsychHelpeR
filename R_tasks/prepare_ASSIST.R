##' Prepare ASSIST
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_ASSIST -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 3 [ADAPT] chunks
##'
##' @title prepare_ASSIST
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_ASSIST <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_ASSIST)
  
  
  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("001", "002") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  ## NameDimension1, NameDimension2 should be the names of the dimensions
  ## Inside each c() create a vector of the item numbers for the dimension
  ## Add lines as needed. If there are no dimensions, keep as is
  items_dimensions = list(
    Total = c("003", "004", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014", "015", "016", "017", "018", "019", "020", "021", "022", "023", "024", "025", "026", "027", "028", "029", "030", "031", "032", "033", "034", "035", "036", "037", "038", "039", "040", "041", "042", "043", "044", "045", "046", "047", "048", "049", "050", "051"),
    ConsumoDrogasLegales = c("003", "004", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014", "015", "016", "017", "018", "019", "020"),
    ConsumoDrogasIlegales = c("021", "022", "023", "024", "025", "026", "027", "028", "029", "030", "031", "032", "033", "034", "035", "036", "037", "038", "039", "040", "041", "042", "043", "044", "045", "046", "047", "048", "049", "050")
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
                                keep_time = FALSE, # Keep time stamp for each response
                                help_prepare = FALSE) # Show n of items, responses,... [CHANGE to TRUE to debug] 
  
  
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
         
         trialid %in% c("ASSIST_001", "ASSIST_002") ~ NA_integer_,
         
         # ITEMS DEL 1 al 51
         
         trialid %in% c("ASSIST_003", "ASSIST_004", "ASSIST_009", "ASSIST_010", "ASSIST_015", "ASSIST_016", "ASSIST_021", "ASSIST_022", "ASSIST_027", "ASSIST_028", "ASSIST_033", "ASSIST_034", "ASSIST_039", "ASSIST_040", "ASSIST_045", "ASSIST_046") & RAW == "Nunca" ~ 0,
         trialid %in% c("ASSIST_003", "ASSIST_004", "ASSIST_009", "ASSIST_010", "ASSIST_015", "ASSIST_016", "ASSIST_021", "ASSIST_022", "ASSIST_027", "ASSIST_028", "ASSIST_033", "ASSIST_034", "ASSIST_039", "ASSIST_040", "ASSIST_045", "ASSIST_046") & RAW == "1 o 2 veces" ~ 2,
         trialid %in% c("ASSIST_003", "ASSIST_004", "ASSIST_009", "ASSIST_010", "ASSIST_015", "ASSIST_016", "ASSIST_021", "ASSIST_022", "ASSIST_027", "ASSIST_028", "ASSIST_033", "ASSIST_034", "ASSIST_039", "ASSIST_040", "ASSIST_045", "ASSIST_046") & RAW == "Mensualmente" ~ 3,
         trialid %in% c("ASSIST_003", "ASSIST_004", "ASSIST_009", "ASSIST_010", "ASSIST_015", "ASSIST_016", "ASSIST_021", "ASSIST_022", "ASSIST_027", "ASSIST_028", "ASSIST_033", "ASSIST_034", "ASSIST_039", "ASSIST_040", "ASSIST_045", "ASSIST_046") & RAW == "Semanalmente" ~ 4,
         trialid %in% c("ASSIST_003", "ASSIST_004", "ASSIST_009", "ASSIST_010", "ASSIST_015", "ASSIST_016", "ASSIST_021", "ASSIST_022", "ASSIST_027", "ASSIST_028", "ASSIST_033", "ASSIST_034", "ASSIST_039", "ASSIST_040", "ASSIST_045", "ASSIST_046") & RAW == "Diariamente o casi a diario" ~ 6,
         
         trialid %in% c("ASSIST_005", "ASSIST_011", "ASSIST_017", "ASSIST_023", "ASSIST_029", "ASSIST_035", "ASSIST_041", "ASSIST_047") & RAW == "Nunca" ~ 0,
         trialid %in% c("ASSIST_005", "ASSIST_011", "ASSIST_017", "ASSIST_023", "ASSIST_029", "ASSIST_035", "ASSIST_041", "ASSIST_047") & RAW == "1 o 2 veces" ~ 4,
         trialid %in% c("ASSIST_005", "ASSIST_011", "ASSIST_017", "ASSIST_023", "ASSIST_029", "ASSIST_035", "ASSIST_041", "ASSIST_047") & RAW == "Mensualmente" ~ 5,
         trialid %in% c("ASSIST_005", "ASSIST_011", "ASSIST_017", "ASSIST_023", "ASSIST_029", "ASSIST_035", "ASSIST_041", "ASSIST_047") & RAW == "Semanalmente" ~ 6,
         trialid %in% c("ASSIST_005", "ASSIST_011", "ASSIST_017", "ASSIST_023", "ASSIST_029", "ASSIST_035", "ASSIST_041", "ASSIST_047") & RAW == "Diariamente o casi a diario" ~ 7,
         
         trialid %in% c("ASSIST_006", "ASSIST_012", "ASSIST_018", "ASSIST_024", "ASSIST_030", "ASSIST_036", "ASSIST_042", "ASSIST_048") & RAW == "Nunca" ~ 0,
         trialid %in% c("ASSIST_006", "ASSIST_012", "ASSIST_018", "ASSIST_024", "ASSIST_030", "ASSIST_036", "ASSIST_042", "ASSIST_048") & RAW == "1 o 2 veces" ~ 5,
         trialid %in% c("ASSIST_006", "ASSIST_012", "ASSIST_018", "ASSIST_024", "ASSIST_030", "ASSIST_036", "ASSIST_042", "ASSIST_048") & RAW == "Mensualmente" ~ 6,
         trialid %in% c("ASSIST_006", "ASSIST_012", "ASSIST_018", "ASSIST_024", "ASSIST_030", "ASSIST_036", "ASSIST_042", "ASSIST_048") & RAW == "Semanalmente" ~ 7,
         trialid %in% c("ASSIST_006", "ASSIST_012", "ASSIST_018", "ASSIST_024", "ASSIST_030", "ASSIST_036", "ASSIST_042", "ASSIST_048") & RAW == "Diariamente o casi a diario" ~ 8,
         
         trialid %in% c("ASSIST_007", "ASSIST_008", "ASSIST_013", "ASSIST_014", "ASSIST_019", "ASSIST_020", "ASSIST_025", "ASSIST_026", "ASSIST_031", "ASSIST_032", "ASSIST_037", "ASSIST_038", "ASSIST_043", "ASSIST_044", "ASSIST_049", "ASSIST_050") & RAW == "No, nunca" ~ 0,
         trialid %in% c("ASSIST_007", "ASSIST_008", "ASSIST_013", "ASSIST_014", "ASSIST_019", "ASSIST_020", "ASSIST_025", "ASSIST_026", "ASSIST_031", "ASSIST_032", "ASSIST_037", "ASSIST_038", "ASSIST_043", "ASSIST_044", "ASSIST_049", "ASSIST_050") & RAW == "Sí, pero no en los últimos 3 meses" ~ 3,
         trialid %in% c("ASSIST_007", "ASSIST_008", "ASSIST_013", "ASSIST_014", "ASSIST_019", "ASSIST_020", "ASSIST_025", "ASSIST_026", "ASSIST_031", "ASSIST_032", "ASSIST_037", "ASSIST_038", "ASSIST_043", "ASSIST_044", "ASSIST_049", "ASSIST_050") & RAW == "Sí, en los últimos 3 meses" ~ 6,
         
         trialid %in% c("ASSIST_051") & RAW == "No, nunca" ~ 0,
         trialid %in% c("ASSIST_051") & RAW == "Sí, pero no en los últimos 3 meses" ~ 1,
         trialid %in% c("ASSIST_051") & RAW == "Sí, en los últimos 3 meses" ~ 2,
         
         
         # ERRATAS, borrar cuando esten corregidas
         trialid %in% c("ASSIST_007", "ASSIST_008", "ASSIST_013", "ASSIST_014", "ASSIST_019", "ASSIST_020", "ASSIST_025", "ASSIST_026", "ASSIST_031", "ASSIST_032", "ASSIST_037", "ASSIST_038", "ASSIST_043", "ASSIST_044", "ASSIST_049", "ASSIST_050") & RAW == "Si, en los ultimos 3 meses" ~ 6,
         trialid %in% c("ASSIST_051") & RAW == "Si, en los ultimos 3 meses" ~ 2,
         trialid %in% c("ASSIST_007", "ASSIST_008", "ASSIST_013", "ASSIST_014", "ASSIST_019", "ASSIST_020", "ASSIST_025", "ASSIST_026", "ASSIST_031", "ASSIST_032", "ASSIST_037", "ASSIST_038", "ASSIST_043", "ASSIST_044", "ASSIST_049", "ASSIST_050") & RAW == "Si, pero no en los ultimos 3 meses" ~ 3,
         trialid %in% c("ASSIST_007", "ASSIST_008", "ASSIST_013", "ASSIST_014", "ASSIST_019", "ASSIST_020", "ASSIST_025", "ASSIST_026", "ASSIST_031", "ASSIST_032", "ASSIST_037", "ASSIST_038", "ASSIST_043", "ASSIST_044", "ASSIST_049", "ASSIST_050") & RAW == "Si, pero no en lo ultimos 3 meses" ~ 3,
         trialid %in% c("ASSIST_051") & RAW == "Si, pero no en los ultimos 3 meses" ~ 1,
         trialid %in% c("ASSIST_051") & RAW == "Si, pero no en lo ultimos 3 meses" ~ 1,
         
         
         
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
    DF_wide_RAW  |>  
    dplyr::mutate(

      # [CHECK] Using correct formula? rowMeans() / rowSums()
      
      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      !!names_list$name_DIRd[1] := rowSums(across(any_of(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[2] := rowSums(across(any_of(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[3] := rowSums(across(any_of(paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR"))), na.rm = TRUE),
      
      # Reliability Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!names_list$name_RELd[1] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_RELd1, "_DIR"))), na.rm = TRUE), 

      # Score Scale
      !!names_list$name_DIRt := rowSums(across(any_of(matches("_DIR$"))), na.rm = TRUE)
      
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
