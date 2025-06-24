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
  
  items_to_ignore = c("002", "003", "005", "007", "009", "013", "016", "017", "020", "023", "024", "030", "031", "033", "040", "045", "047", "051", "053", "056") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  ## NameDimension1, NameDimension2 should be the names of the dimensions
  ## Inside each c() create a vector of the item numbers for the dimension
  ## Add lines as needed. If there are no dimensions, keep as is
  items_dimensions = list(
    CoeficienteEmpatia = c("001", "006", "019", "022", "025", "026", "035", "036", "037", "038", "041", "042", "043", "044", "052", "054", "055", "057", "058", "059", "060", "004", "008", "010", "011", "012", "014", "015", "018", "021", "027", "028", "029", "032", "034", "039", "046", "048", "049", "050")
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
         trialid %in% c("EQ_001", "EQ_006", "EQ_019", "EQ_022", "EQ_025", "EQ_026", "EQ_035", "EQ_036", "EQ_037", "EQ_038", "EQ_041", "EQ_042", "EQ_043", "EQ_044", "EQ_052", "EQ_054", "EQ_055", "EQ_057", "EQ_058", "EQ_059", "EQ_060") & RAW %in% c("Totalmente de acuerdo", "Totalmentede acuerdo") ~ 2,
         trialid %in% c("EQ_001", "EQ_006", "EQ_019", "EQ_022", "EQ_025", "EQ_026", "EQ_035", "EQ_036", "EQ_037", "EQ_038", "EQ_041", "EQ_042", "EQ_043", "EQ_044", "EQ_052", "EQ_054", "EQ_055", "EQ_057", "EQ_058", "EQ_059", "EQ_060") & RAW %in% c("Un poco de acuerdo", "Un pocode acuerdo") ~ 1,
         trialid %in% c("EQ_001", "EQ_006", "EQ_019", "EQ_022", "EQ_025", "EQ_026", "EQ_035", "EQ_036", "EQ_037", "EQ_038", "EQ_041", "EQ_042", "EQ_043", "EQ_044", "EQ_052", "EQ_054", "EQ_055", "EQ_057", "EQ_058", "EQ_059", "EQ_060") & RAW %in% c("Un poco de desacuerdo", "Un pocode desacuerdo") ~ 0,
         trialid %in% c("EQ_001", "EQ_006", "EQ_019", "EQ_022", "EQ_025", "EQ_026", "EQ_035", "EQ_036", "EQ_037", "EQ_038", "EQ_041", "EQ_042", "EQ_043", "EQ_044", "EQ_052", "EQ_054", "EQ_055", "EQ_057", "EQ_058", "EQ_059", "EQ_060") & RAW %in% c( "Totalmente desacuerdo", "Totalmentedesacuerdo") ~ 0,
         trialid %in% c("EQ_004", "EQ_008", "EQ_010", "EQ_011", "EQ_012", "EQ_014", "EQ_015", "EQ_018", "EQ_021", "EQ_027", "EQ_028", "EQ_029", "EQ_032", "EQ_034", "EQ_039", "EQ_046", "EQ_048", "EQ_049", "EQ_050") & RAW %in% c("Totalmente de acuerdo", "Totalmentede acuerdo") ~ 0,
         trialid %in% c("EQ_004", "EQ_008", "EQ_010", "EQ_011", "EQ_012", "EQ_014", "EQ_015", "EQ_018", "EQ_021", "EQ_027", "EQ_028", "EQ_029", "EQ_032", "EQ_034", "EQ_039", "EQ_046", "EQ_048", "EQ_049", "EQ_050") & RAW %in% c("Un poco de acuerdo", "Un pocode acuerdo") ~ 0,
         trialid %in% c("EQ_004", "EQ_008", "EQ_010", "EQ_011", "EQ_012", "EQ_014", "EQ_015", "EQ_018", "EQ_021", "EQ_027", "EQ_028", "EQ_029", "EQ_032", "EQ_034", "EQ_039", "EQ_046", "EQ_048", "EQ_049", "EQ_050") & RAW %in% c("Un poco de desacuerdo", "Un pocode desacuerdo") ~ 1,
         trialid %in% c("EQ_004", "EQ_008", "EQ_010", "EQ_011", "EQ_012", "EQ_014", "EQ_015", "EQ_018", "EQ_021", "EQ_027", "EQ_028", "EQ_029", "EQ_032", "EQ_034", "EQ_039", "EQ_046", "EQ_048", "EQ_049", "EQ_050") & RAW %in% c( "Totalmente desacuerdo", "Totalmentedesacuerdo") ~ 2,
         
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
