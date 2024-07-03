##' Prepare LSNS
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_LSNS -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_LSNS
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_LSNS <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_LSNS)
  
  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  ## NameDimension1, NameDimension2 should be the names of the dimensions
  ## Inside each c() create a vector of the item numbers for the dimension
  ## Add lines as needed. If there are no dimensions, keep as is
  items_dimensions = list(
    Familiares = c("01", "02", "03", "04", "05", "06"),
    Amistados = c("07", "08", "09", "10", "11", "12")
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
                                help_prepare = FALSE) # Show n of items, responses,... [CHANGE to FALSE] 
  
  
  # Create long DIR ------------------------------------------------------------
  DF_long_DIR = 
    DF_long_RAW %>% 
   dplyr::select(id, trialid, RAW) %>%
    
    
    
  # [ADAPT 2/3]: RAW to DIR for individual items -------------------------------
  # ****************************************************************************
  
    # Transformations
    dplyr::mutate(
      DIR =
       dplyr::case_when(
          trialid %in% c("LSNS_01", "LSNS_03", "LSNS_04", "LSNS_07", "LSNS_09", "LSNS_10") & RAW == "Ninguno" ~ 0,
          trialid %in% c("LSNS_01", "LSNS_03", "LSNS_04", "LSNS_07", "LSNS_09", "LSNS_10") & RAW == "Uno" ~ 1,
          trialid %in% c("LSNS_01", "LSNS_03", "LSNS_04", "LSNS_07", "LSNS_09", "LSNS_10") & RAW == "Dos" ~ 2,
          trialid %in% c("LSNS_01", "LSNS_03", "LSNS_04", "LSNS_07", "LSNS_09", "LSNS_10") & RAW == "Tres o cuatro" ~ 3,
          trialid %in% c("LSNS_01", "LSNS_03", "LSNS_04", "LSNS_07", "LSNS_09", "LSNS_10") & RAW == "De cinco a ocho" ~ 4,
          trialid %in% c("LSNS_01", "LSNS_03", "LSNS_04", "LSNS_07", "LSNS_09", "LSNS_10") & RAW == "Nueve o más" ~ 5,
          trialid %in% c("LSNS_01", "LSNS_03", "LSNS_04", "LSNS_07", "LSNS_09", "LSNS_10") & RAW == "Nueve o má" ~ 5,
          
          trialid %in% c("LSNS_02", "LSNS_08") & RAW == "Menos de mensual" ~ 0,
          trialid %in% c("LSNS_02", "LSNS_08") & RAW == "Menos de una vez por mes" ~ 0,
          trialid %in% c("LSNS_02", "LSNS_08") & RAW == "Mensualmente" ~ 1,
          trialid %in% c("LSNS_02", "LSNS_08") & RAW == "Mensual" ~ 1,
          trialid %in% c("LSNS_02", "LSNS_08") & RAW == "Pocas veces al mes" ~ 2,
          trialid %in% c("LSNS_02", "LSNS_08") & RAW == "Algunas veces al mes" ~ 2,
          trialid %in% c("LSNS_02", "LSNS_08") & RAW == "Semanalmente" ~ 3,
          trialid %in% c("LSNS_02", "LSNS_08") & RAW == "Algunas veces por semana" ~ 4,
          trialid %in% c("LSNS_02", "LSNS_08") & RAW == "Diariamente" ~ 5,
          
          trialid %in% c("LSNS_05", "LSNS_06", "LSNS_11", "LSNS_12") & RAW == "Nunca" ~ 0,
          trialid %in% c("LSNS_05", "LSNS_06", "LSNS_11", "LSNS_12") & RAW == "Rara vez" ~ 1,
          trialid %in% c("LSNS_05", "LSNS_06", "LSNS_11", "LSNS_12") & RAW == "A veces" ~ 2,
          trialid %in% c("LSNS_05", "LSNS_06", "LSNS_11", "LSNS_12") & RAW == "Con frecuencia" ~ 3,
          trialid %in% c("LSNS_05", "LSNS_06", "LSNS_11", "LSNS_12") & RAW == "A menudo" ~ 3,
          trialid %in% c("LSNS_05", "LSNS_06", "LSNS_11", "LSNS_12") & RAW == "Con mucha frecuencia" ~ 4,
          trialid %in% c("LSNS_05", "LSNS_06", "LSNS_11", "LSNS_12") & RAW == "Muy a menudo" ~ 4,
          trialid %in% c("LSNS_05", "LSNS_06", "LSNS_11", "LSNS_12") & RAW == "Siempre" ~ 5,
          
          is.na(RAW) ~ NA_real_, # OR NA_character_,
          grepl(items_to_ignore, trialid) ~ NA_real_, # OR NA_character_,
          TRUE ~ 9999 # OR "9999"
        )
    ) %>% 
    
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
    DF_long_DIR %>% 
    tidyr::pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR),
      names_glue = "{trialid}_{.value}") %>% 
    
    # NAs for RAW and DIR items
    dplyr::mutate(!!names_list$name_RAW_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_RAW")) & matches("_RAW$")))),
           !!names_list$name_DIR_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_DIR")) & matches("_DIR$")))))


  
  # [ADAPT 3/3]: Scales and dimensions calculations ----------------------------
  # ****************************************************************************
  
  # Reliability -------------------------------------------------------------
  # REL1 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_DIRd1)
  # items_RELd1 = REL1$item_selection_string
    
  
  # [USE STANDARD NAMES FOR Scales and dimensions: names_list$name_DIRd[1], names_list$name_DIRt,...] 
  # CHECK with: create_formulas(type = "dimensions_DIR", functions = "sum", names(items_dimensions))
  DF_wide_RAW_DIR =
    DF_wide_RAW %>% 
    dplyr::mutate(

      # [CHECK] Using correct formula? rowMeans() / rowSums()
      
      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      !!names_list$name_DIRd[1] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR")), na.rm = TRUE), 
      !!names_list$name_DIRd[2] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR")), na.rm = TRUE),
      
      # Reliability Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!names_list$name_RELd[1] := rowMeans(select(., paste0(short_name_scale_str, "_", items_RELd1, "_DIR")), na.rm = TRUE), 

      # Score Scale
      !!names_list$name_DIRt := rowSums(select(., matches("_DIR$")), na.rm = TRUE)
      
    )
    
  # [END ADAPT 3/3]: ***********************************************************
  # ****************************************************************************


  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
