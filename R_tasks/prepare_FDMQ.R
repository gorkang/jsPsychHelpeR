##' Prepare MDMQ
##' It was mistakenly named FDMQ before. This is a copy so old protocols can work.
##'
##' @title prepare_MDMQ
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_FDMQ <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_MDMQ)

  
  # [ADAPT]: Items to ignore, reverse and dimensions ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("00") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("00") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  names_dimensions = c("Vigilancia", "Hipervigilancia", "Transferencia", "Procastinacion") # If no dimensions, keep names_dimensions = c("")
  
  items_DIRd1 = c("01", "02", "03", "04", "05", "06")
  items_DIRd2 = c("07", "08", "09", "10", "11")
  items_DIRd3 = c("12", "13", "14", "15", "16", "17")
  items_DIRd4 = c("18", "19", "20", "21", "22")
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = names_dimensions,
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE, help_prepare = FALSE)
  
  
  # Create long DIR ------------------------------------------------------------
  DF_long_DIR = 
    DF_long_RAW %>% 
   dplyr::select(id, trialid, RAW) %>%
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************

    dplyr::mutate(
      DIR =
       dplyr::case_when(
          RAW == "No es verdad para mi" ~ 0,
          RAW == "A veces es verdad" ~ 1,
          RAW == "Es verdad para mi" ~ 2,
          TRUE ~ 9999
        )
    ) %>% 
    
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
    DF_long_DIR %>% 
    tidyr::pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR),
      names_glue = "{trialid}_{.value}") %>% 
    
    # NAs for RAW and DIR items
    dplyr::mutate(!!names_list$name_RAW_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_RAW")) & matches("_RAW$")))),
           !!names_list$name_DIR_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_DIR")) & matches("_DIR$")))))
      
    
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)

  DF_wide_RAW_DIR =
    DF_wide_RAW %>% 
    dplyr::mutate(

      # Score Dimensions (use 3 digit item numbers)
      !!names_list$name_DIRd[1] := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd1, "_DIR")), na.rm = TRUE), 
      !!names_list$name_DIRd[2] := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd2, "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[3] := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd3, "_DIR")), na.rm = TRUE), 
      !!names_list$name_DIRd[4] := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd4, "_DIR")), na.rm = TRUE),
      
      # Score Scale
      !!names_list$name_DIRt := rowMeans(select(., matches("_DIR$")), na.rm = TRUE)
      
    )
    
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************


  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
