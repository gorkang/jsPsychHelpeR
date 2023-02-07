##' Prepare IDQ
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_IDQ -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_IDQ
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_IDQ <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_IDQ)

  
  # [ADAPT]: Items to ignore, reverse and dimensions ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("00") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("00") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  names_dimensions = c("edad", "genero") # If no dimensions, keep names_dimensions = c("")
  
  items_DIRd1 = c("02")
  items_DIRd2 = c("04")
  
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
  
    # Transformations
    dplyr::mutate(
      DIR =
       dplyr::case_when(
          RAW == "Mujer" ~ "0",
          RAW == "Hombre" ~ "1",
          RAW == "No binario" ~ "2",
          
          RAW == "No" ~ "0",
          RAW == "Si" ~ "1",
          
          trialid %in% c("IDQ_01", "IDQ_02", "IDQ_04", "IDQ_05", "IDQ_06", "IDQ_07", "IDQ_09", "IDQ_10", "IDQ_11", "IDQ_12", "IDQ_13", 
                         "IDQ_03", "IDQ_08", "IDQ_14" # Were not originally coded [RUT, Direccion, Droga]
                         ) ~ RAW,
          
          is.na(RAW) ~ NA_character_,
          grepl(items_to_ignore, trialid) ~ NA_character_,
          TRUE ~ "9999"
        )
    )  %>% 
    
    # Invert items
    dplyr::mutate(
    #   DIR = 
    #    dplyr::case_when(
    #       DIR == 9999 ~ DIR, # To keep the missing values unchanged
    #       grepl(items_to_reverse, trialid) ~ (6 - DIR),
    #       TRUE ~ DIR
        # )
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
    dplyr::mutate(!!names_list$name_RAW_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_RAW")))),
           !!names_list$name_DIR_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_DIR")))))
      
    
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)
  DF_wide_RAW_DIR = 
    DF_wide_RAW %>% 
      dplyr::mutate(
        !!names_list$name_DIRd[1] := get(paste0(short_name_scale_str, "_", items_DIRd1, "_DIR")), 
        !!names_list$name_DIRd[2] := get(paste0(short_name_scale_str, "_", items_DIRd2, "_DIR"))
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
