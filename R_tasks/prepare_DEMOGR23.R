##' Prepare DEMOGR23
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_DEMOGR -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_DEMOGR
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_DEMOGR23 <- function(DF_clean, short_name_scale_str) {
  
  # DEBUG
  # debug_function(prepare_DEMOGR23)
  
  
  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  ## NameDimension1, NameDimension2 should be the names of the dimensions
  ## Inside each c() create a vector of the item numbers for the dimension
  ## Add lines as needed. If there are no dimensions, keep as is
  items_dimensions = list(
    NameDimension1 = c("000"),
    NameDimension2 = c("000")
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
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  # 1 DEMOGR_01 Indique su nombre                                                              
  # 2 DEMOGR_02 Indique su edad                                                                
  # 3 DEMOGR_03 Indique años de educación formal                                               
  # 4 DEMOGR_04 Indique el código de identificación (Iniciales investigador principal + número)
  
  # Transformations
  dplyr::mutate(
    DIR = 
     dplyr::case_when(
        trialid %in% c("DEMOGR_01", "DEMOGR_02", "DEMOGR_03", "DEMOGR_04") ~ RAW,
        
        
        is.na(RAW) ~ NA_character_,
        grepl(items_to_ignore, trialid) ~ NA_character_,
        TRUE ~ "9999"
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
    dplyr::mutate(!!names_list$name_RAW_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_RAW")))),
           !!names_list$name_DIR_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_DIR")))))
  
  
  DF_wide_RAW_DIR =
    DF_wide_RAW %>% 
    
    
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
  # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)
  
  dplyr::mutate(
    # !!names_list$name_DIRd[1] := get(paste0(short_name_scale_str, "_", items_DIRd1, "_DIR")), 
    # !!names_list$name_DIRd[2] := get(paste0(short_name_scale_str, "_", items_DIRd2, "_DIR"))
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
