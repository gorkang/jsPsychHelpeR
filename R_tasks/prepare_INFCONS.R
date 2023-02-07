##' Prepare INFCONS
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_INFCONS -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_INFCONS
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_INFCONS <- function(DF_clean, short_name_scale_str) {
  
  # DEBUG
  # debug_function(prepare_INFCONS)
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     # dimensions = c("NameDimension1", "NameDimension2"), # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = TRUE, help_prepare = FALSE)
  
  
  # Create long DIR ------------------------------------------------------------
  
  # [ADAPT]: Items to ignore and reverse ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("00|00") # Ignore the following items: If nothing to ignore, keep "00|00"
  items_to_reverse = c("00|00") # Reverse the following items: If nothing to ignore, keep "00|00"
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  DF_long_DIR = 
    DF_long_RAW %>% 
   dplyr::select(id, trialid, stimulus, RAW) %>%
    
    
    # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
  # Transformations
  dplyr::mutate(
    RAW = stimulus,
    DIR = 
     dplyr::case_when(
        grepl("^img/", RAW) ~ 2,
        grepl("Se da en", RAW) ~ 1,
        !grepl("Se da en", RAW) & !grepl("^img/", RAW) ~ 0,
        is.na(RAW) ~ NA_real_,
        grepl(items_to_ignore, trialid) ~ NA_real_,
        TRUE ~ 9999
      )
  ) %>% 
    
    # Invert items
    dplyr::mutate(
      DIR = 
       dplyr::case_when(
          DIR == 9999 ~ DIR, # To keep the missing values unchanged
          grepl(items_to_reverse, trialid) ~ (6 - DIR),
          TRUE ~ DIR
        )
    ) %>% 
   dplyr::select(-stimulus)
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  # Create DF_wide_RAW_DIR -----------------------------------------------------
  DF_wide_RAW_DIR =
    DF_long_DIR %>% 
    tidyr::pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR),
      names_glue = "{trialid}_{.value}") %>% 
    
    # NAs for RAW and DIR items
    dplyr::mutate(!!names_list$name_RAW_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_RAW")))),
           !!names_list$name_DIR_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_DIR"))))) %>% 
    
    
    # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
  # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)
  
  dplyr::mutate(
    
    # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
    # !!names_list$name_DIRd[1] := rowSums(select(., matches("02|04|05") & matches("_DIR$")), na.rm = TRUE), 
    # !!names_list$name_DIRd[2] := rowSums(select(., matches("01|03|08") & matches("_DIR$")), na.rm = TRUE), 
    
    # Score Scale
    !!names_list$name_DIRt := rowMeans(select(., matches("_DIR$")), na.rm = TRUE)
  ) %>% 
    
    # Check value of the Score Scale column and replace using the apropriate string
    dplyr::mutate(!!names_list$name_DIRt := 
            dplyr::case_when(
               !!sym(names_list$name_DIRt) == 0 ~ "control",
               !!sym(names_list$name_DIRt) == 1 ~ "text",
               !!sym(names_list$name_DIRt) == 2 ~ "picture",
               TRUE ~ NA_character_))
  
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
  
}
