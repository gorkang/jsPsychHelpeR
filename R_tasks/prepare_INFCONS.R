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
  standardized_names(short_name_scale = short_name_scale_str, 
                     # dimensions = c("NameDimension1", "NameDimension2"), # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = TRUE)
  
  # Show number of items, responses, etc. [uncomment to help prepare the test] 
  # prepare_helper(DF_long_RAW, show_trialid_questiontext = TRUE)
  
  
  # Create long DIR ------------------------------------------------------------
  
  # [ADAPT]: Items to ignore and reverse ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("00|00") # Ignore the following items: If nothing to ignore, keep "00|00"
  items_to_reverse = c("00|00") # Reverse the following items: If nothing to ignore, keep "00|00"
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  DF_long_DIR = 
    DF_long_RAW %>% 
    select(id, trialid, stimulus, RAW) %>%
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
    # Transformations
    mutate(
      RAW = stimulus,
      DIR = 
        case_when(
          grepl("^img", RAW) ~ 2,
          grepl("Se da en", RAW) ~ 1,
          !grepl("Se da en", RAW) & !grepl("^img", RAW) ~ 0,
          is.na(RAW) ~ NA_real_,
          grepl(items_to_ignore, trialid) ~ NA_real_,
          TRUE ~ 9999
        )
    ) %>% 
    
    # Invert items
    mutate(
      DIR = 
        case_when(
          DIR == 9999 ~ DIR, # To keep the missing values unchanged
          grepl(items_to_reverse, trialid) ~ (6 - DIR),
          TRUE ~ DIR
        )
    ) %>% 
    select(-stimulus)
    
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
    
  # Create DF_wide_RAW_DIR -----------------------------------------------------
  DF_wide_RAW_DIR =
    DF_long_DIR %>% 
    pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR),
      names_glue = "{trialid}_{.value}") %>% 
    
    # NAs for RAW and DIR items
    mutate(!!name_RAW_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_RAW")))),
           !!name_DIR_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_DIR"))))) %>% 
    
    
    # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
  # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)
  
  mutate(
    
    # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
    # !!name_DIRd1 := rowSums(select(., matches("02|04|05") & matches("_DIR$")), na.rm = TRUE), 
    # !!name_DIRd2 := rowSums(select(., matches("01|03|08") & matches("_DIR$")), na.rm = TRUE), 
    
    # Score Scale
    !!name_DIRt := rowMeans(select(., matches("_DIR$")), na.rm = TRUE)
    
  ) %>% 
    mutate(!!name_DIRt := 
             case_when(
               !!sym(name_DIRt) == 0 ~ "control",
               !!sym(name_DIRt) == 1 ~ "text",
               !!sym(name_DIRt) == 2 ~ "picture",
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
