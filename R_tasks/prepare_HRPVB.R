##' Prepare HRPVB
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_HRPVB -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_HRPVB
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_HRPVB <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_HRPVB)

  
  # [ADAPT]: Items to ignore, reverse and dimensions ---------------------------------------
  # ****************************************************************************
  
  description_task = "HRPVB description"
  
  items_to_ignore = c("00") # Ignore these items
  items_to_reverse = c("00") # Reverse these items
  
  names_dimensions = c("Cesarean", "Vaginal")
  description_dimensions = c("Cesarean responses", "Vaginal responses")
  
  items_DIRd1 = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11")
  items_DIRd2 = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11")

  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  
  # Standardized names ------------------------------------------------------
  standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = names_dimensions, 
                     help_names = FALSE)
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = TRUE)
  
  # Show number of items, responses, etc. [uncomment to help prepare the test] 
  # prepare_helper(DF_long_RAW, show_trialid_questiontext = TRUE)
  
  
  # Create long DIR ------------------------------------------------------------
  
  DF_long_DIR = 
    DF_long_RAW %>% 
    select(id, trialid, RAW) %>%
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
    # Transformations
    mutate(
      DIR = RAW
    ) %>% 
    
    # Invert items
    mutate(
      DIR = 
        case_when(
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
    pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR),
      names_glue = "{trialid}_{.value}") %>% 
    
    # NAs for RAW and DIR items
    mutate(!!name_RAW_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_RAW")))),
           !!name_DIR_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_DIR")))))
  
  
  
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************

  DF_wide_RAW_DIR = 
    DF_wide_RAW %>% 
    mutate(
      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      !!name_DIRd1 := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd1, "_01_DIR")), na.rm = TRUE),
      !!name_DIRd2 := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd2, "_02_DIR")), na.rm = TRUE)
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
