##' Prepare RTS
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_RTS -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_RTS
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_RTS <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_RTS)

  # [ADAPT]: Items to ignore, reverse and dimensions ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("01", "06", "07", "10", "11", "14", "15", "17", "23", "24", "27", "28") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("00") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  names_dimensions = c("") # If no dimensions, keep names_dimensions = c("")
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  # Standardized names ------------------------------------------------------
  standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = names_dimensions,
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE)
  
  # Show number of items, responses, etc. [uncomment to help prepare the test] 
  # prepare_helper(DF_long_RAW, show_trialid_questiontext = TRUE)
  
  
  # Create long DIR ------------------------------------------------------------

  DF_long_DIR = 
    DF_long_RAW %>% 
    select(id, trialid, RAW) %>%
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
    mutate(
      DIR =
        case_when(
          grepl("02|03|04|05|08|09|12|13|16|18|19|20|21|22|25|26|29", trialid) & RAW == "Verdadero" ~ 1,
          grepl("02|03|04|05|08|09|12|13|16|18|19|20|21|22|25|26|29", trialid) & RAW == "Falso" ~ 0,
          is.na(RAW) ~ NA_real_,
          trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_real_,
          TRUE ~ 9999
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
    mutate(!!name_RAW_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_RAW")) & matches("_RAW$")))),
           !!name_DIR_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_DIR")) & matches("_DIR$")))))
      
    
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)

  DF_wide_RAW_DIR = 
    DF_wide_RAW %>% 
    mutate(

      # # Score Dimensions (use 3 digit item numbers)
      # !!name_DIRd1 := rowSums(select(., matches("02|04|05") & matches("_DIR$")), na.rm = TRUE), 
      # !!name_DIRd2 := rowSums(select(., matches("01|03|08") & matches("_DIR$")), na.rm = TRUE), 
      
      # Score Scale
      !!name_DIRt := rowSums(select(., matches("_DIR$")), na.rm = TRUE),
      
      # Rasch scores
      !!name_STDt :=  
          case_when(
            RTS_DIRt == 0 ~ 13.7,
            RTS_DIRt == 1 ~ 15.9,
            RTS_DIRt == 2 ~ 18.3,
            RTS_DIRt == 3 ~ 19.9,
            RTS_DIRt == 4 ~ 21.1,
            RTS_DIRt == 5 ~ 22.1,
            RTS_DIRt == 6 ~ 23.1,
            RTS_DIRt == 7 ~ 24.0,
            RTS_DIRt == 8 ~ 24.9,
            RTS_DIRt == 9 ~ 25.7,
            RTS_DIRt == 10 ~ 26.6,
            RTS_DIRt == 11 ~ 27.5,
            RTS_DIRt == 12 ~ 28.5,
            RTS_DIRt == 13 ~ 29.6,
            RTS_DIRt == 14 ~ 30.9,
            RTS_DIRt == 15 ~ 32.5,
            RTS_DIRt == 16 ~ 35.0,
            RTS_DIRt == 17 ~ 37.3,
            TRUE ~ 9999
          )
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
