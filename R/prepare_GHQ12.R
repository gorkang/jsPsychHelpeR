##' prepare_GHQ12
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_TEMPLATE -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_GHQ12
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_GHQ12 <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_GHQ12)

  # Standardized names ------------------------------------------------------
  standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = c("AnxietyDepression", "SocialDisfunction", "LossConfidence"),
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
          RAW == "Nunca" ~ 1,
          RAW == "Siempre" ~ 2,
          RAW == "Casi\\n Siempre" ~ 3,
          RAW == "Casi\\n Nunca" ~ 4,
          RAW == "A veces" ~ 5,
          TRUE ~ 9999
        ))
    
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
    mutate(!!name_RAW_NA := rowSums(is.na(select(., matches("_RAW")))),
           !!name_DIR_NA := rowSums(is.na(select(., matches("_DIR"))))) %>% 
      
    
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions] Check with: standardized_names()

    mutate(

      # Score Scale
      !!name_DIRt := rowSums(select(., matches("_DIR")), na.rm = TRUE),
      
      # Score Dimensions (use 3 digit item numbers)
      !!name_DIRd1 := rowSums(select(., matches("02|04|05|06|07|10|14") & matches("_DIR")), na.rm = TRUE), # NAME SHOULD BE !!name_DIRd1
      !!name_DIRd2 := rowSums(select(., matches("01|03|08|09|11|12|13") & matches("_DIR")), na.rm = TRUE) # NAME SHOULD BE !!name_DIRd2
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
