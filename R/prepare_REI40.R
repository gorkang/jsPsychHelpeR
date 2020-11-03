##' Prepare REI40
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_REI40 -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_REI40
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_REI40 <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_REI40)

  # Standardized names ------------------------------------------------------
  standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = c("RationalAbility", "RationalEngagement", "ExperientialAbility", "ExperiencialEngagement", "Rational", "Experiential"), # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
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

    mutate(
      DIR = RAW
      ) %>% 
    
    # Invert items
    mutate(
      DIR = 
        case_when(
          DIR == 9999 ~ DIR,
          grepl("01|02|03|04|05|11|13|15|17|19|21|26|28|30|34|36|37|38", trialid) ~ (6 - DIR),
          TRUE ~ DIR
        )
    )
    
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
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)

    mutate(

      # Score Dimensions (use 3 digit item numbers)
      !!name_DIRd1 := rowMeans(select(., matches("01|02|03|04|05|06|07|08|09|10") & matches("_DIR$")), na.rm = TRUE),
      !!name_DIRd2 := rowMeans(select(., matches("11|12|13|14|15|16|17|18|19|20") & matches("_DIR$")), na.rm = TRUE),
      !!name_DIRd3 := rowMeans(select(., matches("21|22|23|24|25|26|27|28|29|30") & matches("_DIR$")), na.rm = TRUE),
      !!name_DIRd4 := rowMeans(select(., matches("31|32|33|34|35|36|37|38|39|40") & matches("_DIR$")), na.rm = TRUE),
      
      # Meta-dimensions
      !!name_DIRd5 := rowMeans(select(., matches("01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20") & matches("_DIR$")), na.rm = TRUE),
      !!name_DIRd6 := rowMeans(select(., matches("21|22|23|24|25|26|27|28|29|30|31|32|33|34|35|36|37|38|39|40") & matches("_DIR$")), na.rm = TRUE)
      
      # Score Scale
      # !!name_DIRt := rowMeans(select(., matches("_DIR$")), na.rm = TRUE)
      
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
