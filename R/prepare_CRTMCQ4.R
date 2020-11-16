##' Prepare CRTMCQ4
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_CRTMCQ4 -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_CRTMCQ4
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_CRTMCQ4 <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_CRTMCQ4)
  
  # Standardized names ------------------------------------------------------
  standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = c("Reflectiveness", "Intuitiveness"), # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE)
  
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
    select(id, trialid, RAW) %>%
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
  # Reflectiveness score (0 – 7): 1 point for each correct answer: 
  #   5 pence, 5 minutes, 47 days, 4 days, 29 students, 20 pounds, has lost money, respectively.
  # Intuitiveness score (0 – 7): 1 point for each intuitive incorrect answer:
  #   10 pence, 100 minutes, 24 days, 9 days, 30 students, 10 pounds, is ahead of where he began, respectively.
  
    # Transformations
    mutate(
      DIR =
        case_when(
          trialid == "CRTMCQ4_01" & RAW == "50 pesos" ~ "reflective",
          trialid == "CRTMCQ4_02" & RAW == "5 minutos" ~ "reflective",
          trialid == "CRTMCQ4_03" & RAW == "47 días" ~ "reflective",
          trialid == "CRTMCQ4_04" & RAW == "4 días" ~ "reflective",
          trialid == "CRTMCQ4_05" & RAW == "29 estudiantes" ~ "reflective",
          trialid == "CRTMCQ4_06" & RAW == "20000" ~ "reflective",
          trialid == "CRTMCQ4_07" & RAW == "Ha perdido dinero." ~ "reflective",
          
          trialid == "CRTMCQ4_01" & RAW == "100 pesos" ~ "intuitive",
          trialid == "CRTMCQ4_02" & RAW == "100 minutos" ~ "intuitive",
          trialid == "CRTMCQ4_03" & RAW == "24 días" ~ "intuitive",
          trialid == "CRTMCQ4_04" & RAW == "9 días" ~ "intuitive",
          trialid == "CRTMCQ4_05" & RAW == "30 estudiantes" ~ "intuitive",
          trialid == "CRTMCQ4_06" & RAW == "10000" ~ "intuitive",
          trialid == "CRTMCQ4_07" & RAW == "Ha ganado dinero." ~ "intuitive",
          is.na(RAW) ~ NA_character_,
          grepl(items_to_ignore, trialid) ~ NA_character_,
          TRUE ~ ""
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
    mutate(!!name_RAW_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_RAW")))),
           !!name_DIR_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_DIR"))))) %>%
      
    
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)

    mutate(

      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      !!name_DIRd1 := rowSums(select(., matches("01|02|03|04|05|06|07") & matches("_DIR$")) == "reflective", na.rm = TRUE), 
      !!name_DIRd2 := rowSums(select(., matches("01|02|03|04|05|06|07") & matches("_DIR$")) == "intuitive", na.rm = TRUE)
      
      # Score Scale
      # !!name_DIRt := rowSums(select(., matches("_DIR$")), na.rm = TRUE)
      
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
