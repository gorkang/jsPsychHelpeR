##' Prepare BNT
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_BNT -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_BNT
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_BNT <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_BNT)

  # Standardized names ------------------------------------------------------
  standardized_names(short_name_scale = short_name_scale_str, 
                     # dimensions = c("NameDimension1", "NameDimension2"), # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = TRUE) %>% 
    
    # In BNT we have exit conditions. Add "BNT_03","BNT_04" to make we have all the columns we need to complete the correction
    bind_rows(tibble(trialid = c("BNT_03","BNT_04"))) %>% 
    complete(trialid, nesting(id, experimento)) %>% 
    drop_na(id) 
  
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
  
    # Transformations
    mutate(
      DIR =
        case_when(
          # trialid == "BNT_01" & as.numeric(RAW) >= 400 & as.numeric(RAW) <= 600 ~ 1,
          trialid == "BNT_02" & RAW != 30 ~ 1,
          trialid == "BNT_02" & RAW == 30 ~ 2,
          trialid == "BNT_03" & RAW == 20 ~ 4,
          trialid == "BNT_04" & RAW == 50 ~ 4,
          trialid == "BNT_04" & RAW != 50 ~ 3,
          is.na(RAW) ~ NA_real_,
          grepl(items_to_ignore, trialid) ~ NA_real_,
          TRUE ~ 0
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
    
    # ENMIENDA MANUAL para corregir error en piloto. Si han acertado el BNT_03_DIR, no deberian ver el BNT_04_DIR ------------------
    mutate(BNT_04_DIR = 
             case_when(
               BNT_03_DIR == 4 & BNT_04_DIR == 4 ~ 0,
               TRUE ~ BNT_04_DIR
             )) %>% 
    
    
    
    
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
      !!name_DIRt := rowSums(select(., matches("_DIR$")), na.rm = TRUE)
      
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
