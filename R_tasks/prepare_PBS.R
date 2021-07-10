##' Prepare PBS
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_PBS -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_PBS
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_PBS <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_PBS)

  
  # [ADAPT]: Items to ignore, reverse and dimensions ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("023") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  names_dimensions = c("CreenciasReligiosasTradicionales", "psi", "brujeria", "supersticion", "espiritismo", "FormasVidaExtraordinaria", "precognicion") # If no dimensions, keep names_dimensions = c("")
  
  items_DIRd1 = c("001", "008", "015", "022")
  items_DIRd2 = c("002", "009", "016", "023")
  items_DIRd3 = c("003", "010", "017", "024")
  items_DIRd4 = c("004", "011", "018")
  items_DIRd5 = c("005", "012", "019", "025")
  items_DIRd6 = c("006", "013", "020")
  items_DIRd7 = c("007", "014", "021", "026")

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
          RAW == '1 Muy en desacuerdo' ~ 1,
          RAW == '2 Moderadamente en desacuerdo' ~ 2,
          RAW == '3 Un poco en desacuerdo' ~ 3,
          RAW == '4 No sé / No tengo certeza' ~ 4,
          RAW == '5 Un poco de acuerdo' ~ 5,
          RAW == '6 Moderadamente de acuerdo' ~ 6,
          RAW == '7 Muy de acuerdo' ~ 7,
          TRUE ~ 9999
        )
    ) %>% 
    
    # Invert items 23
    # [TODO]: Item id's will be 3 digits: 023
    mutate(
      DIR = 
        case_when(
          DIR == 9999 ~ DIR,
          trialid %in% paste0(short_name_scale_str, "_", items_to_reverse) ~ (8 - DIR),
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
    mutate(!!name_RAW_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_RAW")) & matches("_RAW$")))),
           !!name_DIR_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_DIR")) & matches("_DIR$")))))
      
    
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)
  
  DF_wide_RAW_DIR =
    DF_wide_RAW %>% 
    mutate(

      # Score Dimensions (use 3 digit item numbers)
      !!name_DIRd1 := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd1, "_DIR")), na.rm = TRUE), 
      !!name_DIRd2 := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd2, "_DIR")), na.rm = TRUE),
      !!name_DIRd3 := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd3, "_DIR")), na.rm = TRUE), 
      !!name_DIRd4 := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd4, "_DIR")), na.rm = TRUE),
      !!name_DIRd5 := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd5, "_DIR")), na.rm = TRUE),
      !!name_DIRd6 := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd6, "_DIR")), na.rm = TRUE),
      !!name_DIRd7 := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd7, "_DIR")), na.rm = TRUE),
      
      # Score Scale
      !!name_DIRt := rowMeans(select(., matches("_DIR$")), na.rm = TRUE)
      
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
