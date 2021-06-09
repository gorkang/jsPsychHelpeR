##' Prepare FORM
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_FORM -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_FORM
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_FORM <- function(DF_clean_form, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_FORM)

  
  # Standardized names ------------------------------------------------------
  standardized_names(short_name_scale = short_name_scale_str, 
                     # dimensions = c("comuna"), # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean_form, short_name_scale = short_name_scale_str, numeric_responses = FALSE)
  
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
    # 1 FORM_01 Indica tu edad                                    
    # 2 FORM_02 Indica tu género                                  
    # 3 FORM_03 Indica tu correo electrónico personal             
    # 4 FORM_04 ¿Alguna vez has tenido un parto vaginal o cesarea?
    # 5 FORM_05 ¿Quieres tener hijos en el futuro?                
    
    
    
    # Transformations
    mutate(
      DIR =
        case_when(
          trialid == "FORM_01" ~ RAW,
          
          trialid == "FORM_02" & RAW == "Masculino" ~ "0",
          trialid == "FORM_02" & RAW == "Femenino" ~ "1",
          trialid == "FORM_02" & RAW == "No binario" ~ "0",
          
          trialid == "FORM_03" ~ RAW,
          
          trialid == "FORM_04" ~ RAW,
          
          trialid == "FORM_05" ~ RAW,

          is.na(RAW) ~ NA_character_,
          grepl(items_to_ignore, trialid) ~ NA_character_,
          TRUE ~ "9999"
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
             !!name_DIR_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_DIR")))))


  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
