##' Prepare DEMOGRsensitive
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_DEMOGRNEELY -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_DEMOGRsensitive
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_DEMOGRsensitive <- function(short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_DEMOGRsensitive)

  

  # Read sensitive data -----------------------------------------------------
  input_files_sensitive = list.files(path = ".vault/data", pattern = "*.csv", full.names = TRUE)
  DF_raw_sensitive = read_data(input_files_sensitive, anonymize = FALSE, save_output = FALSE)
  DF_clean_sensitive = create_clean_data(DF_raw_sensitive)
  
  
  
  # Standardized names ------------------------------------------------------
  standardized_names(short_name_scale = short_name_scale_str, 
                     # dimensions = c("NameDimension1", "NameDimension2"), # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean_sensitive, short_name_scale = short_name_scale_str, numeric_responses = FALSE)
  
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
      DIR = case_when(
      trialid == "DEMOGR3_01" ~ RAW,
      
      trialid == "DEMOGR3_02" & RAW == "Hombre" ~ "1",
      trialid == "DEMOGR3_02" & RAW == "Mujer" ~ "0",
      trialid == "DEMOGR3_02" & RAW == "Otro" ~ "2",
      
      trialid == "DEMOGR3_03" & RAW == "Sí" ~ "1",
      trialid == "DEMOGR3_03" & RAW == "No" ~ "0",
      
      trialid == "DEMOGR3_04" & RAW == "Sí" ~ "1",
      trialid == "DEMOGR3_04" & RAW == "No" ~ "0",
      
      trialid == "DEMOGR3_06" & RAW == "Sí" ~ "1",
      trialid == "DEMOGR3_06" & RAW == "No" ~ "0",
      
      trialid == "DEMOGR3_07" & RAW == "Sí" ~ "1",
      trialid == "DEMOGR3_07" & RAW == "No" ~ "0",
      
      trialid == "DEMOGR3_08" & RAW == "Sí" ~ "1",
      trialid == "DEMOGR3_08" & RAW == "No" ~ "0",
      
      trialid == "DEMOGR3_05" & RAW =="Educación básica incompleta o inferior." ~ "1",
      trialid == "DEMOGR3_05" & RAW =="Básica completa." ~ "2",
      trialid == "DEMOGR3_05" & RAW =="Media incompleta." ~ "3",
      trialid == "DEMOGR3_05" & RAW =="Media completa / Técnica incompleta." ~ "4",
      trialid == "DEMOGR3_05" & RAW =="Universitaria incompleta / Técnica completa" ~ "5",
      trialid == "DEMOGR3_05" & RAW =="Universitaria completa." ~ "6",
      trialid == "DEMOGR3_05" & RAW =="Postgrado (Master, Doctor o equivalente)." ~ "7",
      
      trialid == "DEMOGR3_09" ~ RAW,
      
      
      is.na(RAW) ~ NA_character_,
      grepl(items_to_ignore, trialid) ~ NA_character_,
      TRUE ~ "9999"
      )
    ) %>% 
    mutate(DIR = as.numeric(DIR))
    


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
      
    

  # SENSITIVE ---------------------------------------------------------------

  # Item DEMOGR3_09 contains sensitive data
  DF_output = DF_wide_RAW_DIR %>% select(-starts_with("DEMOGR3_09"))
  DF_sensitive = DF_wide_RAW_DIR %>% select(id, starts_with("DEMOGR3_09"))
  

  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_output)
  
  # Save files --------------------------------------------------------------
  save_files(DF_output, short_name_scale = short_name_scale_str, is_scale = TRUE, is_sensitive = FALSE)
  save_files(DF_sensitive, short_name_scale = short_name_scale_str, is_scale = TRUE, is_sensitive = TRUE)
  
  
  # Output of function ---------------------------------------------------------
  return(DF_output) 
 
}
