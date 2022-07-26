##' Prepare DEMOGR12
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_DEMOGR12 -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_DEMOGR12
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_DEMOGR12 <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_DEMOGR12)

  
  # [ADAPT]: Items to ignore, reverse and dimensions ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("00") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("00") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  names_dimensions = c("edad", "genero") # If no dimensions, keep names_dimensions = c("")
  
  items_DIRd1 = c("01")
  items_DIRd2 = c("02")
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  
  # OUTSIDE FILES -----------------------------------------------------------
  DF_lookup = read_csv("R_tasks/prepare_DEMOGR-lookup.csv", 
                       col_types = 
                         cols(
                           DEMOGR12_comuna_DIRd = col_integer(), #idcomuna
                           comuna = col_character()
                         ))
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
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
    
    # Transformations
    mutate(
      DIR =
        case_when(
          trialid == "DEMOGR12_01" ~ RAW,
          
          trialid == "DEMOGR12_02" & RAW == "Masculino" ~ "0",
          trialid == "DEMOGR12_02" & RAW == "Femenino" ~ "1",
          trialid == "DEMOGR12_02" & RAW == "No binario" ~ "0",
          
          trialid == "DEMOGR12_06" ~ RAW,
          trialid == "DEMOGR12_07" ~ RAW,
          
          is.na(RAW) ~ NA_character_,
          grepl(items_to_ignore, trialid) ~ NA_character_,
          TRUE ~ "9999"
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
      mutate(!!names_list$name_RAW_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_RAW")))),
             !!names_list$name_DIR_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_DIR")))))
  
  
  DF_wide_RAW_DIR =
    DF_wide_RAW %>% 
    
    # Mach a simplified version of the strings (lower case, only standard characters and no extra space at the end)
    mutate(COMUNA = tolower(stringi::stri_trans_general(str = DEMOGR12_06_DIR, id = "Latin-ASCII")) %>% gsub(" $", "", .)) %>% 
    left_join(DF_lookup %>% mutate(COMUNA = tolower(stringi::stri_trans_general(str = comuna, id = "Latin-ASCII")) %>% gsub(" $", "", .)), 
              by = c("COMUNA")) %>% 
    rename(DEMOGR12_comuna_DIRd = DEMOGR_comuna_DIRd) %>% # DEMOGR_comuna_DIRd comes from excel file. DO NOT RENAME
    mutate(DEMOGR12_comuna_DIRd = ifelse(is.na(DEMOGR12_comuna_DIRd), paste0("Not found: ", DEMOGR12_06_DIR), DEMOGR12_comuna_DIRd)) %>%
    select(-COMUNA, -comuna) %>% 
    
    
    # [ADAPT]: Scales and dimensions calculations --------------------------------
    # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)
    
    mutate(
      !!names_list$name_DIRd[1] := get(paste0(short_name_scale_str, "_", items_DIRd1, "_DIR")), 
      !!names_list$name_DIRd[2] := get(paste0(short_name_scale_str, "_", items_DIRd2, "_DIR"))
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
