##' Prepare SRSav
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_SRSav -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_SRSav
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_SRSav <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_SRSav)
  
  
  # [ADAPT]: Items to ignore, reverse and dimensions ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("00") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse1 = c("01", "02", "04") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  items_to_reverse2 = c("08", "14", "15")
  
  
  
  names_dimensions = c("ORA", "NORA", "IR") # If no dimensions, keep names_dimensions = c("")
  
  items_DIRd1 = c("01", "02")
  items_DIRd2 = c("03", "04", "05")
  items_DIRd3 = c("06", "07", "08", "09", "10", "11", "12", "13", "14", "15")
  
  
  
  
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  # Standardized names ------------------------------------------------------
  standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = names_dimensions, # Use names of dimensions, "" or comment out line
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
          grepl("01|02", trialid) & RAW == "Varias veces a la semana" ~ 1,
          grepl("01|02", trialid) & RAW == "Aproximadamente una vez a la semana" ~ 2,
          grepl("01|02", trialid) & RAW == "Varias veces al mes" ~ 3,
          grepl("01|02", trialid) & RAW == "Varias veces al año" ~ 4,
          grepl("01|02", trialid) & RAW == "Raramente" ~ 5,
          grepl("01|02", trialid) & RAW == "Nunca" ~ 6,
          
          grepl("03", trialid) & RAW == "Nunca" ~ 1,
          grepl("03", trialid) & RAW == "Solo ocasionalmente" ~ 2,
          grepl("03", trialid) & RAW == "Varias veces a la semana" ~ 3,
          grepl("03", trialid) & RAW == "Una vez al día" ~ 4,
          grepl("03", trialid) & RAW == "Dos veces al día" ~ 5,
          grepl("03", trialid) & RAW == "Tres o más veces al día" ~ 6,
          
          grepl("04", trialid) & RAW == "Varias veces al día" ~ 1,
          grepl("04", trialid) & RAW == "Diario" ~ 2,
          grepl("04", trialid) & RAW == "Varias veces a la semana" ~ 3,
          grepl("04", trialid) & RAW == "Varias veces al mes" ~ 4,
          grepl("04", trialid) & RAW == "Solo ocasionalmente" ~ 5,
          grepl("04", trialid) & RAW == "Para nada" ~ 6,
          
          grepl("05", trialid) & RAW == "Nunca" ~ 1,
          grepl("05", trialid) & RAW == "Solo ocasionalmente" ~ 2,
          grepl("05", trialid) & RAW == "Varias veces al mes" ~ 3,
          grepl("05", trialid) & RAW == "Varias veces a la semana" ~ 4,
          grepl("05", trialid) & RAW == "Diario" ~ 5,
          grepl("05", trialid) & RAW == "Varias veces al día" ~ 6,
          
          grepl("06|07|08|09|10|11|12", trialid) & RAW == "Definitivamente no es cierto para mí" ~ 1,
          grepl("06|07|08|09|10|11|12", trialid) & RAW == "Tiende a no ser cierto" ~ 2,
          grepl("06|07|08|09|10|11|12", trialid) & RAW == "Inseguro" ~ 3,
          grepl("06|07|08|09|10|11|12", trialid) & RAW == "Tiende a ser verdad" ~ 4,
          grepl("06|07|08|09|10|11|12", trialid) & RAW == "Definitivamente cierto de mí" ~ 5,
          
          grepl("13|14|15", trialid) & RAW == "Definitivamente en desacuerdo" ~ 1,
          grepl("13|14|15", trialid) & RAW == "Tiende a estar en desacuerdo" ~ 2,
          grepl("13|14|15", trialid) & RAW == "Inseguro" ~ 3,
          grepl("13|14|15", trialid) & RAW == "Tiende a estar de acuerdo" ~ 4,
          grepl("13|14|15", trialid) & RAW == "Definitivamente de acuerdo" ~ 5,
          
          TRUE ~ 9999
        )
    ) %>% 
    
    # Invert items
    mutate(
      DIR = 
        case_when(
          DIR == 9999 ~ DIR,
          trialid %in% paste0(short_name_scale_str, "_", items_to_reverse1) ~ (7 - DIR),
          trialid %in% paste0(short_name_scale_str, "_", items_to_reverse2) ~ (6 - DIR),
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
      !!name_DIRd1 := rowSums(select(., paste0(short_name_scale_str, "_", items_DIRd1, "_DIR")), na.rm = TRUE), 
      !!name_DIRd2 := rowSums(select(., paste0(short_name_scale_str, "_", items_DIRd2, "_DIR")), na.rm = TRUE),
      !!name_DIRd3 := rowSums(select(., paste0(short_name_scale_str, "_", items_DIRd3, "_DIR")), na.rm = TRUE), 
      
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
