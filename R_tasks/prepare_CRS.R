##' Prepare CRS
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_TEMPLATE -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_CRS
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_CRS <- function(DF_clean, short_name_scale_str) {
  
  # DEBUG
  # debug_function(prepare_CRS)
  
  
  # [ADAPT]: Items to ignore and reverse ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("00") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("00") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  names_dimensions = c("Intelectual", "Ideologica", "PracticaPublica", "PracticaPrivada", "ExperienciaReligiosa") # If no dimensions, keep names_dimensions = c("")
  
  items_DIRd1 = c("01", "08", "15")
  items_DIRd2 = c("02", "09", "16")
  items_DIRd3 = c("03", "10", "17")
  items_DIRd4 = c("04", "05", "11", "12", "18", "19")
  items_DIRd5 = c("06", "07", "13", "14", "20")
  
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
          RAW == "Muy a menudo" ~ 5,
          RAW == "A menudo" ~ 4,
          RAW == "Ocasionalmente" ~ 3,
          RAW == "Rara vez" ~ 2,
          RAW == "Nunca" ~ 1,
          
          RAW == "Demasiado" ~ 5,
          RAW == "Mucho"~ 4,
          RAW == "Algo" ~ 3,
          RAW == "Un poco" ~ 2,
          RAW == "Nada" ~ 1,

          # is.na(RAW) ~ NA_real_,
          # trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_real_,
          TRUE ~ 9999
        )
    ) %>% 
    
    # Invert items
    mutate(
      DIR = 
        case_when(
          DIR == 9999 ~ DIR, # To keep the missing values unchanged
          trialid %in% paste0(short_name_scale_str, "_", items_to_reverse) ~ (6 - DIR),
          TRUE ~ DIR
        )
    )
    
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
    

  # Create DF_wide_RAW -----------------------------------------------------
  DF_wide_RAW =
    DF_long_DIR %>% 
    pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR),
      names_glue = "{trialid}_{.value}") %>% 
    
    # NAs for RAW and DIR items
    mutate(!!name_RAW_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_RAW")) & matches("_RAW$")))),
           !!name_DIR_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_DIR")) & matches("_DIR$")))))
      
  

  # Reliability -------------------------------------------------------------

  REL1 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_DIRd1)
  REL2 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_DIRd2)
  REL3 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_DIRd3)
  REL4 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_DIRd4)
  REL5 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_DIRd5)
  
  items_RELd1 = REL1$item_selection_string
  items_RELd2 = REL2$item_selection_string
  items_RELd3 = REL3$item_selection_string
  items_RELd4 = REL4$item_selection_string
  items_RELd5 = REL5$item_selection_string
  
  
    
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)
  # Cinco dimensiones: (1) Intelectual (itemes 1, 6, 11), (2) Ideológica (itemes 2, 7, 12), (3) Pártica pública (itemes 3, 8, 13),  (4) Práctica privada (itemes 4, 9, 14), (5) Experiencia religiosa (itemes 5, 10, 15).
  
  DF_wide_RAW_DIR =
    DF_wide_RAW %>% 
    mutate(

      # Score Dimensions (use 3 digit item numbers)
      !!name_DIRd1 := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd1, "_DIR")), na.rm = TRUE), 
      !!name_DIRd2 := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd2, "_DIR")), na.rm = TRUE),
      !!name_DIRd3 := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd3, "_DIR")), na.rm = TRUE), 
      !!name_DIRd4 := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd4, "_DIR")), na.rm = TRUE), 
      !!name_DIRd5 := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd5, "_DIR")), na.rm = TRUE), 
      
      # Reliability Dimensions (use 3 digit item numbers)
      !!name_RELd1 := rowMeans(select(., paste0(short_name_scale_str, "_", items_RELd1, "_DIR")), na.rm = TRUE), 
      !!name_RELd2 := rowMeans(select(., paste0(short_name_scale_str, "_", items_RELd2, "_DIR")), na.rm = TRUE),
      !!name_RELd3 := rowMeans(select(., paste0(short_name_scale_str, "_", items_RELd3, "_DIR")), na.rm = TRUE), 
      !!name_RELd4 := rowMeans(select(., paste0(short_name_scale_str, "_", items_RELd4, "_DIR")), na.rm = TRUE), 
      !!name_RELd5 := rowMeans(select(., paste0(short_name_scale_str, "_", items_RELd5, "_DIR")), na.rm = TRUE), 
      
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
