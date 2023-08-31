##' Prepare REI40
##' REI40 task. 
##' 
##'
##' @title prepare_REI40
##'
##' @param short_name_scale_str 
##' @param DF_clean
##' @param needs_v02_fix If the data comes from an old implementation of REI [fixed Jun 9, 2022], set needs_v02_fix = TRUE to fix the item numbers
##'
##' @return
##' @author gorkang
##' @export
prepare_REI40 <- function(DF_clean, short_name_scale_str, needs_v02_fix = FALSE) {

  # DEBUG
  # debug_function(prepare_REI40)
  
  
  # [ADAPT]: Items to ignore and reverse ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("00") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("01", "02", "03", "04", "05", "11", "13", "15", "17", "19", "21", "26", "28", "30", "34", "36", "37", "38") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  names_dimensions = c("RationalAbility", "RationalEngagement", "ExperientialAbility", "ExperiencialEngagement", "Rational", "Experiential") # If no dimensions, keep names_dimensions = c("")
  
  items_DIRd1 = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10")
  items_DIRd2 = c("11", "12", "13", "14", "15", "16", "17", "18", "19", "20")
  items_DIRd3 = c("21", "22", "23", "24", "25", "26", "27", "28", "29", "30")
  items_DIRd4 = c("31", "32", "33", "34", "35", "36", "37", "38", "39", "40")
  items_DIRd5 = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")
  items_DIRd6 = c("21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40")
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = names_dimensions,
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = TRUE, help_prepare = FALSE)
  

  # TEMPORAL FIX ------------------------------------------------------------
  if (needs_v02_fix == TRUE) {
    DF_dicc = readr::read_csv("R_tasks/prepare_REI40-dictionary.csv", show_col_types = FALSE)
    DF_long_RAW = DF_long_RAW %>% 
      dplyr::left_join(DF_dicc, by = "trialid") %>% 
      dplyr::mutate(trialid = trialid_OK) %>% 
     dplyr::select(-trialid_OK)
  }
  
  
  # Create long DIR ------------------------------------------------------------
  DF_long_DIR = 
    DF_long_RAW %>% 
   dplyr::select(id, trialid, RAW) %>%
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************

    dplyr::mutate(
      DIR = RAW
      ) %>% 
    
    # Invert items
    dplyr::mutate(
      DIR = 
       dplyr::case_when(
          DIR == 9999 ~ DIR,
          trialid %in% paste0(short_name_scale_str, "_", items_to_reverse) ~ (6 - DIR),
          TRUE ~ DIR
        )
    )
    
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
    

  # Create DF_wide_RAW_DIR -----------------------------------------------------
  DF_wide_RAW =
    DF_long_DIR %>% 
    tidyr::pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR),
      names_glue = "{trialid}_{.value}") %>% 
    
    # NAs for RAW and DIR items
    dplyr::mutate(!!names_list$name_RAW_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_RAW")) & matches("_RAW$")))),
           !!names_list$name_DIR_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_DIR")) & matches("_DIR$")))))
  

  
  # Reliability -------------------------------------------------------------
  
  REL1 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_DIRd1)
  REL2 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_DIRd2)
  REL3 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_DIRd3)
  REL4 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_DIRd4)
  REL5 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_DIRd5)
  REL6 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_DIRd6)
    
  items_RELd1 = REL1$item_selection_string
  items_RELd2 = REL2$item_selection_string
  items_RELd3 = REL3$item_selection_string
  items_RELd4 = REL4$item_selection_string
  items_RELd5 = REL5$item_selection_string
  items_RELd6 = REL6$item_selection_string
  
  
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)

  DF_wide_RAW_DIR =
    DF_wide_RAW %>% 
    dplyr::mutate(

      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      !!names_list$name_DIRd[1] := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd1, "_DIR")), na.rm = TRUE), 
      !!names_list$name_DIRd[2] := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd2, "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[3] := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd3, "_DIR")), na.rm = TRUE), 
      !!names_list$name_DIRd[4] := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd4, "_DIR")), na.rm = TRUE), 
      # Score Meta-dimensions
      !!names_list$name_DIRd[5] := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd5, "_DIR")), na.rm = TRUE), 
      !!names_list$name_DIRd[6] := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd6, "_DIR")), na.rm = TRUE), 

      
      # Reliability Dimensions (see standardized_names(help_names = TRUE) for instructions)
      !!names_list$name_RELd[1] := rowMeans(select(., paste0(short_name_scale_str, "_", items_RELd1, "_DIR")), na.rm = TRUE), 
      !!names_list$name_RELd[2] := rowMeans(select(., paste0(short_name_scale_str, "_", items_RELd2, "_DIR")), na.rm = TRUE),
      !!names_list$name_RELd[3] := rowMeans(select(., paste0(short_name_scale_str, "_", items_RELd3, "_DIR")), na.rm = TRUE), 
      !!names_list$name_RELd[4] := rowMeans(select(., paste0(short_name_scale_str, "_", items_RELd4, "_DIR")), na.rm = TRUE),
      # Reliability Meta-dimensions
      !!names_list$name_RELd[5] := rowMeans(select(., paste0(short_name_scale_str, "_", items_RELd5, "_DIR")), na.rm = TRUE), 
      !!names_list$name_RELd[6] := rowMeans(select(., paste0(short_name_scale_str, "_", items_RELd6, "_DIR")), na.rm = TRUE), 
      
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
