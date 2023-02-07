##' Prepare RMET
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_RMET -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_RMET
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_RMET <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  debug_function(prepare_RMET)

  # [ADAPT]: Items to ignore and reverse ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  names_dimensions = c("") # If no dimensions, keep names_dimensions = c("")
  
  items_DIRd1 = c("")
  items_DIRd2 = c("")
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = names_dimensions, # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE, is_experiment = FALSE, help_prepare = FALSE)
  
  
  # Create long DIR ------------------------------------------------------------

  DF_long_DIR = 
    DF_long_RAW %>% 
   dplyr::select(id, trialid, RAW) %>%
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
    # Transformations
    dplyr::mutate(
      DIR =
       dplyr::case_when(
          
          # Example trial.
          # TODO: SHOULD BE 000
          trialid == "RMET_001" & RAW == toupper("En panico") ~ 0, # ALWAYS 0 TO AVOID SUMMING IT
          
          # Experiment trials
          trialid == "RMET_002" & RAW == toupper("Juguetón") ~ 1,
          trialid == "RMET_003" & RAW == toupper("Molesto") ~ 1,
          trialid == "RMET_004" & RAW == toupper("Deseo") ~ 1,
          trialid == "RMET_005" & RAW == toupper("Insistente") ~ 1,
          trialid == "RMET_006" & RAW == toupper("Preocupado") ~ 1,
          trialid == "RMET_007" & RAW == toupper("Fantasiosa") ~ 1,
          trialid == "RMET_008" & RAW == toupper("Intranquilo") ~ 1,
          trialid == "RMET_009" & RAW == toupper("Abatido") ~ 1,
          trialid == "RMET_010" & RAW == toupper("Angustiada") ~ 1,
          trialid == "RMET_011" & RAW == toupper("Prudente") ~ 1,
          trialid == "RMET_012" & RAW == toupper("Arrepentido") ~ 1,
          trialid == "RMET_013" & RAW == toupper("Escéptico") ~ 1,
          trialid == "RMET_014" & RAW == toupper("Expectante") ~ 1,
          trialid == "RMET_015" & RAW == toupper("Acusante") ~ 1,
          trialid == "RMET_016" & RAW == toupper("Abstraída") ~ 1,
          trialid == "RMET_017" & RAW == toupper("Considerado") ~ 1,
          trialid == "RMET_018" & RAW == toupper("Insegura") ~ 1,
          trialid == "RMET_019" & RAW == toupper("Decidida") ~ 1,
          trialid == "RMET_020" & RAW == toupper("Vacilante") ~ 1,
          trialid == "RMET_021" & RAW == toupper("Amistoso") ~ 1,
          trialid == "RMET_022" & RAW == toupper("Fantasiosa") ~ 1,
          trialid == "RMET_023" & RAW == toupper("Angustiada") ~ 1,
          trialid == "RMET_024" & RAW == toupper("Desafiante") ~ 1,
          trialid == "RMET_025" & RAW == toupper("Abstraído") ~ 1,
          trialid == "RMET_026" & RAW == toupper("Interesada") ~ 1,
          trialid == "RMET_027" & RAW == toupper("Hostil") ~ 1,
          trialid == "RMET_028" & RAW == toupper("Prudente") ~ 1,
          trialid == "RMET_029" & RAW == toupper("Interesada") ~ 1,
          trialid == "RMET_030" & RAW == toupper("Reflexiva") ~ 1,
          trialid == "RMET_031" & RAW == toupper("Seductora") ~ 1,
          trialid == "RMET_032" & RAW == toupper("Segura") ~ 1,
          trialid == "RMET_033" & RAW == toupper("Serio") ~ 1,
          trialid == "RMET_034" & RAW == toupper("Fantasioso") ~ 1,
          trialid == "RMET_035" & RAW == toupper("Recelosa") ~ 1,
          trialid == "RMET_036" & RAW == toupper("Nerviosa") ~ 1,
          trialid == "RMET_037" & RAW == toupper("Desconfiado") ~ 1,
          is.na(RAW) ~ NA_real_,
          grepl(items_to_ignore, trialid) ~ NA_real_,
          TRUE ~ 0
        )
    ) %>% 
    
    # Invert items
    dplyr::mutate(
      DIR = 
       dplyr::case_when(
          DIR == 9999 ~ DIR, # To keep the missing values unchanged
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
  
  # REL1 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_DIRd1)
  # items_RELd1 = REL1$item_selection_string
    
  
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)

  DF_wide_RAW_DIR =
    DF_wide_RAW %>% 
    dplyr::mutate(

      # Make sure to use the correct formula: rowMeans() / rowSums()
      
      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!names_list$name_DIRd[1] := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd1, "_DIR")), na.rm = TRUE), 
      # !!names_list$name_DIRd[2] := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd2, "_DIR")), na.rm = TRUE),
      
      # Reliability Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!names_list$name_RELd[1] := rowMeans(select(., paste0(short_name_scale_str, "_", items_RELd1, "_DIR")), na.rm = TRUE), 

      # Score Scale
      !!names_list$name_DIRt := rowSums(select(., matches("_DIR$")), na.rm = TRUE)
      
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
