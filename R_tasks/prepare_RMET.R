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
prepare_RMET <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # debug_function(prepare_RMET)

  # [ADAPT]: Items to ignore and reverse ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("0000") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("0000") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  items_dimensions = list(
    Dimension = "000"
  )
  
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = names(items_dimensions), # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE, is_experiment = FALSE, help_prepare = FALSE)
  
  
  # Create long DIR ------------------------------------------------------------

  DF_long_DIR = 
    DF_long_RAW |> 
   dplyr::select(id, trialid, RAW) |>
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
    # Transformations
    dplyr::mutate(
      DIR =
       dplyr::case_when(
          
          # Example trial.
          # TODO: SHOULD BE 000
          trialid == "RMET_000" & RAW == toupper("En pánico") ~ 0, # ALWAYS 0 TO AVOID SUMMING IT
          
          # Experiment trials
          trialid == "RMET_001" & RAW == "JUGUETÓN" ~ 1,
          trialid == "RMET_002" & RAW == "MOLESTO" ~ 1,
          trialid == "RMET_003" & RAW == "DESEO" ~ 1,
          trialid == "RMET_004" & RAW == "INSISTENTE" ~ 1,
          trialid == "RMET_005" & RAW == "PREOCUPADO" ~ 1,
          trialid == "RMET_006" & RAW == "FANTASIOSA" ~ 1,
          trialid == "RMET_007" & RAW == "INTRANQUILO" ~ 1,
          trialid == "RMET_008" & RAW == "ABATIDO" ~ 1,
          trialid == "RMET_009" & RAW == "ANGUSTIADA" ~ 1,
          trialid == "RMET_010" & RAW == "PRUDENTE" ~ 1,
          trialid == "RMET_011" & RAW == "ARREPENTIDO" ~ 1,
          trialid == "RMET_012" & RAW == "ESCÉPTICO" ~ 1,
          trialid == "RMET_013" & RAW == "EXPECTANTE" ~ 1,
          trialid == "RMET_014" & RAW == "ACUSANTE" ~ 1,
          trialid == "RMET_015" & RAW == "ABSTRAÍDA" ~ 1,
          trialid == "RMET_016" & RAW == "CONSIDERADO" ~ 1,
          trialid == "RMET_017" & RAW == "INSEGURA" ~ 1,
          trialid == "RMET_018" & RAW == "DECIDIDA" ~ 1,
          trialid == "RMET_019" & RAW == "VACILANTE" ~ 1,
          trialid == "RMET_020" & RAW == "AMISTOSO" ~ 1,
          trialid == "RMET_021" & RAW == "FANTASIOSA" ~ 1,
          trialid == "RMET_022" & RAW == "ANGUSTIADA" ~ 1,
          trialid == "RMET_023" & RAW == "DESAFIANTE" ~ 1,
          trialid == "RMET_024" & RAW == "ABSTRAÍDO" ~ 1,
          trialid == "RMET_025" & RAW == "INTERESADA" ~ 1,
          trialid == "RMET_026" & RAW == "HOSTIL" ~ 1,
          trialid == "RMET_027" & RAW == "PRUDENTE" ~ 1,
          trialid == "RMET_028" & RAW == "INTERESADA" ~ 1,
          trialid == "RMET_029" & RAW == "REFLEXIVA" ~ 1,
          trialid == "RMET_030" & RAW == "SEDUCTORA" ~ 1,
          trialid == "RMET_031" & RAW == "SEGURA" ~ 1,
          trialid == "RMET_032" & RAW == "SERIO" ~ 1,
          trialid == "RMET_033" & RAW == "FANTASIOSO" ~ 1,
          trialid == "RMET_034" & RAW == "RECELOSA" ~ 1,
          trialid == "RMET_035" & RAW == "NERVIOSA" ~ 1,
          trialid == "RMET_036" & RAW == "DESCONFIADO" ~ 1,
          is.na(RAW) ~ NA_real_,
          trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_real_, # OR NA_character_
          TRUE ~ 0
        )
    ) |> 
    
    # Invert items
    dplyr::mutate(
      DIR = 
       dplyr::case_when(
          DIR == 9999 ~ DIR, # To keep the missing values unchanged
          # trialid %in% paste0(short_name_scale_str, "_", items_to_reverse) ~ (6 - DIR),
          TRUE ~ DIR
        )
    )
    
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
    

  # Create DF_wide_RAW_DIR -----------------------------------------------------
  DF_wide_RAW =
    DF_long_DIR |> 
    tidyr::pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR),
      names_glue = "{trialid}_{.value}") |> 
    
    # NAs for RAW and DIR items
    dplyr::mutate(!!names_list$name_RAW_NA := rowSums(is.na(across((-matches(paste0(short_name_scale_str, "_", items_to_ignore, "_RAW")) & matches("_RAW$"))))),
                  !!names_list$name_DIR_NA := rowSums(is.na(across((-matches(paste0(short_name_scale_str, "_", items_to_ignore, "_DIR")) & matches("_DIR$"))))))
    
  
  # Reliability -------------------------------------------------------------
  
  # REL1 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_dimensions[[1]])
  # items_RELd1 = REL1$item_selection_string
    
  
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)

  DF_wide_RAW_DIR =
    DF_wide_RAW |> 
    dplyr::mutate(
      # Score Scale
      !!names_list$name_DIRt := rowSums(across(all_of(matches("_DIR$"))), na.rm = TRUE)
      
    )
    
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************


  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE, output_formats = output_formats)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
