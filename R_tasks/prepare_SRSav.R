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
prepare_SRSav <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_SRSav)
  
  
  # [ADAPT]: Items to ignore, reverse and dimensions ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("000")
  items_to_reverse1 = c("001", "002", "004") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("000")
  items_to_reverse2 = c("008", "014", "015")
  
  
  
  items_dimensions = list(
    ORA = c("001", "002"), 
    NORA = c("003", "004", "005"), 
    IR = c("006", "007", "008", "009", "010", "011", "012", "013", "014", "015")
  )
  
  
  
  
  
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = names(items_dimensions), # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE, help_prepare = FALSE)
  
  
  # Create long DIR ------------------------------------------------------------
  DF_long_DIR = 
    DF_long_RAW |> 
    dplyr::select(id, trialid, RAW) |> 
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
    dplyr::mutate(
      DIR =
       dplyr::case_when(
          trialid %in% c("SRSav_001", "SRSav_002") & RAW == "Varias veces a la semana" ~ 1,
          trialid %in% c("SRSav_001", "SRSav_002") & RAW == "Aproximadamente una vez a la semana" ~ 2,
          trialid %in% c("SRSav_001", "SRSav_002") & RAW == "Varias veces al mes" ~ 3,
          trialid %in% c("SRSav_001", "SRSav_002") & RAW == "Varias veces al año" ~ 4,
          trialid %in% c("SRSav_001", "SRSav_002") & RAW == "Raramente" ~ 5,
          trialid %in% c("SRSav_001", "SRSav_002") & RAW == "Nunca" ~ 6,
          
          trialid %in% c("SRSav_003") & RAW == "Nunca" ~ 1,
          trialid %in% c("SRSav_003") & RAW == "Solo ocasionalmente" ~ 2,
          trialid %in% c("SRSav_003") & RAW == "Varias veces a la semana" ~ 3,
          trialid %in% c("SRSav_003") & RAW == "Una vez al día" ~ 4,
          trialid %in% c("SRSav_003") & RAW == "Dos veces al día" ~ 5,
          trialid %in% c("SRSav_003") & RAW == "Tres o más veces al día" ~ 6,
          
          trialid %in% c("SRSav_004") & RAW == "Varias veces al día" ~ 1,
          trialid %in% c("SRSav_004") & RAW == "Diario" ~ 2,
          trialid %in% c("SRSav_004") & RAW == "Varias veces a la semana" ~ 3,
          trialid %in% c("SRSav_004") & RAW == "Varias veces al mes" ~ 4,
          trialid %in% c("SRSav_004") & RAW == "Solo ocasionalmente" ~ 5,
          trialid %in% c("SRSav_004") & RAW == "Para nada" ~ 6,
          
          trialid %in% c("SRSav_005") & RAW == "Nunca" ~ 1,
          trialid %in% c("SRSav_005") & RAW == "Solo ocasionalmente" ~ 2,
          trialid %in% c("SRSav_005") & RAW == "Varias veces al mes" ~ 3,
          trialid %in% c("SRSav_005") & RAW == "Varias veces a la semana" ~ 4,
          trialid %in% c("SRSav_005") & RAW == "Diario" ~ 5,
          trialid %in% c("SRSav_005") & RAW == "Varias veces al día" ~ 6,
          
          # 06|07|08|09|10|11|12
          RAW == "Definitivamente no es cierto para mí" ~ 1,
          RAW == "Tiende a no ser cierto" ~ 2,
          RAW == "Inseguro" ~ 3,
          RAW == "Tiende a ser verdad" ~ 4,
          RAW == "Definitivamente cierto de mí" ~ 5,
          
          # 13|14|15
          RAW == "Definitivamente en desacuerdo" ~ 1,
          RAW == "Tiende a estar en desacuerdo" ~ 2,
          RAW == "Inseguro" ~ 3,
          RAW == "Tiende a estar de acuerdo" ~ 4,
          RAW == "Definitivamente de acuerdo" ~ 5,
          
          TRUE ~ 9999
        )
    ) |> 
    
    # Invert items
    dplyr::mutate(
      DIR = 
       dplyr::case_when(
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
    DF_long_DIR |> 
    tidyr::pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR),
      names_glue = "{trialid}_{.value}") |> 
    
    # NAs for RAW and DIR items
    dplyr::mutate(!!names_list$name_RAW_NA := rowSums(is.na(across((-matches(paste0(short_name_scale_str, "_", items_to_ignore, "_RAW")) & matches("_RAW$"))))),
                  !!names_list$name_DIR_NA := rowSums(is.na(across((-matches(paste0(short_name_scale_str, "_", items_to_ignore, "_DIR")) & matches("_DIR$"))))))
        
    
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)

  DF_wide_RAW_DIR =
    DF_wide_RAW |> 
    dplyr::mutate(

      # Score Dimensions (use 3 digit item numbers)
      !!names_list$name_DIRd[1] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR"))), na.rm = TRUE), 
      !!names_list$name_DIRd[2] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[3] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR"))), na.rm = TRUE), 
      
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
