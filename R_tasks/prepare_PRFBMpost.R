##' Prepare PRFBMpost
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_PRFBMpost -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_PRFBMpost
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_PRFBMpost <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_PRFBMpost)

  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = c("Preferencia", "FuerzaPreferencia", "MotivosMadreBeneficio", "MotivosBebeBeneficio", "MotivosMadreDaño", "MotivosBebeDaño"), # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE, help_prepare = FALSE)
  
  
  # Create long DIR ------------------------------------------------------------
  
  # [ADAPT]: Items to ignore and reverse ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("00|00") # Ignore the following items: If nothing to ignore, keep "00|00"
  items_to_reverse = c("00|00") # Reverse the following items: If nothing to ignore, keep "00|00"
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  DF_long_DIR = 
    DF_long_RAW |> 
   dplyr::select(id, trialid, RAW) |>
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
    # Transformations
    dplyr::mutate(
      DIR =
       dplyr::case_when(
          trialid == "PRFBMpost_001" & RAW == "Parto por cesárea (extracción del bebé por medio de una cirugía con anestesia. Se realiza una incisión abdominal y una incisión para abrir el útero)" ~ 0,
          trialid == "PRFBMpost_001" & RAW == "Parto vaginal (extracción del bebé por el canal vaginal en forma espontánea, con o sin intervenciones como anestesia)" ~ 1,
          
          RAW == "Muy en desacuerdo" ~ 0,
          RAW == "En desacuerdo" ~ 1,
          RAW == "Parcialmente en desacuerdo" ~ 2,
          RAW == "Parcialmente de acuerdo" ~ 3,
          RAW == "De acuerdo" ~ 4,
          RAW == "Muy de acuerdo" ~ 5,
          
          trialid != "PRFBMpost_01" ~ as.numeric(RAW),
          
          is.na(RAW) ~ NA_real_,
          trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_real_, # OR NA_character_
          TRUE ~ 9999
        )
    ) |> 
    
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
    DF_long_DIR |> 
    tidyr::pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR),
      names_glue = "{trialid}_{.value}") |> 
    
    # NAs for RAW and DIR items
    dplyr::mutate(!!names_list$name_RAW_NA := rowSums(is.na(across((-matches(paste0(short_name_scale_str, "_", items_to_ignore, "_RAW")) & matches("_RAW$"))))),
                  !!names_list$name_DIR_NA := rowSums(is.na(across((-matches(paste0(short_name_scale_str, "_", items_to_ignore, "_DIR")) & matches("_DIR$"))))))
  

  
  # [ADAPT 3/3]: Scales and dimensions calculations ----------------------------
  # ****************************************************************************
  
  DF_wide_RAW_DIR =
    DF_wide_RAW  |>  
    dplyr::mutate(

      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      !!names_list$name_DIRd[1] := rowMeans(select(., matches("001") & matches("_DIR$")), na.rm = TRUE), 
      !!names_list$name_DIRd[2] := rowMeans(select(., matches("002|003") & matches("_DIR$")), na.rm = TRUE), 
      !!names_list$name_DIRd[3] := rowMeans(select(., matches("004_beneficio|006_beneficio") & matches("_DIR$")), na.rm = TRUE), 
      !!names_list$name_DIRd[4] := rowMeans(select(., matches("005_beneficio|007_beneficio") & matches("_DIR$")), na.rm = TRUE),
      !!names_list$name_DIRd[5] := rowMeans(select(., matches("004_daño|006_daño") & matches("_DIR$")), na.rm = TRUE), 
      !!names_list$name_DIRd[6] := rowMeans(select(., matches("005_daño|007_daño") & matches("_DIR$")), na.rm = TRUE)
      
      # Score Scale
      # !!names_list$name_DIRt := rowSums(across(all_of(matches("_DIR$"))), na.rm = TRUE)
      
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
