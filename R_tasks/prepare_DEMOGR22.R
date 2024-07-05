##' Prepare DEMOGRfondecyt2022E1
##'
##'
##' @title prepare_DEMOGRfondecyt2022E1
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_DEMOGRfondecyt2022E1 <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_DEMOGRfondecyt2022E1)

  
  # [ADAPT]: Items to ignore, reverse and dimensions ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  items_dimensions = list(
    age = c("01"), 
    gender = c("02")
  )
  
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                                  dimensions = names(items_dimensions),
                                  help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE, help_prepare = FALSE)
  
  # 1 DEMOGRfondecyt2022E1_01 "{\"\"Q0\"\":\"\"Indica tu edad\"\"}"                                    
  # 2 DEMOGRfondecyt2022E1_02 "{\"\"Q0\"\":\"\"Indica tu género\"\"}"                                  
  # 3 DEMOGRfondecyt2022E1_03 "{\"\"Q0\"\":\"\"¿Tienes algún tipo de daltonismo?\"\"}"                 
  # 4 DEMOGRfondecyt2022E1_04 "{\"\"Q0\"\":\"\"Indica tú ocupación\"\"}"                               
  # 5 DEMOGRfondecyt2022E1_05 "{\"\"Q0\"\":\"\"Indica tú carrera\"\"}"                                 
  #### 6 DEMOGRfondecyt2022E1_06 "{\"\"Q0\"\":\"\"Indica tú especialidad medica\"\"}"                                 
  # 7 DEMOGRfondecyt2022E1_07 "{\"\"Q0\"\":\"\"Indica los años de estudio o ejercicio profesional\"\"}"
  
  # Create long DIR ------------------------------------------------------------
  
  DF_long_DIR = 
    DF_long_RAW |> 
   dplyr::select(id, trialid, RAW) |>
    

    # Transformations
    dplyr::mutate(
      DIR =
       dplyr::case_when(
          trialid == "DEMOGRfondecyt2022E1_01" ~ RAW,
          
          trialid == "DEMOGRfondecyt2022E1_02" & RAW == "Masculino" ~ "Male",
          trialid == "DEMOGRfondecyt2022E1_02" & RAW == "Femenino" ~ "Female",
          trialid == "DEMOGRfondecyt2022E1_02" & RAW == "No binario" ~ "Non-binary",
          
          trialid == "DEMOGRfondecyt2022E1_03" & RAW == "Si" ~ "1",
          trialid == "DEMOGRfondecyt2022E1_03" & RAW == "No" ~ "0",
          
          trialid == "DEMOGRfondecyt2022E1_04" ~ RAW,
          trialid == "DEMOGRfondecyt2022E1_05" ~ RAW,
          trialid == "DEMOGRfondecyt2022E1_06" ~ RAW,
          trialid == "DEMOGRfondecyt2022E1_07" ~ RAW,
        
          
          is.na(RAW) ~ NA_character_,
          trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_real_,
          TRUE ~ "9999"
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
  
  
  DF_wide_RAW_DIR =
    DF_wide_RAW |> 
    
    # [ADAPT]: Scales and dimensions calculations --------------------------------
    # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)
    
    dplyr::mutate(
      !!names_list$name_DIRd[1] := get(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR")), 
      !!names_list$name_DIRd[2] := get(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR"))
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
