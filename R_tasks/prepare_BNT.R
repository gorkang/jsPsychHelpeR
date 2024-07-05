##' Prepare BNT 
##' Should work with BNT and BNTen
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_BNT -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_BNT
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_BNT <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # debug_function(prepare_BNT)

  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = TRUE, help_prepare = FALSE) |> 
    
    # In BNT we have exit conditions. Add "BNT_03","BNT_04" to make we have all the columns we need to complete the correction
      # short_name_scale_str can be BNT or BNTen
    dplyr::bind_rows(tibble(trialid = c(paste0(short_name_scale_str, "_03"),paste0(short_name_scale_str, "_04")))) |> 
    tidyr::complete(trialid, nesting(id, experiment)) |> 
    tidyr::drop_na(id) 
  
  
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
          # trialid == "BNTen_01" & as.numeric(RAW) >= 400 & as.numeric(RAW) <= 600 ~ 1,
          trialid %in% c("BNT_02", "BNTen_02") & RAW != 30 ~ 1,
          trialid %in% c("BNT_02", "BNTen_02") & RAW == 30 ~ 2,
          trialid %in% c("BNT_03", "BNTen_03") & RAW == 20 ~ 4,
          trialid %in% c("BNT_04", "BNTen_04") & RAW == 50 ~ 4,
          trialid %in% c("BNT_04", "BNTen_04") & RAW != 50 ~ 3,
          is.na(RAW) ~ NA_real_,
          trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_real_, # OR NA_character_
          TRUE ~ 0
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
    
    # # ENMIENDA MANUAL para corregir error en piloto. Si han acertado el BNT_03_DIR, no deberian ver el BNT_04_DIR ------------------
    # dplyr::mutate(BNT_04_DIR = 
    #         dplyr::case_when(
    #            BNT_03_DIR == 4 & BNT_04_DIR == 4 ~ 0,
    #            TRUE ~ BNT_04_DIR
    #          )) |> 
    
    
    # NAs for RAW and DIR items
  dplyr::mutate(!!names_list$name_RAW_NA := rowSums(is.na(across((-matches(paste0(short_name_scale_str, "_", items_to_ignore, "_RAW")) & matches("_RAW$"))))),
                !!names_list$name_DIR_NA := rowSums(is.na(across((-matches(paste0(short_name_scale_str, "_", items_to_ignore, "_DIR")) & matches("_DIR$"))))))
      
    
  
  # [ADAPT 3/3]: Scales and dimensions calculations ----------------------------
  # ****************************************************************************
  
  DF_wide_RAW_DIR =
    DF_wide_RAW  |>
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
