##' Prepare FORM4
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_FORM4 -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_FORM4
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_FORM4 <- function(DF_clean_form, short_name_scale_str) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_FORM4)

  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     # dimensions = c("comuna"), # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean_form, short_name_scale = short_name_scale_str, numeric_responses = FALSE, help_prepare = FALSE)
  
  
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
    # 1 FORM4_01 Indica tu edad                                    
    # 2 FORM4_02 Indica tu género                                  
    # 3 FORM4_03 Indica tu correo electrónico personal             
    # 4 FORM4_04 ¿Alguna vez has tenido un parto vaginal o cesarea?
    # 5 FORM4_05 ¿Quieres tener hijos en el futuro?                
    
    
    
    # Transformations
    dplyr::mutate(
      DIR =
       dplyr::case_when(
          trialid == "FORM4_01" ~ RAW,
          
          trialid == "FORM4_02" & RAW == "Masculino" ~ "0",
          trialid == "FORM4_02" & RAW == "Femenino" ~ "1",
          trialid == "FORM4_02" & RAW == "No binario" ~ "0",
          
          trialid == "FORM4_03" ~ RAW,
          
          trialid == "FORM4_04" ~ RAW,
          
          trialid == "FORM4_05" ~ RAW,

          is.na(RAW) ~ NA_character_,
          trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_real_,
          TRUE ~ "9999"
        )
    )
    

    # [END ADAPT]: ***************************************************************
    # ****************************************************************************
    
    
    # Create DF_wide_RAW_DIR -----------------------------------------------------
    DF_wide_RAW_DIR =
      DF_long_DIR |> 
      tidyr::pivot_wider(
        names_from = trialid, 
        values_from = c(RAW, DIR),
        names_glue = "{trialid}_{.value}") |> 
      
      # NAs for RAW and DIR items
          dplyr::mutate(!!names_list$name_RAW_NA := rowSums(is.na(across((-matches(paste0(short_name_scale_str, "_", items_to_ignore, "_RAW")) & matches("_RAW$"))))),
                  !!names_list$name_DIR_NA := rowSums(is.na(across((-matches(paste0(short_name_scale_str, "_", items_to_ignore, "_DIR")) & matches("_DIR$"))))))


  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE, output_formats = output_formats)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
