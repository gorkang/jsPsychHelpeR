##' Prepare DEMOGR3
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_DEMOGRNEELY -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_DEMOGR3
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_DEMOGR3 <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_DEMOGR3)


  # NOTE --------------------------------------------------------------------
  # We do not use DF_clean as the data is sensitive
  

  # Read sensitive data -----------------------------------------------------
  input_files_sensitive = list.files(path = ".vault/data_vault", pattern = "*.csv", full.names = TRUE)
  if (length(input_files_sensitive) == 0) cli::cli_abort("DEMGR3 files should be in .vault/data_vault/")
  
  DF_raw_sensitive = read_data(input_files_sensitive, is_sensitive = FALSE, save_output = FALSE)
  DF_clean_sensitive = create_clean_data(DF_raw_sensitive)
  
  
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     # dimensions = c("NameDimension1", "NameDimension2"), # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean_sensitive, short_name_scale = short_name_scale_str, numeric_responses = FALSE, help_prepare = FALSE)
  
  
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
      DIR =dplyr::case_when(
      trialid == "DEMOGR3_01" ~ RAW,
      
      trialid == "DEMOGR3_02" & RAW == "Hombre" ~ "1",
      trialid == "DEMOGR3_02" & RAW == "Mujer" ~ "0",
      trialid == "DEMOGR3_02" & RAW == "Otro" ~ "2",
      
      trialid == "DEMOGR3_03" & RAW == "Sí" ~ "1",
      trialid == "DEMOGR3_03" & RAW == "No" ~ "0",
      
      trialid == "DEMOGR3_04" & RAW == "Sí" ~ "1",
      trialid == "DEMOGR3_04" & RAW == "No" ~ "0",
      
      trialid == "DEMOGR3_06" & RAW == "Sí" ~ "1",
      trialid == "DEMOGR3_06" & RAW == "No" ~ "0",
      
      trialid == "DEMOGR3_07" & RAW == "Sí" ~ "1",
      trialid == "DEMOGR3_07" & RAW == "No" ~ "0",
      
      trialid == "DEMOGR3_08" & RAW == "Sí" ~ "1",
      trialid == "DEMOGR3_08" & RAW == "No" ~ "0",
      
      trialid == "DEMOGR3_05" & RAW =="Educación básica incompleta o inferior." ~ "1",
      trialid == "DEMOGR3_05" & RAW =="Básica completa." ~ "2",
      trialid == "DEMOGR3_05" & RAW =="Media incompleta." ~ "3",
      trialid == "DEMOGR3_05" & RAW =="Media completa / Técnica incompleta." ~ "4",
      trialid == "DEMOGR3_05" & RAW =="Universitaria incompleta / Técnica completa" ~ "5",
      trialid == "DEMOGR3_05" & RAW =="Universitaria completa." ~ "6",
      trialid == "DEMOGR3_05" & RAW =="Postgrado (Master, Doctor o equivalente)." ~ "7",
      
      trialid == "DEMOGR3_09" ~ RAW,
      
      
      is.na(RAW) ~ NA_character_,
      trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_real_,
      TRUE ~ "9999"
      )
    ) |> 
    dplyr::mutate(DIR = as.numeric(DIR))
    


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
   
      
    

  # SENSITIVE ---------------------------------------------------------------

  # Item DEMOGR3_09 contains sensitive data
  DF_output = DF_wide_RAW_DIR |> dplyr::select(-dplyr::starts_with("DEMOGR3_09"))
  DF_sensitive = DF_wide_RAW_DIR |> dplyr::select(id, dplyr::starts_with("DEMOGR3_09"))
  

  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_output)
  
  # Save files --------------------------------------------------------------
  save_files(DF_output, short_name_scale = short_name_scale_str, is_scale = TRUE, is_sensitive = FALSE)
  save_files(DF_sensitive, short_name_scale = short_name_scale_str, is_scale = TRUE, is_sensitive = TRUE)
  
  
  # Output of function ---------------------------------------------------------
  return(DF_output) 
 
}
