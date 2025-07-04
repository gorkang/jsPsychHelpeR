##' Prepare RTS
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_RTS -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_RTS
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_RTS <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_RTS)

  # [ADAPT]: Items to ignore, reverse and dimensions ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("001", "006", "007", "010", "011", "014", "015", "017", "023", "024", "027", "028") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("000")
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("000")
  
  items_dimensions = list(
    NameDimension1 = c("0000")
  )
  
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = names(items_dimensions),
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
          grepl("002|003|004|005|008|009|012|013|016|018|019|020|021|022|025|026|029", trialid) & RAW == "Verdadero" ~ 1,
          grepl("002|003|004|005|008|009|012|013|016|018|019|020|021|022|025|026|029", trialid) & RAW == "Falso" ~ 0,
          is.na(RAW) ~ NA_real_,
          trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_real_,
          TRUE ~ 9999
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

      # # Score Dimensions (use 3 digit item numbers)
      # !!names_list$name_DIRd[1] := rowSums(select(., matches("02|04|05") & matches("_DIR$")), na.rm = TRUE), 
      # !!names_list$name_DIRd[2] := rowSums(select(., matches("01|03|08") & matches("_DIR$")), na.rm = TRUE), 
      
      # Score Scale
      !!names_list$name_DIRt := rowSums(across(all_of(matches("_DIR$"))), na.rm = TRUE),
      
      # Rasch scores
      !!names_list$name_STDt :=  
         dplyr::case_when(
            RTS_DIRt == 0 ~ 13.7,
            RTS_DIRt == 1 ~ 15.9,
            RTS_DIRt == 2 ~ 18.3,
            RTS_DIRt == 3 ~ 19.9,
            RTS_DIRt == 4 ~ 21.1,
            RTS_DIRt == 5 ~ 22.1,
            RTS_DIRt == 6 ~ 23.1,
            RTS_DIRt == 7 ~ 24.0,
            RTS_DIRt == 8 ~ 24.9,
            RTS_DIRt == 9 ~ 25.7,
            RTS_DIRt == 10 ~ 26.6,
            RTS_DIRt == 11 ~ 27.5,
            RTS_DIRt == 12 ~ 28.5,
            RTS_DIRt == 13 ~ 29.6,
            RTS_DIRt == 14 ~ 30.9,
            RTS_DIRt == 15 ~ 32.5,
            RTS_DIRt == 16 ~ 35.0,
            RTS_DIRt == 17 ~ 37.3,
            TRUE ~ 9999
          )
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
