##' Prepare DEMOGR12
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_DEMOGR12 -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_DEMOGR12
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_DEMOGR12 <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_DEMOGR12)

  
  # [ADAPT]: Items to ignore, reverse and dimensions ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("00") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("00") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  items_dimensions = list(
    edad = c("01"), 
    genero = c("02")
  )
  
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  
  # OUTSIDE FILES -----------------------------------------------------------
  DF_lookup = data.table::fread("R_tasks/prepare_DEMOGR-lookup.csv")
  
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
    
    # Transformations
    dplyr::mutate(
      DIR =
       dplyr::case_when(
          trialid == "DEMOGR12_01" ~ RAW,
          
          trialid == "DEMOGR12_02" & RAW == "Masculino" ~ "0",
          trialid == "DEMOGR12_02" & RAW == "Femenino" ~ "1",
          trialid == "DEMOGR12_02" & RAW == "No binario" ~ "0",
          
          trialid == "DEMOGR12_06" ~ RAW,
          trialid == "DEMOGR12_07" ~ RAW,
          
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
    
    # Mach a simplified version of the strings (lower case, only standard characters and no extra space at the end)
    dplyr::mutate(COMUNA = tolower(stringi::stri_trans_general(str = DEMOGR12_06_DIR, id = "Latin-ASCII")) |> gsub(" $", "", .)) |> 
    dplyr::left_join(DF_lookup |> dplyr::mutate(COMUNA = tolower(stringi::stri_trans_general(str = comuna, id = "Latin-ASCII")) |> gsub(" $", "", .)), 
              by = c("COMUNA")) |> 
    dplyr::rename(DEMOGR12_comuna_DIRd = DEMOGR_comuna_DIRd) |> # DEMOGR_comuna_DIRd comes from excel file. DO NOT RENAME
    dplyr::mutate(DEMOGR12_comuna_DIRd = ifelse(is.na(DEMOGR12_comuna_DIRd), paste0("Not found: ", DEMOGR12_06_DIR), DEMOGR12_comuna_DIRd)) |>
   dplyr::select(-COMUNA, -comuna) |> 
    
    
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
