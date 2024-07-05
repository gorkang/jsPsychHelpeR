##' Prepare CRTMCQ4
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_CRTMCQ4 -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_CRTMCQ4
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_CRTMCQ4 <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # debug_function(prepare_CRTMCQ4)
  
  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  items_dimensions = list(
    Reflectiveness = c("001", "002", "003", "004", "005", "006", "007"),
    Intuitiveness = c("001", "002", "003", "004", "005", "006", "007"),
    PreviousKnowledge = c("008")
    )
  
  # [END ADAPT 1/3]: ***********************************************************
  # ****************************************************************************

  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = names(items_dimensions),
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, 
                                short_name_scale = short_name_scale_str, 
                                numeric_responses = FALSE, # [TRUE or FALSE]
                                is_experiment = FALSE, 
                                keep_time = FALSE, # Keep time stamp for each response
                                help_prepare = FALSE) # Show n of items, responses,... [CHANGE to TRUE to debug] 
  

  # Create long DIR ------------------------------------------------------------
  
  DF_long_DIR = 
    DF_long_RAW |> 
   dplyr::select(id, trialid, RAW) |>
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
  # Reflectiveness score (0 – 7): 1 point for each correct answer: 5 pence, 5 minutes, 47 days, 4 days, 29 students, 20 pounds, has lost money, respectively.
  # Intuitiveness score (0 – 7): 1 point for each intuitive incorrect answer: 10 pence, 100 minutes, 24 days, 9 days, 30 students, 10 pounds, is ahead of where he began, respectively.
  
    # Transformations
    dplyr::mutate(
      DIR =
       dplyr::case_when(
          trialid == "CRTMCQ4_001" & RAW %in% c("50 pesos", "5 céntimos") ~ "reflective",
          trialid == "CRTMCQ4_002" & RAW == "5 minutos" ~ "reflective",
          trialid == "CRTMCQ4_003" & RAW == "47 días" ~ "reflective",
          trialid == "CRTMCQ4_004" & RAW == "4 días" ~ "reflective",
          trialid == "CRTMCQ4_005" & RAW == "29 estudiantes" ~ "reflective",
          trialid == "CRTMCQ4_006" & RAW  %in% c("$20.000", "€20") ~ "reflective",
          trialid == "CRTMCQ4_007" & RAW == "ha perdido dinero." ~ "reflective",
          
          trialid == "CRTMCQ4_001" & RAW  %in% c("100 pesos", "10 céntimos") ~ "intuitive",
          trialid == "CRTMCQ4_002" & RAW == "100 minutos" ~ "intuitive",
          trialid == "CRTMCQ4_003" & RAW == "24 días" ~ "intuitive",
          trialid == "CRTMCQ4_004" & RAW == "9 días" ~ "intuitive",
          trialid == "CRTMCQ4_005" & RAW == "30 estudiantes" ~ "intuitive",
          trialid == "CRTMCQ4_006" & RAW  %in% c("$10.000", "€10") ~ "intuitive",
          trialid == "CRTMCQ4_007" & RAW == "ha ganado dinero." ~ "intuitive",
          
          trialid == "CRTMCQ4_008" ~ gsub(".*\\((.*)\\)$", "\\1", RAW),
          
          is.na(RAW) ~ NA_character_,
          trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_character_,
          TRUE ~ ""
        )
    ) 
  
  # [END ADAPT 2/3]: ***********************************************************
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
    DF_wide_RAW |> 
    
    dplyr::mutate(
      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      !!names_list$name_DIRd[1] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR"))) == "reflective", na.rm = TRUE), 
      !!names_list$name_DIRd[2] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR"))) == "intuitive", na.rm = TRUE),
      !!names_list$name_DIRd[3] := get(paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR"))
      
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
