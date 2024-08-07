##' Prepare Report
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_Report -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_Report
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_Report <- function(DF_clean, short_name_scale_str, output_formats) {
  
  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_Report)
  
  # [ADAPT]: Items to ignore and reverse ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  items_dimensions = list(
    informe = c("001"), 
    alias = c("001_1"), 
    SoloContacto = c("001_2"), 
    EmailFuturo = c("002")
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
  
  
  
  # Transformations
  dplyr::mutate(
    DIR =
     dplyr::case_when(
        
        trialid %in% c("Report_001_1", "Report_001_2") ~ RAW,
        trialid == "Report_001" & RAW == "No deseo recibir el informe" ~ "0",
        trialid == "Report_001" & RAW == "Si, deseo recibir el informe" ~ "1",
        
        trialid == "Report_002" & RAW == "No, no acepto que mis datos sean reutilizados" ~ "0",
        trialid == "Report_002" & RAW == "Si, acepto ser contactado en el futuro" ~ "1",
        
        is.na(RAW) ~ NA_character_,
        trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_character_,
        TRUE ~ "9999"
      )
  )  
    
    # # Invert items
    # dplyr::mutate(
    #   DIR = 
    #    dplyr::case_when(
    #       DIR == 9999 ~ DIR, # To keep the missing values unchanged
    #       trialid %in% paste0(short_name_scale_str, "_", items_to_reverse) ~ (6 - DIR),
    #       TRUE ~ DIR
    #     )
    # )
  
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
      
      # Make sure to use the correct formula: rowMeans() / rowSums()
      
      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      !!names_list$name_DIRd[1] := get(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR")),
      # !!names_list$name_DIRd[2] := ifelse(suppressWarnings(ncol(DF_wide_RAW |> dplyr::select(Report_001_1_DIR))) > 0, get(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR")), NA_character_),
      # !!names_list$name_DIRd[3] := ifelse(suppressWarnings(ncol(DF_wide_RAW |> dplyr::select(Report_001_1_DIR))) > 0, get(paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR")), NA_character_),
      !!names_list$name_DIRd[2] := ifelse(suppressWarnings(!is.null(DF_wide_RAW$Report_001_1_DIR)) > 0, get(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR")), NA_character_),
      !!names_list$name_DIRd[3] := ifelse(suppressWarnings(!is.null(DF_wide_RAW$Report_001_1_DIR)) > 0, get(paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR")), NA_character_),
      !!names_list$name_DIRd[4] := ifelse(get(paste0(short_name_scale_str, "_", items_dimensions[[4]], "_DIR")) == 1, get(!!names_list$name_DIRd[3]), NA_character_)
      
      
      # Reliability Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!names_list$name_RELd[1] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_RELd1, "_DIR"))), na.rm = TRUE), 
      
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
