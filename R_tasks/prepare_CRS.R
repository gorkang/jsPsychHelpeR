##' Prepare CRS
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_TEMPLATE -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_CRS
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_CRS <- function(DF_clean, short_name_scale_str, output_formats) {
  
  # DEBUG
  # targets::tar_load_globals()
  # debug_function(prepare_CRS)
  
  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  ## NameDimension1, NameDimension2 should be the names of the dimensions
  ## Inside each c() create a vector of the item numbers for the dimension
  ## Add lines as needed. If there are no dimensions, keep as is
  items_dimensions = list(
    Intelectual = c("001", "008", "015"),
    Ideologica = c("002", "009", "016"),
    PracticaPublica = c("003", "010", "017"),
    PracticaPrivada = c("004", "005", "011", "012", "018", "019"),
    ExperienciaReligiosa = c("006", "007", "013", "014", "020")
  )
  
  # [END ADAPT 1/3]: ***********************************************************
  # ****************************************************************************

  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                                  dimensions = names(items_dimensions),
                                  help_names = FALSE) # [KEEP as FALSE]
  
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
    
    
    # [ADAPT 2/3]: RAW to DIR for individual items -------------------------------
    # ****************************************************************************
  
    # Transformations
    dplyr::mutate(
      DIR =
       dplyr::case_when(
          RAW == "Muy a menudo" ~ 5,
          RAW == "A menudo" ~ 4,
          RAW == "Ocasionalmente" ~ 3,
          RAW == "Rara vez" ~ 2,
          RAW == "Nunca" ~ 1,
          
          RAW == "Demasiado" ~ 5,
          RAW == "Mucho"~ 4,
          RAW == "Algo" ~ 3,
          RAW == "Un poco" ~ 2,
          RAW == "Nada" ~ 1,

          # is.na(RAW) ~ NA_real_,
          # trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_real_,
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
    
  # [END ADAPT 2/3]: ***********************************************************
  # ****************************************************************************
  

  # Create DF_wide_RAW -----------------------------------------------------

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

  REL1 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_dimensions[[1]])
  REL2 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_dimensions[[2]])
  REL3 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_dimensions[[3]])
  REL4 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_dimensions[[4]])
  REL5 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_dimensions[[5]])
  
  items_RELd1 = REL1$item_selection_string
  items_RELd2 = REL2$item_selection_string
  items_RELd3 = REL3$item_selection_string
  items_RELd4 = REL4$item_selection_string
  items_RELd5 = REL5$item_selection_string
  
  
    
  # [ADAPT 3/3]: Scales and dimensions calculations ----------------------------
  # ****************************************************************************

  DF_wide_RAW_DIR =
    DF_wide_RAW |> 
    dplyr::mutate(

      # Score Dimensions (use 3 digit item numbers)
      !!names_list$name_DIRd[1] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR"))), na.rm = TRUE),  
      !!names_list$name_DIRd[2] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR"))), na.rm = TRUE),  
      !!names_list$name_DIRd[3] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR"))), na.rm = TRUE),  
      !!names_list$name_DIRd[4] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[4]], "_DIR"))), na.rm = TRUE),  
      !!names_list$name_DIRd[5] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[5]], "_DIR"))), na.rm = TRUE),  
      
      # Reliability Dimensions (use 3 digit item numbers)
      !!names_list$name_RELd[1] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_RELd1, "_DIR"))), na.rm = TRUE), 
      !!names_list$name_RELd[2] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_RELd2, "_DIR"))), na.rm = TRUE),
      !!names_list$name_RELd[3] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_RELd3, "_DIR"))), na.rm = TRUE), 
      !!names_list$name_RELd[4] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_RELd4, "_DIR"))), na.rm = TRUE), 
      !!names_list$name_RELd[5] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_RELd5, "_DIR"))), na.rm = TRUE), 
      
      # Score Scale
      !!names_list$name_DIRt := rowSums(across(all_of(matches("_DIR$"))), na.rm = TRUE)
      
    )
    
  # [END ADAPT 3/3]: ***********************************************************
  # ****************************************************************************
  

  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE, output_formats = output_formats)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
