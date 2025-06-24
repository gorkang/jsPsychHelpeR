##' Prepare DEBRIEF39
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_DEBRIEF39 -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_DEBRIEF39
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_DEBRIEF39 <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # debug_function(prepare_DEBRIEF39)

  
  
  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  numeric_items = c("DEBRIEF39_002")
  
  ## NameDimension1, NameDimension2 should be the names of the dimensions
  ## Inside each c() create a vector of the item numbers for the dimension
  ## Add lines as needed. If there are no dimensions, keep as is
  items_dimensions = list(
    Check = c("001"),
    Effort = c("002"),
    Comments = c("003")
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
    DF_long_RAW %>% 
    # If using keep_time = TRUE above, use this and add timestamp to the select() call
    # dplyr::mutate(timestamp = as.POSIXlt(datetime, format = "%Y-%m-%dT%H%M%S")) |> 
    dplyr::select(id, trialid, RAW) %>%
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
    # Transformations
    mutate(
      DIR =
        case_when(
          trialid == "DEBRIEF39_001" & RAW == "11" ~ "1",
          trialid == "DEBRIEF39_001" & RAW != "11" ~ "0",
          
          trialid == "DEBRIEF39_002" & RAW == "casi nada" ~ "1",
          trialid == "DEBRIEF39_002" & RAW == "muy poco" ~ "2",
          trialid == "DEBRIEF39_002" & RAW == "algo" ~ "3",
          trialid == "DEBRIEF39_002" & RAW == "bastante" ~ "4",
          is.na(RAW) ~ NA_character_,
          grepl(items_to_ignore, trialid) ~ NA_character_,
          TRUE ~ RAW
        )
    ) 
    
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
    

  # Create DF_wide_RAW_DIR -----------------------------------------------------
  DF_wide_RAW =
    DF_long_DIR %>% 
    tidyr::pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR),
      names_glue = "{trialid}_{.value}") %>% 
    
    # NAs for RAW and DIR items
    dplyr::mutate(!!names_list$name_RAW_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_RAW")) & matches("_RAW$")))),
           !!names_list$name_DIR_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_DIR")) & matches("_DIR$"))))) |> 
    
    # Transform numeric items
    mutate(!!paste0(numeric_items, "_DIR") := as.numeric(!!sym(paste0(numeric_items, "_DIR"))))


    
  # [ADAPT 3/3]: Scales and dimensions calculations ----------------------------
  # ****************************************************************************
  
  # Reliability -------------------------------------------------------------
  # REL1 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_dimensions[[1]])
  # items_RELd1 = REL1$item_selection_string
  
  
  # [USE STANDARD NAMES FOR Scales and dimensions: names_list$name_DIRd[1], names_list$name_DIRt,...] 
  # CHECK with: create_formulas(type = "dimensions_DIR", functions = "sum", names(items_dimensions))
  DF_wide_RAW_DIR =
    DF_wide_RAW %>% 
    dplyr::mutate(
      
      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      !!names_list$name_DIRd[1] := get(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR")),
      !!names_list$name_DIRd[2] := get(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR")),
      !!names_list$name_DIRd[3] := get(paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR"))
      
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
