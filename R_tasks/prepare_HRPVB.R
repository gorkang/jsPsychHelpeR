##' Prepare HRPVB
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_HRPVB -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_HRPVB
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_HRPVB <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_HRPVB)

  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "HRPVB description"
  
  items_to_ignore = c("00") # Ignore these items
  items_to_reverse = c("00") # Reverse these items

  items_dimensions = list(
    Cesarean = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11") %>% paste0("_01"),
    Vaginal = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11") %>% paste0("_02"),
    CesareanMother = c("05", "06", "07", "08", "09", "10", "11") %>% paste0("_01"),
    CesareanBaby = c("01", "02", "03", "04") %>% paste0("_01"),
    VaginalMother = c("05", "06", "07", "08", "09", "10", "11") %>% paste0("_02"),
    VaginalBaby = c("01", "02", "03", "04") %>% paste0("_02")
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
                                numeric_responses = TRUE, 
                                is_experiment = FALSE, 
                                help_prepare = FALSE) # Show n of items, responses,... [CHANGE to FALSE] 
  
  
  # Create long DIR ------------------------------------------------------------
  
  DF_long_DIR = 
    DF_long_RAW %>% 
   dplyr::select(id, trialid, RAW) %>%
    
    
  # [ADAPT 2/3]: RAW to DIR for individual items -------------------------------
  # ****************************************************************************
  
    # Transformations
    dplyr::mutate(
      DIR = RAW
    ) %>% 
    
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
  

  # Create DF_wide_RAW_DIR -----------------------------------------------------
  DF_wide_RAW =
    DF_long_DIR %>% 
    tidyr::pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR),
      names_glue = "{trialid}_{.value}") %>% 
    
    # NAs for RAW and DIR items
    dplyr::mutate(!!names_list$name_RAW_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_RAW")))),
           !!names_list$name_DIR_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_DIR")))))
  
  
  
  # [ADAPT 3/3]: Scales and dimensions calculations ----------------------------
  # ****************************************************************************
  
  # [USE STANDARD NAMES FOR Scales and dimensions: names_list$name_DIRd[1], names_list$name_DIRt,...] 
  # CHECK with: create_formulas(type = "dimensions_DIR", functions = "sum", names(items_dimensions))
  DF_wide_RAW_DIR = 
    DF_wide_RAW %>% 
    dplyr::mutate(
      !!names_list$name_DIRd[1] := rowMeans(select(., paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR")), na.rm = TRUE), 
      !!names_list$name_DIRd[2] := rowMeans(select(., paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[3] := rowMeans(select(., paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR")), na.rm = TRUE), 
      !!names_list$name_DIRd[4] := rowMeans(select(., paste0(short_name_scale_str, "_", items_dimensions[[4]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[5] := rowMeans(select(., paste0(short_name_scale_str, "_", items_dimensions[[5]], "_DIR")), na.rm = TRUE), 
      !!names_list$name_DIRd[6] := rowMeans(select(., paste0(short_name_scale_str, "_", items_dimensions[[6]], "_DIR")), na.rm = TRUE)
      
    )
    
  # [END ADAPT 3/3]: ***********************************************************
  # ****************************************************************************
  

  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
