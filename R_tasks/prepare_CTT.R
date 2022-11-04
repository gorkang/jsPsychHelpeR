##' Prepare CTT
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_CTT -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_CTT
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_CTT <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_CTT)
  # get_dimensions_googledoc(short_name_text = "CTT")
  
  
  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  ## NameDimension1, NameDimension2 should be the names of the dimensions
  ## Inside each c() create a vector of the item numbers for the dimension
  ## Add lines as needed. If there are no dimensions, keep as is
  items_dimensions = list(
    AciertoError = c("01"),
    DecisionCorrecta = c("02")
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
                                is_experiment = TRUE, 
                                help_prepare = FALSE) # Show n of items, responses,... [CHANGE to FALSE] 
  
  
  # Create long DIR ------------------------------------------------------------
  DF_long_DIR = 
    DF_long_RAW %>% 
    select(id, trialid, condition_between, RAW) %>%
    
    
    
  # [ADAPT 2/3]: RAW to DIR for individual items -------------------------------
  # ****************************************************************************
  
    # Transformations
    mutate(
      DIR =
        case_when(
          trialid %in% c("CTT_01") & condition_between == 1 & RAW == "Iría a la entrevista" ~ "1",
          trialid %in% c("CTT_01") & condition_between == 1 & RAW == "se devolvería a revisar la puerta del carro (auto)" ~ "1",
          
          trialid %in% c("CTT_01") & condition_between == 2 & RAW == "Iría a la entrevista" ~ "0",
          trialid %in% c("CTT_01") & condition_between == 2 & RAW == "se devolvería a revisar la puerta del carro (auto)" ~ "1",
          
          trialid %in% c("CTT_01") & condition_between == 3 & RAW == "Iría a la entrevista" ~ "1",
          trialid %in% c("CTT_01") & condition_between == 3 & RAW == "se devolvería a revisar la puerta del carro (auto)" ~ "0",
          
          trialid %in% c("CTT_02") ~ RAW,
          
          is.na(RAW) ~ NA_character_, # OR NA_character_,
          grepl(items_to_ignore, trialid) ~ NA_character_, # OR NA_character_,
          TRUE ~ "9999" # OR "9999"
        )
    ) %>% 
    
    mutate(DIR = as.numeric(DIR))
    
  # [END ADAPT 2/3]: ***********************************************************
  # ****************************************************************************
    

  # Create DF_wide_RAW_DIR -----------------------------------------------------
  DF_wide_RAW =
    DF_long_DIR %>% 
    pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR),
      names_glue = "{trialid}_{.value}") %>% 
    
    # NAs for RAW and DIR items
    mutate(!!names_list$name_RAW_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_RAW")) & matches("_RAW$")))),
           !!names_list$name_DIR_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_DIR")) & matches("_DIR$")))))


  
  # [ADAPT 3/3]: Scales and dimensions calculations ----------------------------
  # ****************************************************************************
  
  # Reliability -------------------------------------------------------------
  # REL1 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_DIRd1)
  # items_RELd1 = REL1$item_selection_string
    
  
  # [USE STANDARD NAMES FOR Scales and dimensions: names_list$name_DIRd[1], names_list$name_DIRt,...] 
  # CHECK with: create_formulas(type = "dimensions_DIR", functions = "sum", names(items_dimensions))
  DF_wide_RAW_DIR =
    DF_wide_RAW %>% 
    mutate(

      # [CHECK] Using correct formula? rowMeans() / rowSums()
      
      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      !!names_list$name_DIRd[1] := rowMeans(select(., paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR")), na.rm = TRUE), 
      !!names_list$name_DIRd[2] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR")), na.rm = TRUE),
      
      # Reliability Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!names_list$name_RELd[1] := rowMeans(select(., paste0(short_name_scale_str, "_", items_RELd1, "_DIR")), na.rm = TRUE), 

      # Score Scale
      # !!names_list$name_DIRt := rowSums(select(., matches("_DIR$")), na.rm = TRUE)
      
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
