##' Prepare SPSI
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_SPSI -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_SPSI
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_SPSI <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_SPSI)
  

  
  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  ## NameDimension1, NameDimension2 should be the names of the dimensions
  ## Inside each c() create a vector of the item numbers for the dimension
  ## Add lines as needed. If there are no dimensions, keep as is
  items_dimensions = list(
    PositiveProblemOrientation = c("007", "009", "019", "028", "038"),
    NegativeProblemOrientation = c("001", "002", "006", "012", "013", "017", "032", "036", "041", "050"),
    RationalProblemSolving = c("011", "029", "033", "044", "049", "005", "020", "039", "047", "048", "018", "024", "040", "043", "046", "025", "026", "027", "035", "037"),
    ProblemDefinitionFormulation = c("011", "029", "033", "044", "049"),
    GenerationAlternativeSolutions = c("005", "020", "039", "047", "048"),
    DecisionMaking = c("018", "024", "040", "043", "046"),
    SolutionImplementationVerification = c("025", "026", "027", "035", "037"),
    ImpulsivityCarelessnessStyle = c("003", "004", "008", "015", "021", "022", "034", "045", "051", "052"),
    AvoidanceStyle = c("010", "014", "016", "023", "030", "031", "042")
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
    
    
    
  # [ADAPT 2/3]: RAW to DIR for individual items -------------------------------
  # ****************************************************************************
  
    # Transformations
    dplyr::mutate(
      DIR =
       dplyr::case_when(
           RAW == "Nada en absoluto" ~ 0,
           RAW == "Escasamente" ~ 1,
           RAW == "Moderadamente" ~ 2,
           RAW == "Bastante" ~ 3,
           RAW == "Completamente" ~ 4,
          is.na(RAW) ~ NA_real_, # OR NA_character_,
          trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_real_, # OR NA_character_,
          TRUE ~ 9999 # OR "9999"
        )
    ) %>% 
    
    # Invert items [CAN BE DELETED IF NOT USED or DIR is non-numeric]
    dplyr::mutate(
      DIR = 
       dplyr::case_when(
          DIR == 9999 ~ DIR, # To keep the missing values unchanged
          trialid %in% paste0(short_name_scale_str, "_", items_to_reverse) ~ (6 - DIR), # REVIEW and replace 6 by MAX + 1
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
    dplyr::mutate(!!names_list$name_RAW_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_RAW")) & matches("_RAW$")))),
           !!names_list$name_DIR_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_DIR")) & matches("_DIR$")))))


  
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

      # [CHECK] Using correct formula? rowMeans() / rowSums()
      
      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      !!names_list$name_DIRd[1] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[2] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[3] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[4] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[4]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[5] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[5]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[6] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[6]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[7] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[7]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[8] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[8]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[9] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[9]], "_DIR")), na.rm = TRUE),
      # Reliability Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!names_list$name_RELd[1] := rowMeans(select(., paste0(short_name_scale_str, "_", items_RELd1, "_DIR")), na.rm = TRUE), 

      # Score Scale
      !!names_list$name_DIRt := rowSums(select(., matches("_DIR$")), na.rm = TRUE)
      
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
