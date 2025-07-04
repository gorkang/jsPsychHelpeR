##' Prepare CRQ
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_CRQ -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_CRQ
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_CRQ <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_CRQ)
  
  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  ## NameDimension1, NameDimension2 should be the names of the dimensions
  ## Inside each c() create a vector of the item numbers for the dimension
  ## Add lines as needed. If there are no dimensions, keep as is

  items_dimensions = list(
    # Demograficos = c("01", "02", "03", "04", "05", "06", "07", "08"), # NO ES UNA DIMENSION
    JuventudEstimul = c("009", "010", "011", "012"),
    AdultezEstimul = c("009", "010", "011", "012"),
    RecienteEstimul = c("009", "010", "011", "012"),
    
    JuventudFormacion = c("013", "014", "015", "016"),
    AdultezFormacion = c("013", "014", "015", "016"),
    RecienteFormacion = c("013", "014", "015", "016"),
    
    JuventudHobbies = c("017", "018", "019", "020", "021", "022", "023", "024", "025", "026", "027", "028", "029"),
    AdultezHobbies = c("017", "018", "019", "020", "021", "022", "023", "024", "025", "026", "027", "028", "029"),
    RecienteHobbies = c("017", "018", "019", "020", "021", "022", "023", "024", "025", "026", "027", "028", "029"),
    
    JuventudVSocial = c("030", "031", "032"),
    AdultezVSocial = c("030", "031", "032"),
    RecienteVSocial = c("030", "031", "032")
  )
  
  
  # All items and their questions
  questions_per_item = c("", "_Q1", "_Q2")
  all_items = paste0(short_name_scale_str, "_", items_dimensions |> unlist())
  all_items_questions = paste0(rep(all_items, each = length(questions_per_item)), questions_per_item, sep = "")
  
  
  
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
                                help_prepare = FALSE) # Show n of items, responses,... [CHANGE to FALSE] 
  
  
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
          
          trialid %in% all_items_questions ~ as.numeric(RAW),
          
          is.na(RAW) ~ NA_real_, # OR NA_character_,
          trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_real_, # OR NA_character_,

          TRUE ~ 9999 # OR "9999"
        )
    ) |> 
    
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
  
  # Reliability -------------------------------------------------------------
  # REL1 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_dimensions[[1]])
  # items_RELd1 = REL1$item_selection_string
    
  
  # [USE STANDARD NAMES FOR Scales and dimensions: names_list$name_DIRd[1], names_list$name_DIRt,...] 
  # CHECK with: create_formulas(type = "dimensions_DIR", functions = "sum", names(items_dimensions))
  DF_wide_RAW_DIR =
    DF_wide_RAW |> 
    dplyr::mutate(

      # [CHECK] Using correct formula? rowMeans() / rowSums()
      
      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      !!names_list$name_DIRd[1] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[1]], questions_per_item[1], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[2] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[2]], questions_per_item[2], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[3] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[3]], questions_per_item[3], "_DIR"))), na.rm = TRUE),
      
      !!names_list$name_DIRd[4] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[4]], questions_per_item[1], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[5] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[5]], questions_per_item[2], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[6] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[6]], questions_per_item[3], "_DIR"))), na.rm = TRUE),
      
      !!names_list$name_DIRd[7] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[7]], questions_per_item[1], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[8] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[8]], questions_per_item[2], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[9] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[9]], questions_per_item[3], "_DIR"))), na.rm = TRUE),
      
      !!names_list$name_DIRd[10] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[10]], questions_per_item[1], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[11] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[11]], questions_per_item[2], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[12] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[12]], questions_per_item[3], "_DIR"))), na.rm = TRUE)
      
      # Reliability Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!names_list$name_RELd[1] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_RELd1, "_DIR"))), na.rm = TRUE), 

      # Score Scale
      # !!names_list$name_DIRt := rowSums(across(all_of(matches("_DIR$"))), na.rm = TRUE)
      
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
