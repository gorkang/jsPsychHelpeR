##' Prepare CTS2
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_CTS2 -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_CTS2
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_CTS2 <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_CTS2)
  
  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  items_dimensions = list(
    Negotiation = c("001", "002", "007", "020", "030", "039"),
    PsychAggression = c("003", "013", "015", "018", "025", "033", "034", "035"),
    PhysicalAssault = c("004", "005", "009", "011", "014", "017", "019", "022", "023", "027", "031", "037"),
    SexualCoercion = c("008", "010", "024", "026", "029", "032", "038"),
    Injury = c("006", "012", "016", "021", "028", "036"),
    
    PsychAggressionPrevalence = c("003", "013", "015", "018", "025", "033", "034", "035"),
    PhysicalAssaultPrevalence = c("004", "005", "009", "011", "014", "017", "019", "022", "023", "027", "031", "037"),
    SexualCoercionPrevalence = c("008", "010", "024", "026", "029", "032", "038"),
    InjuryPrevalence = c("006", "012", "016", "021", "028", "036")
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
    # If using keep_time = TRUE above, use this and add timestamp to the select() call
    # dplyr::mutate(timestamp = as.POSIXlt(datetime, format = "%Y-%m-%dT%H%M%S")) |> 
    dplyr::select(id, trialid, RAW) |>
    
    
    
  # [ADAPT 2/3]: RAW to DIR for individual items -------------------------------
  # ****************************************************************************
  
    # Transformations
    dplyr::mutate(
      DIR =
        dplyr::case_when(
          RAW == "Esto nunca ha ocurrido" ~ 0,
          RAW == "No en esta relación, pero si ha ocurrido en otra" ~ 0,
          RAW == "Una vez durante la relación" ~ 1,
          RAW == "Dos veces durante la relación" ~ 2,
          RAW == "3 a 5 veces durante la relación" ~ 4,
          RAW == "6 a 10 veces durante la relación" ~ 8,
          RAW == "11 a 20 veces durante la relación" ~ 15,
          RAW == "Más de 20 veces durante la relación" ~ 25,
          is.na(RAW) ~ NA_real_, # OR NA_character_,
          trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_real_, # OR NA_character_,
          TRUE ~ 9999 # OR "9999"
        ),
      # Needed for prevalence, as the option No durante la relación is 0 for the scale, but 1 for prevalence
      DIR_prev =
        dplyr::case_when(
          RAW == "Esto nunca ha ocurrido" ~ 0,
          RAW == "No en esta relación, pero si ha ocurrido en otra" ~ 0,
          RAW == "Una vez durante la relación" ~ 1,
          RAW == "Dos veces durante la relación" ~ 1,
          # RAW == "3 a 5 veces durante la relación pero ha ocurrido antes" ~ 1,
          RAW == "3 a 5 veces durante la relación" ~ 1,
          RAW == "6 a 10 veces durante la relación" ~ 1,
          RAW == "11 a 20 veces durante la relación" ~ 1,
          RAW == "Más de 20 veces durante la relación" ~ 1,
          is.na(RAW) ~ NA_real_, # OR NA_character_,
          trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_real_, # OR NA_character_,
          TRUE ~ 9999 # OR "9999"
        )
    )
    
  # [END ADAPT 2/3]: ***********************************************************
  # ****************************************************************************
    

  # Create DF_wide_RAW_DIR -----------------------------------------------------
  DF_wide_RAW =
    DF_long_DIR |> 
    tidyr::pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR, DIR_prev),
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
      !!names_list$name_DIRd[1] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[2] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[3] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[4] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[4]], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[5] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[5]], "_DIR"))), na.rm = TRUE),


    # Prevalence
      !!names_list$name_DIRd[6] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[6]], "_DIR_prev"))), na.rm = TRUE),
      !!names_list$name_DIRd[7] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[7]], "_DIR_prev"))), na.rm = TRUE),
      !!names_list$name_DIRd[8] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[8]], "_DIR_prev"))), na.rm = TRUE),
      !!names_list$name_DIRd[9] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[9]], "_DIR_prev"))), na.rm = TRUE)
      
      # CTS2_PsychAggressionPrevalence_DIRd = ifelse(names_list$name_DIRd[2] > 0, 1, 0),
      # CTS2_PhysicalAssaultPrevalence_DIRd = ifelse(names_list$name_DIRd[3] > 0, 1, 0),
      # CTS2_SexualCoercionPrevalence_DIRd = ifelse(names_list$name_DIRd[4] > 0, 1, 0),
      # CTS2_InjuryPrevalence_DIRd = ifelse(names_list$name_DIRd[5] > 0, 1, 0)
    ) |> 
    select(-ends_with("_DIR_prev"))
    
  # [END ADAPT 3/3]: ***********************************************************
  # ****************************************************************************


  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE, output_formats = output_formats)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
