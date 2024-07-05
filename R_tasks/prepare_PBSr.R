##' Prepare PBSr
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_PBSr -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_PBSr
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_PBSr <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_PBSr)

  
  # [ADAPT]: Items to ignore, reverse and dimensions ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("023") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  items_dimensions = list(
    CreenciasReligiosasTradicionales = c("001", "008", "015", "022"), 
    psi = c("002", "009", "016", "023"), 
    brujeria = c("003", "010", "017", "024"), 
    supersticion = c("004", "011", "018"), 
    espiritismo = c("005", "012", "019", "025"), 
    FormasVidaExtraordinaria = c("006", "013", "020"), 
    precognicion = c("007", "014", "021", "026")
  )
  

  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
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
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
    dplyr::mutate(
      DIR =
       dplyr::case_when(
          RAW == '1 Muy en desacuerdo' ~ 1,
          RAW == '2 Moderadamente en desacuerdo' ~ 2,
          RAW == '3 Un poco en desacuerdo' ~ 3,
          RAW == '4 No sé / No tengo certeza' ~ 4,
          RAW == '5 Un poco de acuerdo' ~ 5,
          RAW == '6 Moderadamente de acuerdo' ~ 6,
          RAW == '7 Muy de acuerdo' ~ 7,
          TRUE ~ 9999
        )
    ) |> 
    
    # Invert items 23
    # [TODO]: Item id's will be 3 digits: 023
    dplyr::mutate(
      DIR = 
       dplyr::case_when(
          DIR == 9999 ~ DIR,
          trialid %in% paste0(short_name_scale_str, "_", items_to_reverse) ~ (8 - DIR),
          TRUE ~ DIR
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
        
    
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)
  
  DF_wide_RAW_DIR =
    DF_wide_RAW |> 
    dplyr::mutate(

      # Score Dimensions (use 3 digit item numbers)
      !!names_list$name_DIRd[1] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR"))), na.rm = TRUE), 
      !!names_list$name_DIRd[2] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[3] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR"))), na.rm = TRUE), 
      !!names_list$name_DIRd[4] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[4]], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[5] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[5]], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[6] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[6]], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[7] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[7]], "_DIR"))), na.rm = TRUE),
      
      # Score Scale
            !!names_list$name_DIRt := rowMeans(across(all_of(matches("_DIR$"))), na.rm = TRUE)

      
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
