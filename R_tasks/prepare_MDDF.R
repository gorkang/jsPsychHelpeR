##' Prepare MDDF
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_MDDF -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_MDDF
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_MDDF <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # debug_function(prepare_MDDF)
  # Conditional questions: "MDDF_03" "MDDF_06" "MDDF_09" "MDDF_12" "MDDF_18" "MDDF_21" "MDDF_24" "MDDF_27" "MDDF_30" "MDDF_36" "MDDF_47" "MDDF_53" "MDDF_56" "MDDF_59"
  
  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  ## NameDimension1, NameDimension2 should be the names of the dimensions
  ## Inside each c() create a vector of the item numbers for the dimension
  ## Add lines as needed. If there are no dimensions, keep as is
  items_dimensions = list(
    DisgustoMoralDumbfounding = c("01", "02", "03", "04", "05", "06"),
    DisgustoMoralSinDumbfounding = c("07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21"),
    DisgustoNoMoral = c("22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41"),
    SituacionesControl = c("42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63")
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
          
          # CORRECCION TEST COMPLETO
          # 
          # trialid %in% c("MDDF_01", "MDDF_04", "MDDF_07", "MDDF_10", "MDDF_13", "MDDF_16", "MDDF_19", "MDDF_22", "MDDF_25", "MDDF_28", "MDDF_31", "MDDF_34", "MDDF_37", "MDDF_40", "MDDF_43", "MDDF_46", "MDDF_49", "MDDF_52", "MDDF_55", "MDDF_58", "MDDF_61") & RAW == "Muy inadecuado" ~ "0",
          # trialid %in% c("MDDF_01", "MDDF_04", "MDDF_07", "MDDF_10", "MDDF_13", "MDDF_16", "MDDF_19", "MDDF_22", "MDDF_25", "MDDF_28", "MDDF_31", "MDDF_34", "MDDF_37", "MDDF_40", "MDDF_43", "MDDF_46", "MDDF_49", "MDDF_52", "MDDF_55", "MDDF_58", "MDDF_61") & RAW == "Medianamente inadecuado" ~ "1",
          # trialid %in% c("MDDF_01", "MDDF_04", "MDDF_07", "MDDF_10", "MDDF_13", "MDDF_16", "MDDF_19", "MDDF_22", "MDDF_25", "MDDF_28", "MDDF_31", "MDDF_34", "MDDF_37", "MDDF_40", "MDDF_43", "MDDF_46", "MDDF_49", "MDDF_52", "MDDF_55", "MDDF_58", "MDDF_61") & RAW == "Poco inadecuado" ~ "2",
          # trialid %in% c("MDDF_01", "MDDF_04", "MDDF_07", "MDDF_10", "MDDF_13", "MDDF_16", "MDDF_19", "MDDF_22", "MDDF_25", "MDDF_28", "MDDF_31", "MDDF_34", "MDDF_37", "MDDF_40", "MDDF_43", "MDDF_46", "MDDF_49", "MDDF_52", "MDDF_55", "MDDF_58", "MDDF_61") & RAW == "Sin opinion" ~ "3",
          # trialid %in% c("MDDF_01", "MDDF_04", "MDDF_07", "MDDF_10", "MDDF_13", "MDDF_16", "MDDF_19", "MDDF_22", "MDDF_25", "MDDF_28", "MDDF_31", "MDDF_34", "MDDF_37", "MDDF_40", "MDDF_43", "MDDF_46", "MDDF_49", "MDDF_52", "MDDF_55", "MDDF_58", "MDDF_61") & RAW == "Un poco adecuado" ~ "4",
          # trialid %in% c("MDDF_01", "MDDF_04", "MDDF_07", "MDDF_10", "MDDF_13", "MDDF_16", "MDDF_19", "MDDF_22", "MDDF_25", "MDDF_28", "MDDF_31", "MDDF_34", "MDDF_37", "MDDF_40", "MDDF_43", "MDDF_46", "MDDF_49", "MDDF_52", "MDDF_55", "MDDF_58", "MDDF_61") & RAW == "Medianamente adecuado" ~ "5",
          # trialid %in% c("MDDF_01", "MDDF_04", "MDDF_07", "MDDF_10", "MDDF_13", "MDDF_16", "MDDF_19", "MDDF_22", "MDDF_25", "MDDF_28", "MDDF_31", "MDDF_34", "MDDF_37", "MDDF_40", "MDDF_43", "MDDF_46", "MDDF_49", "MDDF_52", "MDDF_55", "MDDF_58", "MDDF_61") & RAW == "Altamente adecuado" ~ "6",
          # 
          # trialid %in% c("MDDF_02", "MDDF_05", "MDDF_08", "MDDF_11", "MDDF_14", "MDDF_17", "MDDF_20", "MDDF_23", "MDDF_26", "MDDF_29", "MDDF_32", "MDDF_35", "MDDF_38", "MDDF_41", "MDDF_44", "MDDF_47", "MDDF_50", "MDDF_53", "MDDF_56", "MDDF_59", "MDDF_62") & RAW == "Si" ~ "1",
          # trialid %in% c("MDDF_02", "MDDF_05", "MDDF_08", "MDDF_11", "MDDF_14", "MDDF_17", "MDDF_20", "MDDF_23", "MDDF_26", "MDDF_29", "MDDF_32", "MDDF_35", "MDDF_38", "MDDF_41", "MDDF_44", "MDDF_47", "MDDF_50", "MDDF_53", "MDDF_56", "MDDF_59", "MDDF_62") & RAW == "No" ~ "0",

          # CORRECCION PROTOCOLO 23 ## Se excluyen varios items para reducir la duracion de la prueba
          RAW == "Muy inadecuado" ~ "0",
          RAW == "Medianamente inadecuado" ~ "1",
          RAW == "Poco inadecuado" ~ "2",
          RAW == "Sin opinion" ~ "3",
          RAW == "Un poco adecuado" ~ "4",
          RAW == "Medianamente adecuado" ~ "5",
          RAW == "Altamente adecuado" ~ "6",

          RAW == "Si" ~ "1",
          RAW == "No" ~ "0",
          
          is.na(RAW) ~ NA_character_,
          trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_character_,
          TRUE ~ RAW 
        ),
      DIR = as.numeric(DIR)
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
  
  DF_wide_RAW_DIR =
    DF_wide_RAW |> 
    dplyr::mutate(
      # TODO: review one_of().
      !!names_list$name_DIRd[1] := rowSums(across(all_of(one_of(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR")))), na.rm = TRUE),
      !!names_list$name_DIRd[2] := rowSums(across(all_of(one_of(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR")))), na.rm = TRUE),
      !!names_list$name_DIRd[3] := rowSums(across(all_of(one_of(paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR")))), na.rm = TRUE),
      !!names_list$name_DIRd[4] := rowSums(across(all_of(one_of(paste0(short_name_scale_str, "_", items_dimensions[[4]], "_DIR")))), na.rm = TRUE),
      
      
      # Reliability Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!names_list$name_RELd[1] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_RELd1, "_DIR"))), na.rm = TRUE), 

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
