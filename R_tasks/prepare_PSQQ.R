##' Prepare PSQQ
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_PSQQ -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 3 [ADAPT] chunks
##'
##' @title prepare_PSQQ
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_PSQQ <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_PSQQ)
  
  
  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("022") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  ## NameDimension1, NameDimension2 should be the names of the dimensions
  ## Inside each c() create a vector of the item numbers for the dimension
  ## Add lines as needed. If there are no dimensions, keep as is
  items_dimensions = list(
    CalidadSubjetivaDeSueno = c("017"),
    LatenciaDeSueno = c("002", "005"),
    DuracionDelSueno = c("004"),
    PerturbacionesDelSueno = c("005", "006", "007", "008", "009", "010", "011", "012", "013", "016"),
    UtilizacionDeMedicacionParaDormir = c("018"),
    DisfuncionDuranteElDia = c("019", "020"),
    CalidadDeSuenoActual = c("021"),
    EdadPercibida = c("022"),
    EficienciaHabitualDeSueno = c("HoraAcostarse_DIRd", "HoraLevantarse_DIRd", "004"),
    HoraAcostarse = c("001_hours", "001_minutes"),
    HoraLevantarse = c("003_hours", "003_minutes")
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
         
         trialid %in% c("PSQQ_002") & RAW == "Menos de 15 min" ~ 0,
         trialid %in% c("PSQQ_002") & RAW == "Entre 16-30 min" ~ 1,
         trialid %in% c("PSQQ_002") & RAW == "Entre 31-60 min" ~ 2,
         trialid %in% c("PSQQ_002") & RAW == "Más de 60 min" ~ 3,
         
         trialid %in% c("PSQQ_001_hours", "PSQQ_001_minutes", "PSQQ_003_hours", "PSQQ_003_minutes") ~ as.numeric(RAW),

         trialid %in% c("PSQQ_004") & RAW > 7 ~ 0,
         trialid %in% c("PSQQ_004") & RAW == 7 ~ 1,
         trialid %in% c("PSQQ_004") & RAW %in% c(5, 6) ~ 2,
         trialid %in% c("PSQQ_004") & RAW < 5 ~ 3,
         
         trialid %in% c("PSQQ_005", "PSQQ_006", "PSQQ_007", "PSQQ_008", "PSQQ_009", "PSQQ_010", "PSQQ_011", "PSQQ_012", "PSQQ_013", "PSQQ_016") & RAW == "Ninguna vez en el último mes" ~ 0,
         trialid %in% c("PSQQ_005", "PSQQ_006", "PSQQ_007", "PSQQ_008", "PSQQ_009", "PSQQ_010", "PSQQ_011", "PSQQ_012", "PSQQ_013", "PSQQ_016") & RAW == "Menos de una vez a la semana" ~ 1,
         trialid %in% c("PSQQ_005", "PSQQ_006", "PSQQ_007", "PSQQ_008", "PSQQ_009", "PSQQ_010", "PSQQ_011", "PSQQ_012", "PSQQ_013", "PSQQ_016") & RAW == "Una o dos veces a la semana" ~ 2,
         trialid %in% c("PSQQ_005", "PSQQ_006", "PSQQ_007", "PSQQ_008", "PSQQ_009", "PSQQ_010", "PSQQ_011", "PSQQ_012", "PSQQ_013", "PSQQ_016") & RAW == "Tres o más veces a la semana" ~ 3,
         
         # trialid's 14 and 15 are not scored
         
         trialid %in% c("PSQQ_017") & RAW == "Muy buena" ~ 0,
         trialid %in% c("PSQQ_017") & RAW == "Bastante buena" ~ 1,
         trialid %in% c("PSQQ_017") & RAW == "Bastante mala" ~ 2,
         trialid %in% c("PSQQ_017") & RAW == "Muy mala" ~ 3,
         
         trialid %in% c("PSQQ_018", "PSQQ_019") & RAW == "Ninguna vez en el último mes" ~ 0,
         trialid %in% c("PSQQ_018", "PSQQ_019") & RAW == "Menos de una vez a la semana" ~ 1,
         trialid %in% c("PSQQ_018", "PSQQ_019") & RAW == "Una o dos veces a la semana" ~ 2,
         trialid %in% c("PSQQ_018", "PSQQ_019") & RAW == "Tres o más veces a la semana" ~ 3,
         
         trialid %in% c("PSQQ_020") & RAW == "Ningún problema" ~ 0,
         trialid %in% c("PSQQ_020") & RAW == "Sólo un leve problema" ~ 1,
         trialid %in% c("PSQQ_020") & RAW == "Un problema" ~ 2,
         trialid %in% c("PSQQ_020") & RAW == "Un grave problema" ~ 3,
         
         trialid %in% c("PSQQ_021") & RAW == "Muy bien" ~ 0,
         trialid %in% c("PSQQ_021") & RAW == "Bien" ~ 1,
         trialid %in% c("PSQQ_021") & RAW == "Regular" ~ 2,
         trialid %in% c("PSQQ_021") & RAW == "Mal" ~ 3,
         trialid %in% c("PSQQ_021") & RAW == "Muy mal" ~ 4,
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
    DF_wide_RAW  |>  
    dplyr::mutate(
    !!names_list$name_DIRd[1] := get(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR")),
    !!names_list$name_DIRd[2] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR"))), na.rm = TRUE),
    !!names_list$name_DIRd[3] := get(paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR")),
    !!names_list$name_DIRd[4] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[4]], "_DIR"))), na.rm = TRUE),
    !!names_list$name_DIRd[5] := get(paste0(short_name_scale_str, "_", items_dimensions[[5]], "_DIR")),
    !!names_list$name_DIRd[6] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[6]], "_DIR"))), na.rm = TRUE),
    !!names_list$name_DIRd[7] := get(paste0(short_name_scale_str, "_", items_dimensions[[7]], "_DIR")),
    !!names_list$name_DIRd[8] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[8]], "_DIR"))), na.rm = TRUE),
    
    # Make sure all hours/mins vars have two digits
    PSQQ_001_hours_DIR = sprintf("%02d", as.numeric(get(paste0(short_name_scale_str, "_", items_dimensions[[10]][1], "_DIR")))),
    PSQQ_001_minutes_DIR = sprintf("%02d", as.numeric(get(paste0(short_name_scale_str, "_", items_dimensions[[10]][2], "_DIR")))),
    PSQQ_003_hours_DIR = sprintf("%02d", as.numeric(get(paste0(short_name_scale_str, "_", items_dimensions[[11]][1], "_DIR")))),
    PSQQ_003_minutes_DIR = sprintf("%02d", as.numeric(get(paste0(short_name_scale_str, "_", items_dimensions[[11]][2], "_DIR")))),
    
    
    # Calculate PSQQ_HoraAcostarse_DIRd and PSQQ_HoraLevantarse_DIRd
    !!names_list$name_DIRd[10] := data.table::as.ITime(paste0(get(paste0(short_name_scale_str, "_", items_dimensions[[10]][1], "_DIR")), get(paste0(short_name_scale_str, "_", items_dimensions[[10]][2], "_DIR"))), "%H%M"), 
    !!names_list$name_DIRd[11] := data.table::as.ITime(paste0(get(paste0(short_name_scale_str, "_", items_dimensions[[11]][1], "_DIR")), get(paste0(short_name_scale_str, "_", items_dimensions[[11]][2], "_DIR"))), "%H%M"), 
    
    # With PSQQ_HoraAcostarse_DIRd and PSQQ_HoraLevantarse_DIRd get total time in bed
    HorasCama_DIRd = ifelse(difftime(time1 = get(names_list$name_DIRd[11]), time2 = get(names_list$name_DIRd[10]), units = "hours") < 0,
                            as.numeric(24 + difftime(time1 = get(names_list$name_DIRd[11]), time2 = get(names_list$name_DIRd[10]), units = "hours")),
                            as.numeric(difftime(time1 = get(names_list$name_DIRd[11]), time2 = get(names_list$name_DIRd[10]), units = "hours"))
                            ),
    
    # Efficiency is hours slept / hours in bed
    !!names_list$name_DIRd[9] := (get(paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR")) / HorasCama_DIRd) * 100,
    
    
    !!names_list$name_DIRd[2] :=
      case_when(
        get(names_list$name_DIRd[2]) == 0 ~ 0,
        get(names_list$name_DIRd[2]) %in% c(1, 2) ~ 1,
        get(names_list$name_DIRd[2]) %in% c(3, 4) ~ 2,
        get(names_list$name_DIRd[2]) %in% c(5, 6) ~ 3,
      ),
    
    !!names_list$name_DIRd[4] :=
      case_when(
        get(names_list$name_DIRd[4]) == 0 ~ 0,
        get(names_list$name_DIRd[4]) %in% c(1:9) ~ 1,
        get(names_list$name_DIRd[4]) %in% c(10:18) ~ 2,
        get(names_list$name_DIRd[4]) %in% c(19:27) ~ 3,
      ),
    
    !!names_list$name_DIRd[6] :=
      case_when(
        get(names_list$name_DIRd[6]) == 0 ~ 0,
        get(names_list$name_DIRd[6]) %in% c(1, 2) ~ 1,
        get(names_list$name_DIRd[6]) %in% c(3, 4) ~ 2,
        get(names_list$name_DIRd[6]) %in% c(5, 6) ~ 3,
      ),
    
    
    # Total minus Perceived age and PSQQ_HoraAcostarse_DIRd and PSQQ_HoraLevantarse_DIRd
    !!names_list$name_DIRt := rowSums(across(all_of(names_list$name_DIRd[c(1:6, 9)])), na.rm = TRUE),
    
    # Back to char to be able to save
    !!names_list$name_DIRd[10] := as.character(get(names_list$name_DIRd[10])),
    !!names_list$name_DIRd[11] := as.character(get(names_list$name_DIRd[11]))
    
    )

  
# DF_wide_RAW_DIR |> select(PSQQ_001_hours_DIR, PSQQ_001_minutes_DIR, PSQQ_003_hours_DIR, PSQQ_003_minutes_DIR, matches("DIRd")) |> View()
  
  
  # [END ADAPT 3/3]: ***********************************************************
  # ****************************************************************************

  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE, output_formats = output_formats)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
