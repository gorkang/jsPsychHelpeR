##' Prepare IFS
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_IFS -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 3 [ADAPT] chunks
##'
##' @title prepare_IFS
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_IFS <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_IFS)
  
  
  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("018", "020", "022", "024", "025", "026", "028", "030")
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  ## NameDimension1, NameDimension2 should be the names of the dimensions
  ## Inside each c() create a vector of the item numbers for the dimension
  ## Add lines as needed. If there are no dimensions, keep as is
  items_dimensions = list(
    Total = c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014", "015", "016", "017", "018", "019", "020", "021", "022", "023", "024", "025", "026", "027", "028", "029", "030", "031"),
    SeriesMotoras = c("001"),
    InstruccionesConflictivas = c("002"),
    ControlInhibitorioMotor = c("003"),
    RepeticionDigitosAtras = c("004", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014", "015"),
    MesesAtras = c("016"),
    MemoriaTrabajoVisual = c("017"),
    Refranes = c("019", "021", "023"),
    ControlInhibitorioVerbal = c("027", "029", "031")
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
    bind_rows(
      tibble(id = "999", trialid = "IFS_017", RAW = "")
    ) |> 
    
    
    
  # [ADAPT 2/3]: RAW to DIR for individual items -------------------------------
  # ****************************************************************************
  
    # Transformations
    dplyr::mutate(
      DIR =
       dplyr::case_when(
         trialid %in% c("IFS_001") & RAW == "No logró 3 series consecutivas con el examinador" ~ 0,
         trialid %in% c("IFS_001") & RAW == "No completó al menos 3 consecutivas solo. Pero logró 3 series consecutivas con el examinador" ~ 1,
         trialid %in% c("IFS_001") & RAW == "Completó al menos 3 series consecutivas por si solo" ~ 2,
         trialid %in% c("IFS_001") & RAW == "Completó 6 series consecutivas solo" ~ 3,
         
         trialid %in% c("IFS_002") & RAW == "Golpeó <b>como el examinador</b> al menos 4 veces consecutivas" ~ 0,
         trialid %in% c("IFS_002") & RAW == "Cometió mas de dos errores" ~ 1,
         trialid %in% c("IFS_002") & RAW == "Cometió uno o dos errores" ~ 2,
         trialid %in% c("IFS_002") & RAW == "Sin errores" ~ 3,
         
         trialid %in% c("IFS_003") & RAW == "Golpeó <b>como el examinador</b> al menos 4 veces consecutivas" ~ 0,
         trialid %in% c("IFS_003") & RAW == "Cometió mas de dos errores" ~ 1,
         trialid %in% c("IFS_003") & RAW == "Cometió uno o dos errores" ~ 2,
         trialid %in% c("IFS_003") & RAW == "Sin errores" ~ 3,
         
         trialid %in% c("IFS_004", "IFS_005", "IFS_006", "IFS_007", "IFS_008", "IFS_009", "IFS_010", "IFS_011", "IFS_012", "IFS_013", "IFS_014", "IFS_015") & RAW == "No" ~ 0,
         trialid %in% c("IFS_004", "IFS_005", "IFS_006", "IFS_007", "IFS_008", "IFS_009", "IFS_010", "IFS_011", "IFS_012", "IFS_013", "IFS_014", "IFS_015") & RAW == "Si" ~ 1,
         
         trialid %in% c("IFS_016") & RAW == "Cometió mas de un error" ~ 0,
         trialid %in% c("IFS_016") & RAW == "Cometió un error" ~ 1,
         trialid %in% c("IFS_016") & RAW == "Sin errores" ~ 2,
         
         # IFS_017  tipo multiple choice: Utilizar el numero de opciones elejidas como puntaje de la pregunta
         trialid %in% c("IFS_017") ~ ifelse(nchar(RAW) > 0, str_count(RAW,";") + 1, 0),
         
         
         trialid %in% c("IFS_019", "IFS_021", "IFS_023") & RAW == "Correcto (respuesta ideal)" ~ 1,
         trialid %in% c("IFS_019", "IFS_021", "IFS_023") & RAW == "Se acerca pero no es ideal (se parece a la correcta, dió un ejemplo)" ~ 0.5,
         trialid %in% c("IFS_019", "IFS_021", "IFS_023") & RAW == "No se parece en nada a la respuesta ideal o no tiene relación al refrán" ~ 0,
         
         trialid %in% c("IFS_027", "IFS_029", "IFS_031") & RAW == "Palabra exacta" ~ 0,
         trialid %in% c("IFS_027", "IFS_029", "IFS_031") & RAW == "Palabra diferente pero tiene una relación semántica" ~ 1,
         trialid %in% c("IFS_027", "IFS_029", "IFS_031") & RAW == "Palabra no tiene relación con el texto" ~ 2,
         

         # ERRATA
         trialid %in% c("IFS_027", "IFS_029", "IFS_031") & RAW == "Palabra diferente pero tiene una relacion semantica" ~ 1,
         trialid %in% c("IFS_027", "IFS_029", "IFS_031") & RAW == "Palabra no tiene relacion con el texto" ~ 2,
         
       
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
  # BAD_ITEMS = DF_long_DIR |> filter(DIR == 9999) |> distinct(trialid) |> pull(trialid)
  # DF_long_DIR |> filter(trialid %in% BAD_ITEMS) |> View()
  # DF_long_DIR |> filter(trialid == "IFS_017") |> View()
  
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

      # [CHECK] Using correct formula? rowMeans() / rowSums()
      
      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      !!names_list$name_DIRd[1] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[2] := get(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR")),
      !!names_list$name_DIRd[3] := get(paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR")),
      !!names_list$name_DIRd[4] := get(paste0(short_name_scale_str, "_", items_dimensions[[4]], "_DIR")),
      !!names_list$name_DIRd[5] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[5]], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[6] := get(paste0(short_name_scale_str, "_", items_dimensions[[6]], "_DIR")),
      !!names_list$name_DIRd[7] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[7]], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[8] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[8]], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[9] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[9]], "_DIR"))), na.rm = TRUE),
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
