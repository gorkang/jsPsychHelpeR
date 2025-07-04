##' Prepare SDG
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_SDG -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_SDG
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_SDG <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_SDG)

  # [ADAPT]: Items to ignore, reverse and dimensions ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("00") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("00") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  items_dimensions = list(
    NameDimension1 = c("000")
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

    # Transformations
    dplyr::mutate(
      DIR =
       dplyr::case_when(
          
          # TODO: This give WARNINGS -----
          # trialid %in% c("SDG_001") ~ as.numeric(RAW), # Gives warning
          trialid %in% c("SDG_001", "SDG_005") ~ RAW,
          
          trialid == "SDG_002" & RAW == "Casado (a)" ~ "1",
          trialid == "SDG_002" & RAW == "Conviviente" ~ "2",
          trialid == "SDG_002" & RAW == "Viudo (a)" ~ "3",
          trialid == "SDG_002" & RAW == "Divorciado/anulado (a)" ~ "4",
          trialid == "SDG_002" & RAW == "Separado (a)" ~ "5",
          trialid == "SDG_002" & RAW == "Soltero/sin pareja" ~ "6",
          trialid == "SDG_002" & RAW == "Soltero/con pareja" ~ "7",
          
          trialid == "SDG_003" & RAW == "Sin Estudios" ~ "1",
          trialid == "SDG_003" & RAW == "Básica Incompleta" ~ "2",
          trialid == "SDG_003" & RAW == "Básica Completa" ~ "3",
          trialid == "SDG_003" & RAW == "Media Incompleta" ~ "4",
          trialid == "SDG_003" & RAW == "Media Completa" ~ "5",
          trialid == "SDG_003" & RAW == "Técnica Incompleta" ~ "6",
          trialid == "SDG_003" & RAW == "Técnica Completa" ~ "7",
          trialid == "SDG_003" & RAW == "Universitaria Incompleta" ~ "8",
          trialid == "SDG_003" & RAW == "Universitaria Completa o más" ~ "9",
          trialid == "SDG_003" & RAW == "No sabe o no aplica" ~ "10",
          
          trialid == "SDG_004" & RAW == "Si tengo registro." ~ "1",
          trialid == "SDG_004" & RAW == "No tengo registro." ~ "2",
          
          # TODO: This give WARNINGS -----
          # trialid == "SDG_005" ~ as.numeric(RAW), # Gives warning
          
          trialid == "SDG_006" & RAW == "Trabaja a tiempo completo" ~ "1",
          trialid == "SDG_006" & RAW == "Trabaja a tiempo parcial" ~ "2",
          trialid == "SDG_006" & RAW == "Trabaja esporádicamente" ~ "3",
          trialid == "SDG_006" & RAW == "Está desempleado(a), pero busca trabajo" ~ "4",
          trialid == "SDG_006" & RAW == "Es estudiante" ~ "5",
          trialid == "SDG_006" & RAW == "No trabaja, ni busca trabajo" ~ "6",
          trialid == "SDG_006" & RAW == "Es ama de casa" ~ "7",
          trialid == "SDG_006" & RAW == "Está jubilado o pensionado" ~ "8",
          trialid == "SDG_006" & RAW == "Es rentista" ~ "9",
          trialid == "SDG_006" & RAW == "No sabe/No responde" ~ "10",
          
          trialid == "SDG_007" & RAW == "Profesionales" ~ "1",
          trialid == "SDG_007" & RAW == "Ejecutivos, administrativos, gerentes" ~ "2",
          trialid == "SDG_007" & RAW == "Comerciantes, vendedores y cajeros" ~ "3",
          trialid == "SDG_007" & RAW == "Trabajos de apoyo administrativo, incluyendo trabajos eclesiásticos administrativos" ~ "4",
          trialid == "SDG_007" & RAW == "Trabajo con productos de precisión o artesanías. Técnicos en reparación." ~ "5",
          trialid == "SDG_007" & RAW == "Operador de máquinas, instalador, inspectores" ~ "6",
          trialid == "SDG_007" & RAW == "Ocupaciones de transporte y manejo de carga" ~ "7",
          trialid == "SDG_007" & RAW == "Obreros, limpiadores de equipos, ayudantes y peones" ~ "8",
          trialid == "SDG_007" & RAW == "Ocupaciones de servicio, excepto empleados de casa particular" ~ "9",
          trialid == "SDG_007" & RAW == "Agricultor/ gerente de agricultura" ~ "10",
          trialid == "SDG_007" & RAW == "Campesino" ~ "11",
          trialid == "SDG_007" & RAW == "Fuerzas Armadas" ~ "12",
          trialid == "SDG_007" & RAW == "Empleados casa particular" ~ "13",
          trialid == "SDG_007" & RAW == "Otro" ~ "14",
          
          trialid == "SDG_008" ~ "0", # IS AN OPEN QUESTION 
          
          is.na(RAW) ~ NA_character_,
          trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_character_,
          TRUE ~ "9999"
        )
    ) |> 
    
    # When a task combines numbers and characters in RAW, we need to first create a DIR var with numbers as characters and then convert all to numbers 
    dplyr::mutate(DIR = as.numeric(DIR)) |> 
    
    # Invert items
    dplyr::mutate(
      DIR = 
       dplyr::case_when(
          DIR == 9999 ~ DIR, # To keep the missing values unchanged
          trialid %in% paste0(short_name_scale_str, "_", items_to_reverse) ~ (6 - DIR),
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
    DF_wide_RAW

  # [END ADAPT]: ***************************************************************
  # ****************************************************************************


  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE, output_formats = output_formats)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
