##' Prepare CEL
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_CEL -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_CEL
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_CEL <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_CEL)

  
  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  items_dimensions = list(
    Derivativo = c("01", "02", "03", "04", "05", "06"),
    Democratico = c("01", "02", "03", "04", "05", "06"),
    Directivo = c("01", "02", "03", "04", "05", "06")
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
                                numeric_responses = FALSE, 
                                is_experiment = FALSE, 
                                help_prepare = FALSE) # Show n of items, responses,... [CHANGE to FALSE] 
  
  
  # Create long DIR ------------------------------------------------------------
  DF_long_DIR = 
    DF_long_RAW %>% 
    select(id, trialid, RAW) %>%
    
    
    
  # [ADAPT 2/3]: RAW to DIR for individual items -------------------------------
  # ****************************************************************************
  
    # Transformations
    mutate(
      DIR =
        case_when(
          trialid %in% c("CEL_01") & RAW == "Espera hasta que estén listos para hablar." ~ "Derivativo",
          trialid %in% c("CEL_01") & RAW == "Sugiere que el grupo vote qué hacer ahora." ~ "Democratico",
          trialid %in% c("CEL_01") & RAW == "Asigna tareas específicas a diferentes personas y los ayuda a completar con sus tareas." ~ "Directivo",
          
          trialid %in% c("CEL_02") & RAW == "Reduce su liderazgo. Deja que las personas en el grupo lideren lo más posible." ~ "Derivativo",
          trialid %in% c("CEL_02") & RAW == "Se asegura que se llegue a un acuerdo en cada punto, antes de proceder." ~ "Democratico",
          trialid %in% c("CEL_02") & RAW == "Mantiene al grupo bajo firme control, sino el grupo perderá su momentum." ~ "Directivo",
          
          trialid %in% c("CEL_03") & RAW == "Lo expresaría tal cual. Delinearía los cambios y observaría que éstos se lleven a cabo." ~ "Directivo",
          trialid %in% c("CEL_03") & RAW == "Propondría los cambios. Explicaría, por qué son necesarios y luego permitiría que el grupo decida qué hacer." ~ "Democratico",
          trialid %in% c("CEL_03") & RAW == "No haría nada al respecto, esto podría amenazar la productividad del grupo." ~ "Derivativo",
          
          trialid %in% c("CEL_04") & RAW == "Deja al grupo solo." ~ "Derivativo",
          trialid %in% c("CEL_04") & RAW == "Lentamente se inserta para ir dándole al grupo una mayor dirección." ~ "Directivo",
          trialid %in% c("CEL_04") & RAW == "Le pregunta al grupo si usted debiera proveer... y luego cumple con sus deseos." ~ "Democratico",
          
          trialid %in% c("CEL_05") & RAW == "Deja que todos en el grupo manifiesten su opinión. Sin intervenir." ~ "Derivativo",
          trialid %in% c("CEL_05") & RAW == "Lleva a votación la sugerencia sobre el receso." ~ "Democratico",
          trialid %in% c("CEL_05") & RAW == "Propone un nuevo curso de acción para el grupo. Si nadie se opone firmemente, designa tareas y luego observa que estas se lleven a cabo." ~ "Directivo",
          
          trialid %in% c("CEL_06") & RAW == "Sugiere que el grupo avance hacia otro tema. Y si nadie se opone, hace un listado de los posibles temas." ~ "Democratico",
          trialid %in% c("CEL_06") & RAW == "Elige una actividad para el grupo y asigna tareas." ~ "Directivo",
          trialid %in% c("CEL_06") & RAW == "Se mantiene en silencio hasta que el grupo tome una decisión." ~ "Derivativo",
          is.na(RAW) ~ NA_character_,
          grepl(items_to_ignore, trialid) ~ NA_character_,
          TRUE ~ "9999"
        )
    ) 
    
    
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
      !!names_list$name_DIRd[1] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR")) == names(items_dimensions[1]), na.rm = TRUE),
      !!names_list$name_DIRd[2] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR")) == names(items_dimensions[2]), na.rm = TRUE),
      !!names_list$name_DIRd[3] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR")) == names(items_dimensions[3]), na.rm = TRUE),
      
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
