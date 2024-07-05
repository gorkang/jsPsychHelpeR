##' Prepare AIM
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_AIM -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_AIM
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_AIM <- function(DF_clean, short_name_scale_str, output_formats = output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # debug_function(prepare_AIM)

  # OUTSIDE FILES -----------------------------------------------------------
  DF_lookup = data.table::fread("R_tasks/prepare_AIM-lookup.csv")
           
  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  items_dimensions = list(
    TramoIngreso = c("004", "005", "006", "007", "008", "009", "010")
    )
  
  # [END ADAPT 1/3]: ***********************************************************
  # ****************************************************************************
  
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = names(items_dimensions), # Use names of dimensions, "" or comment out line
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
        RAW == "Sin estudios formales." ~ "1",
        RAW == "Básica incompleta; primaria o preparatoria incompleta." ~ "2",
        RAW == "Básica completa; primaria o preparatoria completa." ~ "3",
        RAW == "Media científico humanista o media técnico profesional incompleta; humanidades incompletas." ~ "4",
        RAW == "Media científico humanista o media técnico profesional completa; humanidades completas." ~ "5",
        RAW == "Instituto técnico (CFT) o instituto profesional incompleto (carreras de 1 a 3 años)." ~ "6",
        RAW == "Instituto técnico (CFT) o instituto profesional completo (carreras de 1 a 3 años); hasta suboficial de FFAA y Carabineros." ~ "7",
        RAW == "Universitaria incompleta (carreras de 4 o más años)." ~ "8",
        RAW == "Universitaria completa (carreras de 4 o más años); oficial de FFAA y Carabineros." ~ "9",
        RAW == "Postgrado (postítulo, master, magíster, doctor)." ~ "10",
        
        RAW == "Trabajadores no calificados en ventas y servicios, peones agropecuarios, forestales, construcción, etc." ~ "1",
        RAW == "Obreros, operarios y artesanos de artes mecánicas y de otros oficios." ~ "2",
        RAW == "Trabajadores de los servicios y vendedores de comercio y mercados." ~ "3",
        RAW == "Agricultores y trabajadores calificados agropecuarios y pesqueros." ~ "4",
        RAW == "Operadores de instalaciones y máquinas y montadores / conductores de vehículos." ~ "5",
        RAW == "Empleados de oficina públicos y privados." ~ "6",
        RAW == "Técnicos y profesionales de nivel medio (incluye hasta suboficiales FFAA y Carabineros)." ~ "7",
        RAW == "Profesionales, científicos e intelectuales." ~ "8",
        RAW == "Alto ejecutivo (gerente general o gerente de área o sector) de empresa privadas o pública- Director o dueño de grandes empresa- Alto directivo del poder ejecutivo, de los cuerpos legislativos y la administración pública (incluye oficiales de FFAA y Carabineros)." ~ "9",
        
        RAW == "Otros grupos no identificados (incluye rentistas, incapacitados, etc.)" ~ "10",
        RAW == "Otros grupos no identificados (incluye rentistas, personas en situación de discapacidad, etc.)" ~ "10",
        
        
        # These are numbers. Need to store them as characters here and postpone the processing
        trialid == "AIM_003" ~ RAW, 
        
        RAW == "Menos de 120 mil" ~ "1", 
        RAW == "120 mil – 207 mil" ~ "2", 
        RAW == "208 mil – 361 mil" ~ "3", 
        RAW == "362 mil – 630 mil" ~ "4", 
        RAW == "631 mil – 1.099.000" ~ "5", 
        RAW == "1.100.000 – 1.916.000" ~ "6", 
        RAW == "Más de 1.916.000" ~ "7", 
        RAW == "NS/NR" ~ "99",
        
        RAW == "Menos de 194 mil" ~ "1", 
        RAW == "194 mil – 336 mil" ~ "2", 
        RAW == "337 mil – 586 mil" ~ "3", 
        RAW == "587 mil – 1.023.000" ~ "4", 
        RAW == "1.024.000 – 1.785.000" ~ "5", 
        RAW == "1.786.000 – 3.113.000" ~ "6", 
        RAW == "Más de 3.113.000" ~ "7", 
        RAW == "NS/NR" ~ "99",
        
        RAW == "Menos de 257 mil" ~ "1", 
        RAW == "257 mil – 446 mil" ~ "2", 
        RAW == "447 mil – 779 mil" ~ "3", 
        RAW == "780 mil – 1.359.000" ~ "4", 
        RAW == "1.360.000 – 2.370.000" ~ "5", 
        RAW == "2.371.000 – 4.135.000" ~ "6", 
        RAW == "Más de 4.135.000" ~ "7", 
        RAW == "NS/NR" ~ "99",
        
        RAW == "Menos de 314 mil" ~ "1", 
        RAW == "314 mil – 546 mil" ~ "2", 
        RAW == "547 mil – 953 mil" ~ "3", 
        RAW == "954 mil – 1.662.000" ~ "4", 
        RAW == "1.663.000 – 2.899.000" ~ "5", 
        RAW == "2.900.000 – 5.057.000" ~ "6", 
        RAW == "Más de 5.057.000" ~ "7", 
        RAW == "NS/NR" ~ "99",
        
        RAW == "Menos de 367 mil" ~ "1", 
        RAW == "367 mil – 638 mil" ~ "2", 
        RAW == "639 mil – 1.114.000 mil" ~ "3", 
        RAW == "1.115.000 mil – 1.943.000" ~ "4", 
        RAW == "1.944.000 – 3.389.000" ~ "5", 
        RAW == "3.3980.000 – 5.912.000" ~ "6", 
        RAW == "Más de 5.912.000" ~ "7", 
        RAW == "NS/NR" ~ "99",
        
        RAW == "Menos de 417 mil" ~ "1", 
        RAW == "417 mil – 725 mil" ~ "2", 
        RAW == "726 mil – 1.265.000 mil" ~ "3", 
        RAW == "1.266.000 mil – 2.207.000" ~ "4", 
        RAW == "2.208.000 – 3.850.000" ~ "5", 
        RAW == "3.851.000 – 6.717.000" ~ "6", 
        RAW == "Más de 6.717.000" ~ "7", 
        RAW == "NS/NR" ~ "99",
        
        RAW == "Menos de 464 mil" ~ "1", 
        RAW == "464 mil – 808 mil" ~ "2", 
        RAW == "809 mil – 1.409.000 mil" ~ "3", 
        RAW == "1.410.000 mil – 2.459.000" ~ "4", 
        RAW == "2.460.000 – 4.289.000" ~ "5", 
        RAW == "4.290.000 – 7.482.000" ~ "6", 
        RAW == "Más de 7.482.000" ~ "7", 
        RAW == "NS/NR" ~ "99",
        
        is.na(RAW) ~ NA_character_,
        TRUE ~ "9999"
      )) |> 
    
    # When a task combines numbers and characters in RAW, we need to first create a DIR var with numbers as characters and then convert all to numbers 
    dplyr::mutate(DIR = as.numeric(DIR)) |> 
    
    # Here we process the AIM_03 numbers
    dplyr::mutate(DIR = 
            dplyr::case_when(
               trialid == "AIM_003" & DIR < 1 ~ 1,
               trialid == "AIM_003" & DIR < 7 ~ DIR,
               trialid == "AIM_003" & DIR >= 7 ~ 7,
               TRUE ~ DIR
             ))
  
    
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
  
  
    
  # [ADAPT 3/3]: Scales and dimensions calculations ----------------------------
  # ****************************************************************************
  
  DF_wide_RAW_DIR =
    DF_wide_RAW  |>  
    dplyr::mutate(
      !!names_list$name_DIRd[1] := rowMeans(across(any_of(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR"))), na.rm = TRUE)
      ) |> 
    # Join DF_lookup to find AIM_DIRt 
    dplyr::left_join(DF_lookup, by = c("AIM_001_DIR", "AIM_002_DIR", "AIM_TramoIngreso_DIRd"))
    
  # [END ADAPT 3/3]: ***********************************************************
  # ****************************************************************************


  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE, output_formats = output_formats)

  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 

}
