##' Prepare DEMOGR
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_DEMOGR -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_DEMOGR
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_DEMOGR <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_DEMOGR)

  
  # [ADAPT]: Items to ignore, reverse and dimensions ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("00") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("00") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  names_dimensions = c("edad", "genero") # If no dimensions, keep names_dimensions = c("")
  
  items_DIRd1 = c("01")
  items_DIRd2 = c("02")
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  
  # OUTSIDE FILES -----------------------------------------------------------
  DF_lookup = readr::read_csv("R_tasks/prepare_DEMOGR-lookup.csv", 
                       col_types = 
                         cols(
                           DEMOGR_comuna_DIRd = col_integer(), #idcomuna
                           comuna = col_character()
                         ))
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = names_dimensions,
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE, help_prepare = FALSE)
  
  
  # Create long DIR ------------------------------------------------------------
  
  DF_long_DIR = 
    DF_long_RAW %>% 
   dplyr::select(id, trialid, RAW) %>%
    
  
    # [ADAPT]: RAW to DIR for individual items -----------------------------------
    # ****************************************************************************
    # 1 DEMOGR_01 Indica tu edad                                                              
    # 2 DEMOGR_02 Indica tu género                                                            
    # 3 DEMOGR_03 NA                                                                          
    # 4 DEMOGR_04 Introduce tu número de celular (9 1234 5678)                                
    # 5 DEMOGR_05 ¿Tienes algún tipo de daltonismo?                                           
    # 6 DEMOGR_06 NA                                                                          
    # 7 DEMOGR_07 Indica tú nivel de estudios                                                 
    # 8 DEMOGR_08 ¿Qué estas estudiando?                                                      
    # 9 DEMOGR_10 Año completado de tus estudios                                              
    # 10 DEMOGR_11 NA                                                                          
    # 11 DEMOGR_12 Indica el nivel de estudios de tu madre                                     
    # 12 DEMOGR_13 Indica el nivel de estudios de tu padre                                     
    # 13 DEMOGR_14 ¿Qué tipo de seguro de salud tienes?                                        
    # 14 DEMOGR_15 ¿Alguna vez has estado embarazada?                                          
    # 15 DEMOGR_16 ¿Alguna vez has tenido un parto vaginal o cesarea?                          
    # 16 DEMOGR_17 ¿Quieres tener hijos en el futuro?                                          
    # 17 DEMOGR_18 ¿Cuántos hijos(as) le gustaría tener?                                       
    # 18 DEMOGR_19 Indica si alguna de las siguientes personas o grupos ha tenido a una cesarea
    
    
    # Transformations
    dplyr::mutate(
      DIR =
       dplyr::case_when(
          trialid == "DEMOGR_01" ~ RAW,
          
          trialid == "DEMOGR_02" & RAW == "Masculino" ~ "0",
          trialid == "DEMOGR_02" & RAW == "Femenino" ~ "1",
          trialid == "DEMOGR_02" & RAW == "No binario" ~ "2",
          
          trialid == "DEMOGR_03" ~ RAW,
          
          trialid == "DEMOGR_04" ~ RAW,
          
          trialid == "DEMOGR_05" | trialid == "DEMOGR_10" | trialid == "DEMOGR_11" & RAW =="Sin educación formal" ~ "1", 
          trialid == "DEMOGR_05" | trialid == "DEMOGR_10" | trialid == "DEMOGR_11" & RAW =="Básica incompleta" ~ "2", 
          trialid == "DEMOGR_05" | trialid == "DEMOGR_10" | trialid == "DEMOGR_11" & RAW =="Básica completa" ~ "3", 
          trialid == "DEMOGR_05" | trialid == "DEMOGR_10" | trialid == "DEMOGR_11" & RAW =="Media incompleta" ~ "4", 
          trialid == "DEMOGR_05" | trialid == "DEMOGR_10" | trialid == "DEMOGR_11" & RAW =="Media completa" ~ "5", 
          trialid == "DEMOGR_05" | trialid == "DEMOGR_10" | trialid == "DEMOGR_11" & RAW =="Superior en Centro de Formación Técnica o en Instituto Profesional incompleta" ~ "6", 
          trialid == "DEMOGR_05" | trialid == "DEMOGR_10" | trialid == "DEMOGR_11" & RAW =="Superior en Centro de Formación Técnica o en Instituto Profesional completa" ~ "7",  
          trialid == "DEMOGR_05" | trialid == "DEMOGR_10" | trialid == "DEMOGR_11" & RAW =="Superior en Universidad incompleta" ~ "8", 
          trialid == "DEMOGR_05" | trialid == "DEMOGR_10" | trialid == "DEMOGR_11" & RAW =="Superior en Universidad completa" ~ "9", 
          trialid == "DEMOGR_05" | trialid == "DEMOGR_10" | trialid == "DEMOGR_11" & RAW =="Postgrado incompleta" ~ "10", 
          trialid == "DEMOGR_05" | trialid == "DEMOGR_10" | trialid == "DEMOGR_11" & RAW =="Postgrado completa" ~ "11", 
          trialid == "DEMOGR_05" | trialid == "DEMOGR_10" | trialid == "DEMOGR_11" & RAW =="Magíster o Doctorado incompleta" ~ "12", 
          trialid == "DEMOGR_05" | trialid == "DEMOGR_10" | trialid == "DEMOGR_11" & RAW =="Magíster o Doctorado completa" ~ "13", 
          
          trialid == "DEMOGR_06" ~ RAW,
          trialid == "DEMOGR_07" ~ RAW,
          trialid == "DEMOGR_08" ~ RAW,
          trialid == "DEMOGR_09" ~ RAW,
          
          trialid == "DEMOGR_11" ~ RAW,
          
          trialid == "DEMOGR_12" ~ RAW,
          trialid == "DEMOGR_13" ~ RAW,
          trialid == "DEMOGR_14" ~ RAW,
          
          trialid == "DEMOGR_15" & RAW == "No" ~ "0",
          trialid == "DEMOGR_15" & RAW == "Si" ~ "1",
          
          trialid == "DEMOGR_16" ~ RAW,
          
          trialid == "DEMOGR_17" & RAW == "No" ~ "0",
          trialid == "DEMOGR_17" & RAW == "Si" ~ "1",
          trialid == "DEMOGR_17" & RAW == "No se" ~ "2",
          
          
          trialid == "DEMOGR_18" ~ RAW,
          
          trialid == "DEMOGR_19" & RAW == "" ~ "0",
          trialid == "DEMOGR_19" & RAW == "Madre" ~ "1",
          trialid == "DEMOGR_19" & RAW == "Hermanas" ~ "2",
          trialid == "DEMOGR_19" & RAW == "Amigas cercanas" ~ "3",
          trialid == "DEMOGR_19" & RAW == "Madre; Hermanas" ~ "4",
          trialid == "DEMOGR_19" & RAW == "Madre; Amigas cercanas" ~ "5",
          trialid == "DEMOGR_19" & RAW == "Hermanas; Amigas cercanas" ~ "6",
          trialid == "DEMOGR_19" & RAW == "Madre; Hermanas; Amigas cercanas" ~ "7",
          
          # trialid == "DEMOGR_16" & RAW == "Parto por cesárea (parto quirúrgico de un infante a través de una incisión en el abdomen y útero de la madre)" ~ "0",
          # trialid == "DEMOGR_16" & RAW == "Parto vaginal (parto que ocurre por vía vaginal, con o sin intervenciones como anestesia)" ~ "1",
          
          
          is.na(RAW) ~ NA_character_,
          grepl(items_to_ignore, trialid) ~ NA_character_,
          TRUE ~ "9999"
        )
    )

    
    # [END ADAPT]: ***************************************************************
    # ****************************************************************************
    
    
    # Create DF_wide_RAW_DIR -----------------------------------------------------
    DF_wide_RAW =
      DF_long_DIR %>% 
      tidyr::pivot_wider(
        names_from = trialid, 
        values_from = c(RAW, DIR),
        names_glue = "{trialid}_{.value}") %>% 
      
      # NAs for RAW and DIR items
      dplyr::mutate(!!names_list$name_RAW_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_RAW")))),
             !!names_list$name_DIR_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_DIR")))))
  
  
  DF_wide_RAW_DIR =
    DF_wide_RAW %>% 
    
    # Mach a simplified version of the strings (lower case, only standard characters and no extra space at the end)
    dplyr::mutate(COMUNA = tolower(stringi::stri_trans_general(str = DEMOGR_06_DIR, id = "Latin-ASCII")) %>% gsub(" $", "", .)) %>% 
    dplyr::left_join(DF_lookup %>% dplyr::mutate(COMUNA = tolower(stringi::stri_trans_general(str = comuna, id = "Latin-ASCII")) %>% gsub(" $", "", .)), 
              by = c("COMUNA")) %>% 
    dplyr::mutate(DEMOGR_comuna_DIRd = ifelse(is.na(DEMOGR_comuna_DIRd), paste0("Not found: ", DEMOGR_06_DIR), DEMOGR_comuna_DIRd)) %>%
   dplyr::select(-COMUNA, -comuna) %>% 
    
    # dplyr::left_join(DF_lookup, by = c("DEMOGR_06_DIR" = "comuna")) %>% 
    # dplyr::mutate(DEMOGR_comuna_DIRd = ifelse(is.na(DEMOGR_comuna_DIRd), paste0("Not found: ", DEMOGR_06_DIR), DEMOGR_comuna_DIRd)) %>% 
  
    
    # [ADAPT]: Scales and dimensions calculations --------------------------------
    # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)
    
    dplyr::mutate(
      !!names_list$name_DIRd[1] := get(paste0(short_name_scale_str, "_", items_DIRd1, "_DIR")), 
      !!names_list$name_DIRd[2] := get(paste0(short_name_scale_str, "_", items_DIRd2, "_DIR"))
    )
    
    # [END ADAPT]: ***************************************************************
    # ****************************************************************************


  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
