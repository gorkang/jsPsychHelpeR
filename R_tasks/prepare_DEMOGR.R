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

  # OUTSIDE FILES -----------------------------------------------------------
  DF_lookup = read_csv("R_tasks/prepare_DEMOGR-lookup.csv", 
                       col_types = 
                         cols(
                           DEMOGR_comuna_DIRd = col_integer(), #idcomuna
                           comuna = col_character()
                         ))
  
  # Standardized names ------------------------------------------------------
  standardized_names(short_name_scale = short_name_scale_str, 
                     # dimensions = c("comuna"), # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE)
  
  # Show number of items, responses, etc. [uncomment to help prepare the test] 
  # prepare_helper(DF_long_RAW, show_trialid_questiontext = TRUE)
  
  
  # Create long DIR ------------------------------------------------------------
  
  # [ADAPT]: Items to ignore and reverse ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("00|00") # Ignore the following items: If nothing to ignore, keep "00|00"
  items_to_reverse = c("00|00") # Reverse the following items: If nothing to ignore, keep "00|00"
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  DF_long_DIR = 
    DF_long_RAW %>% 
    select(id, trialid, RAW) %>%
    
  
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
    mutate(
      DIR =
        case_when(
          trialid == "DEMOGR_01" ~ RAW,
          
          trialid == "DEMOGR_02" & RAW == "Masculino" ~ "0",
          trialid == "DEMOGR_02" & RAW == "Femenino" ~ "1",
          trialid == "DEMOGR_02" & RAW == "No binario" ~ "0",
          
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
          trialid == "DEMOGR_15" ~ RAW,
          trialid == "DEMOGR_16" ~ RAW,
          trialid == "DEMOGR_17" ~ RAW,
          trialid == "DEMOGR_18" ~ RAW,
          trialid == "DEMOGR_19" ~ RAW,
          
          # trialid == "DEMOGR_16" & RAW == "Parto por cesárea (parto quirúrgico de un infante a través de una incisión en el abdomen y útero de la madre)" ~ "0",
          # trialid == "DEMOGR_16" & RAW == "Parto vaginal (parto que ocurre por vía vaginal, con o sin intervenciones como anestesia)" ~ "1",
          
          
          
          
          
          is.na(RAW) ~ NA_character_,
          grepl(items_to_ignore, trialid) ~ NA_character_,
          TRUE ~ "9999"
        )
    )
    
    # Invert items
    # mutate(
    #   DIR = 
    #     case_when(
    #       DIR == 9999 ~ DIR, # To keep the missing values unchanged
    #       grepl(items_to_reverse, trialid) ~ (6 - DIR),
    #       TRUE ~ DIR
    #     )
    # )
    
    # [END ADAPT]: ***************************************************************
    # ****************************************************************************
    
    
    # Create DF_wide_RAW_DIR -----------------------------------------------------
    DF_wide_RAW_DIR =
      DF_long_DIR %>% 
      pivot_wider(
        names_from = trialid, 
        values_from = c(RAW, DIR),
        names_glue = "{trialid}_{.value}") %>% 
      
      # NAs for RAW and DIR items
      mutate(!!name_RAW_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_RAW")))),
             !!name_DIR_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_DIR"))))) %>% 
    
    left_join(DF_lookup, by = c("DEMOGR_06_DIR" = "comuna"))
    # left_join(DF_lookup, by = c("DEMOGR_06_DIR" = "comuna"))
  
    
    
    # [ADAPT]: Scales and dimensions calculations --------------------------------
    # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)
    
    # mutate(
    # 
    # # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
    # !!name_DIRd1 := rowSums(select(., matches("16") & matches("_DIR$")), na.rm = TRUE)
    # !!name_DIRd2 := rowSums(select(., matches("17") & matches("_DIR$")), na.rm = TRUE)
    # 
    # # Score Scale
    # # !!name_DIRt := rowSums(select(., matches("_DIR$")), na.rm = TRUE)
    # 
    # )
    
    # [END ADAPT]: ***************************************************************
    # ****************************************************************************


  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
