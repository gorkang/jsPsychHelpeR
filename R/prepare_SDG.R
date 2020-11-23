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
prepare_SDG <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_SDG)

  # Standardized names ------------------------------------------------------
  standardized_names(short_name_scale = short_name_scale_str, 
                     # dimensions = c("NameDimension1", "NameDimension2"), # Use names of dimensions, "" or comment out line
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

    # Transformations
    mutate(
      DIR =
        case_when(
          
          trialid %in% c("SDG_01") ~ as.numeric(RAW),
          
          trialid == "SDG_02" & RAW == "Casado (a)" ~ 1,
          trialid == "SDG_02" & RAW == "Conviviente" ~ 2,
          trialid == "SDG_02" & RAW == "Viudo (a)" ~ 3,
          trialid == "SDG_02" & RAW == "Divorciado/anulado (a)" ~ 4,
          trialid == "SDG_02" & RAW == "Separado (a)" ~ 5,
          trialid == "SDG_02" & RAW == "Soltero/sin pareja" ~ 6,
          trialid == "SDG_02" & RAW == "Soltero/con pareja" ~ 7,
          
          trialid == "SDG_03" & RAW == "Sin Estudios" ~ 1,
          trialid == "SDG_03" & RAW == "Básica Incompleta" ~ 2,
          trialid == "SDG_03" & RAW == "Básica Completa" ~ 3,
          trialid == "SDG_03" & RAW == "Media Incompleta" ~ 4,
          trialid == "SDG_03" & RAW == "Media Completa" ~ 5,
          trialid == "SDG_03" & RAW == "Técnica Incompleta" ~ 6,
          trialid == "SDG_03" & RAW == "Técnica Completa" ~ 7,
          trialid == "SDG_03" & RAW == "Universitaria Incompleta" ~ 8,
          trialid == "SDG_03" & RAW == "Universitaria Completa o más" ~ 9,
          trialid == "SDG_03" & RAW == "No sabe o no aplica" ~ 10,
          
          trialid == "SDG_04" & RAW == "Si tengo registro." ~ 1,
          trialid == "SDG_04" & RAW == "No tengo registro." ~ 2,
          
          trialid == "SDG_05" ~ as.numeric(RAW),
          
          trialid == "SDG_06" & RAW == "Trabaja a tiempo completo" ~ 1,
          trialid == "SDG_06" & RAW == "Trabaja a tiempo parcial" ~ 2,
          trialid == "SDG_06" & RAW == "Trabaja esporádicamente" ~ 3,
          trialid == "SDG_06" & RAW == "Está desempleado(a), pero busca trabajo" ~ 4,
          trialid == "SDG_06" & RAW == "Es estudiante" ~ 5,
          trialid == "SDG_06" & RAW == "No trabaja, ni busca trabajo" ~ 6,
          trialid == "SDG_06" & RAW == "Es ama de casa" ~ 7,
          trialid == "SDG_06" & RAW == "Está jubilado o pensionado" ~ 8,
          trialid == "SDG_06" & RAW == "Es rentista" ~ 9,
          trialid == "SDG_06" & RAW == "No sabe/No responde" ~ 10,
          
          is.na(RAW) ~ NA_real_,
          grepl(items_to_ignore, trialid) ~ NA_real_,
          TRUE ~ 9999
        )
    # ) %>% 
    # 
    # # Invert items
    # mutate(
    #   DIR = 
    #     case_when(
    #       DIR == 9999 ~ DIR, # To keep the missing values unchanged
    #       grepl(items_to_reverse, trialid) ~ (6 - DIR),
    #       TRUE ~ DIR
    #     )
    )
    
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
      
    
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)

    mutate(

      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!name_DIRd1 := rowSums(select(., matches("02|04|05") & matches("_DIR$")), na.rm = TRUE), 
      # !!name_DIRd2 := rowSums(select(., matches("01|03|08") & matches("_DIR$")), na.rm = TRUE)
      
      # Score Scale
      # !!name_DIRt := rowSums(select(., matches("_DIR$")), na.rm = TRUE)
      
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
