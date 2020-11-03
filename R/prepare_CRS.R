##' Prepare CRS
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_TEMPLATE -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_CRS
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_CRS <- function(DF_clean, short_name_scale_str) {
  
  # DEBUG
  # debug_function(prepare_CRS)

  # Standardized names ------------------------------------------------------
  standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = c("Intelectual", "Ideologica", "PracticaPublica", "PracticaPrivada", "ExperienciaReligiosa"), # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE)
  
  # Show number of items, responses, etc. [uncomment to help prepare the test] 
  # prepare_helper(DF_long_RAW, show_trialid_questiontext = TRUE)
  
  
  # Create long DIR ------------------------------------------------------------
  DF_long_DIR = 
    DF_long_RAW %>% 
    select(id, trialid, RAW) %>%
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
    mutate(
      DIR =
        case_when(
          RAW == "Muy a menudo" ~ 5,
          RAW == "A menudo" ~ 4,
          RAW == "Ocasionalmente" ~ 3,
          RAW == "Rara vez" ~ 2,
          RAW == "Nunca" ~ 1,
          
          RAW == "Demasiado" ~ 5,
          RAW == "Mucho"~ 4,
          RAW == "Algo" ~ 3,
          RAW == "Un poco" ~ 2,
          RAW == "Nada" ~ 1,

          TRUE ~ 9999
        )) #%>% filter(DIR == 9999) %>% distinct(RAW)
    
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
    mutate(!!name_RAW_NA := rowSums(is.na(select(., matches("_RAW")))),
           !!name_DIR_NA := rowSums(is.na(select(., matches("_DIR"))))) %>% 
      
    
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)
  # Cinco dimensiones: (1) Intelectual (itemes 1, 6, 11), (2) Ideológica (itemes 2, 7, 12), (3) Pártica pública (itemes 3, 8, 13),  (4) Práctica privada (itemes 4, 9, 14), (5) Experiencia religiosa (itemes 5, 10, 15).
    mutate(

      # Score Dimensions (use 3 digit item numbers)
      !!name_DIRd1 := rowMeans(select(., matches("01|06|11") & matches("_DIR$")), na.rm = TRUE), 
      !!name_DIRd2 := rowMeans(select(., matches("02|07|12") & matches("_DIR$")), na.rm = TRUE),
      !!name_DIRd3 := rowMeans(select(., matches("03|08|13") & matches("_DIR$")), na.rm = TRUE),
      !!name_DIRd4 := rowMeans(select(., matches("04|09|14") & matches("_DIR$")), na.rm = TRUE),
      !!name_DIRd5 := rowMeans(select(., matches("05|10|15") & matches("_DIR$")), na.rm = TRUE),
      
      # Score Scale
      !!name_DIRt := rowMeans(select(., matches("_DIR$")), na.rm = TRUE)
      
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
