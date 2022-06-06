##' Prepare FauxPasEv
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_FauxPasEv -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_FauxPasEv
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_FauxPasEv <- function(DF_clean, short_name_scale_str) {

  
  # Cada item, 9 preguntas.
  # 1 = instrucciones.
  # 2 = si/no
  # 3 = Sara, Alicia
  # 4 = ABIERTA
  # 5 = ABIERTA
  # 6 = ABIERTA
  # 7 = ABIERTA
  
  # DF_long_RAW %>% distinct(trialid)
  # 1:180
  # 2:9
  
  # DEBUG
  # targets::tar_load_globals()
  # debug_function(prepare_FauxPasEv)

  # [ADAPT]: Items to ignore and reverse ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  names_dimensions = c("") # If no dimensions, keep names_dimensions = c("")
  
  items_DIRd1 = c("")
  items_DIRd2 = c("")
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  # Standardized names ------------------------------------------------------
  standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = names_dimensions, # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE, is_experiment = FALSE)
  
  # Show number of items, responses, etc. [uncomment to help prepare the test] 
  # prepare_helper(DF_long_RAW, show_trialid_questiontext = TRUE)
  
  
  # Create long DIR ------------------------------------------------------------
  
  DF_long_DIR = 
    DF_long_RAW %>% 
    select(id, trialid, RAW) %>%
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
    # Transformations
    mutate(
      DIR =
        case_when(
          
        # Pregunta 1 / Item 2/9
          # Historias con Faux Pas
          trialid == 'faux_pas_ev_011' & RAW == 'Si' ~ 1,
          trialid == 'faux_pas_ev_029' & RAW == 'Si' ~ 1,
          trialid == 'faux_pas_ev_056' & RAW == 'Si' ~ 1,
          trialid == 'faux_pas_ev_092' & RAW == 'Si' ~ 1,
          trialid == 'faux_pas_ev_101' & RAW == 'Si' ~ 1,
          trialid == 'faux_pas_ev_110' & RAW == 'Si' ~ 1,
          trialid == 'faux_pas_ev_119' & RAW == 'Si' ~ 1,
          trialid == 'faux_pas_ev_128' & RAW == 'Si' ~ 1,
          trialid == 'faux_pas_ev_137' & RAW == 'Si' ~ 1,
          trialid == 'faux_pas_ev_155' & RAW == 'Si' ~ 1,
          trialid == 'faux_pas_ev_011' & RAW == 'No' ~ 0,
          trialid == 'faux_pas_ev_029' & RAW == 'No' ~ 0,
          trialid == 'faux_pas_ev_056' & RAW == 'No' ~ 0,
          trialid == 'faux_pas_ev_092' & RAW == 'No' ~ 0,
          trialid == 'faux_pas_ev_101' & RAW == 'No' ~ 0,
          trialid == 'faux_pas_ev_110' & RAW == 'No' ~ 0,
          trialid == 'faux_pas_ev_119' & RAW == 'No' ~ 0,
          trialid == 'faux_pas_ev_128' & RAW == 'No' ~ 0,
          trialid == 'faux_pas_ev_137' & RAW == 'No' ~ 0,
          trialid == 'faux_pas_ev_155' & RAW == 'No' ~ 0,
          
          # Historias sin Faux Pas
          trialid == 'faux_pas_ev_002' & RAW == 'No' ~ 2,
          trialid == 'faux_pas_ev_020' & RAW == 'No' ~ 2,
          trialid == 'faux_pas_ev_038' & RAW == 'No' ~ 2,
          trialid == 'faux_pas_ev_047' & RAW == 'No' ~ 2,
          trialid == 'faux_pas_ev_065' & RAW == 'No' ~ 2,
          trialid == 'faux_pas_ev_074' & RAW == 'No' ~ 2,
          trialid == 'faux_pas_ev_083' & RAW == 'No' ~ 2,
          trialid == 'faux_pas_ev_146' & RAW == 'No' ~ 2,
          trialid == 'faux_pas_ev_002' & RAW == 'Si' ~ 0,
          trialid == 'faux_pas_ev_020' & RAW == 'Si' ~ 0,
          trialid == 'faux_pas_ev_038' & RAW == 'Si' ~ 0,
          trialid == 'faux_pas_ev_047' & RAW == 'Si' ~ 0,
          trialid == 'faux_pas_ev_065' & RAW == 'Si' ~ 0,
          trialid == 'faux_pas_ev_074' & RAW == 'Si' ~ 0,
          trialid == 'faux_pas_ev_083' & RAW == 'Si' ~ 0,
          trialid == 'faux_pas_ev_146' & RAW == 'Si' ~ 0,
          
          
          
          is.na(RAW) ~ NA_real_,
          grepl(items_to_ignore, trialid) ~ NA_real_,
          TRUE ~ 9999
        )
    ) %>% 
    
    # Invert items
    mutate(
      DIR = 
        case_when(
          DIR == 9999 ~ DIR, # To keep the missing values unchanged
          trialid %in% paste0(short_name_scale_str, "_", items_to_reverse) ~ (6 - DIR),
          TRUE ~ DIR
        )
    )
    
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
    

  # Create DF_wide_RAW_DIR -----------------------------------------------------
  DF_wide_RAW =
    DF_long_DIR %>% 
    pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR),
      names_glue = "{trialid}_{.value}") %>% 
    
    # NAs for RAW and DIR items
    mutate(!!name_RAW_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_RAW")) & matches("_RAW$")))),
           !!name_DIR_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_DIR")) & matches("_DIR$")))))
  
  
  # Reliability -------------------------------------------------------------
  
  # REL1 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_DIRd1)
  # items_RELd1 = REL1$item_selection_string
    
  
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)

  DF_wide_RAW_DIR =
    DF_wide_RAW %>% 
    mutate(

      # Make sure to use the correct formula: rowMeans() / rowSums()
      
      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!name_DIRd1 := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd1, "_DIR")), na.rm = TRUE), 
      # !!name_DIRd2 := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd2, "_DIR")), na.rm = TRUE),
      
      # Reliability Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!name_RELd1 := rowMeans(select(., paste0(short_name_scale_str, "_", items_RELd1, "_DIR")), na.rm = TRUE), 

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
