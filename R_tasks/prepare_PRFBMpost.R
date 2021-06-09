##' Prepare PRFBMpost
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_PRFBMpost -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_PRFBMpost
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_PRFBMpost <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_PRFBMpost)

  # Standardized names ------------------------------------------------------
  standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = c("Preferencia", "FuerzaPreferencia", "MotivosMadreBeneficio", "MotivosBebeBeneficio", "MotivosMadreDaño", "MotivosBebeDaño"), # Use names of dimensions, "" or comment out line
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
          trialid == "PRFBMpost_01" & RAW == "Parto por cesárea (extracción del bebé por medio de una cirugía con anestesia. Se realiza una incisión abdominal y una incisión para abrir el útero)" ~ 0,
          trialid == "PRFBMpost_01" & RAW == "Parto vaginal (extracción del bebé por el canal vaginal en forma espontánea, con o sin intervenciones como anestesia)" ~ 1,
          
          RAW == "Muy en desacuerdo" ~ 0,
          RAW == "En desacuerdo" ~ 1,
          RAW == "Parcialmente en desacuerdo" ~ 2,
          RAW == "Parcialmente de acuerdo" ~ 3,
          RAW == "De acuerdo" ~ 4,
          RAW == "Muy de acuerdo" ~ 5,
          
          trialid != "PRFBMpost_01" ~ as.numeric(RAW),
          
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
          grepl(items_to_reverse, trialid) ~ (6 - DIR),
          TRUE ~ DIR
        )
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
      !!name_DIRd1 := rowMeans(select(., matches("01") & matches("_DIR$")), na.rm = TRUE), 
      !!name_DIRd2 := rowMeans(select(., matches("02|03") & matches("_DIR$")), na.rm = TRUE), 
      !!name_DIRd3 := rowMeans(select(., matches("04_beneficio|06_beneficio") & matches("_DIR$")), na.rm = TRUE), 
      !!name_DIRd4 := rowMeans(select(., matches("05_beneficio|07_beneficio") & matches("_DIR$")), na.rm = TRUE),
      !!name_DIRd5 := rowMeans(select(., matches("04_daño|06_daño") & matches("_DIR$")), na.rm = TRUE), 
      !!name_DIRd6 := rowMeans(select(., matches("05_daño|07_daño") & matches("_DIR$")), na.rm = TRUE)
      
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
