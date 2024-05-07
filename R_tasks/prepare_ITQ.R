##' Prepare ITQ
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_ITQ -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_ITQ
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_ITQ <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_ITQ)
  

  
  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  ## NameDimension1, NameDimension2 should be the names of the dimensions
  ## Inside each c() create a vector of the item numbers for the dimension
  ## Add lines as needed. If there are no dimensions, keep as is
  items_dimensions = list(
    
    ExperienciaTraumatica = c("001"),
    CuandoOcurrio = c("002"),
    
    # PTSD
    ReExperimentacion = c("003", "004"),
    Evitacion = c("005", "006"),
    SensacionAmenza = c("007", "008"),
    PTSD = c("003", "004", "005", "006", "007", "008"),
    
    # AAO
    DesregulacionAfectiva = c("012", "013"),
    AutoconceptoNegativo = c("014", "015"),
    AlteracionesRelaciones = c("016", "017"),
    AlteracionesAutoOrganizacion = c("012", "013", "014", "015", "016", "017")
    
    # PTSDdiagnostico
    # DeterioroFuncionalTept = c("009", "010", "011"),
    # AutoOrganizacionC = c("018", "019", "020"),
    
  )
  
  # HAY 10 DIMENSIONES DIAGNOSTICAS (SI/NO) Y 10 DIMENSIONES SUMATORIAS.
  # DEFINIR SI SE HACEN LAS 20, O SOLO 10.
  
  
  
  
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
    DF_long_RAW %>% 
    # If using keep_time = TRUE above, use this and add timestamp to the select() call
    # dplyr::mutate(timestamp = as.POSIXlt(datetime, format = "%Y-%m-%dT%H%M%S")) |> 
    dplyr::select(id, trialid, RAW) %>%
    
    
    
  # [ADAPT 2/3]: RAW to DIR for individual items -------------------------------
  # ****************************************************************************
  
    # Transformations
    dplyr::mutate(
      DIR =
       dplyr::case_when(
         trialid %in% c("ITQ_001", "ITQ_002") ~ RAW,
         RAW == "Nada" ~ "0",
         RAW == "Poco" ~ "1",
         RAW == "Moderadamente" ~ "2",
         RAW == "Bastante" ~ "3",
         RAW == "Extremadamente" ~ "4",
          is.na(RAW) ~ NA_character_,
          trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_character_,
          TRUE ~ "9999"
        )
    ) 
    
    # Invert items [CAN BE DELETED IF NOT USED or DIR is non-numeric]
    # dplyr::mutate(
    #   DIR = 
    #    dplyr::case_when(
    #       DIR == 9999 ~ DIR, # To keep the missing values unchanged
    #       trialid %in% paste0(short_name_scale_str, "_", items_to_reverse) ~ (6 - DIR), # REVIEW and replace 6 by MAX + 1
    #       TRUE ~ DIR
    #     )
    # )
    
  # [END ADAPT 2/3]: ***********************************************************
  # ****************************************************************************
    

  # Create DF_wide_RAW_DIR -----------------------------------------------------
  DF_wide_RAW =
    DF_long_DIR %>% 
    tidyr::pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR),
      names_glue = "{trialid}_{.value}") %>% 
    
    # NAs for RAW and DIR items
    dplyr::mutate(!!names_list$name_RAW_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_RAW")) & matches("_RAW$")))),
           !!names_list$name_DIR_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_DIR")) & matches("_DIR$")))))


  
  # [ADAPT 3/3]: Scales and dimensions calculations ----------------------------
  # ****************************************************************************
  
  # Reliability -------------------------------------------------------------
  # REL1 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_dimensions[[1]])
  # items_RELd1 = REL1$item_selection_string
    
  
  # [USE STANDARD NAMES FOR Scales and dimensions: names_list$name_DIRd[1], names_list$name_DIRt,...] 
  # CHECK with: create_formulas(type = "dimensions_DIR", functions = "sum", names(items_dimensions))
  DF_wide_RAW_DIR =
    DF_wide_RAW %>% 
    
    mutate(across(paste0("ITQ_", create_number_series("3-20"), "_DIR"), as.numeric)) %>% 
    dplyr::mutate(

      # [CHECK] Using correct formula? rowMeans() / rowSums()
      
      !!names_list$name_DIRd[1] := get(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR")),
      !!names_list$name_DIRd[2] := get(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR")),
      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      !!names_list$name_DIRd[3] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[4] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[4]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[5] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[5]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[6] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[6]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[7] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[7]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[8] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[8]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[9] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[9]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[10] := rowSums(select(., paste0(short_name_scale_str, "_", items_dimensions[[10]], "_DIR")), na.rm = TRUE)
      
      # Score Scale
      # !!names_list$name_DIRt := rowSums(select(., matches("_DIR$")), na.rm = TRUE)
      
    ) |> 

  
  # Criterios diagnosticos
    mutate(
      
      # PTSDDiag
      ITQ_ReExperimentacionDiag_DIRd = ifelse(ITQ_003_DIR > 2 | ITQ_004_DIR > 2, 1, 0),
      ITQ_EvitacionDiag_DIRd = ifelse(ITQ_005_DIR > 2 | ITQ_006_DIR > 2, 1, 0),
      ITQ_SensacionAmenzaDiag_DIRd = ifelse(ITQ_007_DIR > 2 | ITQ_008_DIR > 2, 1, 0),
      ITQ_DeterioroFuncionalDiag_DIRd = ifelse(ITQ_009_DIR > 2 | ITQ_010_DIR > 2 | ITQ_011_DIR > 2, 1, 0),
      PTSDDiag_TEMP = ifelse(ITQ_ReExperimentacionDiag_DIRd == 1 & ITQ_EvitacionDiag_DIRd == 1 & ITQ_SensacionAmenzaDiag_DIRd == 1 & ITQ_DeterioroFuncionalDiag_DIRd == 1, 1, 0),
      
      
      # PTSDComplexDiag
      ITQ_DesregulacionAfectivaDiag_DIRd = ifelse(ITQ_012_DIR > 2 | ITQ_013_DIR > 2, 1, 0),
      ITQ_AutoconceptoNegativoDiag_DIRd = ifelse(ITQ_014_DIR > 2 | ITQ_015_DIR > 2, 1, 0),
      ITQ_AlteracionesRelacionesDiag_DIRd = ifelse(ITQ_016_DIR > 2 | ITQ_017_DIR > 2, 1, 0),
      ITQ_DeterioroFuncionalAutoOrgDiag_DIRd = ifelse(ITQ_018_DIR > 2 | ITQ_019_DIR > 2 | ITQ_020_DIR > 2, 1, 0),
      
      # Diagnostico
      # Se cumplen los criterios de PTSDDiag y NO el de DeterioroFuncionalAutoOrgDiag
      ITQ_PTSDDiag_DIRd = ifelse(ITQ_ReExperimentacionDiag_DIRd == 1 & ITQ_EvitacionDiag_DIRd == 1 & ITQ_SensacionAmenzaDiag_DIRd == 1 & ITQ_DeterioroFuncionalDiag_DIRd == 1 & ITQ_DeterioroFuncionalAutoOrgDiag_DIRd == 0, 1, 0),
      
      # Se cumplen los de PTSD y TAMBIEN DeterioroFuncionalAutoOrgDiag
      ITQ_PTSDComplexDiag_DIRd = ifelse(PTSDDiag_TEMP == 1 & ITQ_DesregulacionAfectivaDiag_DIRd == 1 & ITQ_AutoconceptoNegativoDiag_DIRd == 1 & ITQ_AlteracionesRelacionesDiag_DIRd == 1 & ITQ_DeterioroFuncionalAutoOrgDiag_DIRd, 1, 0)
    ) |> 
    select(-PTSDDiag_TEMP)
  

    
  # [END ADAPT 3/3]: ***********************************************************
  # ****************************************************************************


  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
