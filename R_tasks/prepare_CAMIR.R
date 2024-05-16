##' Prepare CAMIR
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_CAMIR -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_CAMIR
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_CAMIR <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_CAMIR)
  
  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  ## NameDimension1, NameDimension2 should be the names of the dimensions
  ## Inside each c() create a vector of the item numbers for the dimension
  ## Add lines as needed. If there are no dimensions, keep as is
  items_dimensions = list(
    SeguridadDisponibilidadApoyoFigurasApego = c("003", "006", "007", "011", "013", "021", "030"),
    PreocupacionFamiliar = c("012", "014", "018", "026", "031", "032"),
    InterferenciaPadres = c("004", "020", "025", "027"),
    ValorAutoridadPadres = c("005", "019", "029"),
    PermisividadParental = c("002", "015", "022"),
    AutosuficienciaRencorPadres = c("008", "009", "016", "024"),
    TraumatismoInfantil = c("001", "010", "017", "023", "028")
  )
   
    
    items_dimensionsT = list(
      
    # T scores: (((Ss mean - Study mean)/Study SD)*10)+50
    SeguridadDisponibilidadApoyoFigurasApegoT = "",
    PreocupacionFamiliarT = "",
    InterferenciaPadresT = "",
    ValorAutoridadPadresT = "",
    PermisividadParentalT = "",
    AutosuficienciaRencorPadresT = "",
    TraumatismoInfantilT = ""
    
  )
    
    names_listT = standardized_names(short_name_scale = short_name_scale_str, 
                                    dimensions = names(items_dimensionsT),
                                    help_names = FALSE)$name_DIRd 
    
  
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
         RAW == "Muy falso" ~ 1,
         RAW == "Falso" ~ 2,
         RAW == "Ni verdadero ni falso" ~ 3,
         RAW == "Verdadero" ~ 4,
         RAW == "Muy verdadero" ~ 5,
          is.na(RAW) ~ NA_real_, # OR NA_character_,
          trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_real_, # OR NA_character_,
          TRUE ~ 9999 # OR "9999"
        )
    ) %>% 
    
    # Invert items [CAN BE DELETED IF NOT USED or DIR is non-numeric]
    dplyr::mutate(
      DIR = 
       dplyr::case_when(
          DIR == 9999 ~ DIR, # To keep the missing values unchanged
          trialid %in% paste0(short_name_scale_str, "_", items_to_reverse) ~ (6 - DIR), # REVIEW and replace 6 by MAX + 1
          TRUE ~ DIR
        )
    )
    
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
  DF_wide_RAW_DIR_temp =
    DF_wide_RAW %>% 
    dplyr::mutate(

      # [CHECK] Using correct formula? rowMeans() / rowSums()
      
      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      !!names_list$name_DIRd[1] := rowMeans(select(., paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[2] := rowMeans(select(., paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[3] := rowMeans(select(., paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[4] := rowMeans(select(., paste0(short_name_scale_str, "_", items_dimensions[[4]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[5] := rowMeans(select(., paste0(short_name_scale_str, "_", items_dimensions[[5]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[6] := rowMeans(select(., paste0(short_name_scale_str, "_", items_dimensions[[6]], "_DIR")), na.rm = TRUE),
      !!names_list$name_DIRd[7] := rowMeans(select(., paste0(short_name_scale_str, "_", items_dimensions[[7]], "_DIR")), na.rm = TRUE),
      
        
      # Score Scale
      !!names_list$name_DIRt := rowSums(select(., matches("_DIR$")), na.rm = TRUE)
      
    ) 
    # select(names_list$name_DIRd[1])
    
  # names_list$name_DIRd
  DF_summary = DF_wide_RAW_DIR_temp |> 
    select(names_list$name_DIRd) |> 
    pivot_longer(cols = everything()) |> 
    group_by(name) |> 
    summarise(MEAN = mean(value), SD = sd(value))
  
  # DF_summary[1,]$MEAN
  # X2 = (((get(names_list$name_DIRd[1]) - DF_summary[DF_summary$name == names_list$name_DIRd[1],]$MEAN))),
  
  
  DF_wide_RAW_DIR = 
    DF_wide_RAW_DIR_temp |>
    # select(names_list$name_DIRd[1]) %>% 
    mutate(   
      # T scores: (((Ss mean - Study mean)/Study SD)*10)+50
      !!names_listT[1] := (((get(names_list$name_DIRd[1]) - DF_summary[DF_summary$name == names_list$name_DIRd[1], ]$MEAN) / DF_summary[DF_summary$name == names_list$name_DIRd[1], ]$SD) * 10) + 50, 
      !!names_listT[2] := (((get(names_list$name_DIRd[2]) - DF_summary[DF_summary$name == names_list$name_DIRd[2], ]$MEAN) / DF_summary[DF_summary$name == names_list$name_DIRd[2], ]$SD) * 10) + 50, 
      !!names_listT[3] := (((get(names_list$name_DIRd[3]) - DF_summary[DF_summary$name == names_list$name_DIRd[3], ]$MEAN) / DF_summary[DF_summary$name == names_list$name_DIRd[3], ]$SD) * 10) + 50, 
      !!names_listT[4] := (((get(names_list$name_DIRd[4]) - DF_summary[DF_summary$name == names_list$name_DIRd[4], ]$MEAN) / DF_summary[DF_summary$name == names_list$name_DIRd[4], ]$SD) * 10) + 50, 
      !!names_listT[5] := (((get(names_list$name_DIRd[5]) - DF_summary[DF_summary$name == names_list$name_DIRd[5], ]$MEAN) / DF_summary[DF_summary$name == names_list$name_DIRd[5], ]$SD) * 10) + 50,
      !!names_listT[6] := (((get(names_list$name_DIRd[6]) - DF_summary[DF_summary$name == names_list$name_DIRd[6], ]$MEAN) / DF_summary[DF_summary$name == names_list$name_DIRd[6], ]$SD) * 10) + 50,
      !!names_listT[7] := (((get(names_list$name_DIRd[7]) - DF_summary[DF_summary$name == names_list$name_DIRd[7], ]$MEAN) / DF_summary[DF_summary$name == names_list$name_DIRd[7], ]$SD) * 10) + 50
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
