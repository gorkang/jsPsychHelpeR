##' Prepare CRTv
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_CRTv -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_CRTv
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_CRTv <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_CRTv)

  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  ## NameDimension1, NameDimension2 should be the names of the dimensions
  ## Inside each c() create a vector of the item numbers for the dimension
  ## Add lines as needed. If there are no dimensions, keep as is
  items_dimensions = list(
    NameDimension1 = c("000")
  )
  
  # [END ADAPT 1/3]: ***********************************************************
  # ****************************************************************************
  
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     # dimensions = c("NameDimension1", "NameDimension2"), # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE, help_prepare = FALSE)
  
  
  # Create long DIR ------------------------------------------------------------
  DF_long_DIR = 
    DF_long_RAW |> 
   dplyr::select(id, trialid, RAW) |>
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
  # [REMEMBER]: These regular expressions are most likely WRONG
    dplyr::mutate(
      DIR =
       dplyr::case_when(
          grepl("001", trialid) & grepl("mar[i-í]a", RAW, ignore.case = T) ~ 1,
          grepl("002", trialid) & (grepl("segundo", RAW, ignore.case = T) | grepl("2", RAW, ignore.case = T) | grepl("dos", RAW, ignore.case = T)) ~ 1,
          ##No captura alguna: "Se verá cuando mueran"
          grepl("003", trialid) & ((grepl("ningun", RAW, ignore.case = T) & !grepl("muerto", RAW, ignore.case = T)) |
                                    grepl("breviv", RAW, ignore.case = T) | grepl("vivo", RAW, ignore.case = T)  | grepl("no est[aá]n", RAW, ignore.case = T)) ~ 1,
          grepl("004", trialid) & ((!grepl("p.jaro", RAW) & !grepl("ardilla", RAW, ignore.case = T) & !grepl("mono", RAW, ignore.case = T))) ~ 1,
          grepl("005", trialid) & grepl("no", RAW, ignore.case = T) ~ 1,
          # No captura: Suponiendo que existió Moises y dicha arca, a lo menos 2
          grepl("006", trialid) &  ((grepl("no", RAW, ignore.case = T) | grepl("ninguno", RAW, ignore.case = T) | (grepl("no[ée]", RAW, ignore.case = T))) |
            (!grepl("no se", RAW) & !grepl("", RAW) & !grepl(" ", RAW) & !grepl("[12]", RAW, ignore.case = T) & !grepl("[unodos]", RAW, ignore.case = T)))  ~ 1,
          grepl("007", trialid) & ((grepl("humo", RAW, ignore.case = T) | grepl("el[e-é]ctrico", RAW, ignore.case = T))) ~ 1,
          grepl("008", trialid) & grepl("f[o-ó][so]foro", RAW, ignore.case = T) ~ 1,
          grepl("009", trialid) & (!grepl("sí", RAW, ignore.case = T) & (!grepl("por qu[eé] no", RAW, ignore.case = T)) &
                                    (grepl("muerto", RAW, ignore.case = T) | grepl("muri[oó]", RAW, ignore.case = T) |
                                       grepl("imposible", RAW, ignore.case = T) |
                                       (grepl("no", RAW, ignore.case = T) & !grepl("cuñada", RAW, ignore.case = T))))  ~ 1,
          
          grepl("010", trialid) & ((grepl("ninguna", RAW, ignore.case = T) | grepl("amarilla", RAW, ignore.case = T))) ~ 1,
          TRUE ~ 0
        )
      ) 

  
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

      # Score Scale
      !!names_list$name_DIRt := rowSums(across(all_of(matches("_DIR$"))), na.rm = TRUE)
      
    )
    
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************


  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE, output_formats = output_formats)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
