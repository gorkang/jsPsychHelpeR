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
prepare_CRTv <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_CRTv)

  # Standardized names ------------------------------------------------------
  standardized_names(short_name_scale = short_name_scale_str, 
                     # dimensions = c("NameDimension1", "NameDimension2"), # Use names of dimensions, "" or comment out line
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
  
  # [REMEMBER]: These regular expressions are most likely WRONG
    mutate(
      DIR =
        case_when(
          grepl("01", trialid) & grepl("mar[i-í]a", RAW, ignore.case = T) ~ 1,
          grepl("02", trialid) & (grepl("segundo", RAW, ignore.case = T) | grepl("2", RAW, ignore.case = T) | grepl("dos", RAW, ignore.case = T)) ~ 1,
          ##No captura alguna: "Se verá cuando mueran"
          grepl("03", trialid) & ((grepl("ningun", RAW, ignore.case = T) & !grepl("muerto", RAW, ignore.case = T)) |
                                    grepl("breviv", RAW, ignore.case = T) | grepl("vivo", RAW, ignore.case = T)  | grepl("no est[aá]n", RAW, ignore.case = T)) ~ 1,
          grepl("04", trialid) & ((!grepl("p.jaro", RAW) & !grepl("ardilla", RAW, ignore.case = T) & !grepl("mono", RAW, ignore.case = T))) ~ 1,
          grepl("05", trialid) & grepl("no", RAW, ignore.case = T) ~ 1,
          # No captura: Suponiendo que existió Moises y dicha arca, a lo menos 2
          grepl("06", trialid) &  ((grepl("no", RAW, ignore.case = T) | grepl("ninguno", RAW, ignore.case = T) | (grepl("no[ée]", RAW, ignore.case = T))) |
            (!grepl("no se", RAW) & !grepl("", RAW) & !grepl(" ", RAW) & !grepl("[12]", RAW, ignore.case = T) & !grepl("[unodos]", RAW, ignore.case = T)))  ~ 1,
          grepl("07", trialid) & ((grepl("humo", RAW, ignore.case = T) | grepl("el[e-é]ctrico", RAW, ignore.case = T))) ~ 1,
          grepl("08", trialid) & grepl("f[o-ó][so]foro", RAW, ignore.case = T) ~ 1,
          grepl("09", trialid) & (!grepl("sí", RAW, ignore.case = T) & (!grepl("por qu[eé] no", RAW, ignore.case = T)) &
                                    (grepl("muerto", RAW, ignore.case = T) | grepl("muri[oó]", RAW, ignore.case = T) |
                                       grepl("imposible", RAW, ignore.case = T) |
                                       (grepl("no", RAW, ignore.case = T) & !grepl("cuñada", RAW, ignore.case = T))))  ~ 1,
          
          grepl("10", trialid) & ((grepl("ninguna", RAW, ignore.case = T) | grepl("amarilla", RAW, ignore.case = T))) ~ 1,
          TRUE ~ 0
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
    mutate(!!name_RAW_NA := rowSums(is.na(select(., matches("_RAW")))),
           !!name_DIR_NA := rowSums(is.na(select(., matches("_DIR"))))) %>% 
      
    
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)

    mutate(

      # Score Scale
      !!name_DIRt := rowSums(select(., matches("_DIR$")), na.rm = TRUE)
      
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
