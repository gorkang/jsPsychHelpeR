##' Prepare CMApost
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_CMApost -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_CMApost
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_CMApost <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # debug_function(prepare_CMApost)

  # words_list = list(one = c('Hotel', 'River', 'Tree', 'Skin', 'Gold', 'Market', 'Paper', 'Child', 'King', 'Book'), 
  #                   two = c('Sky', 'Ocean', 'Flag', 'Dollar', 'Wife', 'Machine', 'Home', 'Earth', 'College', 'Butter'),
  #                   three = c('Woman', 'Rock', 'Blood', 'Corner', 'Shoes', 'Letter', 'Girl', 'House', 'Valley', 'Engine'),
  #                   four = c('Water', 'Church', 'Doctor', 'Palace', 'Fire', 'Garden', 'Sea', 'Village', 'Baby', 'Table'))
  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  ## NameDimension1, NameDimension2 should be the names of the dimensions
  ## Inside each c() create a vector of the item numbers for the dimension
  ## Add lines as needed. If there are no dimensions, keep as is
  items_dimensions = list(
    NameDimension1 = c("000"),
    NameDimension2 = c("000")
  )
  
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
                                help_prepare = TRUE) # Show n of items, responses,... [CHANGE to FALSE] 
  
  
  

  # Match pre and post words ------------------------------------------------

    # GET CMApre items to EXTRACT condition (list of words shown)
    DICC_MemoryCondition = 
      # Get data from CMApre TASK
      DF_clean |> dplyr::filter(trialid == "CMApre_004") |> dplyr::select(id, stimulus) |>  
      # Split stimulus to get a 10 item vector
      dplyr::rowwise() |> 
      dplyr::mutate(WORDSseen = stringr::str_split(gsub("\\[|\\]", "", stimulus), pattern = ","))
    
    # DF_long_RAW |>
    #   dplyr::left_join(DICC_MemoryCondition, by = "id") |> 
    #  dplyr::select(id, trialid, RAW, WORDSseen) |>
    #   # dplyr::filter(id %in% c("989898", "testing")) |> 
    #   dplyr::rowwise() |>
    #   dplyr::mutate(CMApost_Matches_DIRd = length(intersect(unlist(stringr::str_split(RAW, pattern = "; ")), 
    #                                     unlist(WORDSseen))),
    #          CMApost_Responses_DIRd = length(unlist(stringr::str_split(RAW, pattern = "; "))),
    #          CMApost_Misses_DIRd = CMApost_Responses_DIRd - CMApost_Matches_DIRd)
    # 
    # 
  
  # Create long DIR ------------------------------------------------------------
  DF_long_DIR = 
    DF_long_RAW |> 
   dplyr::select(id, trialid, RAW) |>
    
    
    
  # [ADAPT 2/3]: RAW to DIR for individual items -------------------------------
  # ****************************************************************************
  
    # Transformations
    dplyr::mutate(
      DIR = RAW
    ) |> 
    
    
    
    # HOW MANY WORDS MATCH?
    dplyr::left_join(DICC_MemoryCondition, by = "id") |>
    #dplyr::select(id, trialid, RAW, WORDSseen) |>
    # dplyr::filter(id %in% c("989898", "testing")) |>
    dplyr::rowwise() |>
    dplyr::mutate(CMApost_Matches_DIRd = length(intersect(unlist(stringr::str_split(DIR, pattern = "; ")),
                                      unlist(WORDSseen))),
           CMApost_Responses_DIRd = length(unlist(stringr::str_split(DIR, pattern = "; "))),
           CMApost_Misses_DIRd = CMApost_Responses_DIRd - CMApost_Matches_DIRd) |> 
   dplyr::select(-WORDSseen) |> 
    dplyr::rename(CMApost_Stimulus_DIRd = stimulus)

    
  # [END ADAPT 2/3]: ***********************************************************
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
  
  # Reliability -------------------------------------------------------------
  # REL1 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_dimensions[[1]])
  # items_RELd1 = REL1$item_selection_string
    
  
  # [USE STANDARD NAMES FOR Scales and dimensions: names_list$name_DIRd[1], names_list$name_DIRt,...] 
  # CHECK with: create_formulas(type = "dimensions_DIR", functions = "sum", names(items_dimensions))
  DF_wide_RAW_DIR =
    DF_wide_RAW |> 
    dplyr::mutate(

      # [CHECK] Using correct formula? rowMeans() / rowSums()
      
      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!names_list$name_DIRd[1] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR"))), na.rm = TRUE),  
      # !!names_list$name_DIRd[2] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR"))), na.rm = TRUE),
      
      # Reliability Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!names_list$name_RELd[1] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_RELd1, "_DIR"))), na.rm = TRUE), 

      # Score Scale
      # !!names_list$name_DIRt := rowSums(across(all_of(matches("_DIR$"))), na.rm = TRUE)
      
    ) |> 
   dplyr::select(id, matches("006"), matches("_NA$"), dplyr::everything())
    
  # [END ADAPT 3/3]: ***********************************************************
  # ****************************************************************************


  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE, output_formats = output_formats)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
