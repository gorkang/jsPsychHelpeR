##' Prepare WaisMatrices
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_WaisMatrices -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_WaisMatrices
##'
##' @param short_name_scale_str 
##' @param DF_clean
##' @param item_Age
##'
##' @return
##' @author gorkang
##' @export
prepare_WaisMatrices <- function(DF_clean, short_name_scale_str, item_Age = NULL) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_WaisMatrices)
  
  # item_Age = NULL
  # item_Age = "DEMOGR40_01"
  
  
  # Age DF ------------------------------------------------------------------
  
  # Used to go from direct to standardized
  if (!is.null(item_Age)) {
    
    DF_age = DF_clean |> 
      filter(trialid == item_Age) |> 
      select(id, response) |> 
      rename(age = response) |> 
      mutate(age = as.numeric(age))
  }
  
  
  # Standard table --------------------------------------------------------------
  
  # Create and expand the direct to standardized table so we can use it in a left join afterwards
  
  TABLE_raw = 
    tibble::tibble(age = "16-17", direct = c("0-1", "2", "3-4", "5","6-7", "8-10", "11-13", "14-15", "16-17", "18", "19", "20", "21", "22", "23", "24", "25", "26", NA), standard = 1:19) |>
    dplyr::bind_rows(tibble::tibble(age = "18-19", direct = c("0-2", "3", "4-5", "6", "7-9", "10-11", "12-13", "14-16", "17-18", "19","20", "21", "22", "23", "24", "25", "26", NA, NA), standard = 1:19)) |>
    dplyr::bind_rows(tibble::tibble(age = "20-24", direct = c("0-2", "3", "4-5", "6-7", "8-9", "10-11", "12-13", "14-16", "17-18", "19","20", "21", "22", "23", "24", "25", "26", NA, NA), standard = 1:19)) |>
    dplyr::bind_rows(tibble::tibble(age = "25-29", direct = c("0-2", "3", "4-5", "6-7", "8-9", "10-11", "12-13", "14-15", "16-17", "18", "19","20", "21", "22", "23", "24", "25", "26", NA), standard = 1:19)) |>
    dplyr::bind_rows(tibble::tibble(age = "30-34", direct = c("0-1", "2-3", "4", "5-6", "7-8", "9", "10-11", "12-13", "14-15", "16-17", "18-19", "20", "21", "22", "23", "24", "25", "26", NA), standard = 1:19)) |>
    dplyr::bind_rows(tibble::tibble(age = "35-44", direct = c("0-1", "2", "3-4", "5", "6-7", "8", "9-10", "11-12", "13", "14-15", "16-17", "18-19", "20", "21-22", "23", "24", "25", "26", NA), standard = 1:19)) |>
    dplyr::bind_rows(tibble::tibble(age = "45-54", direct = c("0-1", "2", "3-4", "5", "6", "7", "8-9", "10", "11-12", "13-14", "15", "16-17", "18-19", "20-21", "22", "23", "24", "25", "26"), standard = 1:19)) |>
    dplyr::bind_rows(tibble::tibble(age = "55-64", direct = c("0-1", "2", "3", NA, "4", "5", "6-7", "8", "9", "10-11", "12-13", "14-15", "16-17", "18-19", "20", "21-22", "23", "24-25", "26"), standard = 1:19)) |>
    dplyr::bind_rows(tibble::tibble(age = "65-69", direct = c("0-1", "2", "3", NA, "4", "5", "6", "7", "8", "9", "10-11", "12-13", "14-15", "16-18", "19", "20-21", "22-23", "24-25", "26"), standard = 1:19)) |>
    dplyr::bind_rows(tibble::tibble(age = "70-74", direct = c("0-1", "2", "3", NA, NA, "4", "5", "6", "7-8", "9", "10-11", "12-13", "14-15", "16-18", "19", "20-21", "22-23", "24-25", "26"), standard = 1:19)) |>
    dplyr::bind_rows(tibble::tibble(age = "75-79", direct = c("0-1", "2", "3", NA, NA, "4", "5", "6", "7", "8", "9-10", "11-13", "14-15", "16-17", "18-19", "20", "21-22", "23-24", "25-26"), standard = 1:19)) |>
    dplyr::bind_rows(tibble::tibble(age = "80-84", direct = c("0-1", "2", "3", NA, NA, NA, "4", "5", "6", "7-8", "9-10", "11-12", "13-15", "16-17", "18-19", "20", "21", "22-23", "24-26"), standard = 1:19)) |>
    dplyr::bind_rows(tibble::tibble(age = "85-90", direct = c("0-1", "2", "3", NA, NA, NA, NA, "4", "5", "6-7", "8-9", "10-12", "13-15", "16-17", "18-19", "20", "21", "22-23", "24-26"), standard = 1:19)) |>
  tidyr::drop_na() 
  
  # Long version of table
  TABLE_standard_long = 
    TABLE_raw |> 
    tidyr::separate_longer_delim(age, delim = "-") |> 
    dplyr::arrange(age, direct)
  
  
  # Function to fill missing age duplicating direct-standard columns
  fill_age <- function(age_check, TABLE_standard_long ) {
    
    # find value closest to age_check. If exists, closest_age == age_check
    ages <- TABLE_standard_long$age
    closest_age <- max(ages[ages <= age_check])
    
    # Get chunk for closest age and rename to age_check
    out_chunk = TABLE_standard_long |> dplyr::filter(age == closest_age) |> mutate(age = age_check)
    
    return(out_chunk)
    
  }
  
  
  # Recreate full table
  TABLE_standard = 18:90 |> 
    map_df(~ {
      fill_age(.x, TABLE_standard_long)
    }) |> 
    tidyr::separate_longer_delim(direct, delim = "-") |> 
    dplyr::bind_rows(tibble::tibble(direct = as.character(0:26))) |> 
    tidyr::complete(age, nesting(direct)) |>
    mutate(direct = as.numeric(direct)) |> 
    dplyr::arrange(age, direct) |> 
    tidyr::fill(standard)

  
  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  ## NameDimension1, NameDimension2 should be the names of the dimensions
  ## Inside each c() create a vector of the item numbers for the dimension
  ## Add lines as needed. If there are no dimensions, keep as is
  # items_dimensions = list(
  #   direct = sprintf("%03d", 1:26)
  # 
  # )
  
  
  
  # [END ADAPT 1/3]: ***********************************************************
  # ****************************************************************************
  
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                                  # dimensions = names(items_dimensions),
                                  help_names = FALSE) # [KEEP as FALSE]
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, 
                                short_name_scale = short_name_scale_str, 
                                numeric_responses = TRUE, # [TRUE or FALSE]
                                is_experiment = FALSE, 
                                keep_time = TRUE, # Keep time stamp for each response
                                help_prepare = FALSE) # Show n of items, responses,... [CHANGE to TRUE to debug] 

  # Time in each item  
  # DF_long_RAW |> 
  #   ggplot(aes(as.numeric(rt)/1000)) +
  #   geom_histogram() +
  #   theme_minimal()
  # 
  
  # Create long DIR ------------------------------------------------------------
  
  
  DF = DF_long_RAW |> 
    # filter(id %in% c(1970, 1948)) |>
    filter(!trialid %in% c("WaisMatrices_A", "WaisMatrices_B")) |> 
    dplyr::select(id, trialid, RAW)
  
  # Complete items 1 to 3 for those that did not have them (by default, if not present, are correct)
  DF_1_4 =  
    DF |> 
    filter(trialid %in% c("WaisMatrices_001", "WaisMatrices_002", "WaisMatrices_003", "WaisMatrices_004")) |> 
    # In WaisMatrices the first 3 items are only presented if items 4 or five are missed.
    dplyr::bind_rows(tibble::tibble(trialid = c("WaisMatrices_001","WaisMatrices_002", "WaisMatrices_003"), RAW = rep(1, 3))) |> 
    tidyr::complete(trialid, nesting(id), fill = list(RAW = 1)) |> 
    tidyr::drop_na(id) 
  
  
  # Complete the rest of the items, with all 0's
  DF_5_26 =  
    DF |> 
    # Keep WaisMatrices_004 as all have it
    filter(!trialid %in% c("WaisMatrices_001", "WaisMatrices_002", "WaisMatrices_003")) |> 
    
    dplyr::bind_rows(tibble::tibble(trialid = paste0("WaisMatrices_", sprintf("%03d", 5:26)), RAW = rep(0, 22))) |> 
    tidyr::complete(id, nesting(trialid), fill = list(RAW = 0)) |> 
    tidyr::drop_na(id) |> 
    # Get rid of WaisMatrices_004 as it is in DF_1_4
    filter(!trialid == "WaisMatrices_004") 
    
  

  # Join all
   DF_long_DIR = DF_1_4 |> bind_rows(DF_5_26) |> 
    # Transformations
    dplyr::mutate(DIR = RAW)
    
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
      
      # !!names_list$name_DIRd[1] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR"))), na.rm = TRUE),
      
      # Score Scale
      !!names_list$name_DIRt := rowSums(across(all_of(matches("_DIR$"))), na.rm = TRUE)
      
    )
    
  # [END ADAPT 3/3]: ***********************************************************
  # ****************************************************************************

  # DF_wide_RAW_DIR |> select(id, WaisMatrices_direct_DIRd) |> View()

# Standardized --------------------------------------------------------------

  # Only if item_Age is set we can get standard scores
  
  if (!is.null(item_Age)) {
    DF_wide_RAW_STD = 
      DF_wide_RAW_DIR |> 
      left_join(DF_age, by = "id") |> 
      # select(id, age, WaisMatrices_direct_DIRd) |> 
      left_join(TABLE_standard, by = c("age", "WaisMatrices_DIRt" = "direct")) |> 
      rename(WaisMatrices_STDt = standard) |> 
      select(-age)
  } else {
    DF_wide_RAW_STD = DF_wide_RAW_DIR
  }
    
  
  
  
    # WaisMatrices_DIRt
  
  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_STD)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_STD, short_name_scale = short_name_scale_str, is_scale = TRUE)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_STD) 
 
}

