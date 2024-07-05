##' Prepare WaisWorkingMemory
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_WaisWorkingMemory -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_WaisWorkingMemory
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_WaisWorkingMemory <- function(DF_clean, short_name_scale_str, item_Age = NULL) {

  # DEBUG
  # targets::tar_load_globals()
  # debug_function(prepare_WaisWorkingMemory)
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
    tibble::tibble(age = "16-17", direct = c("0-10", "11", "12", "13", "14-15", "16-17", "18", "19-20", "21-22", "23-24", "25-26", "27-28", "29-30", "31-33", "34-35", "36-37", "38-39", "40-42", "43-48"), standard = 1:19) |> 
    dplyr::bind_rows(tibble::tibble(age = "18-19", direct = c("0-11", "12", "13-14", "15", "16-17", "18", "19", "20-21", "22-23", "24-25", "26-27", "28-29", "30-31", "32-33", "34-36", "37-38", "39-40", "41-43", "44-48"), standard = 1:19)) |> 
    dplyr::bind_rows(tibble::tibble(age = "20-24", direct = c("0-11", "12", "13-14", "15", "16-17", "18", "19-20", "21-22", "23-24", "25-26", "27-28", "29-30", "31-32", "33-34", "35-36", "37-38", "39-40", "41-43", "44-48"), standard = 1:19)) |> 
    dplyr::bind_rows(tibble::tibble(age = "25-29", direct = c("0-10", "11", "12", "13", "14-15", "16-17", "18", "19-20", "21-22", "23-24", "25-26", "27-28", "29-30", "31-33", "34-35", "36-37", "38-39", "40-42", "43-48"), standard = 1:19)) |> 
    dplyr::bind_rows(tibble::tibble(age = "30-34", direct = c("0-9", "10", "11", "12-13", "14", "15-16", "17-18", "19", "20-21", "22-23", "24-25", "26-27", "28-30","31-32", "33-34", "35-36", "37-38", "39-41", "42-48"), standard = 1:19)) |> 
    dplyr::bind_rows(tibble::tibble(age = "35-44", direct = c("0-9", "10", "11", "12-13", "14", "15-16", "17", "18-19", "20-21", "22-23", "24-25", "26-27", "28-30","31-32", "33-34", "35-36", "37-38", "39-40", "41-48"), standard = 1:19)) |> 
    dplyr::bind_rows(tibble::tibble(age = "45-54", direct = c("0-7", "8", "9", "10-11", "12", "13-14", "15-16", "17", "18-19", "20-21", "22-23", "24-25", "26-27", "28-30","31-32", "33-34", "35-36", "37-38", "39-48"), standard = 1:19)) |> 
    dplyr::bind_rows(tibble::tibble(age = "55-64", direct = c("0-6", "7", "8-9", "10", "11-12", "13", "14-15", "16-17", "18-19", "20", "21-22", "23-24", "25-26", "27-28", "29-30", "31-33", "34-35", "36-37", "38-48"), standard = 1:19)) |> 
    dplyr::bind_rows(tibble::tibble(age = "65-69", direct = c("0-5", "6-7", "8", "9-10", "11", "12-13", "14", "15-16", "17-18", "19-20", "21", "22-23", "24-26", "27-28", "29-30", "31-32", "33-34", "35-37", "38-48"), standard = 1:19)) |> 
    dplyr::bind_rows(tibble::tibble(age = "70-74", direct = c("0-5", "6", "7", "8-9", "10", "11-12", "13", "14-15", "16-17", "18-19", "20", "21-22", "23-24", "25-26", "27-29", "30-31", "32-33", "34-35", "36-48"), standard = 1:19)) |> 
    dplyr::bind_rows(tibble::tibble(age = "75-79", direct = c("0-5", "6", "7", "8-9", "10", "11-12", "13", "14-15", "16-17", "18-19", "20", "21-22", "23-24", "25-26", "27-28", "29-30", "31-32", "33-34", "35-48"), standard = 1:19)) |> 
    dplyr::bind_rows(tibble::tibble(age = "80-84", direct = c("0-4", "5", "6", "7", "8-9", "10", "11-12", "13-14", "15", "16-17", "18-19", "20-21", "22", "23-24", "25-26", "27-28", "29-30", "31-32", "33-48"), standard = 1:19)) |> 
    dplyr::bind_rows(tibble::tibble(age = "85-90", direct = c("0-4", "5", "6", "7", "8-9", "10", "11-12", "13-14", "15", "16-17", "18-19", "20-21", "22", "23-24", "25-26", "27-28", "29-30", "31-32", "33-48"), standard = 1:19)) |> 
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
      dplyr::bind_rows(tibble::tibble(direct = as.character(0:48))) |> 
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
  items_dimensions = list(
    Direct = c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014", "015", "016"),
    Inverse = c("019", "020", "021", "022", "023", "024", "025", "026", "027", "028", "029", "030", "031", "032", "033", "034"),
    Sequential = c("037", "038", "039", "040", "041", "042", "043", "044", "045", "046", "047", "048", "049", "050", "051", "052")
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
                                help_prepare = FALSE) # Show n of items, responses,... [CHANGE to FALSE] 
  
  

  # Create long DIR ------------------------------------------------------------
  
  DF_long_DIR =  
    DF_long_RAW |> 
    dplyr::select(id, trialid, RAW) |>
    dplyr::filter(!grepl("audio", trialid)) |> 
    dplyr::mutate(RAW = as.numeric(RAW)) |> 
    
    # Join empty DF with all items
    dplyr::bind_rows(tibble::tibble(trialid = paste0("WaisWorkingMemory_", sprintf("%03d", 1:52)), RAW = rep(0, 52))) |> 
    
    # Filter out test items
    filter(!trialid %in% c("WaisWorkingMemory_017", "WaisWorkingMemory_018", "WaisWorkingMemory_035", "WaisWorkingMemory_036")) |> 
    
    # Complete items for those that did not reach them
    tidyr::complete(id, nesting(trialid), fill = list(RAW = 0)) |> 
    tidyr::drop_na(id) |> 
    
    dplyr::mutate(DIR = as.numeric(RAW))
  
    
  
  # CHECKS
  # DF_long_DIR |> count(id)
  # DF_long_DIR |> pivot_wider(names_from = trialid, values_from = RAW) |> View()
  
    
  # [ADAPT 2/3]: RAW to DIR for individual items -------------------------------
  # ****************************************************************************
  
    # Transformations
    
    
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
      !!names_list$name_DIRd[1] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR"))), na.rm = TRUE), 
      !!names_list$name_DIRd[2] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR"))), na.rm = TRUE),
      !!names_list$name_DIRd[3] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR"))), na.rm = TRUE),
      # !!names_list$name_DIRd[4] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[4]], "_DIR"))), na.rm = TRUE),
      
      # Reliability Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!names_list$name_RELd[1] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_RELd1, "_DIR"))), na.rm = TRUE), 

      # Score Scale
      !!names_list$name_DIRt := rowSums(across(all_of(matches("_DIR$"))), na.rm = TRUE)
      
    )
    
  # [END ADAPT 3/3]: ***********************************************************
  # ****************************************************************************

  
  
  # Standardized --------------------------------------------------------------
  
  # Only if item_Age is set we can get standard scores
  
  if (!is.null(item_Age)) {
    DF_wide_RAW_STD = 
      DF_wide_RAW_DIR |> 
      left_join(DF_age, by = "id") |>
      # select(id, age, WaisWorkingMemory_DIRt) |>
      left_join(TABLE_standard, by = c("age", "WaisWorkingMemory_DIRt" = "direct")) |> 
      rename(WaisWorkingMemory_STDt = standard) |> 
      select(-age)
  } else {
    DF_wide_RAW_STD = DF_wide_RAW_DIR
  }
  

  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_STD)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_STD, short_name_scale = short_name_scale_str, is_scale = TRUE)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_STD) 
 
}
