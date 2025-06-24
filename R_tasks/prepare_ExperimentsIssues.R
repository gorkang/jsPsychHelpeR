##' Prepare ExperimentsIssues
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_ExperimentsIssues -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_ExperimentsIssues
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_ExperimentsIssues <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # debug_function(prepare_ExperimentsIssues)

  
  
  # [ADAPT 1/3]: Items to ignore and reverse, dimensions -----------------------
  # ****************************************************************************
  
  description_task = "" # Brief description here
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep as is
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep as is
  
  ## NameDimension1, NameDimension2 should be the names of the dimensions
  ## Inside each c() create a vector of the item numbers for the dimension
  ## Add lines as needed. If there are no dimensions, keep as is
  items_dimensions = list(
    IssuesExperiments = c("002", "004"),
    FrequencyIssues = c("001"),
    WhoIsGod = c("003"),
    Experience = c("005")
    
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
                                keep_time = FALSE, # Keep timestamp for each response
                                help_prepare = FALSE) # Show n of items, responses,... [CHANGE to FALSE] 
  
  
  # Create long DIR ------------------------------------------------------------
  DF_long_DIR = 
    DF_long_RAW %>% 
    # If using keep_time = TRUE above, use this and add timestamp to the select() call
    # dplyr::mutate(timestamp = as.POSIXlt(datetime, format = "%Y-%m-%dT%H%M%S")) |> 
    dplyr::select(id, trialid, RAW) %>%
    
    
    
  # [ADAPT 2/3]: RAW to DIR for individual items -------------------------------
  # ****************************************************************************
  
    # Transformations
    dplyr::mutate(DIR = 
      dplyr::case_when(
        trialid == "ExperimentsIssues_003" & RAW == "Of course" ~ "Me",
        trialid == "ExperimentsIssues_003" & RAW == "No, but my Research assistant is" ~ "My research assistant",
        trialid == "ExperimentsIssues_005" & RAW == "Student (Master, PhD,...)" ~ "1",
        trialid == "ExperimentsIssues_005" & RAW == "Postdoc or non-ternure track Academic" ~ "2",
        trialid == "ExperimentsIssues_005" & RAW == "Early Career Academic" ~ "3",
        trialid == "ExperimentsIssues_005" & RAW == "Senior academic" ~ "4",
        TRUE ~ RAW
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
  
  # Add if they do not exist
  cols <- c(ExperimentsIssues_001_DIR = NA_real_, ExperimentsIssues_002_DIR = NA_real_, ExperimentsIssues_003_DIR = NA_real_, ExperimentsIssues_004_DIR = NA_real_)
  
  
  
  DF_wide_RAW_DIR =
    DF_wide_RAW %>% 
    
    # Add missing columns, if any
    add_column(!!!cols[setdiff(names(cols), names(DF_wide_RAW))]) |> 
    
    tidyr::unite(!!names_list$name_DIRd[1], paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR"), sep = "; ") |> 
    
    dplyr::mutate(
      !!names_list$name_DIRd[2] := get(paste0(short_name_scale_str, "_", items_dimensions[[2]], "_DIR")),
      !!names_list$name_DIRd[3] := get(paste0(short_name_scale_str, "_", items_dimensions[[3]], "_DIR")),
      !!names_list$name_DIRd[4] := get(paste0(short_name_scale_str, "_", items_dimensions[[4]], "_DIR"))
    )
    
  # [END ADAPT 3/3]: ***********************************************************
  # ****************************************************************************
  
  
  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE, output_formats = output_formats)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
