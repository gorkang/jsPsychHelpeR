##' Prepare ERQ
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change:
##'   - Name of function: prepare_ERQ -> prepare_[value of short_name_scale_str]
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_ERQ
##'
##' @param short_name_scale_str
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_ERQ <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_ERQ)

  # Standardized names ------------------------------------------------------
  standardized_names(short_name_scale = short_name_scale_str,
                     dimensions = c("ReEvaluacionCognitiva", "SupresionEmocional"), # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready

  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = TRUE)

  # Show number of items, responses, etc. [uncomment to help prepare the test]
  # prepare_helper(DF_long_RAW, show_trialid_questiontext = TRUE)


  # Create long DIR ------------------------------------------------------------
  DF_long_DIR =
    DF_long_RAW %>%
    select(id, trialid, RAW) %>%


  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************

    mutate(
      DIR = RAW
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

      # Score Dimensions (use 3 digit item numbers)
      !!name_DIRd1 := rowSums(select(., matches("01|03|05|07|08|10") & matches("_DIR$")), na.rm = TRUE), 
      !!name_DIRd2 := rowSums(select(., matches("02|04|06|09") & matches("_DIR$")), na.rm = TRUE),

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
