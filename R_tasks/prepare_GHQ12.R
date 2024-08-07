##' Prepare GHQ12
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change:
##'   - Name of function: prepare_GHQ12 -> prepare_[value of short_name_scale_str]
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_GHQ12
##'
##' @param short_name_scale_str
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_GHQ12 <- function(DF_clean, short_name_scale_str, output_formats) {

  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(prepare_GHQ12)


  # [ADAPT]: Items to ignore, reverse and dimensions ---------------------------------------
  # ****************************************************************************

  items_to_ignore = c("00") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("00") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")

  items_dimensions = list(
    NameDimension1 = c("")
  )


  # [END ADAPT]: ***************************************************************
  # ****************************************************************************


  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str,
                                  dimensions = names(items_dimensions),
                                  help_names = FALSE) # help_names = FALSE once the script is ready

  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE, help_prepare = FALSE)


  # Create long DIR ------------------------------------------------------------

  DF_long_DIR =
    DF_long_RAW |>
   dplyr::select(id, trialid, RAW) |>


  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************

    # Transformations
    dplyr::mutate(
      DIR =
       dplyr::case_when(

          # REVISAR valores asignados ****
          RAW == "Mucho más que lo habitual" ~ 3,
          RAW == "Algo más que lo habitual" ~ 2,
          RAW == "No más que lo habitual" ~ 1,
          RAW == "No, en lo absoluto" ~ 0,

          RAW == "Más útil que lo habitual" ~ 0,
          RAW == "Igual que lo habitual" ~ 1,
          RAW == "menos útil que lo habitual" ~ 2, #### Eliminar cuando este corregido
          RAW == "Menos útil que lo habitual" ~ 2, #### TENDRIA QUE CORREGIRSE A "Menos útil que lo habitual"
          RAW == "Mucho menos útil que lo habitual" ~ 3,

          # ******************************

          RAW == "Mejor que lo habitual" ~ 0,
          RAW == "Igual que lo habitual" ~ 1,
          RAW == "Menos que lo habitual" ~ 2,
          RAW == "Mucho menos que lo habitual" ~ 3,

          RAW == "No, en absoluto" ~ 0,
          RAW == "No más que lo habitual" ~ 1,
          RAW == "Bastante más que lo habitual" ~ 2,
          RAW == "Mucho más" ~ 3,

          RAW == "Más que lo habitual" ~ 0,
          RAW == "Igual que lo habitual" ~ 1,
          RAW == "Menos útil que lo habitual" ~ 2,
          RAW == "Mucho menos" ~ 3,

          RAW == "Más capaz que lo habitual" ~ 0,
          RAW == "Igual que lo habitual" ~ 1,
          RAW == "Menos capaz que lo habitual" ~ 2,
          RAW == "Mucho menos" ~ 3,

          RAW == "Más feliz que lo habitual" ~ 0,
          RAW == "Aproximadamente lo mismo que lo habitual" ~ 1,
          RAW == "Menos feliz que lo habitual" ~ 2,
          RAW == "Mucho menos que lo habitual" ~ 3,

          is.na(RAW) ~ NA_real_,
          trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_real_,
          TRUE ~ 9999
        )
    ) |>

    # Invert items
    dplyr::mutate(
      DIR =
       dplyr::case_when(
          DIR == 9999 ~ DIR, # To keep the missing values unchanged
          trialid %in% paste0(short_name_scale_str, "_", items_to_reverse) ~ (6 - DIR),
          TRUE ~ DIR
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
  


  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)

  DF_wide_RAW_DIR =
    DF_wide_RAW |>
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
