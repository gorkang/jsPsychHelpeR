testthat::test_that('Check if DF_raw', {
  
  # DEBUG
  # targets::tar_load_globals()
  # targets::tar_load(c(DF_raw))
  
  # Name of test (should reflect the name of the file) ----------------------
  
  name_of_test = "DF_raw"
  # cat(cli::style_underline(cli::col_yellow(paste0("\n\nRunning: ", cli::col_silver(name_of_test, paste(rep(" ", 40), collapse = " ")),"\n\n"))))
  
  # Test: trialids are correctly build  ----------------------------------------------------------------
  
  DF_problematic_trialids = 
    DF_raw |>
    dplyr::filter(!grepl("[a-zA-Z0-9]{1,100}_[0-9]{2}|^Instructions|^Fullscreen", trialid)) |> 
    dplyr::filter(trial_type != "fullscreen") |> 
    dplyr::distinct(trialid, experiment) |> 
   tidyr::drop_na(trialid) 
  
  offenders =
    DF_problematic_trialids |> 
    dplyr::distinct(trialid, .keep_all = TRUE) |> 
    dplyr::mutate(message = 
            dplyr::case_when(
               is.na(trialid) ~ paste0(experiment, ": NA"),
               trialid == "" ~ paste0(experiment, ": empty"),
               TRUE ~ paste0(experiment, ": ", trialid))) |> 
    dplyr::pull(message)
  

  
  # Test: we have the canonical columns in DF_raw -------------------------------------------------------------------

    canonical_names_columns =  c("filename", "trial_type", "trial_index", "time_elapsed", "internal_node_id", "view_history", "rt", "trialid", "stimulus", "response", "id", "project", "experiment", "version", "datetime",
                                 "procedure", "success", "url", "question_order", "slider_start", "button_pressed", 
                                 "condition_within", "condition_between",
                                 "timeout", "failed_images", "failed_audio", "failed_video",
                                 "input")

    non_canonical_names = names(DF_raw)[!names(DF_raw) %in% canonical_names_columns]
  
  
  # Warning and log ---------------------------------------------------------
  
  if (nrow(DF_problematic_trialids) > 0) {
    
    data.table::fwrite(DF_problematic_trialids, here::here(paste0("outputs/tests_outputs/test-", name_of_test, ".csv")))
    
    
    cat(cli::col_red("\nERROR in", paste0("test-", name_of_test), "\n"),
        cli::col_red("  - Some of the items have non-supported trialids:"), offenders, "\n",
        cli::col_yellow("  - # of Issues: "), cli::col_red(length(offenders)), "\n",
        cli::col_green("  - trialid should be: "), cli::col_black("SHORTNAMESCALE_DD or Instructions or Instructions_DD; e.g. CRT7_01, Instructions, Instructions_01"), "\n",
        cli::col_silver("  - DF with details stored in:", paste0("'outputs/tests_outputs/test-", name_of_test, ".csv'"), "\n\n"))
  }
  
  if (length(non_canonical_names) > 0) {
    
    # data.table::fwrite(non_canonical_names, here::here(paste0("outputs/tests_outputs/test-", name_of_test, ".csv")))
    
    cat(cli::col_red("\nERROR in", paste0("test-", name_of_test), "\n"),
        cli::col_red("  - Some of the items have non-canonical names:"), non_canonical_names, "\n",
        cli::col_yellow("  - # of Issues: "), cli::col_red(length(non_canonical_names)), "\n"
        # cli::col_green("  - trialid should be: "), cli::col_black("SHORTNAMESCALE_DD or Instructions or Instructions_DD; e.g. CRT7_01, Instructions, Instructions_01"), "\n"
        # cli::col_silver("  - DF with details stored in:", paste0("'outputs/tests_outputs/test-", name_of_test, ".csv'"), "\n\n")
    )
    
  }
  
  
  # Actual expectation -------------------------------------------------------------
  
  # Test1
  testthat::expect_gt(nrow(DF_raw), 0) # Checks that we have some rows in the DF
  testthat::expect_length(offenders, 0)
  
  #Test2
  testthat::expect_true(all(names(DF_raw) %in% canonical_names_columns))
  # testthat::expect_equal(canonical_names_columns, names(DF_raw))

})
