testthat::test_that('Check if DF_raw', {
  
  # DEBUG
  # targets::tar_load(c(DF_raw))
  
  # Name of test (should reflect the name of the file) ----------------------
  
  name_of_test = "DF_raw"
  cat(crayon::underline(crayon::yellow(paste0("\n\nRunning: ", crayon::silver(name_of_test, paste(rep(" ", 40), collapse = " ")),"\n\n"))))
  
  # Test: trialids are correctly build  ----------------------------------------------------------------
  
  DF_problematic_trialids = 
    DF_raw %>%
    filter(!grepl("[a-zA-Z0-9]{1,100}_[0-9]{2}|^Instructions", trialid)) %>% 
    filter(trial_type != "fullscreen") %>% 
    distinct(trialid, experimento) %>% 
    drop_na(trialid) 
  
  offenders =
    DF_problematic_trialids %>% 
    distinct(trialid, .keep_all = TRUE) %>% 
    mutate(message = 
             case_when(
               is.na(trialid) ~ paste0(experimento, ": NA"),
               trialid == "" ~ paste0(experimento, ": empty"),
               TRUE ~ paste0(experimento, ": ", trialid))) %>% 
    pull(message)
  

  
  # Test: we have the canonical columns in DF_raw -------------------------------------------------------------------

    canonical_names_columns =  c("filename", "trial_type", "trial_index", "time_elapsed", "internal_node_id", "view_history", "rt", "trialid", "stimulus", "response", "id", "project", "experimento", "version", "datetime")
    #success
  
  
  
  # Warning and log ---------------------------------------------------------
  
  if (nrow(DF_problematic_trialids) > 0) {
    
    write_csv(DF_problematic_trialids, here::here(paste0("outputs/tests_outputs/test-", name_of_test, ".csv")))
    
    cat(crayon::red("\nERROR in", paste0("test-", name_of_test), "\n"),
        crayon::red("  - Some of the items have non-supported trialids:"), offenders, "\n",
        crayon::yellow("  - # of Issues: "), crayon::red(length(offenders)), "\n",
        crayon::green("  - trialid should be: "), crayon::black("SHORTNAMESCALE_DD or Instructions or Instructions_DD; e.g. CRT7_01, Instructions, Instructions_01"), "\n",
        crayon::silver("  - DF with details stored in:", paste0("'outputs/tests_outputs/test-", name_of_test, ".csv'"), "\n\n"))
    
  }
  
  # Actual expectation -------------------------------------------------------------
  
  # Test1
  testthat::expect_gt(nrow(DF_raw), 0) # Checks that we have some rows in the DF
  testthat::expect_length(offenders, 0)
  
  #Test2
  testthat::expect_true(all(names(DF_raw) %in% canonical_names_columns))
  # testthat::expect_equal(canonical_names_columns, names(DF_raw))

  
  
})
